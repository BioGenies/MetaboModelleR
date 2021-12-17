library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      tabPanel("Data", 
               fileInput("data", "Choose .xlsx file:",
                         multiple = FALSE,
                         accept = c(".xlsx")),
               h3("Selected:"),
               tableOutput("data_selected")
      ),
      tabPanel("Analysis", 
               column(4,
                      selectInput("compound", "Select compound:", 
                                  choices = NULL),
                      h3("Normality"),
                      "Shapiro-Wilk Normality Test:",
                      tableOutput("shapiro"),
                      h3("Between groups comprison"),
                      "T-test:",
                      tableOutput("ttest"),
                      "Wilcoxon signed-rank test:",
                      tableOutput("wilcoxon")
               ),
               column(8,
                      plotOutput("dist_plot"),
                      downloadButton("download_png", "Download png")
               )
      ),
      tabPanel("Download pdf",
               downloadButton("download_one_compound", "Download analysis for selected compound."),
               downloadButton("download_all", "Download all"))
    )
  )
)

server <- function(input, output, session) {
  
  data_selected <- reactive({
    file <- input[["data"]]
    ext <- tools::file_ext(file[["datapath"]])
    
    req(file)
    validate(need(ext == "xlsx", "Please upload a xlsx file"))
    
    read_excel(file[["datapath"]])
  })
  
  output[["data_selected"]] <- renderTable({
    head(data_selected())
  })
  
  compound <- reactive({
    input[["compound"]]
  })
  
  data_one_cmp <- reactive({
    raw_dat <- data_selected()
    pivot_longer(data = raw_dat, cols = -Compound) %>% 
      mutate(group_label = substr(name, 0, 2)) %>% 
      filter(Compound == compound())
  })
  

  observe({
    dat <- data_selected() 
    
    updateSelectInput(session, "compound", 
                      choices = unique(dat[["Compound"]]))
  })
  
  plot_out <- reactive({
    dat <- data_one_cmp()
    group_label <- unique(dat[["group_label"]])
    cmp <- unique(dat[["Compound"]])
    
    hist <- ggplot(dat, aes(x = value, fill = group_label)) + 
      geom_histogram() +
      xlab("") +
      ggtitle("Histogram")
    
    qqplot <- ggplot(dat, aes(sample = value, color = group_label)) + 
      stat_qq() + 
      stat_qq_line() +
      ggtitle("Quantile-quantile chart")
    
    boxplot <- ggplot(dat, aes(x="", y = value, fill = group_label)) +
      geom_boxplot() +
      ggtitle("Boxplot")
    
    (hist + boxplot + qqplot)* facet_wrap(~ group_label, ncol = 1)* theme(legend.position = "bottom") + plot_annotation(paste0("Compound ", cmp))
  })
  
  
  output[["dist_plot"]] <- renderPlot({
    plot_out()
  })
  
  
  output[["download_png"]] <- downloadHandler(filename = paste0(compound(), "_plot.png"),
                                              content = function(file){
                                                ggsave(file, plot_out(), device = "png", height = 300,
                                                       width = 400, units = "mm")})
  
  shapiro_res <- reactive({
    dat <- data_one_cmp()
    
    group_label <- unique(dat[["group_label"]])
    
    lapply(group_label, function(ith_group) {
      filter(dat, group_label == ith_group) %>%
        pull(value) %>%
        shapiro.test() %>%
        getElement("p.value") %>%
        data.frame(group_label = ith_group, pval = .)
    }) %>%
      bind_rows() %>%
      mutate(adjusted_pval = p.adjust(pval, method = "BH"))
  })
  
  output[["shapiro"]] <- renderTable({
    shapiro_res()
  })
  
  ttest_res <- reactive({
    dat <- data_one_cmp()
    
    group_label <- unique(dat[["group_label"]])
    
    dat %>% 
      t.test(value ~ group_label, data = .) %>% 
      getElement("p.value") %>% 
      data.frame(pval = .) %>% 
      mutate(adjusted_pval = p.adjust(pval, method = "BH"))
  })
  
  output[["ttest"]] <- renderTable({
    ttest_res()
  })
  
  wilcoxon_res <- reactive({
    dat <- data_one_cmp()
    
    group_label <- unique(dat[["group_label"]])
    
    dat %>% 
      wilcox.test(value ~ group_label, data = .) %>% 
      getElement("p.value") %>% 
      data.frame(pval = .) %>% 
      mutate(adjusted_pval = p.adjust(pval, method = "BH"))
  })
  
  output[["wilcoxon"]] <- renderTable({
    wilcoxon_res()
  })
  
  
  output[["download_all"]] <- downloadHandler(
    filename = "report_all.pdf",
    content = function(file) {

      tempReport <- file.path(tempdir(), "report_all.Rmd")
      file.copy("report_all.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(data = data_selected())

      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output[["download_one_compound"]] <- downloadHandler(
    filename = paste0(compound(), "_report.pdf"),
    content = function(file) {
      
      tempReport <- file.path(tempdir(), "report_one.Rmd")
      file.copy("report_one.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data = data_one_cmp(),
                     plot = plot_out(),
                     shapiro = shapiro_res(),
                     ttest = ttest_res(),
                     wilcoxon = wilcoxon_res())
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
}

shinyApp(ui, server)

