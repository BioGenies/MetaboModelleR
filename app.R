library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(DT)
library(shinycssloaders)

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
      tabPanel("Groups", 
               br(),
               column(5, 
                      h3("You can change group name here:"),
                      h5("1. Select rows from the table."),
                      textInput("change_group_txt", 
                                label = "2. Type the group name here"),
                      actionButton(inputId = "change_group_btn", 
                                   label = "3. Press the button to change the group name"),
                      br(),
                      br(),
                      h5("Check if groups are paired:"),
                      checkboxInput("paired", "Paired", FALSE)),
               column(7, 
                      DT::dataTableOutput("group_dt"),
                      verbatimTextOutput("debug"))
               
      ),
      tabPanel("Analysis", 
               column(6,
                      selectInput("compound", "Select compound:", 
                                  choices = NULL),
                      h3("Normality"),
                      "Shapiro-Wilk Normality Test:",
                      withSpinner(tableOutput("shapiro")),
                      h3("Between groups comprison"),
                      withSpinner(tableOutput("tests"))
               ),
               column(6,
                      withSpinner(plotOutput("dist_plot")),
                      downloadButton("download_png", "Download png")
               )
      ),
      tabPanel("Download report",
               h3("You can download a PDF file with the analysis."),
               br(),
               h5("For one compound:"),
               downloadButton("download_one_compound", 
                              "Download analysis for selected compound."),
               br(),
               br(),
               h5("For all compounds from the file (It can take even a few minutes):"),
               downloadButton("download_all", "Download all"))
    )
  )
)

server <- function(input, output, session) {
  
  rv_df <- reactiveValues()
  
  output[["group_dt"]] <- DT::renderDataTable({
    rv_df[["group_df"]] %>% 
      datatable(editable = FALSE, options = list(paging = FALSE))
  })
  
  observeEvent(data_selected(), {
    rv_df[["group_df"]] <- data.frame(sample_name = setdiff(colnames(data_selected()), "Compound")) %>% 
      mutate(group = "A")
  })
  
  observeEvent(input[["change_group_btn"]], {
    rv_df[["group_df"]][input[["group_dt_rows_selected"]], "group"] <- input[["change_group_txt"]]
  })
  
  output[["debug"]] <- renderPrint({
    input[["group_dt_rows_selected"]]
  })
  
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
  
  observe({
    dat <- data_selected() 
    
    updateSelectInput(session, "compound", 
                      choices = unique(dat[["Compound"]]))
  })
  
  
  #Analysis
  
  data_prepared <- reactive({
    raw_dat <- data_selected()
    pivot_longer(data = raw_dat, cols = -Compound) %>% 
      group_by(Compound) %>% 
      mutate(group_label = rv_df[["group_df"]][["group"]])
  })
  
  
  
  
  plot_out <- reactive({
    dat <- data_prepared() %>% 
      filter(Compound == input[["compound"]])
    
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
    
    boxplot <- ggplot(dat, aes(x = "", y = value, fill = group_label)) +
      geom_boxplot() +
      ggtitle("Boxplot")
    
    (hist + boxplot + qqplot)* facet_wrap(~ group_label, ncol = 1)* theme(legend.position = "bottom") + plot_annotation(paste0("Compound ", cmp))
  })
  
  
  output[["dist_plot"]] <- renderPlot({
    plot_out()
  })
  
  
  output[["download_png"]] <- downloadHandler(filename = paste0(input[["compound"]], 
                                                                "_plot.png"),
                                              content = function(file){
                                                ggsave(file, 
                                                       plot_out(), 
                                                       device = "png", 
                                                       height = 300,
                                                       width = 400, 
                                                       units = "mm")})
  
  shapiro_res <- reactive({
    
    dat <- data_prepared()
    
    group_label <- unique(dat[["group_label"]])
    compound <- unique(unique(dat[["Compound"]]))
    
    lapply(group_label, function(ith_group) {
      lapply(compound, function(ith_compound) {
        dat %>% 
          filter(group_label == ith_group, Compound == ith_compound) %>%
          pull(value) %>%
          shapiro.test() %>%
          getElement("p.value") %>%
          data.frame(group_label = ith_group, 
                     Compound = ith_compound, 
                     pval = .)
      }) %>% bind_rows()
    }) %>%
      bind_rows() %>%
      group_by(group_label) %>% 
      mutate(adjusted_pval = p.adjust(pval, method = "BH"))
  })
  
  shapiro_out <- reactive({
    shapiro_res() %>% 
      filter(Compound == input[["compound"]]) %>% 
      select(-Compound)
  })
  
  output[["shapiro"]] <- renderTable({
    shapiro_out()
  })
  
  comparison_tests <- reactive({
    dat <- data_prepared()
    
    cmp <- unique(unique(dat[["Compound"]]))
    
    is_paired <- input[["paired"]]
    
    if(length(unique(dat[["group_label"]])) == 1) {
      "For between group comparison you need to set at least two groups."
      
    } else {
      if(is_paired & (length(unique(table(dat[["group_label"]]))) != 1)) {
        
        return("For pairwise comparison groups need to be equinumerous.")
        
      } else {
        lapply(cmp, function(ith_compound) {
          rbind(dat %>% 
                  filter(Compound == ith_compound) %>%
                  t.test(value ~ group_label, 
                         data = .,
                         paired = is_paired) %>% 
                  getElement("p.value") %>% 
                  data.frame(test = "T-test",
                             Compound = ith_compound, 
                             pval = .),
                dat %>% 
                  filter(Compound == ith_compound) %>%
                  wilcox.test(value ~ group_label, 
                              data = .,
                              paired = is_paired) %>% 
                  getElement("p.value") %>% 
                  data.frame(test = "Wilcoxon signed-rank test",
                             Compound = ith_compound, 
                             pval = .))
        }) %>% 
          bind_rows() %>%
          group_by(test) %>% 
          mutate(adjusted_pval = p.adjust(pval, method = "BH"))
      }
    }
  })
  
  comparison_out <- reactive({
    
    if(is.data.frame(comparison_tests())) {
      
      comparison_tests() %>% 
        filter(Compound == input[["compound"]]) %>% 
        mutate(paired = input[["paired"]]) %>% 
        select(-Compound)
    } else {
      
      comparison_tests()
    }
  })
  
  
  output[["tests"]] <- renderTable({
    comparison_out()
  })
  
  
  output[["download_all"]] <- downloadHandler(
    filename = "report_all.pdf",
    content = function(file) {
      
      tempReport <- file.path(tempdir(), "report_all.Rmd")
      file.copy("report_all.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data = data_prepared(),
                     shapiro = shapiro_res(),
                     tests = comparison_tests())
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output[["download_one_compound"]] <- downloadHandler(
    filename = paste0(input[["compound"]], "_report.pdf"),
    content = function(file) {
      
      tempReport <- file.path(tempdir(), "report_one.Rmd")
      file.copy("report_one.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data = filter(data_prepared(), 
                                   Compound == input[["compound"]]),
                     plot = plot_out(),
                     shapiro = shapiro_out(),
                     tests = comparison_out())
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
}

shinyApp(ui, server)

