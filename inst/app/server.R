library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(DT)
library(shinycssloaders)
source("ui.R")

server <- function(input, output, session) {
  
  rv_df <- reactiveValues()
  error <- reactiveValues()
  
  output[["group_dt"]] <- DT::renderDataTable({
    rv_df[["group_df"]] %>% 
      DT::datatable(editable = FALSE, options = list(paging = FALSE))
  })
  
  observeEvent(data_selected(), {
    rv_df[["group_df"]] <- 
      data.frame(sample_name = setdiff(colnames(data_selected()), 
                                       "Compound")) %>% 
      mutate(group = "A")
  })
  
  observeEvent(input[["change_group_btn"]], {
    rv_df[["group_df"]][input[["group_dt_rows_selected"]], "group"] <- 
      input[["change_group_txt"]]
  })
  
  output[["debug"]] <- renderPrint({
    input[["group_dt_rows_selected"]]
  })
  
  data_selected <- reactive({
    file <- input[["data"]]
    ext <- tools::file_ext(file[["datapath"]])
    
    req(file)
    validate(need(ext == "xlsx", "Please upload a xlsx file"))
    
    dat <- readxl::read_excel(file[["datapath"]], sheet = 1)
    
    if(!("Compound" %in% colnames(dat))) {
      error[["error"]] <- "ERROR! There is no column named 'Compound'! 
      Please provide new data."
    } 
    dat
    
  })
  
  
  output[["error_compound"]] <- renderText({
    error[["error"]]
  })
  
  
  output[["data_selected"]] <- renderTable({
    head(data_selected())
  })
  
  observe({
    dat <- data_selected() 
    
    updateSelectInput(session, "compound", 
                      choices = unique(dat[["Compound"]]))
  })
  
  compounds <- reactive({
    unique(data_prepared()[["Compound"]])
  })
  
  
  #Analysis
  
  data_prepared <- reactive({
    group_vector <- setNames(rv_df[["group_df"]][["group"]], 
                             setdiff(colnames(data_selected()), 
                                     "Compound"))
    data_selected() %>% 
      pivot_longer(cols = -Compound) %>% 
      mutate(group_label = group_vector[name]) %>% 
      mutate(value = as.numeric(value))
  })
  
  
  plot_out <- reactive({
    dat <- data_prepared() %>% 
      filter(Compound == input[["compound"]])
    
    group_label <- unique(dat[["group_label"]])
    
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
    
    (hist + boxplot + qqplot)* facet_wrap(~ group_label, ncol = 1)* 
      theme(legend.position = "bottom") + 
      plot_annotation(paste0("Compound ", input[["compound"]]))
  })
  
  
  output[["dist_plot"]] <- renderPlot({
    plot_out()
  })
  
  
  cmp_name <- reactive({
    input[["compound"]]
  })
  
  output[["download_png"]] <- 
    downloadHandler(filename = function() paste0(cmp_name(), "_plot.png"),
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
    compound <- compounds()
    
    lapply(group_label, function(ith_group) {
      lapply(compound, function(ith_compound) {
        tmp_dat <- dat %>% 
          filter(group_label == ith_group, Compound == ith_compound) %>%
          pull(value)
        
        tryCatch({
          shapiro.test(tmp_dat) %>%
            getElement("p.value") %>%
            data.frame(group_label = ith_group, 
                       Compound = ith_compound, 
                       pval = .)
        }, error = function(cond) {
          return(data.frame(group_label = ith_group, 
                            Compound = ith_compound, 
                            pval = NA))
        })
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
    
    cmp <- compounds()
    
    is_paired <- input[["paired"]]
    
    if(length(unique(dat[["group_label"]])) == 1) {
      "For between group comparison you need to set at least two groups."
      
    } else {
      if(is_paired & (length(unique(table(dat[["group_label"]]))) != 1)) {
        
        return("For pairwise comparison groups need to be equinumerous.")
        
      } else {
        lapply(cmp, function(ith_compound) {
          
          tmp_dat <- dat %>% 
            filter(Compound == ith_compound)
          
          t_test_one_res <- tryCatch({
            t.test(value ~ group_label, 
                   data = tmp_dat,
                   paired = is_paired) %>% 
              getElement("p.value") %>% 
              data.frame(test = "T-test",
                         Compound = ith_compound, 
                         pval = .)
          }, error = function(cond) {
            return(data.frame(test = "T-test",
                              Compound = ith_compound, 
                              pval = NA))
          })
          
          wilcoxon_one_res <- tryCatch({
            wilcox.test(value ~ group_label, 
                        data = tmp_dat,
                        paired = is_paired) %>% 
              getElement("p.value") %>% 
              data.frame(test = "Wilcoxon signed-rank test",
                         Compound = ith_compound, 
                         pval = .)
          }, error = function(cond) {
            return(data.frame(test = "Wilcoxon signed-rank test",
                              Compound = ith_compound, 
                              pval = NA))
          })
          
          rbind(t_test_one_res, wilcoxon_one_res)
          
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
  
  output[["shapiro_table"]] <- DT::renderDataTable({
    datatable(data = shapiro_res(),
              class = "table-bordered table-condensed",
              extensions = "Buttons",
              options = list(pageLength = 5,
                             dom = "tBip",
                             autoWidth = TRUE,
                             buttons = c("excel", "csv")),
              filter = "bottom",
              rownames = FALSE)
  }, server = FALSE)
  
  output[["tests_table"]] <- DT::renderDataTable({
    datatable(data = comparison_tests(),
              class = "table-bordered table-condensed",
              extensions = "Buttons",
              options = list(pageLength = 5,
                             dom = "tBip",
                             autoWidth = TRUE,
                             buttons = c("excel", "csv")),
              filter = "bottom",
              rownames = FALSE)
  }, server = FALSE)
  
}