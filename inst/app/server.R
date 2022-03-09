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
  error[["error"]] <- TRUE
  groups_ok <- reactiveValues()
  sheets <- reactiveValues()
  
  output[["group_dt"]] <- DT::renderDataTable({
    rv_df[["group_df"]] %>% 
      DT::datatable(editable = FALSE, 
                    options = list(paging = FALSE),
                    colnames = c("Sample name", "Group"))
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
    
    sheets[["sheets"]] <- readxl::excel_sheets(file[["datapath"]])
    
    
    sheet <- if(input[["sheet"]] == "") {
      1
    } else {
      input[["sheet"]]
    }
    
    dat <- readxl::read_excel(file[["datapath"]], 
                              sheet = sheet)
    
    
    if(!("Compound" %in% colnames(dat))) {
      error[["error"]] <- TRUE
      showNotification("ERROR! There is no column named 'Compound'! 
                         Please provide new data.",
                       duration = NULL,
                       type = "error",
                       closeButton = FALSE,
                       id = "compound")
      
    } else {
      error[["error"]] <- FALSE
      removeNotification(id = "compound")
    }
    dat
    
  })
  
  observe({
    updateSelectInput(session, 
                      "sheet", 
                      choices = sheets[["sheets"]])
  })
  
  
  output[["data_selected"]] <- renderTable({
    head(data_selected())
  })
  
  observe({
    dat <- data_selected() 
    if(!error[["error"]]) {
      updateSelectInput(session, "compound", 
                        choices = unique(dat[["Compound"]]))
    }
  })
  
  data_prepared_tmp <- reactive({
    if(!error[["error"]]) {
      group_vector <- setNames(rv_df[["group_df"]][["group"]], 
                               setdiff(colnames(data_selected()), 
                                       "Compound"))
      
      data_selected()  %>% 
        mutate(across(everything(), ~replace(., . ==  "NaN" , NA))) %>% 
        mutate_at(vars(-("Compound")), as.numeric) %>% 
        pivot_longer(cols = -Compound) %>% 
        mutate(group_label = group_vector[name])
    }
  })
  
  data_prepared <- reactive({
    switch(input[["transform"]],
           None = {
             data_prepared_tmp()
           },
           Logarithm = {
             data_prepared_tmp() %>% 
               mutate(value = log(value))
           },
           `Inverse hyperbolic sine` = {
             data_prepared_tmp() %>% 
               mutate(value = asinh(value))
           })
  })
  
  compounds <- reactive({
    unique(data_prepared()[["Compound"]])
  })
  
  
  
  observe({
    
    groups <- data_prepared()[["group_label"]]
    
    if(input[["tabs"]] == "Groups") {
      
      if(length(unique(groups)) == 1) {
        
        removeNotification(id = "valid")
        removeNotification(id = "paired_error")
        showNotification("For between group comparison you need to set at least two groups.",
                         duration = NULL,
                         type = "error",
                         closeButton = FALSE,
                         id = "g_error")
        groups_ok[["is_ok"]] <- FALSE
      } else {
        
        if(length(unique(table(groups))) != 1) {
          
          if(input[["paired"]]) {
            
            removeNotification(id = "valid")
            removeNotification(id = "g_error")
            showNotification("For pairwise comparison groups need to be equinumerous.",
                             duration = NULL,
                             type = "error",
                             closeButton = FALSE,
                             id = "paired_error")
            groups_ok[["is_ok"]] <- FALSE
          } else {
            
            groups_ok[["is_ok"]] <- TRUE
            
            removeNotification(id = "paired_error")
            removeNotification(id = "g_error")
            showNotification("The data is valid.",
                             duration = NULL,
                             type = "message",
                             closeButton = FALSE,
                             id = "valid")
          }
          
        } else {
          
          groups_ok[["is_ok"]] <- TRUE
          
          removeNotification(id = "paired_error")
          removeNotification(id = "g_error")
          showNotification("The data is valid.",
                           duration = NULL,
                           type = "message",
                           closeButton = FALSE,
                           id = "valid")
        }
      }
    } else {
      removeNotification(id = "paired_error")
      removeNotification(id = "g_error")
      removeNotification(id = "valid")
    }
    
    print(groups_ok[["is_ok"]])
  })
  
  
  #Analysis
  
  output[["plot_raw_data"]] <- renderPlot({
    if(input[["transform"]] != "None") {
      dat <- data_prepared_tmp() %>% 
        filter(Compound == input[["compound"]])
      
      ggplot(dat, aes(x = value, fill = group_label)) + 
        geom_histogram() +
        xlab("") +
        ggtitle("Data before transformation") +
        facet_wrap(~ group_label, ncol = 1)
    }
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
    if(groups_ok[["is_ok"]]) {
      plot_out()
    }
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
    
    if(groups_ok[["is_ok"]]) {
      
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
        mutate(adjusted_pval = p.adjust(pval, method = "BH"),
               transformation = input[["transform"]])
    } else {
      return("Invalid groups!")
    }
  })
  
  shapiro_out <- reactive({
    if(is.data.frame(shapiro_res())) {
      shapiro_res() %>% 
        filter(Compound == input[["compound"]]) %>% 
        select(-Compound)
    } else {
      
      shapiro_res()
    }
  })
  
  output[["shapiro"]] <- renderTable({
    shapiro_out()
  })
  
  comparison_tests <- reactive({
    
    if(groups_ok[["is_ok"]]) {
      dat <- data_prepared()
      
      cmp <- compounds()
      
      is_paired <- input[["paired"]]
      
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
        mutate(adjusted_pval = p.adjust(pval, method = "BH"),
               transformation = input[["transform"]])
    } else {
      
      return("Invalid groups!")
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