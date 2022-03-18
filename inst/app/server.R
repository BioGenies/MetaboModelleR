library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(DT)
library(shinycssloaders)
source("ui.R")

server <- function(input, output, session) {
  
  rv_df <- reactiveValues()
  error <- reactiveValues()
  error[["error"]] <- TRUE
  groups_ok <- reactiveValues()
  sheets <- reactiveValues()
  
  #Data 
  
  data_selected <- reactive({
    file <- input[["data"]]
    ext <- tools::file_ext(file[["datapath"]])
    
    req(file)
    validate(need(ext == "xlsx", "Please upload a xlsx file"))
    
    sheets[["sheets"]] <- readxl::excel_sheets(file[["datapath"]])
    
    
    sheet <- if(!(input[["sheet"]] %in% sheets[["sheets"]])) {
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
  
  # Groups
  
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
      mutate(group = "Control")
  })
  
  observeEvent(input[["change_group_btn"]], {
    rv_df[["group_df"]][input[["group_dt_rows_selected"]], "group"] <- 
      input[["change_group_txt"]]
  })
  
  
  data_prepared_tmp <- reactive({
    if(!error[["error"]]) {
      group_vector <- setNames(rv_df[["group_df"]][["group"]], 
                               setdiff(colnames(data_selected()), 
                                       "Compound"))
      
      data_selected()  %>%
        mutate(Compound = as.factor(Compound)) %>% 
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
    
    ((hist + qqplot) * facet_wrap(~ group_label, ncol = 1, scales = "free") + boxplot)* 
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
      
      if("Shapiro–Wilk test" %in% input[["tests"]]) {
        
        lapply(group_label, function(ith_group) {
          prediction_percentage <- 0
          withProgress(message = paste0("Shapiro-Wilk (group ", ith_group, "):"), style = "old", value = 0, {
            shapiro_res_raw <- lapply(compound, function(ith_compound) {
              prediction_percentage <<- prediction_percentage + 1/length(compound)*100
              incProgress(1/length(compound), detail = paste0(round(prediction_percentage, 0), 
                                                             "% compounds"))
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
            })
          })
          bind_rows(shapiro_res_raw)
        }) %>%
          bind_rows() %>%
          group_by(group_label) %>% 
          mutate(adjusted_pval = p.adjust(pval, method = "BH"),
                 transformation = input[["transform"]])
        
      } else {
        return("Not performed")
      }
      
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
      
      results <- lapply(cmp, function(ith_compound) {
        
        groups <- unique(setdiff(data_prepared()[["group_label"]], "Control"))
        
        lapply(groups, function(gr){
          
          tmp_dat <- dat %>% 
            filter(Compound == ith_compound,
                   group_label %in% c("Control", gr)) 
          
          avg_res <- tmp_dat %>% 
            group_by(group_label) %>% 
            mutate(avg = mean(value, na.rm = TRUE)) %>% 
            select(avg, group_label) %>% 
            unique()
          
          AV_control <- avg_res %>% 
            filter(group_label == "Control") %>% 
            pull(avg)
          AV_case <- avg_res %>% 
            filter(group_label == gr) %>% 
            pull(avg)
          p_change <- 100 * (AV_case - AV_control) / AV_control
          FC <- AV_case / AV_control
          
          part_res <- data.frame(Compound = ith_compound,
                                 Case_name = gr,
                                 AV_control = AV_control,
                                 AV_case = AV_case,
                                 p_change = p_change,
                                 FC = FC)
          # T test
          
          if("Student's t-test" %in% input[["tests"]]) {
            t_test_one_res <- tryCatch({
              t.test(value ~ group_label, 
                     data = tmp_dat,
                     paired = is_paired) %>% {
                       data.frame(test_T_pval = .[["p.value"]])
                     } 
              
            }, error = function(cond) {
              
              data.frame(test_T_pval = NA)
            })
            
            part_res <- cbind(part_res, t_test_one_res)
          }
          
          # Wilcoxon Mann test
          
          if("Mann–Whitney U-test" %in% input[["tests"]]) {
            
            wilcoxon_one_res <- tryCatch({
              wilcox.test(value ~ group_label, 
                          data = tmp_dat,
                          paired = is_paired) %>% {
                            data.frame(test_U_pval = .[["p.value"]])
                          }
              
            }, error = function(cond) {
              
              data.frame(test_U_pval = NA)
            })
            
            part_res <- cbind(part_res, wilcoxon_one_res)
            
          }
          
          part_res
          
        }) %>% 
          bind_rows()
        
      }) %>% 
        bind_rows()
      
      
      if("Student's t-test" %in% input[["tests"]]) {
        results <- results %>% 
          mutate(test_T_adj_pval = p.adjust(test_T_pval, method = "BH"))
      }
      
      if("Mann–Whitney U-test" %in% input[["tests"]]) {
        results <- results %>% 
          mutate(test_U_adj_pval = p.adjust(test_U_pval, method = "BH"))
      }
      
      if(all(c("Mann–Whitney U-test", "Student's t-test")  %in% input[["tests"]])) {
        results <- results %>% 
          relocate(test_U_pval, .after = test_T_adj_pval)
      }
      results
      
      
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
  
  # Summary
  
  observe({
    if(input[["tabs"]] == "Analysis") {
      updateSelectInput(session,
                        "group_case",
                        choices = setdiff(data_prepared()[["group_label"]], "Control"),
                        selected = setdiff(data_prepared()[["group_label"]], "Control")[1])
    }
  })
  
  
  
  significant_t <- reactive({
    
    if(!input[["adjusted"]]) {
      tmp_dat <- comparison_tests() %>% 
        select(Compound, Case_name, AV_control, AV_case, test_T_pval) %>% 
        rename(p_val = test_T_pval)
    } else {
      tmp_dat <- comparison_tests() %>% 
        select(Compound, Case_name, AV_control, AV_case, test_T_adj_pval) %>% 
        rename(p_val = test_T_adj_pval)
    }
    
    dat_box <- data_prepared() %>% 
      group_by(group_label, Compound) %>% 
      mutate(lo = boxplot.stats(value)[["conf"]][1],
             hi = boxplot.stats(value)[["conf"]][2]) %>% 
      filter(group_label %in% c("Control", input[["group_case"]])) %>% 
      select(Compound, group_label, lo, hi) %>% 
      unique() %>% 
      mutate(group_label = ifelse(group_label == "Control", "AV_control", "AV_case")) %>% 
      rename(name = "group_label")
    
    tryCatch({
      p <- tmp_dat %>% 
        filter(Case_name == input[["group_case"]],
               p_val < input[["sig_level"]]) %>% 
        select(-p_val, -Case_name)   %>% 
        pivot_longer(cols = -Compound) %>% 
        merge(dat_box, by = c("Compound", "name")) %>% 
        ggplot(aes(x = value, y = Compound, color = name)) +
        geom_point() +
        ggtitle(paste0("Significant differences AV Control vs. AV ", input[["group_case"]], " by Student's t-test.")) +
        scale_y_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),
                                                       width = 40)) +
        coord_flip() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        geom_errorbar(aes(xmin = lo, xmax = hi, color = name, width = 0.1))
      print(p)
    }, error = function(cond) {
      return(ggplot())
    })
  })
  
  
  
  output[["plot_test_t"]] <- renderPlot({ 
    significant_t()
  })
  
  output[["download_png_t"]] <- 
    downloadHandler(filename = "significant_t_test.png",
                    content = function(file){
                      ggsave(file, 
                             significant_t(), 
                             device = "png", 
                             height = 300,
                             width = 700, 
                             units = "mm")})
  
  significant_u <- reactive({
    if(!input[["adjusted"]]) {
      tmp_dat <- comparison_tests() %>% 
        select(Compound, Case_name, AV_control, AV_case, test_U_pval) %>% 
        rename(p_val = test_U_pval)
    } else {
      tmp_dat <- comparison_tests() %>% 
        select(Compound, Case_name, AV_control, AV_case, test_U_adj_pval) %>% 
        rename(p_val = test_U_adj_pval)
    }
    
    dat_box <- data_prepared() %>% 
      group_by(group_label, Compound) %>% 
      mutate(lo = boxplot.stats(value)[["conf"]][1],
             hi = boxplot.stats(value)[["conf"]][2]) %>% 
      filter(group_label %in% c("Control", input[["group_case"]])) %>% 
      select(Compound, group_label, lo, hi) %>% 
      unique() %>% 
      mutate(group_label = ifelse(group_label == "Control", "AV_control", "AV_case")) %>% 
      rename(name = "group_label")
    
    tryCatch({
      p <- tmp_dat %>% 
        filter(Case_name == input[["group_case"]],
               p_val < input[["sig_level"]]) %>% 
        select(-p_val, -Case_name)   %>% 
        pivot_longer(cols = -Compound) %>% 
        merge(dat_box, by = c("Compound", "name")) %>% 
        ggplot(aes(x = value, y = Compound, color = name)) +
        geom_point() +
        ggtitle(paste0("Significant differences AV Control vs. AV ", input[["group_case"]], " by Mann–Whitney U-test.")) +
        scale_y_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),
                                                       width = 40)) +
        coord_flip() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        geom_errorbar(aes(xmin = lo, xmax = hi, color = name, width = 0.1))
      
      print(p)
    }, error = function(cond) {
      return(ggplot())
    })
    
    
  })
  
  output[["plot_test_u"]] <- renderPlot({ 
    significant_u()
  })
  
  output[["download_png_u"]] <- 
    downloadHandler(filename = "significant_u_test.png",
                    content = function(file){
                      ggsave(file, 
                             significant_u(), 
                             device = "png", 
                             height = 300,
                             width = 700, 
                             units = "mm")})
  
  
  
  # Reports
  
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
                             buttons = list( 
                               list(extend = 'excel', filename = "shapiro_results"),
                               list(extend = 'csv', filename =  "shapiro_results")
                             )
              ),
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
                             buttons = list( 
                               list(extend = 'excel', filename = "comparison_results"),
                               list(extend = 'csv', filename =  "comparison_results")
                             )
              ),
              filter = "bottom",
              rownames = FALSE)
  }, server = FALSE)
  
}