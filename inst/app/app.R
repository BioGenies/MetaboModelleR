library(shiny)
library(metaboR)
library(shinythemes)
library(readxl)
library(data.table)
library(DT)
library(shinycssloaders)
library(shinyWidgets)

source("supplementary_shiny.R")


ui <- navbarPage(theme = shinytheme("flatly"),
                 id = "navbar_id",
                 title = "MetaboModelleR - Biocrates",
                 tabPanel("About",
                          column(9,
                                 h3("MetaboModelleR: efficient processing of GC-MS and LC-MS data"),
                                 "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris bibendum fermentum nisl, et mollis felis hendrerit sed. Aenean consequat lectus quis leo porta sagittis. Phasellus sed justo gravida nunc dictum posuere. Fusce diam dolor, consectetur ut molestie non, malesuada vitae dui. Etiam mi erat, laoreet ac efficitur tincidunt, pharetra a justo. Donec semper commodo mauris, vitae feugiat metus sagittis sit amet. Aliquam a justo nec nibh semper porttitor at id ex.",
                                 h3("Lorem Ipsum"),
                                 "Nam interdum ligula nibh, nec efficitur nunc pellentesque sit amet. Maecenas elit neque, iaculis vitae nibh et, laoreet gravida libero. Nam vehicula quis dui vel bibendum. Cras vitae vehicula arcu, eget feugiat eros. Praesent sit amet ligula imperdiet, consectetur nisi at, interdum mauris. Etiam maximus metus in neque pellentesque, sit amet viverra turpis egestas. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Aliquam erat volutpat.",
                                 h3("Acknowledgements"),
                                 "This project was supported by The Excellence Initiative - Research 
           University programme by Medical University of Białystok and Ministry 
           of Education and Science."
                          ),
                          column(3, align = "center",
                                 br(),
                                 img(src='logo_umb.jpg', align = "center", 
                                     height = '300px', width = '300'),
                          )
                 ),
                 tabPanel("Load data",
                          column(6, 
                                 align = "center",
                                 style = "background-color:#C3D2D5;",
                                 h3("Biocrates data"),
                                 fileInput("biocrates_path", "Choose .xlsx file:",
                                           multiple = FALSE,
                                           accept = c(".xlsx"))
                          ),
                          column(6, 
                                 align = "center",
                                 style = "background-color:#C3D2D5;",
                                 h3("Clinical data"),
                                 fileInput("clinical_path", "Choose .xlsx file:",
                                           multiple = FALSE,
                                           accept = c(".xlsx"))
                          ),
                          column(12, 
                                 align = "center",
                                 style = "background-color:#C3D2D5;padding:10px;margin-bottom:10px;",
                                 h4("Select files and click Load files!"),
                                 actionButton("read_data_btn", "Load files", 
                                              width = "300px", 
                                              icon = icon("file-lines")),
                                 br()
                          ),
                          column(10,
                                 tabsetPanel(
                                   tabPanel("Biocrates data",
                                            shinycssloaders::withSpinner(
                                              DT::dataTableOutput("biocrates_data")
                                            )
                                   ),
                                   tabPanel("Clinical data",
                                            shinycssloaders::withSpinner(
                                              DT::dataTableOutput("clinical_data")
                                            )
                                   )
                                 )
                          ),
                 ),
                 navbarMenu("< LOD",
                            tabPanel("Remove sparse metabolites",
                                     column(3, 
                                            offset = 1,
                                            align = "center",
                                            style = "background-color:#C3D2D5;padding:10px;margin-bottom:10px;",
                                            numericInput("LOD_thresh", 
                                                         "Select maximum ratio of <LOD allowed for each metabolite and click Remove button!",
                                                         value = 0.3,
                                                         min = 0,
                                                         max = 1,
                                                         step = 0.01),
                                            fluidRow(actionButton("remove_btn", 
                                                                  label = "Remove!"),
                                                     actionButton("undo_btn", 
                                                                  label = "Undo!"))
                                            ,
                                            br(),
                                            br(),
                                            htmlOutput("sparse_to_remove")
                                     ),
                                     column(5, 
                                            offset = 1, 
                                            shinycssloaders::withSpinner(
                                              DT::dataTableOutput("LOD_data")
                                            )
                                     )
                            ),
                            tabPanel("Complete < LOD",
                                     column(4, 
                                            style = "background-color:#C3D2D5;padding:10px;margin-bottom:10px;",
                                            h4("Complete LOD values based on the table below."),
                                            fluidRow(
                                              align = "center",
                                              actionButton("complete_btn", 
                                                           label = "Complete!"),
                                              actionButton("complete_undo_btn", 
                                                           label = "Undo!"),
                                            ),
                                            
                                            br(),
                                            shinycssloaders::withSpinner(
                                              DT::dataTableOutput("LOD_table")
                                            )
                                     ),
                                     column(6, offset = 1, 
                                            shinycssloaders::withSpinner(
                                              DT::dataTableOutput("CV_data")
                                            )
                                     )
                            )
                 ),
                 tabPanel("Quality control",
                          column(3, 
                                 style = "background-color:#C3D2D5;padding:10px;margin-bottom:10px;",
                                 numericInput("CV_thresh",
                                              "Provide threshold (%) for CV values (QC Level):",
                                              value = 30,
                                              min = 0,
                                              max = 100,
                                              step = 5),
                                 h4("The following metabolites will be removed."),
                                 h4("Select the ones which should remain."),
                                 column(6,
                                        checkboxGroupButtons(
                                          inputId = "select_metabolites",
                                          choices = c(""),
                                          direction = "vertical"
                                        ),
                                 ),
                                 column(6,
                                        actionButton("remove_CV_btn",
                                                     label = "Remove!"),
                                        actionButton("undo_CV_btn",
                                                     label = "Undo!")
                                 )
                          ),
                          column(5, offset = 1,
                                 h3("Table of coefficient variation:"),
                                 shinycssloaders::withSpinner(
                                   DT::dataTableOutput("QC_table")
                                 )
                          )
                 )
                 
)

server <- function(input, output, session) {
  
  dat <- reactiveValues()
  
  ##### loading data
  observeEvent(input[["read_data_btn"]], {
    dat <- read_data_app(input, dat)
    dat[["removed_LOD"]] <- dat[["biocrates_data"]]
  })
  
  output[["clinical_data"]] <-  DT::renderDataTable({
    req(dat[["clinical_data"]])
    clinical_data <- dat[["clinical_data"]]
    cols1 <- colnames(clinical_data)[sapply(clinical_data, is.numeric)]
    custom_datatable(clinical_data[, (cols1) := round(.SD, 2), .SDcols = cols1],
                     scrollY = 300)
  })
  
  output[["biocrates_data"]] <-  DT::renderDataTable({
    req(dat[["biocrates_data"]])
    custom_datatable(dat[["biocrates_data"]],
                     scrollY = 300)
  })
  
  # removing LOD 
  observeEvent(input[["remove_btn"]], {
    dat[["removed_LOD"]] <- remove_sparse_metabolites(dat[["biocrates_data"]], 
                                                      LOD_threshold = input[["LOD_thresh"]])
  })
  
  
  observeEvent(input[["undo_btn"]], {
    dat[["removed_LOD"]] <- dat[["biocrates_data"]]
  })
  
  
  data_sparse_metabolites <- reactive({
    req(dat[["removed_LOD"]])
    unique(melt(dat[["removed_LOD"]],
                id.vars = c("Plate Bar Code", "Sample_ID", "Sample Type"),
                variable.name = "Compound",
                value.name = "Value")[ 
                  , -c("Plate Bar Code", "Sample_ID", "Sample Type")
                ][, `% < LOD` := round(mean(Value == "< LOD"), 3), by = Compound][
                  , -c("Value")
                ])
  })
  
  output[["LOD_data"]] <- DT::renderDataTable({
    custom_datatable(data_sparse_metabolites(), 
                     scrollY = 600,
                     paging = FALSE)
  })
  
  # output[["LOD_data"]] <- DT::renderDataTable({
  #   sparse_dat <- data_sparse_metabolites()
  #   to_remove <- sparse_dat[`% < LOD` > input[["LOD_thresh"]], Compound]
  #   formatStyle(
  #     custom_datatable(sparse_dat, 
  #                      scrollY = 600,
  #                      paging = FALSE), 
  #     'Compound',
  #     target = "row",
  #     backgroundColor = styleEqual(to_remove, c("darksalmon"))
  #   )
  # })
  
  output[["sparse_to_remove"]] <- renderUI({
    metabolites_sparcity <- data_sparse_metabolites()
    HTML(paste("The following metabolites will be removed: <br/> <br/>",
               paste(metabolites_sparcity[`% < LOD` > input[["LOD_thresh"]], Compound],
                     collapse = ", ")))
  })
  
  
  # completed LOD table
  observeEvent(input[["complete_btn"]], {
    dat[["CV_data"]] <- handle_LOD(dat[["removed_LOD"]])
    dat[["CV_table"]] <- attr(dat[["CV_data"]], "CV_table")
    dat[["to_remove"]] <- as.vector(dat[["CV_table"]][`QC Level 2` >= input[["CV_thresh"]], Compound])
    
    updateCheckboxGroupButtons(session, inputId = "select_metabolites", 
                               choices = dat[["to_remove"]],
                               checkIcon = list(
                                 yes = tags$i(class = "fa fa-check-square", 
                                              style = "color: white"),
                                 no = tags$i(class = "fa fa-square-o", 
                                             style = "color: white")))
  })
  
  observeEvent(input[["complete_undo_btn"]], {
    dat[["CV_data"]] <- melt(dat[["biocrates_data"]],
                             id.vars = c("Plate Bar Code", "Sample_ID", "Sample Type"),
                             variable.name = "Compound",
                             value.name = "Value")[, -c("Sample Type")]
  })
  
  output[["LOD_table"]] <- DT::renderDataTable({
    req(dat[["biocrates_data"]])
    
    LOD_table <- attr(dat[["biocrates_data"]], "LOD_table")
    custom_datatable(LOD_table, 
                     scrollY = 550,
                     paging = FALSE)
  })
  
  output[["CV_data"]] <- DT::renderDataTable({
    req(dat[["CV_data"]])
    custom_datatable(dat[["CV_data"]][, -c("Sample Type", "Plate Bar Code")], 
                     scrollY = 600)
  })
  
  # remove CV
  
  output[["QC_table"]] <- DT::renderDataTable({
    req(dat[["CV_table"]])
    
    formatStyle(
      custom_datatable(dat[["CV_table"]][, lapply(.SD, round, 3), Compound], 
                       scrollY = 550,
                       paging = FALSE),
      'Compound',
      target = "row",
      backgroundColor = styleEqual(dat[["to_remove"]], 
                                   c("#f7b2ad"))
    )
  })
  
  observeEvent(input[["CV_thresh"]] & input[["undo_CV_btn"]] & input[["navbar_id"]] == "Quality control", {
    req(dat[["CV_data"]])
    req(input[["CV_thresh"]])
    
    to_remove <- as.vector(dat[["CV_table"]][`QC Level 2` >= input[["CV_thresh"]], Compound])
    dat[["to_remove"]] <- to_remove
    selected <- input[["select_metabolites"]]
    
    updateCheckboxGroupButtons(session, inputId = "select_metabolites", 
                               choices = to_remove,
                               selected = selected,
                               checkIcon = list(
                                 yes = tags$i(class = "fa fa-check-square", 
                                              style = "color: white"),
                                 no = tags$i(class = "fa fa-square-o", 
                                             style = "color: white")))
  })
  
  observeEvent(input[["remove_CV_btn"]], {
    req(dat[["CV_table"]])
    req(dat[["CV_data"]])
    req(dat[["to_remove"]])
    
    to_remove <- setdiff(dat[["to_remove"]], input[["select_metabolites"]])
    dat[["processed_data"]] <- remove_high_CV(dat[["CV_data"]], to_remove)
    dat[["CV_table"]] <- dat[["CV_table"]][!(Compound %in% to_remove)]
    
    updateCheckboxGroupButtons(session, inputId = "select_metabolites", 
                               choices = setdiff(dat[["to_remove"]], to_remove),
                               selected = input[["select_metabolites"]],
                               checkIcon = list(
                                 yes = tags$i(class = "fa fa-check-square", 
                                              style = "color: white"),
                                 no = tags$i(class = "fa fa-square-o", 
                                             style = "color: white")))
  })
  
  observeEvent(input[["undo_CV_btn"]], {
    req(dat[["CV_data"]])
    dat[["CV_table"]] <- attr(dat[["CV_data"]], "CV_table")
    dat[["processed_data"]] <- dat[["CV_data"]]
    
    updateCheckboxGroupButtons(session, inputId = "select_metabolites", 
                               choices = dat[["to_remove"]],
                               selected = input[["select_metabolites"]],
                               checkIcon = list(
                                 yes = tags$i(class = "fa fa-check-square", 
                                              style = "color: white"),
                                 no = tags$i(class = "fa fa-square-o", 
                                             style = "color: white")
                               )
    )
  })
}


shinyApp(ui, server)