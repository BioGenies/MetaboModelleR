library(shiny)
library(metaboR)
library(shinythemes)
library(readxl)
library(data.table)
library(DT)
library(shinycssloaders)

source("supplementary_shiny.R")


ui <- navbarPage(theme = shinytheme("flatly"),
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
                                 actionButton("read_data_btn", "Load files", 
                                              width = "300px", 
                                              icon = icon("file-lines")),
                                 br()),
                          column(6, 
                                 shinycssloaders::withSpinner(
                                   DT::dataTableOutput("biocrates_data")
                                 )
                          ),
                          column(6, 
                                 shinycssloaders::withSpinner(
                                   DT::dataTableOutput("clinical_data")
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
                                                         "Select maximum ratio of <LOD allowed for each metabolite and click `remove` button!",
                                                         value = 0.3,
                                                         min = 0,
                                                         max = 1,
                                                         step = 0.01),
                                            actionButton("remove_btn", 
                                                         label = "Remove!")
                                     ),
                                     column(5, 
                                            offset = 1, 
                                            shinycssloaders::withSpinner(
                                              DT::dataTableOutput("LOD_data")
                                            )
                                     )
                            ),
                            tabPanel("Complete < LOD",
                                     column(3, 
                                            offset = 1,
                                            style = "background-color:#C3D2D5;padding:10px;margin-bottom:10px;",
                                            h4("Complete LOD values based on the table below."),
                                            actionButton("complete_btn", 
                                                         label = "Complete!"),
                                            br(),
                                            shinycssloaders::withSpinner(
                                              DT::dataTableOutput("LOD_table")
                                            )
                                     ),
                                     column(8, 
                                            shinycssloaders::withSpinner(
                                              DT::dataTableOutput("CV_data")
                                            )
                                     )
                            )
                 )
                 
)

server <- function(input, output, session) {
  
  dat <- reactiveValues()
  
  ##### loading data
  observeEvent(input[["read_data_btn"]], {
    dat <- read_data_app(input, dat)
  })
  
  output[["clinical_data"]] <-  DT::renderDataTable({
    req(dat[["clinical_data"]])
    clinical_data <- dat[["clinical_data"]]
    cols1 <- colnames(clinical_data)[sapply(clinical_data, is.numeric)]
    custom_datatable(clinical_data[, (cols1) := round(.SD, 2), .SDcols = cols1])
  })
  
  output[["biocrates_data"]] <-  DT::renderDataTable({
    req(dat[["biocrates_data"]])
    custom_datatable(dat[["biocrates_data"]])
  })
  
  # removing LOD 
  observeEvent(input[["remove_btn"]], {
    dat[["removed_LOD"]] <- remove_sparse_metabolites(dat[["biocrates_data"]], 
                                                      LOD_threshold = input[["LOD_thresh"]])
    dat[["CV_data"]] <- dat[["removed_LOD"]]
  })
  
  
  output[["LOD_data"]] <- DT::renderDataTable({
    req(dat[["removed_LOD"]])
    LOD_display <- unique(melt(dat[["removed_LOD"]],
                               id.vars = c("Plate Bar Code", "Sample_ID", "Sample Type"),
                               variable.name = "Compound",
                               value.name = "Value")[ 
                                 , -c("Plate Bar Code", "Sample_ID", "Sample Type")
                               ][, `% < LOD` := round(mean(Value == "< LOD"), 3), by = Compound][
                                 , -c("Value")
                               ])
    custom_datatable(LOD_display, 
                     scrollY = 600,
                     paging = FALSE)
  })
  
  # completed LOD table
  observeEvent(input[["complete_btn"]], {
    dat[["CV_data"]] <- handle_LOD(dat[["removed_LOD"]])
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
    
    custom_datatable(dat[["CV_data"]], 
                     scrollY = 550)
  })
  
}


shinyApp(ui, server)