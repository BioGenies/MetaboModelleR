
library(shiny)
library(metaboR)
library(shinythemes)
library(readxl)
library(data.table)
library(DT)
library(shinycssloaders)

source("supplementary_shiny.R")


custom_datatable <- function(dat) {
  DT::datatable(dat,
                editable = FALSE, 
                options = list(paging = FALSE, 
                               scrollX = TRUE,
                               scrollY = 650,
                               autoWidth = TRUE))
}



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
                                           accept = c(".xlsx")),
                                 h5("The data should:"),
                                 HTML("- Pellentesque lobortis ante ut cons <br/>
                                 - Sed luctus, orci nec rutrum tempor <br/>
                                 -  In varius erat sed finibus mollis <br/>
                                 - Nunc ornare, neque at feugiat pharetra <br/>")
                          ),
                          column(6, 
                                 align = "center",
                                 style = "background-color:#C3D2D5;",
                                 h3("Clinical data"),
                                 fileInput("clinical_path", "Choose .xlsx file:",
                                           multiple = FALSE,
                                           accept = c(".xlsx")),
                                 h5("The data should:"),
                                 HTML("- Pellentesque lobortis ante ut cons <br/>
                                 - Sed luctus, orci nec rutrum tempor <br/>
                                 -  In varius erat sed finibus mollis <br/>
                                 - Nunc ornare, neque at feugiat pharetra <br/>"),
                          ),
                          column(12, 
                                 align = "center",
                                 style = "background-color:#C3D2D5;padding:10px;margin-bottom:10px",
                                 actionButton("read_data", "Load files", 
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
                 tabPanel("LOD",
                 )
                 
)

server <- function(input, output, session) {
  
  dat <- reactiveValues()
  
  observeEvent(input[["read_data"]], {
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
  
}


shinyApp(ui, server)