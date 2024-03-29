library(MetaboModelleR)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(DT)
library(shinycssloaders)
library(impute)


ui <- navbarPage(id = "tabs",
                 title = "MetaboModelleR",
                 tabPanel("About",
                          includeMarkdown("MetaboModelleR.md")),
                 tabPanel("Data", 
                          tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
                          tags$style(".shiny-notification {
                                         position:fixed;
                                         top: calc(10%);
                                         left: calc(30%);
                                         }
                                         "),
                          fileInput("data", "Choose .xlsx file:",
                                    multiple = FALSE,
                                    accept = c(".xlsx")),
                          selectInput("sheet", 
                                      "Select sheet:", 
                                      choices = NULL,
                                      selected = NULL),
                          selectInput(inputId = "imputation",
                                      label = "Choose imputation method:",
                                      choices = c("zeros",
                                                  "median",
                                                  "1/2 minimum",
                                                  "kNN"),
                                      selected = "kNN"),
                          h3("Selected:"),
                          verbatimTextOutput("error_compound"),
                          tableOutput("data_selected")
                 ),
                 
                 ###############################################################
                 
                 tabPanel("Groups", 
                          tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #C1E1CE !important;}')),
                          column(4, 
                                 h3("You can change group name here:"),
                                 br(),
                                 h5("1. Select rows from the table."),
                                 textInput("change_group_txt", 
                                           label = "2. Type the group name here"),
                                 actionButton(inputId = "change_group_btn", 
                                              label = "3. Press the button to change the group name"),
                                 br(),
                                 br(),
                                 h5("Check if groups are paired:"),
                                 checkboxInput("paired", "Paired", FALSE),
                                 br(),
                                 h3("Set another parameters:"),
                                 checkboxGroupInput("tests", 
                                                    "Choose tests to perform",
                                                    choices = c("Shapiro–Wilk test",
                                                                "Student's t-test",
                                                                "Mann–Whitney U-test"),
                                                    selected = c("Shapiro–Wilk test",
                                                                 "Student's t-test",
                                                                 "Mann–Whitney U-test")),
                          ),
                          column(4, 
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 DT::dataTableOutput("group_dt")),
                          column(4)
                          
                 ),
                 
                 ###############################################################
                 
                 tabPanel("Distribution", 
                          h3("Here, you can apply some transformations to the data."),
                          column(4,
                                 DT::dataTableOutput("transformation_df")),
                          column(8,
                                 radioButtons(inputId = "transform",
                                              label = "Choose transformation:",
                                              choices = c("None",
                                                          "Logarithm",
                                                          "Square-Root",
                                                          "Reciprocal",
                                                          "Inverse hyperbolic sine")),
                                 actionButton(inputId = "apply_transformation", 
                                              label = "Apply!"),
                                 plotOutput("dist_plot")),
                          
                          

                          
                    

                          # column(12, 
                          #        h3("Normality"),
                          #        "Shapiro-Wilk Normality Test:",
                          #        tableOutput("shapiro")
                          # ),
                          # br(),
                          # downloadButton("download_png", "Download png"),
                          # br(),
                          # br(),
                          # plotOutput("plot_raw_data"),
                          
                 ),
                 
                 ###############################################################
                 
                 tabPanel("Analysis", 
                          tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "progress.css")),
                          column(12, selectInput("compound", 
                                                 "Select compound:", 
                                                 choices = NULL)),
                          column(12, 
                                 h3("Data statistics"),
                                 tableOutput("stat")
                          ),
                          h3("Between groups comparison"),
                          tableOutput("tests"),
                          br(),
                          br(),
                          br(),
                          br(),
                 ),
                 
                 ###############################################################
                 
                 tabPanel("Summary",
                          selectInput("group_case", 
                                      "Select case group:", 
                                      choices = NULL,
                                      selected = NULL),
                          numericInput("sig_level",
                                       label = "Provide significance level from (0, 1) interval",
                                       value = 0.05,
                                       min = 1e-100,
                                       max = 1,
                                       step = 0.001),
                          checkboxInput("adjusted", "Use adjusted p-value", FALSE),
                          shinycssloaders::withSpinner(uiOutput("plot_test_t_ui"), 
                                                       color = "#479F8D"),
                          downloadButton("download_png_t", "Download png"),
                          br(),
                          br(),
                          shinycssloaders::withSpinner(uiOutput("plot_test_u_ui"), 
                                                       color = "#479F8D"),
                          downloadButton("download_png_u", "Download png"),
                          br(),
                          br()
                 ),
                 
                 ###############################################################
                 
                 tabPanel("Download report",
                          h3("You can download a PDF file with the analysis"),
                          h5("For one compound:"),
                          downloadButton("download_one_compound", 
                                         "Download analysis for selected compound"),
                          br(),
                          br(),
                          h5("For all compounds from the file (It can take even a few minutes):"),
                          downloadButton("download_all", "Download analysis for all compounds"),
                          br(),
                          br(),
                          h3("or you can download a table with the results for all compounds"),
                          h4("Normality"),
                          shinycssloaders::withSpinner(DT::dataTableOutput("shapiro_table"), 
                                                       color = "#479F8D"),
                          br(),
                          br(),
                          h4("Between groups comparison"),
                          shinycssloaders::withSpinner(DT::dataTableOutput("tests_table"), 
                                                       color = "#479F8D"),
                          h4("Imputed data"),
                          shinycssloaders::withSpinner(DT::dataTableOutput("data_imputed_table"), 
                                                       color = "#479F8D")
                          
                          
                 )
)
