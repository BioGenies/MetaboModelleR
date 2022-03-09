

ui <- fluidPage(
  mainPanel(
    tabsetPanel(id = "tabs",
                tabPanel("Data", 
                         fluidRow(fileInput("data", "Choose .xlsx file:",
                                            multiple = FALSE,
                                            accept = c(".xlsx")),
                                  selectInput("sheet", 
                                              "Select sheet:", 
                                              choices = NULL,
                                              selected = NULL)),
                         
                         h3("Selected:"),
                         verbatimTextOutput("error_compound"),
                         tableOutput("data_selected")
                ),
                tabPanel("Groups", 
                         tags$head(tags$style(HTML(".shiny-notification {
                                         position:fixed;
                                         top: calc(38%);
                                         left: calc(1%);
                                         }
                                         "))),
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
                                checkboxInput("paired", "Paired", FALSE),
                                br(),
                                br(),
                                br(),
                                br(),
                                radioButtons(inputId = "transform",
                                             label = "Choose transformation:",
                                             choices = c("None", 
                                                         "Logarithm", 
                                                         "Inverse hyperbolic sine"))
                         ),
                         column(7, 
                                DT::dataTableOutput("group_dt"))
                         
                ),
                tabPanel("Analysis", 
                         fluidRow(
                           column(12, selectInput("compound", 
                                                  "Select compound:", 
                                                  choices = NULL)),
                           column(6,
                                  h3("Normality"),
                                  "Shapiro-Wilk Normality Test:",
                                  shinycssloaders::withSpinner(tableOutput("shapiro"))
                           ),
                           column(6,
                                  h3("Between groups comparison"),
                                  shinycssloaders::withSpinner(tableOutput("tests"))
                           )
                         ),
                         fluidRow(
                           shinycssloaders::withSpinner(plotOutput("dist_plot")),
                         ),
                         fluidRow(
                           br(),
                           downloadButton("download_png", "Download png")
                         ),
                         br(),
                         br(),
                         fluidRow(
                           shinycssloaders::withSpinner(plotOutput("plot_raw_data"))
                         ),
                         br(),
                         br(),
                ),
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
                         DT::dataTableOutput("shapiro_table"),
                         br(),
                         br(),
                         h4("Between groups comparison"),
                         DT::dataTableOutput("tests_table")
                         
                         
                )
    )
  )
)
