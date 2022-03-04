

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
                      DT::dataTableOutput("group_dt"))
               
      ),
      tabPanel("Analysis", 
               column(6,
                      selectInput("compound", "Select compound:", 
                                  choices = NULL),
                      h3("Normality"),
                      "Shapiro-Wilk Normality Test:",
                      shinycssloaders::withSpinner(tableOutput("shapiro")),
                      h3("Between groups comparison"),
                      shinycssloaders::withSpinner(tableOutput("tests"))
               ),
               column(6,
                      shinycssloaders::withSpinner(plotOutput("dist_plot")),
                      downloadButton("download_png", "Download png")
               )
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
