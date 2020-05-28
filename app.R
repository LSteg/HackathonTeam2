### notes
## Can do:Put in table (not the updated one yet), and filter it based on datatype, platform, project, cohort, sex and age
## Can not do yet: Mouse stuff, sorting dataframe by columns based on stuff clicked at in pop up window 
#(popup is working but just not functional yet). Also, Red button is not implemented since it actually is nice if df updates 
#with every updated filter selection

library(shiny)
library(DT)
library(shinyalert)
library(shinyBS)
library(dplyr)

# Define UI ----

MyDat <- read.csv("~/Hackathon/ExampleData.csv")
Human <- MyDat[MyDat$Species == "Human",]

ui <- fluidPage(
  titlePanel(h1("Complex Disease Epigenetics Group Database", align = "center")),
  tabsetPanel(
    tabPanel("Human Data",
      sidebarLayout(
        sidebarPanel(
          helpText(h2("Filter  database")),
          selectizeInput("hDatatype", label =  "Data Type", sort(unique(df$DataType)), selected = NULL, multiple = TRUE,
                         options = NULL),
          selectizeInput("hPlatform", label =  "Platform", sort(unique(df$Platform)), selected = NULL, multiple = TRUE,
                         options = NULL),
          selectizeInput("hProject", label =  "Project", sort(unique(df$Project)), selected = NULL, multiple = TRUE,
                         options = NULL),
          selectizeInput("hCohort", label =  "Cohort", sort(unique(df$HumanCohort)), selected = NULL, multiple = TRUE,
                         options = NULL),
          checkboxGroupInput("hSex", label = "Sex", 
                             choices = sort(unique(df$Sex)),
                             selected = sort(unique(df$Sex))),
          sliderInput("hAge", label = ("Age"), min = min(na.omit(df$Human_AgeYears)), 
                      max = max(na.omit(df$Human_AgeYears)), value = c(min(na.omit(df$Human_AgeYears)), 
                                                                       max(na.omit(df$Human_AgeYears)))),
          #actionButton("hButton", "Click here, Jon!", icon("paper-plane"), 
          #             style="color: #fff; background-color: #ff0000; border-color: #ff0000")
        ),
        mainPanel(
          actionButton("hColumnButton", "Column Selection"),
          bsModal("Modal","Columns to be display","hColumnButton",checkboxGroupInput("hColumn", label = "Columns", choices = colnames(df), selected = colnames(df))),
          DT::dataTableOutput("data")       

        )
      )
    ),
    tabPanel("Mouse Data", h3("Test"))
  )
)

# Define server logic ----
server <- function(input, output) {
  df <- reactive({
    req(input$hSex)
    req(input$hAge)
    #The filter is a little messy with some if statements, otherwise the dataframe would be empty until you 
    #choose proper options in all filters, which is annoying. Also, have to update the dataframe with every filter, 
    #thats why I always use Human <- xxxx
    Human <- filter(Human, Sex %in% input$hSex & 
             between(Human_AgeYears, input$hAge[1], input$hAge[2]))
    if (length(input$hDatatype > 0)){Human <- filter(Human,DataType %in% input$hDatatype)}
    else {Human}
    if (length(input$hPlatform > 0)){Human <- filter(Human,Platform %in% input$hPlatform)}
    else {Human}
    if (length(input$hProject > 0)){Human <- filter(Human,Project %in% input$hProject)}
    else {Human}
    if (length(input$hCohort > 0)){Human <- filter(Human,HumanCohort %in% input$hCohort)}
    else {Human}
  })
  
  output$data <- DT::renderDataTable({
    DT::datatable(df(), options = list(scrollX = TRUE))
  })

  { "example second tab" }
}


# Run the app ----
shinyApp(ui = ui, server = server)
