#ui.R for Roth vs. Traditional

library(shiny)


# Define UI for application that takes inputs and draws plot
shinyUI(fluidPage(
  
  inputPanel(
    fixedRow(
      column(width = 3.6, offset = 0.0,
             numericInput("income", label = h5("Adjusted Gross Income:"), 
                          value = 50000)),
      
      width = 3.6, offset = 0.0,
             selectInput("status", 
                         label = h5("Status:"),
                         choices = list("single","joint"),
                         selected = "single"),
      column(width = 3.6, offset = 0.0,
             selectInput("state", 
                         label = h5("State:"),
                         choices = list("Alabama","Alaska","Arizona",
                                        "Arkansas",
                                        "California","Colorado",
                                        "Connecticut","D.C.",
                                        "Delaware","Florida",
                                        "Georgia","Hawaii","Idaho",
                                        "Illinois","Indiana","Iowa",
                                        "Kansas","Kentucky","Louisiana",
                                        "Maine","Maryland","Massachusetts",
                                        "Michigan","Minnesota","Mississippi",
                                        "Missouri","Montana","Nebraska",
                                        "Nevada",
                                        "New Hampshire","New Jersey",
                                        "New Mexico",
                                        "New York","North Carolian", "North Dakota",
                                        "Ohio","Oklahoma","Oregon",
                                        "Pennsylvania","Rhode Island",
                                        "South Carolina","South Dakota",
                                        "Tennessee","Texas","Utah","Vermont",
                                        "Virginia","West Virginia",
                                        "Wisconsin"),
                         selected = "Alabama")),
      uiOutput("localityPanel")
    )
  ),
  mainPanel(textOutput("text1")
  )
))