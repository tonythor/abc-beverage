library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)


github_url <- "https://github.com/tonythor/abc-beverage/raw/develop/data/"
train_fn <- "StudentData_Training.xlsx"
test_fn <- "StudentEvaluation_Test.xlsx"
train_url <- paste0(github_url, train_fn) 
test_url <- paste0(github_url, test_fn)
download.file(train_url, destfile = train_fn, mode = "wb")
download.file(test_url, destfile = test_fn, mode = "wb")

train <- read_excel(train_fn) %>%
  rename_with(~ gsub(" ", "", .x) %>% tolower()) %>%
  mutate(brandcodenumeric = as.numeric(as.factor(brandcode))) %>%
  select(-brandcode) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

ui <- fluidPage(
  fluidRow(
    column(6, offset = 3,  # Center the dropdown and plot in the middle
           selectInput("variable", "View a variable boxplot:", 
                       choices = names(train %>% select(where(is.numeric))),
                       selected = names(train %>% select(where(is.numeric)))[1]),  # Default selection
           plotOutput("boxPlot")
    )
  )
)

server <- function(input, output) {
  output$boxPlot <- renderPlot({
    ggplot(train, aes_string(x = factor(0), y = input$variable)) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", input$variable), y = input$variable, x = NULL) +
      theme_minimal()
  })
}


shinyApp(ui = ui, server = server)
