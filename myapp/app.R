# load packages ----
library(shiny)
library(palmerpenguins)
library(tidyverse)
library(DT)

# user interface ----
ui <- fluidPage(
  # app title ----
  tags$h1("Marcus' Palmer Penguin App"),
  
  # app subtitle ----
  p(strong(
    "Exploring Antarctic Punguins and Temperatures"
  )),
  
  # body mass slider input ----
  sliderInput(
    inputId = "body_mass",
    label = "Select a range of body masses (g)",
    min = 2700,
    max = 6300,
    value = c(3000, 4000)
  ),
  
  # body mass output ----
  shiny::plotOutput(outputId = "bodyMass_scatterPlot"),
  
  # body mass table output ----
  DT::dataTableOutput(outputId = "bodyMass_dataTable")
  
)

# break ----

# server instructions ----
server <- function(input, output) {
  # render scatterplot ----
  output$bodyMass_scatterPlot <- renderPlot({
    # filter body masses ----
    body_mass_df <- reactive({
      penguins %>%
        filter(body_mass_g %in% input$body_mass[1]:input$body_mass[2])
    })
    
    # code to generate scatterplot here
    ggplot(
      na.omit(body_mass_df()),
      aes(
        x = flipper_length_mm,
        y = bill_length_mm,
        color = species,
        shape = species
      )
    ) +
      geom_point() +
      scale_color_manual(values = c(
        "Adelie" = "#FEA346",
        "Chinstrap" = "#B251F1",
        "Gentoo" = "#4BA4A4"
      )) +
      scale_shape_manual(values = c(
        "Adelie" = 19,
        "Chinstrap" = 17,
        "Gentoo" = 15
      )) +
      labs(x = "Flipper length (mm)",
           y = "Bill length (mm)")
  })
  
  output$bodyMass_dataTable <- DT::renderDataTable(DT::datatable(
    penguins,
    caption = "hello",
    options = list(pageLength = 5)
  ))
  
}

# combine UI & server into an app ----
shinyApp(ui = ui, server = server)