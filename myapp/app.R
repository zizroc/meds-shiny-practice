# load packages ----
library(shiny)
library(palmerpenguins)
library(tidyverse)

# user interface ----
ui <- fluidPage(# app title ----
                tags$h1("Marcus' Palmer Penguin App"), 
                
                # app subtitle ----
                p(strong("Exploring Antarctic Punguins and Temperatures")), 
                
                # body mass slider input ----
                sliderInput(inputId = "body_mass", 
                            label = "Select a range of body masses (g)", 
                            min = 2700, max = 6300, value = c(3000, 4000)), 
                
                # body mass output ----
                plotOutput(outputId = "bodyMass_scatterPlot")
                )

# break ----

# server instructions ----
server <- function(input, output){
  
  # render scatterplot ----
  output$bodyMass_scatterPlot <- renderPlot({
    
    # filter body masses ----
    body_mass_df <- reactive({
      penguins %>% 
        filter(body_mass_g %in% input$body_mass[1]:input$body_mass[2])
    })
    
    # code to generate scatterplot here
    ggplot(na.omit(body_mass_df()), 
           aes(x = flipper_length_mm, y = bill_length_mm, 
               color = species, shape = species)) + 
      geom_point() + 
      scale_color_manual(values = c("red", "purple", "green")) + 
      labs(
        x = "Flipper length (mm)", 
        y = "Bill length (mm)"
      )
    
  })
  
}

# combine UI & server into an app ----
shinyApp(ui = ui, server = server)