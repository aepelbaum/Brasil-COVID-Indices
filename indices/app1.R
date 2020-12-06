#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Carrega as libraries

library(tidyverse)
library(sf)
library(rgdal)
library(rgeos)
# https://cran.r-project.org/web/packages/geobr/vignettes/intro_to_geobr.html
library(geobr)
library(ggthemes)


#Le o arquivo de UFs com seus índices
df_estados_indice <- read_csv2("data/uf_nivel.csv", col_names = TRUE,
                               locale = locale(encoding = "ISO-8859-1"), col_types = NULL)

df_estados <- read_state(year=2018) %>% 
    rename("UF" = name_state) %>% 
    mutate(UF=str_replace_all(UF, " De "," de ")) %>% 
    mutate(UF=str_replace_all(UF, " Do "," do "))


df_brasil_indice <- df_estados %>% 
    left_join(df_estados_indice, by = "UF")

# Define UI for application that draws a map
ui <- fluidPage(
    
    # Application title
    titlePanel("Níveis de infecção por COVID-19 no Brasil"),
    
    sidebarLayout(
        sidebarPanel(
            # checkboxInput("alto", label = "Alto", value = FALSE, width = NULL),
            # checkboxInput("baixo", label = "Baixo", value = FALSE, width = NULL),
            # checkboxInput("estavel", label = "Estável", value = FALSE, width = NULL),
            checkboxGroupInput(inputId = "niveis",
                              label = "Níveis de infecção:",
                              choices = c("Alto" = "alto", "Baixo" = "baixo", "Estável" = "estavel"),
                              selected = "alto")
    ),
        
    #https://github.com/gpilgrim2670/SwimMap/blob/master/app.R
        
        mainPanel(
            plotOutput("drawMap")
        )
    )
)

# Define server logic required to draw a map
server <- function(input, output) {
    
    output$drawMap <- renderPlot({
        
          fill_color_alto <- "white"
          fill_color_baixo <- "white"
          fill_color_estavel <- "white"
        
            if(input$niveis %in% c("alto")) {
              fill_color_alto <- "red"
              fill_color_baixo <- "white"
              fill_color_estavel <- "white"
          }  
          if(input$niveis %in% c("baixo")) {
              fill_color_alto <- "white"
              fill_color_baixo <- "green"
              fill_color_estavel <- "white"
          }
          if(input$niveis %in% c("estavel")) {
              fill_color_alto <- "white"
              fill_color_baixo <- "white"
              fill_color_estavel <- "yellow"
          }  
        
          df_brasil_indice %>%
            ggplot() + 
            geom_sf(size=.15, show.legend = FALSE) +
            geom_sf(fill = fill_color_alto, data = df_brasil_indice %>% filter(Nivel %in% input$alto)) +
            geom_sf(fill = fill_color_baixo, data = df_brasil_indice %>% filter(Nivel %in% input$baixo)) +
            geom_sf(fill = fill_color_estavel, data = df_brasil_indice %>% filter(Nivel %in% input$estavel)) +
            scale_fill_manual() +
            theme_minimal() +
            coord_sf(datum = NA)

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
