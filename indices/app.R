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
            radioButtons("nivel", label = "Selecione",
                         choices = list("Alto" = "alto",
                                        "Baixo" = "baixo",
                                        "Estável" = "estavel"), 
                         selected = "alto")
        ),
        
        
        mainPanel(
            plotOutput("drawMap")
        )
    )
)

# Define server logic required to draw a map
server <- function(input, output) {
    
    output$drawMap <- renderPlot({
        fill_var <-  input$nivel
        if(input$nivel == "alto") {
            fill_label <-  "Alto"
            my_colors <-  (c(nivel = "red"))
        }  
        if(input$nivel == "baixo") {
            fill_label <-  "Baixo"
            my_colors <-  (c(nivel = "green"))
        }
        if(input$nivel == "estavel") {
            fill_label <-  "Estável"
            my_colors <-  (c(nivel = "yellow"))
        }  
        df_brasil_indice %>%
            #geom_sf(aes_string(fill = fill_var)) +
            #scale_fill_manual(name = "Nível", values = my_colors) +
            #theme_map() + 
            #labs(x = NULL, 
            #     y = NULL,
            #     title = "Níveis de infecção por COVID-19 no Brasil")

            #ggplot() +
            #geom_sf(aes_string(fill = fill_var), color= "black", size=.15) +
            #scale_fill_continuous(type = "gradient", low = "white", high = high_color, name = fill_label) +
            #coord_sf(datum = NA) +
            #theme(legend.position = "bottom", legend.direction = "vertical")
        
        ggplot() +
            geom_sf(aes_string()) +
            scale_fill_manual(name = "Nível", values = my_colors) +
            theme_map() + 
            labs(x = NULL, 
                 y = NULL)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
