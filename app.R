# App zur Auswertung des ADP-IV Fragebogens

# https://www.meduniwien.ac.at/hp/psychoanalyse/forschung/diagnostik-downloads/diagnostik-von-persoenlichkeitsstoerungen/

# (C) Nicolas Rost, 2021



# Packages ----

library(shiny)
library("semantic.dashboard")


item_nums <- c(1:94)



# 1. UI ----



## 1.1 Sidebar ----

# select icons from https://semantic-ui.com/elements/icon.html

sidebar <- dashboardSidebar(
  
  side = "left",

  sidebarMenu(
    menuItem("Item Eingabe", tabName = "item_values", 
             icon = icon("comment")),
    menuItem("Kategoriale Diagnostik", tabName = "kat_diag", 
             icon = icon("calculator")),
    menuItem("Dimensionale Diagnostik", tabName = "dim_diag", 
             icon = icon("calculator")),
    menuItem("Erläuterungen", tabName = "erlaeuterungen", 
             icon = icon("th")),
    menuItem("Kontakt", tabName = "kontakt", 
             icon = icon("info circle"))
    )
)



## 1.2 Body ----

body <- dashboardBody(
  
  tabItems(
    
    
    ### 1.2.1 Item Eingabe ----
    
    tabItem(tabName = "item_values",
            
            fluidRow(
              
              box(
                title = "Items",
                color = "blue",
                width = 16,
              
                h1("Items"),
                p("Bitte geben Sie hier die entsprechenenden Item-Werte aus dem Fragebogen ein."),
              
                tabPanel("Items", uiOutput("items"))

              )
            
            )
            
    ),
    

    
    ### 1.2.2 Kategoriale Diagnostik
    tabItem(tabName = "kat_diag"),
    
    
    ### 1.2.3 Dimensionale Diagnostik
    tabItem(tabName = "dim_diag"),

    
    ### 1.2.4 Erläuterungen ----
    tabItem(tabName = "erlaeuterungen"),
    
    
    ### 1.2.5 Kontakt ----
    tabItem(tabName = "kontakt")
    
  )
  
)
            
      
      
## 1.3 Defining UI ----

ui <- dashboardPage(
  
  dashboardHeader(
    h1("ADP-IV Auswertung", style = "color:black", align = "center"),
    color = "blue",
    disable = FALSE
  ),
  sidebar,
  body
)





# 2. Server ----

server <- function(input, output) {
  
  values <- list()
  for (i in item_nums){
   values[[i]] <- box(
     title = paste0("Item ", i),
     sliderInput(paste0("item_", i),
                 paste0("Item ", i),
                 min = 1,
                 max = 7,
                 value = 1,
                 width = "70%"),
     conditionalPanel(condition = paste0("input.item_", i, " >= 5"),
                      sliderInput(paste0("add_item_", i),
                                  paste0("Zusatz Item ", i),
                                  min = 1, 
                                  max = 3,
                                  value = 1,
                                  width = "30%"))
                 )

  }
  output$items <- renderUI(values)

}





# 3. Run App ----
shinyApp(ui = ui, server = server)
