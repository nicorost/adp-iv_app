# App zur Auswertung des ADP-IV Fragebogens

# https://www.meduniwien.ac.at/hp/psychoanalyse/forschung/diagnostik-downloads/diagnostik-von-persoenlichkeitsstoerungen/

# (C) Nicolas Rost, 2021



# Packages ----

library("shiny")
library("semantic.dashboard")


source("utils.R")


item_nums <- c(1:94)


color <- "blue"





# 1. UI ----



## 1.1 Sidebar ----

# select icons from https://semantic-ui.com/elements/icon.html

sidebar <- dashboardSidebar(
  
  side = "left",
  color = color,

  sidebarMenu(
    menuItem("Item Eingabe", tabName = "item_values", 
             icon = icon("comment")),
    menuItem("Kategoriale Diagnostik", tabName = "kat_diag", 
             icon = icon("calculator")),
    menuItem("Dimensionale Diagnostik", tabName = "dim_diag", 
             icon = icon("calculator")),
    menuItem("Erläuterungen", tabName = "erlaeuterungen", 
             icon = icon("info circle")),
    menuItem("Kontakt", tabName = "kontakt", 
             icon = icon("paper plane"))
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
                color = color,
                width = 16,
              
                h1("Items"),
                p("Bitte geben Sie hier die entsprechenenden Item-Werte aus dem Fragebogen ein."),
              
                tabPanel("Items", uiOutput("items"))

              )
            
            )
            
    ),
    

    
    ### 1.2.2 Kategoriale Diagnostik ----
    tabItem(tabName = "kat_diag",
            
            fluidRow(
              
              box(
                
                title = "Kategoriale Diagnostik",
                color = color,
                width = 16,
                
                h1("Kategoriale Diagnostik"),
                strong("Der  ADP-IV  sieht  zwei  Scoring-Algorithmen  vor:"),
                br(),
                strong("T>4  und  D>1  sowie  T>5  und  D>1."),
                br(),
                p("Dies  bedeutet,  dass  ein  Item  dann  als  erfüllt  angesehen  wird,  wenn  im  dimensionalen  Rating  das  Trait-Item  5  oder  mehr  bzw.  6  oder  mehr  beträgt  und  darüber  hinaus  das  Distress-Rating  zwei oder mehr beträgt. Der T>4 und D>1 Algorithmus eignet sich eher für Screening-Zwecke während der strengere T>5 und D>1 Algorithmus eher für den wissenschaftlichen Einsatz geeignet ist. Der gewähl-te Algorithmus ist oben auf dem Auswertungsbogen anzukreuzen.")
                
              ),
              
              box(
                
                title = "Scoring-Algorithmus",
                color = color,
                width = 16,
                
                h1("Scoring-Algorithmus"),
                strong("Bitte Scoring-Algorithmus auswählen:"),
                br(),
                radioButtons("kat_alg", 
                             NULL,
                             list("T>4 & D>1" = "t4_d1",
                                  "T>5 & D>1" = "t5_d1"),
                             selected = "t4_d1",
                             inline = T)
                
              ),
              
              box(
                
                title = "Diagnosen",
                color = color,
                width = 16,
                
                h1("Kategoriale Diagnosen"),
                tableOutput("paranoid")
                
              )
              
            )
            
    ),
    
    
    ### 1.2.3 Dimensionale Diagnostik ----
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
    color = color,
    disable = FALSE
  ),
  sidebar,
  body
)





# 2. Server ----

server <- function(input, output) {
  
  
  ## 2.1 Item Eingabe ----
  
  values <- list()
  for (i in item_nums){
   values[[i]] <- box(
     title = paste0("Item ", i),
     sliderInput(paste0("item_", i),
                 paste0("Trait Item ", i),
                 min = 1,
                 max = 7,
                 value = 1,
                 width = "50%"),
     conditionalPanel(condition = paste0("input.item_", i, " >= 5"),
                      sliderInput(paste0("leid_item_", i),
                                  paste0("Distress Item ", i),
                                  min = 1, 
                                  max = 3,
                                  value = 1,
                                  width = "18%"))
     
                 )
   
  }
  
  werte <- reactive({
    ant_mat <- matrix(nrow = length(item_nums), ncol = 3)
    for (i in item_nums) {
      ant_mat[i,1] <- paste0(item_nums[i])
      ant_mat[i,2] <- input[[paste0("item_", i)]]
      ant_mat[i,3] <- input[[paste0("leid_item_", i)]]
    }
    data.frame(
      Item     = ant_mat[,1],
      Trait    = as.numeric(ant_mat[,2]),
      Distress = as.numeric(ant_mat[,3])
    )
  })
  
  output$Antworten <- renderTable(werte())
  output$items <- renderUI(values)
  
  
  # 2.2 Kategoriale Diagnostik ----
  
  kat_results <- reactive({
    score_kat_diag(werte(), input$kat_alg)
  })
  
  output$paranoid <- renderTable({kat_results()})
    
}





# 3. Run App ----
shinyApp(ui = ui, server = server)
