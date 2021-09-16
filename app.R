# App zur Auswertung des ADP-IV Fragebogens

# https://www.meduniwien.ac.at/hp/psychoanalyse/forschung/diagnostik-downloads/diagnostik-von-persoenlichkeitsstoerungen/

# (C) Nicolas Rost, 2021



# Packages ----

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(shiny, semantic.dashboard, shinyWidgets, writexl)


source("utils.R")


n_items <- 94
item_nums <- c(1:n_items)


color <- "teal"





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
                strong("Der ADP-IV sieht zwei verschiedene Scoring-Algorithmen vor:"),
                br(),
                strong("T>4 & D>1 sowie T>5 & D>1."),
                br(),
                p("Ein Item wird dann als erfüllt angesehen, wenn das zugehörige Trait-Rating 5 oder mehr bzw. 6 oder mehr beträgt und gleichzeitig das Distress-Rating zwei oder mehr beträgt. Der", em("T>4 & D>1"), "Algorithmus dient laut", a("Manual", href="https://www.meduniwien.ac.at/hp/fileadmin/psychoanalyse/pdf/ADP-IV_Anleitung_zur_Auswertung.pdf"), "eher Screening-Zwecken, während der strengere", em("T>5 & D>1"), "Algorithmus eher für den wissenschaftlichen Einsatz geeignet ist. Der gewünschte Algorithmus ist oben auszuwählen."),
                
              
                box(
                  
                  title = "Scoring-Algorithmus",
                  width = 16,
                  collapsible = F,
                  
                  h3("Scoring-Algorithmus"),
                  strong("Bitte Scoring-Algorithmus auswählen:"),
                  br(),
                  br(),
                  prettyRadioButtons("kat_alg", 
                                     NULL,
                                     list("T>4 & D>1" = "t4_d1",
                                          "T>5 & D>1" = "t5_d1"),
                                     selected = "t4_d1",
                                     inline = T,
                                     shape = "curve")
                
                )
                
              ),
              
              box(
                
                title = "Diagnosen",
                color = color,
                width = 16,
                
                h1("Kategoriale Diagnosen"),
                tableOutput("kategorial")
                
              ),
              
              box(
                
                title = "Download",
                color = color,
                width = 16/2,
                
                h1("Diagnostik speichern?"),
                downloadButton("kat_save",
                               label = "Als .xlsx speichern",
                               class = "butt"),
                tags$head(tags$style(".butt{color:teal}"))
                
              )
              
            )
            
    ),
    
    
    ### 1.2.3 Dimensionale Diagnostik ----
    tabItem(tabName = "dim_diag",
            
            fluidRow(
              
              box(
                
                title = "Dimensionale Diagnostik",
                color = color,
                width = 16,
                
                h1("Dimensionale Diagnostik"),
                p("Für jedes Item wird der Trait-Score ausgelesen, anschließend wird für jede einzelne Persönlichkeitsstörung die", strong("Summe aller Trait-Scores"), "gebildet. Aus den Summenscores der jeweiligen Persönlichkeitsstörungen werden dann Cluster-Summenscores und ein Gesamtsummenscore gebildet.")
                
              ),
              
              box(
                
                title = "Cluster A",
                color = color,
                width = 16/3,
                h3("Cluster A"),
                tableOutput("cluster_a")
              
              ), 
              
              box(
              
                title = "Cluster B",
                color = color,
                width = 16/3,
                h3("Cluster B"),
                tableOutput("cluster_b")
              
              ),
                
              box(
                
                title = "Cluster C",
                color = color,
                width = 16/3,
                h3("Cluster C"),
                tableOutput("cluster_c")
              
              ),
                
              box(
                
                title = "Gesamt",
                color = color,
                width = 16/3,
                h3("Gesamt"),
                tableOutput("gesamt")
                
              ),
                
              box(
                
                title = "Nicht Näher Bezeichnet",
                color = color,
                width = 16/3,
                h3("Nicht Näher Bezeichnet"),
                tableOutput("nnb")
                
              ),
              
              box(
                
                title = "Download",
                color = color,
                width = 16/2,
                
                h1("Diagnostik speichern?"),
                downloadButton("dim_save",
                               label = "Als .xlsx speichern",
                               class = "butt"),
                tags$head(tags$style(".butt{color:teal}"))
                
              )
              
            )
            
    ),
    
    
    ### 1.2.4 Erläuterungen ----
    tabItem(tabName = "erlaeuterungen",
            
            fluidRow(
              
              box(
                
                title = "Informationen",
                color = color,
                
                h2("Diagnostik von Persönlichkeitsstörungen"),
                p("Der ADP-IV dient zur Diagnostik von Persönlichkeitsstörungen, für die oft zeitaufwändige klinische Interviews verwendet werden. Ein vergleichsweise schnell durchführbarer Screening-Fragebogen kann daher eine wertvolle Ergänzung darstellen."),
                p("Ursprünglich wurde der ADP-IV von den belgischen Autoren Chris Schotte und Dirk De Doncker entwickelt. Doering et al. übersetzten und validierten ihn und stellten ihn Kliniker:innen und Wissenschaftler:innen frei zur Verfügung. Der Fragebogen sowie zugehörige Auswertungsbögen und -anleitung können", a("hier", href="https://www.meduniwien.ac.at/hp/psychoanalyse/forschung/diagnostik-downloads/diagnostik-von-persoenlichkeitsstoerungen/"), "kostenfrei heruntergeladen werden."),
                p("Die Originalpublikationen zur deutschsprachigen Version finden sich in der Zeitschrift für Psychosomatische Medizin und Psychotherapie, Heft 02/2007 und 03/2008."),
              
                box(
                  
                  title = "Informationen zum Fragebogen",
                  collapsible = F,
                  
                  p("Die 94 Items des ADP-IV beziehen sich auf verschiedene Persönlichkeitszüge und -eigenschaften und ermöglichen somit eine", strong("Selbsteinschätzung von Persönlichkeitsstörungen"), "."),
                  p("Der ADP-IV Fragebogen ist speziell auf Persönlichkeitseigenschaften ausgerichtet, die problematisch und schwer zu ändern sein können. Er fragt daher nach Persönlichkeitszügen, die Ursache von Stress, Problemen, Konflikten usw. sein und ein ausgewogenes und befriedigendes Leben in der Gesellschaft behindern können."),
                  p("Die Auswertung liefert zum einen", em("kategoriale"), "Diagnosen, gestattet aber auch eine", em("dimensionale"), "Diagnostik und die Erstellung eines Profils der Persönlichkeitspathologie."),
                  p("Zusätzliche und detailliertere Informationen finden sich auf der", a("Homepage zum Fragebogen", href="https://www.meduniwien.ac.at/hp/psychoanalyse/forschung/diagnostik-downloads/diagnostik-von-persoenlichkeitsstoerungen/"), ".")
                  
                )
                
              ),
              
              box(
                
                title = "Benutzung der App",
                color = color,
                
                h2("Benutzung der App"),
                p("Diese Shiny-App soll bei der Auswertung des Fragebogens helfen, indem sie die Persönlichkeitsdiagnostik basierend auf einer", strong("anonymen Eingabe"), "der Item-Werte automatisiert."),
                p("Die Item-Werte können im obersten Tab mithilfe der Schieberegler eingegeben werden. Durch die Eingabe wird automatisch die Diagnostik erstellt."),
                p("Zur Auswertung kann zwischen der", strong("kategorialen"), "und", strong("dimensionalen"), "Diagnostik ausgewählt werden. Bei der kategorialen Diagnostik kann zudem zwischen den beiden Scoring-Algorithmen gewählt werden. Die Standard-Einstellung entspricht dabei dem", em("T>4 & D>1"), "Scoring."),
                p("Die App" , strong("erfasst keine personenbezogenen Daten"), "und solche können und sollen auch nicht eingegeben werden. Die App dient lediglich der Ersparnis der manuellen Auswertung des Fragebogens.")
              
              )
              
            )
            
    ),
    
    
    ### 1.2.5 Kontakt ----
    tabItem(tabName = "kontakt",
    
            fluidRow(
              
              box(
                
                title = "Kontakt",
                color = color,
                width = 16,
                collapsible = F,
                
                p("Diese App wurde erstellt von", a("Nicolas Rost", href="https://nicorost.github.io/"), "."),
                p("Der Code ist öffentlich zugänglich auf", a("Github", href="https://github.com/nicorost/adp_iv.app"), "."),
                p("Als Grundlage für das Design diente", a("diese App", href="https://nprct.shinyapps.io/nprct/"), "."),
                p("Fragen? ➔ nico.rost@posteo.de"),
                br(),
                p("Alle Informationen zum Fragebogen sowie alle nötigen PDF-Dokumente finden Sie", a("hier", href="https://www.meduniwien.ac.at/hp/psychoanalyse/forschung/diagnostik-downloads/diagnostik-von-persoenlichkeitsstoerungen/"), ".")
                
              )
              
            )
            
    )
    
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
     setSliderColor(rep(color, (2 * n_items)), c(1:(n_items * 2))),
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

  output$kategorial <- renderTable({kat_results()})

  output$kat_save <- downloadHandler(
    filename = function() {
      paste0("ADP_IV_kategoriale_Diagnostik_", strftime(Sys.time(), format = "%Y%m%d_%Hh%Mm%Ss"), ".xlsx")
    },
    content = function(file) {
      data <- kat_results()
      write_xlsx(data, file)
    }
  )

  # 2.3 Dimensionale Diagnostik ----

  dim_results <- reactive({
    score_dim_diag(werte())
  })

  output$cluster_a <- renderTable({dim_results()[[1]]})
  output$cluster_b <- renderTable({dim_results()[[2]]})
  output$cluster_c <- renderTable({dim_results()[[3]]})
  output$gesamt    <- renderTable({dim_results()[[4]]})
  output$nnb       <- renderTable({dim_results()[[5]]})

  output$dim_save <- downloadHandler(
    filename = function() {
      paste0("ADP_IV_dimensionale_Diagnostik_", strftime(Sys.time(), format = "%Y%m%d_%Hh%Mm%Ss"), ".xlsx")
    },
    content = function(file) {
      datalist <- list("Cluster A" = dim_results()[[1]],
                       "Cluster B" = dim_results()[[2]],
                       "Cluster C" = dim_results()[[3]],
                       "Gesamt" = dim_results()[[4]],
                       "Nicht Näher Bezeichnet" = dim_results()[[5]])
      write_xlsx(datalist, file)
    }
  )
  
}





# 3. Run App ----
shinyApp(ui = ui, server = server)
