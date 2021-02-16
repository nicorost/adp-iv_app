# App zur Auswertung des ADP-IV Fragebogens

# https://www.meduniwien.ac.at/hp/psychoanalyse/forschung/diagnostik-downloads/diagnostik-von-persoenlichkeitsstoerungen/

# (C) Nicolas Rost, 2021



# Packages ----

library(shiny)
library("semantic.dashboard")





# 1. UI ----


## 1.1 Sidebar ----

sidebar <- dashboardSidebar(
  
  side = "left",
  color = "blue",
  size = "thin",
  inverted = TRUE,
  
  sidebarMenu(
        menuItem("T-Test", tabName = "tcalculation", 
             icon = icon("calculator")),
    menuItem("Chisquare-Test", tabName = "chisquarecalculation", 
             icon = icon("calculator")),
    menuItem("ANOVA", tabName = "anovacalculation", 
             icon = icon("calculator")),
    menuItem("Explanations", tabName = "explanation", icon = icon("th")),
    menuItem("Reference & Contact", tabName = "reference", icon = icon("info circle"))
    )
)


## 1.2 Body ----

body <- dashboardBody(
  tabItems(
    
    # -----------------Power Calculation T-test-------------
    tabItem(tabName = "tcalculation",
            
            
            fluidRow(
              
              
              # ----------Power Analysis Output---------
              box(
                title = "Power Analysis",
                color = "blue",
                
                h1("Power Calculation"),
                p("Below you can find results from power analysis for the", strong("nested-precision Randomised Controlled Trial (npRCT)"),". You can set parameters for power analysis in the box to the right. Results from power analysis will change accordingly below."),
                box(
                  title = "Results",
                  color = "blue",
                  collapsible = FALSE,
                  
                  h3("Estimated Sample Sizes"),
                  strong(textOutput("t.n_total")),
                  br(),
                  strong(textOutput("t.n_traditional")),
                  strong(textOutput("t.n_precision")),
                  textOutput("twarning"),
                  tags$head(tags$style("#twarning{color: red;
                                 
                                 font-style: italic;
                                 }"
                  )
                  ),
                  
                  br(),
                  textOutput("t.n_saved"),
                  tags$hr(),
                  p("Please note that ", em("per group"), "refers to main groups of the nested RCTs, which are the two groups intervention ", em("A"), " versus ", em("B"), " for the ", em("traditional"), "RCT and the two groups ", em("randomisation"), " versus ", em("stratification"), " for the ", em("precision"), " RCT. See the explanations tab for details.")
                  
                )
              ),
              
              
              # ----------Set RCT Parameters------------
              box(
                title = "Set Parameters",
                color = "blue",
                
                
                # ----------Traditional RCT Parameters------------
                h2("Traditional RCT"),
                sliderInput("t.d1",
                            "Effect Size (d)",
                            step = 0.01,
                            min = 0,
                            max = 1,
                            value = 0.5),
                br(),
                sliderInput("t.power1",
                            "Power",
                            step = 0.01,
                            min = 0.5,
                            max = 1,
                            value = 0.8),
                br(),
                sliderInput("t.sig.level1",
                            "Significance Level",
                            step = 0.001,
                            min = 0.001,
                            max = 0.1,
                            value = 0.05),
                br(),
                radioButtons("t.alternative1", 
                             NULL,
                             c("Two Sided Test"= "two.sided",
                               "One Sided Test"= "greater"),
                             selected = "two.sided"),
                
                
                # ----------Precision RCT Parameters------------
                h2("Precision RCT"),
                sliderInput("t.d2",
                            "Effect Size (d)",
                            step = 0.01,
                            min = 0,
                            max = 1,
                            value = 0.5),
                br(),
                sliderInput("t.power2",
                            "Power",
                            step = 0.01,
                            min = 0.5,
                            max = 1,
                            value = 0.8),
                br(),
                sliderInput("t.sig.level2",
                            "Significance Level",
                            step = 0.001,
                            min = 0.001,
                            max = 0.1,
                            value = 0.05),
                br(),
                radioButtons("t.alternative2", 
                             NULL,
                             c("Two Sided Test"= "two.sided",
                               "One Sided Test"= "greater"),
                             selected = "two.sided")
                
              )
              
              
              
              
            )        
            
            
    ),
    
    # --------------------Power Calculation Chisquare----
    
    tabItem(tabName = "chisquarecalculation",
            
            
            fluidRow(
              
              
              # ----------Power Analysis Output---------
              box(
                title = "Power Analysis",
                color = "blue",
                
                h1("Power Calculation"),
                p("Below you can find results from power analysis for the", strong("nested-precision Randomised Controlled Trial (npRCT)"),". You can set parameters for power analysis in the box to the right. Results from power analysis will change accordingly below."),
                box(
                  title = "Results",
                  color = "blue",
                  collapsible = FALSE,
                  
                  h3("Estimated Sample Sizes"),
                  strong(textOutput("chisquare.n_total")),
                  br(),
                  strong(textOutput("chisquare.n_traditional")),
                  strong(textOutput("chisquare.n_precision")),
                  textOutput("chisquarewarning"),
                  tags$head(tags$style("#chisquarewarning{color: red;
                                 
                                 font-style: italic;
                                 }"
                  )
                  ),
                  
                  br(),
                  textOutput("chisquare.n_saved"),
                  tags$hr(),
                  textOutput("chisquare.k")
                  
                  
                )
              ),
              
              
              # ----------Set RCT Parameters------------
              box(
                title = "Set Parameters",
                color = "blue",
                
                
                # ----------General Parameters------------
                h2("General Parameters"),
                sliderInput("chisquare.k",
                            "Number of groups",
                            step = 1,
                            min = 2,
                            max = 10,
                            value = 2),
                br(),
                sliderInput("chisquare.ycat",
                            "Number of outcome variable categories",
                            step = 1,
                            min = 2,
                            max = 10,
                            value = 2),
                
                # ----------Traditional RCT Parameters------------
                h2("Traditional RCT"),
                sliderInput("chisquare.w1",
                            "Effect Size (W)",
                            step = 0.01,
                            min = 0,
                            max = 1,
                            value = 0.3),
                br(),
                sliderInput("chisquare.power1",
                            "Power",
                            step = 0.01,
                            min = 0.5,
                            max = 1,
                            value = 0.8),
                br(),
                sliderInput("chisquare.sig.level1",
                            "Significance Level",
                            step = 0.001,
                            min = 0.001,
                            max = 0.1,
                            value = 0.05),
                
                
                # ----------Precision RCT Parameters------------
                h2("Precision RCT"),
                sliderInput("chisquare.w2",
                            "Effect Size (W)",
                            step = 0.01,
                            min = 0,
                            max = 1,
                            value = 0.3),
                br(),
                sliderInput("chisquare.power2",
                            "Power",
                            step = 0.01,
                            min = 0.5,
                            max = 1,
                            value = 0.8),
                br(),
                sliderInput("chisquare.sig.level2",
                            "Significance Level",
                            step = 0.001,
                            min = 0.001,
                            max = 0.1,
                            value = 0.05)
                
                
              )
              
              
              
              
            )        
            
            
    ),
    
    
    
    # --------------------Power Calculation ANOVA--------
    
    tabItem(tabName = "anovacalculation",
            
            
            fluidRow(
              
              
              # ----------Power Analysis Output---------
              box(
                title = "Power Analysis",
                color = "blue",
                
                h1("Power Calculation"),
                p("Below you can find results from power analysis for the", strong("nested-precision Randomised Controlled Trial (npRCT)"),". You can set parameters for power analysis in the box to the right. Results from power analysis will change accordingly below."),
                box(
                  title = "Results",
                  color = "blue",
                  collapsible = FALSE,
                  
                  h3("Estimated Sample Sizes"),
                  strong(textOutput("anova.n_total")),
                  br(),
                  strong(textOutput("anova.n_traditional")),
                  strong(textOutput("anova.n_precision")),
                  textOutput("anovawarning"),
                  tags$head(tags$style("#anovawarning{color: red;
                                 
                                 font-style: italic;
                                 }"
                  )
                  ),
                  
                  br(),
                  textOutput("anova.n_saved"),
                  tags$hr(),
                  textOutput("anova.k")
                  
                )
              ),
              
              
              # ----------Set RCT Parameters------------
              box(
                title = "Set Parameters",
                color = "blue",
                
                # ----------General Parameters------------
                h2("General Parameters"),
                sliderInput("anova.k",
                            "Number of groups",
                            step = 1,
                            min = 2,
                            max = 10,
                            value = 2),
                
                
                # ----------Traditional RCT Parameters------------
                h2("Traditional RCT"),
                sliderInput("anova.f1",
                            "Effect Size (F)",
                            step = 0.01,
                            min = 0,
                            max = 0.75,
                            value = 0.25),
                br(),
                sliderInput("anova.power1",
                            "Power",
                            step = 0.01,
                            min = 0.5,
                            max = 1,
                            value = 0.8),
                br(),
                sliderInput("anova.sig.level1",
                            "Significance Level",
                            step = 0.001,
                            min = 0.001,
                            max = 0.1,
                            value = 0.05),
                
                
                
                # ----------Precision RCT Parameters------------
                h2("Precision RCT"),
                sliderInput("anova.d2",
                            "Effect Size (d)",
                            step = 0.01,
                            min = 0,
                            max = 1,
                            value = 0.5),
                br(),
                sliderInput("anova.power2",
                            "Power",
                            step = 0.01,
                            min = 0.5,
                            max = 1,
                            value = 0.8),
                br(),
                sliderInput("anova.sig.level2",
                            "Significance Level",
                            step = 0.001,
                            min = 0.001,
                            max = 0.1,
                            value = 0.05),
                br(),
                radioButtons("anova.alternative2", 
                             NULL,
                             c("Two Sided Test"= "two.sided",
                               "One Sided Test"= "greater"),
                             selected = "two.sided")
                
                
              )
              
              
              
              
            )        
            
            
    ),
    
    
    
    # -----------------Explanation Tab----------------------
    tabItem(tabName = "explanation",
            
            
            
            fluidRow(
              
              
              
              
              
              
              # ----------npRCT Explanation--------------------
              box(
                title = "npRCT Explanation",
                color = "blue",
                
                p("The ", strong("nested-precision Randomised Controlled Trial (npRCT)")," is a research design to facilitate the move towards precision medicine by combining ", em("traditional"), " intervention testing (e.g., intervention A ", em("versus"), " B) with testing of a ", em("precision"), " algorithm (e.g., randomisation to intervention A or B ", em("versus"), " stratification to intervention A or B)."),
                p("The design of the npRCT is outlined below based on the publication of a letter to the journal [INCLUDE JOURNAL NAME HERE]."),
                br(),
                tags$hr(),
                h4(em("Traditional"), " RCT"),
                p("Randomisation to"),
                tags$ol(
                  tags$li("Group 1: Intervention A"), 
                  tags$li("Group 2: Intervention B")
                ),
                
                tags$hr(),
                h4(em("Precision"), " RCT"),
                tags$ol(
                  tags$li("Group 1: Randomisation (to)"),
                  tags$ol(tags$li("Subgroup 1: Intervention A"), 
                          tags$li("Subgroup 2: Intervention B")),
                  tags$li("Group 2: Stratification (to)"),
                  tags$ol(tags$li("Subgroup 3: Intervention A"), 
                          tags$li("Subgroup 4: Intervention B"))
                  
                ),
                
              ),
              
              
              
              # ----------Aims of the npRCT---------------------
              
              box(
                title = "Aims of the npRCT",
                color = "blue",
                
                p("The nested-precision Randomised Controlled Trial (npRCT) has two main aims. First, we hope the structure provided by the npRCT facilitates the move towards precision medicine by integrating traditional intervention testing with testing of a precision medicine algorithm. Second, the npRCT will reduce the number of participants needed for these two intervention testing approaches through its increased power, which we deem ethically and economically favourable. We describe these two aims in the sections below."),
                h2("Move Towards Precision Medicine"),
                p("Precision medicine aims to make intervention allocation decisions for single individuals based on each individuals’ characteristics (e.g., age, sex, blood pressure, etc.) with the hope that such decision rules lead to better outcomes than allocation decisions that are not based on any algorithms."),
                p("Often, researchers and/ or clinicians do not know precise allocation rules- such as specific genetic mutations- that can fully explain the efficacy of a specific intervention. Given the complexity and multicausal etiology of many diseases, it is likely that only a combination of characteristics can determine the efficacy of an intervention for a specific individual. As such, researchers need to explore such combinations and test empirically whether any algorithm integrating such characteristics can indeed improve outcomes."),
                p("The construct of the npRCT allows for a combination of exploratory and hypothesis-testing stages in the traditional and precision RCT, respectively. Specifically, during the ", em("traditional"), " RCT, researchers can explore whether they can identify any single or combination of variables that may indicate a preferable response for one treatment over the other. If this ", em("precision algorithm"), " is deemed effective for stratification, it can then be used in the ", em("precision"), " part of the npRCT to test stratification versus randomisation to the treatment condition."),
                
                h2("Increased Power & Ethical Favourability"),
                p("Due to the nesting of the npRCT, randomised participants of ", em("traditional"), " and ", em("precision"), " RCTs can be shared for answering the research question of general intervention efficacy. "),
                p("As such, the npRCT requires less participants to-be-recruited as compared to two independently run RCTs. To be precise, ½ times the sample size of the ", em("precision"), " RCT (i.e., all individuals that are still randomised) can be subtracted from the sample size of the ", em("traditional"), " RCT. Due to this increased power, we deem the npRCT ethically favourable to running these RCTs independently.")
                
              ),
              
              # ----------Practical considerations--------------
              
              box(
                title = "Practical Considerations",
                color = "blue",
                
                p("If you play around with the sliders of this power calculation app, you can see that the sample sizes for ", em("traditional"), " and ", em("precision"), " RCTs, respectively, can vary quite substantially. If the effect size for the ", em("precision"), " RCT is set much smaller than the to-be-detected effect size of the traditional RCT, you can get very small (or even negative) sample size requirements for the ", em("traditional"), " RCT as there would be ", em("sufficient"), " participants included in the ", em("precision"), " RCT."),
                
                p("Here, it is important to consider the practical needs for identification of the precision algorithm. While you may have ", em("sufficient"), " participants available in the ", em("precision"), " RCT for testing intervention A versus B (i.e., the ", em("traditional"), " research question), for instance, there may not be sufficient time or participants available to actually explore potential precision algorithms. In this case, sample size requirements and power considerations have to be weighed against the practical requirements for the identification of the precision algorithm."),
                
                p("As an example, consider a RCT of psychotherapy versus antidepressant medication for depression (i.e., interventions that are about equally effective). We can use the ", em("traditional"), " RCT to develop a precision algorithm by analysing whether potential participant characteristics (e.g., demographics, symptoms, etc.) predict whether they will respond more to one intervention than the other. Importantly, however, exploration of potential precision algorithms will already require a specific sample size, so that estimations are reasonable and one can be confident enough in results. As such, it is important to make such practical considerations over and above the statistical power estimates as obtained from this app. 
Related to this, if the ", em("traditional"), " RCT is ongoing while the ", em("precision algorithm"), " is being developed, one has to allow sufficient time (in terms of sample recruitment) to move from exploration to a decision on a to-be-tested precision algorithm.
"),
                
                h2("Blinding"),
                p("As a last practical note, researchers need to establish procedures during precision algorithm identification that ensure blinding of research personnel. This is highly relevant as knowledge of the precision algorithm might influence researchers’ interaction with participants. For instance, if an algorithm favoured intervention A for females and intervention B for males, researchers may act differently towards participants based on knowledge of their potential ", em("best treatment match"), ". As a consequence, we would advise researchers to use proper blinding procedures. This could be achieved, for instance, by outsourcing precision algorithm identification- and subsequent allocation of individuals to interventions- to personnel not directly involved in the RCT and in particular not in contact with study participants.")
              ),
              
              # ----------How to use the app: An example--------
              
              box(
                title = "How to use the app: An example",
                color = "blue",
                
                p("To make the use of the app clearer, consider a research group interested in testing interventions for depression. Common interventions for depression are antidepressant medications (e.g., selective serotonin reuptake inhibitors) and psychotherapy (e.g., cognitive behavioural therapy), which are about equally effective. The research group would like to empirically test efficacy of these interventions on reducing depressive symptoms in their university hospital. As such, they start planning a ", em("traditional")," RCT comparing antidepressant medication (ADM) versus psychotherapy (PT). As depressive symptoms are usually measured on a continuous scale and they have two groups, the easiest statistical analysis method is a t-test. The researchers are interested in a medium effect between interventions and use common statistical parameters such as a two-sided significance threshold of 0.05 and a power of 0.8. If they ran this study outside the npRCT design, this would mean the researchers needed to recruit 64 participants per group (i.e., 128 participants in total)."),
                
                p("As this hypothetical research group is very interested in identifying variables that can predict preferable response to PT over ADM and vice versa, however, they plan to use the npRCT design. The research group is optimistic in their ability to find a precision algorithm with medium effect size compared to not using the algorithm. They use the same statistical parameters (significance threshold of 0.05, & power of 0.8) but because they consider their algorithm to be better and not worse than random treatment allocation, they use a one-sided test. When they plug all values into the npRCT app, the app offers the following results:"),
                tags$ol(tags$li("They will require a total npRCT sample size of 176 individuals"), 
                        tags$li("In the traditional RCT, 38 participants are needed for each PT and ADM groups"),
                        tags$li("In the precision RCT, 50 participants are needed for each the randomised and stratified groups and participants are randomised to randomisation or stratification")),
                
                p("In a next step, the researchers start recruitment for their study and they instruct an independent statistician to use the data from the traditional RCT as they come in to identify a stratification algorithm using a pre-specified statistical procedure. When recruitment approaches the end of the traditional RCT, the statistician notifies the researchers whether they were successful in identifying a precision algorithm with the needed effect size. Luckily, the data indicate that such an algorithm is likely to exist since older individuals seem to respond better to ADM and younger individuals to PT (note that this is a completely hypothetical scenario). If the data hadn’t been suggestive of such an algorithm, the researchers could have decided to not move on to the ", em("precision"), " RCT phase and stick with the ", em("traditional"), " RCT, but now move on to test their algorithm by randomising participants to allocation based on randomisation (i.e., participants are randomised twice; first to randomisation, then to PT or ADM) or stratification (i.e., the algorithm decides whether participants receive PT or ADM)."),
                
                p("Once recruitment of the npRCT is finished, the researchers can then analyse data from all individuals who were randomised to PT or ADM (i.e., the full ", em("traditional"), " RCT sample and half of the ", em("precision"), " RCT sample) for answering the research question of general efficacy (i.e., is PT or ADM better for treating depressive symptoms in this sample). To evaluate the age-based precision algorithm, the researchers test whether the stratified group fared better than the randomised group in the ", em("precision"), " RCT."),
                
                p("Due to use of the npRCT design, the researchers have answered two research questions in one large design instead of independent studies, which meant they needed to recruit 26 participants less per group (i.e., 52 participants in total). This thus saves research funding and reduces burden of study participation in many individuals."),
                
                p("This example outlined a hypothetical application of the npRCT with two groups (PT versus ADM) and a continuous outcome (depressive symptoms). Beyond this, the npRCT can be extended to use of categorical outcome variables (e.g., remission versus no remission) and multiple groups (e.g., PT versus ADM versus exercise). To this end, the ANOVA and Chisquare tabs of the app can be used for power calculation.")
                
              )
              
              
              
            )
            
    ),
    
    
    # -----------------Reference Tab----------------------
    tabItem(tabName = "reference",
            
            
            
            fluidRow(
              
              
              
              # ----------Reference box---------------------
              box(
                title = "Reference",
                color = "blue",
                
                #h2("Ethical Implications"),
                p(strong("This is where the letter is published."))
                
                
              ),
              
              box(
                title = "Contact",
                color = "blue",
                
                
                p("This app was programmed by Nils Kappelmann. Code is available via ", 
                  a("this Github Repository", 
                    href="https://github.molgen.mpg.de/mpip/npRCT.app"), "."),
                p("You can reach me via email under n.kappelmann@gmail.com")
                
                
                
              )
              
              
            )
            
    )
    
  )
)


## 1.3 Defining UI

ui <- dashboardPage(
  
  dashboardHeader(
    h1("ADP-IV Auswertung", style = "color:black"),
    color = "blue",
    disable = FALSE
  ),
  sidebar,
  body
)




# 2. Server ----

server <- function(input, output) {
  
}





# 3. Run App ----
shinyApp(ui = ui, server = server)