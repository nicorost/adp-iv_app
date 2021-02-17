# helper functions


# packages
library(tidyverse)


# categorical scoring ----

# subdiagnosis items
paranoid_items      <- as.character(c(1, 13, 25, 37, 49, 61, 73))
schizoid_items      <- as.character(c(2, 14, 26, 38, 50, 62, 74))
schizotypisch_items <- as.character(c(3, 15, 27, 39, 50, 51, 63, 75, 85))
vermeidend_items    <- as.character(c(8, 20, 32, 44, 56, 68, 80))
dependent_items     <- as.character(c(9, 21, 33, 45, 57, 69, 81, 90))
zwanghaft_items     <- as.character(c(10, 22, 34, 46, 58, 70, 82, 91))
antisozial_items    <- as.character(c(4, 16, 28, 40, 52, 64, 76, 86))
borderline_items    <- as.character(c(5, 17, 29, 41, 53, 65, 77, 87, 92, 94))
histrionisch_items  <- as.character(c(6, 18, 30, 42, 54, 66, 78, 88))
narzisstisch_items  <- as.character(c(7, 19,31, 43, 55, 67, 79, 89, 93))
nnb_depressiv_items <- as.character(c(11, 23, 35, 47, 59, 71, 83))
nnb_passiv_items    <- as.character(c(12, 24, 36, 48, 60, 72, 84))

score_kat_diag <- function(antworten, alg) {
  
  if (alg == "t4_d1") {
    antworten <- antworten %>% 
      mutate(erfuellt = ifelse(Trait > 4 & Distress > 1, 1, 0))
  } else if (alg == "t5_d1") {
    antworte <- antworten %>% 
      mutate(erfuellt = ifelse(Trait > 5 & Distress > 1, 1, 0))
  }
  
  paranoid      <- antworten %>% filter(Item %in% paranoid_items)
  schizoid      <- antworten %>% filter(Item %in% schizoid_items)
  schizotypisch <- antworten %>% filter(Item %in% schizotypisch_items)
  vermeidend    <- antworten %>% filter(Item %in% vermeidend_items)
  dependent     <- antworten %>% filter(Item %in% dependent_items)
  zwanghaft     <- antworten %>% filter(Item %in% zwanghaft_items)
  antisozial    <- antworten %>% filter(Item %in% antisozial_items)
  borderline    <- antworten %>% filter(Item %in% borderline_items)
  histrionisch  <- antworten %>% filter(Item %in% histrionisch_items)
  narzisstisch  <- antworten %>% filter(Item %in% narzisstisch_items)
  nnb_depressiv <- antworten %>% filter(Item %in% nnb_depressiv_items)
  nnb_passiv    <- antworten %>% filter(Item %in% nnb_passiv_items)
  
  # borderline exception: one of two items is enough --> don't count twice
  if (borderline$erfuellt[5] == 1 & borderline$erfuellt[6] == 1) {
    borderline$erfuellt[5] == 0
  }
  
  diag_paranoid      <- ifelse(sum(paranoid$erfuellt) > 3, "ja", "nein")
  diag_schizoid      <- ifelse(sum(schizoid$erfuellt) > 3, "ja", "nein")
  diag_schizotypisch <- ifelse(sum(schizotypisch$erfuellt) > 4, "ja", "nein")
  diag_vermeidend    <- ifelse(sum(vermeidend$erfuellt) > 3, "ja", "nein")
  diag_dependent     <- ifelse(sum(dependent$erfuellt) > 4, "ja", "nein")
  diag_zwanghaft     <- ifelse(sum(zwanghaft$erfuellt) > 3, "ja", "nein")
  diag_antisozial    <- ifelse(sum(antisozial$erfuellt) > 2, "ja", "nein")
  diag_borderline    <- ifelse(sum(borderline$erfuellt) > 4, "ja", "nein")
  diag_histrionisch  <- ifelse(sum(histrionisch$erfuellt) > 4, "ja", "nein")
  diag_narzisstisch  <- ifelse(sum(narzisstisch$erfuellt) > 4, "ja", "nein")
  diag_nnb_depressiv <- ifelse(sum(nnb_depressiv$erfuellt) > 4, "ja", "nein")
  diag_nnb_passiv    <- ifelse(sum(nnb_passiv$erfuellt) > 3, "ja", "nein")
  
  output <- data.frame(
    Diagnose = c("Paranoid",
                 "Schizoid",
                 "Schizotypisch",
                 "Vermeidend-Selbstunsicher",
                 "Dependent",
                 "Zwanghaft",
                 "Antisozial",
                 "Borderline", 
                 "Histrionisch",
                 "Narzisstisch",
                 "NNB-Depressiv",
                 "NNB-Passiv-Aggressiv"),
    `Cut-Off` = c("> 3",
                  "> 3", 
                  "> 4",
                  "> 3", 
                  "> 4", 
                  "> 3",
                  "> 2",
                  "> 4",
                  "> 4",
                  "> 4",
                  "> 4",
                  "> 3"),
    `Anzahl erfuellter Kriterien` = c(as.integer(sum(paranoid$erfuellt)),
                                      as.integer(sum(schizoid$erfuellt)),
                                      as.integer(sum(schizotypisch$erfuellt)),
                                      as.integer(sum(vermeidend$erfuellt)),
                                      as.integer(sum(dependent$erfuellt)),
                                      as.integer(sum(zwanghaft$erfuellt)),
                                      as.integer(sum(antisozial$erfuellt)),
                                      as.integer(sum(borderline$erfuellt)),
                                      as.integer(sum(histrionisch$erfuellt)),
                                      as.integer(sum(narzisstisch$erfuellt)),
                                      as.integer(sum(nnb_depressiv$erfuellt)),
                                      as.integer(sum(nnb_passiv$erfuellt))),
    `Diagnose erfuellt?` = c(diag_paranoid,
                             diag_schizoid,
                             diag_schizotypisch,
                             diag_vermeidend,
                             diag_dependent,
                             diag_zwanghaft,
                             diag_antisozial,
                             diag_borderline,
                             diag_histrionisch,
                             diag_narzisstisch,
                             diag_nnb_depressiv,
                             diag_nnb_passiv),
    check.names = F
  )
  
  return(output)
  
}
