#' LISTE SANG 2025
#' --------------------------------
#' 
#' Extracting ID of individuals that were already blood sampled in previous breeding seasons
#' This generates lists (by site) we check when we catch birds.
#' If the bird ID is on the list, it means it is already associated with a blood sample
#' Meaning we don't need to take a blood sample
#' 
#' There is an additional constraint for 2025 (and probably future years)
#' Because of a future research on senescence, we have to sample every bird that is 4 years old or more
#' So I have to remove older birds from the lists 
#' It means that I only keep the last two years in the lists
#' (Birds captured three years ago or before should be 4 years or more this year anyway)
#' It means that I only keep birds that were 1 or 2 years old last year
#' or birds that were less that were 1 year old the year before (first breeding attempt)
#' 
#' 
library(tidyverse)
library(openxlsx)

#' object "repo" directs to the folder with the updated data. I am masking the address to this repository
repo 

output <- "C:/Users/FARGEVIEILLE/Documents/GitHub/cefe_tit/outputs/liste_sang_2025/"

#' call the dataset containg information about blood sample (and bird ID)
morpho <- openxlsx::read.xlsx(paste0(repo, "SIE MORPH 1976-2024.xlsx"), detectDates = TRUE)

#' Create a function to extract bird ID per site (or cluster of sites)

sang <- function (plot, sp) {
  
  data <- morpho %>% 
    
    # Select individuals associated with a blood sample of more than 10 µL within the last 2 years
    # Usually, I keep the last 3 years, but as we need samples for senescence, I only keep the last 2 years
    dplyr::filter(an <= max(as.numeric(an)) & an >= (max(as.numeric(an)) - 1) & quantite_sang >=10) %>% 
    
    # Keep only columns of interest
    dplyr::select(espece, lieu, an, bague, age) %>% 
    
    # Keep only one row for each ID
    dplyr::distinct(bague, .keep_all = TRUE) %>% 
    
    # Sort by ID (easier to find in the list during fieldwork)
    dplyr::arrange(bague)
  
  # Create a list specific to the targetted site and species
  liste <- data %>% 
    dplyr::filter(lieu %in% plot & espece == sp) %>% 
    
  # Create a column to calculate minimum age
    dplyr::mutate(min_age = dplyr::case_when(stringr::str_detect(age, "A") ~ as.numeric(stringr::str_remove(age, "[AIJP]")) + 1,
                                             TRUE ~ as.numeric(stringr::str_remove(age, "[AIJP]")))) %>% 
    
  # Filter to only keep individuals that will be less than 4 years old in the next breeding season
    # (they were less than three years old last year, or less than 2 years old the year before)
    dplyr::filter((an == max(as.numeric(an)) & min_age < 3) | (an == max(as.numeric(an) - 1) & min_age < 2)) %>% 
  
  # Create a dataframe containing only bird ID
    select(bague)
  
  # Create the name of the csv file
  texte <- paste0("liste_sang_", sp, "_", plot, ".csv")
  
  # Save bird ID as a csv file
  write.csv(liste, paste0(output, texte), row.names = FALSE)

} 


# Blue tit from site "Avapessa"
sang("ava", "ble")

# Blue tit from site "Feliceto"
sang("fel", "ble")

#Blue tit from site "Muro"
sang("mur", "ble")

# Blue tit from sites "Arinelle" & "Filagna"
sang("ari", "ble")
sang("fil", "ble")

# Blue tit from site "Grassa"
sang("gra", "ble")

# Blue tit from site "Rouvière"
sang("rou", "ble")

# Great tit from site "Rouviere"
sang("rou", "cha")

# Great tit from sites "CEFE" or "Fac"
sang("cef", "cha")
sang("fac", "cha")

# Great tit from site "Jardin botanique"
sang("bot", "cha")

# Great tit from site "Font-Colombe"
sang("font", "cha")

# Great tit from site "Grammont"
sang("gram", "cha")

# Great tit from site "Mas-Nouguier"
sang("mas", "cha")

# Great tit from site "Mosson"
sang("mos", "cha")

# Great tit from site "Zoo de Lunaret"
sang("zoo", "cha")


#'Improvement:
#' - Finding a way to include two sites within a csv file
#' - For ODK purposes, I should have the complete list (maybe adding site, sex and species) 
         