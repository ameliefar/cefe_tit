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
#' or birds that were 1 year old the year before (first breeding attempt)
#' 
#' 
library(tidyverse)
library(openxlsx)

#' object "repo" directs to the folder with the updated data. I am hiding the address to this repository
#' object "repo1" directs to the folder with ongoing recorded data. I am hinding the address to this repository
repo <- 
repo1 <- 

output <- "C:/Users/FARGEVIEILLE/Documents/GitHub/cefe_tit/outputs/liste_sang_2025/"

#' call the dataset containg information about blood sample (and bird ID)
morpho_old <- openxlsx::read.xlsx(paste0(repo, "SIE MORPH 1976-2024.xlsx"), detectDates = TRUE)
morpho_new <- openxlsx::read.xlsx(paste0(repo1, "MORPH_2025.xlsx"), detectDates = TRUE)
morph <- rbind(morpho_old, morpho_new)


#' Create a function to extract bird ID per site

sang <- function (plot, sp) {
  
  data <- morpho %>% 
    
    # Select individuals associated with a blood sample of more than 10 µL within the last 3 years
    dplyr::filter(an <= max(as.numeric(an)) & an >= (max(as.numeric(an)) - 3) & quantite_sang >=10) %>% 
    
    # Keep only columns of interest
    dplyr::select(espece, lieu, an, bague, age) %>% 
    
    # Keep only one row for each ID
    dplyr::distinct(bague, .keep_all = TRUE) %>% 
    
    # Sort by ID (easier to find in the list during fieldwork)
    dplyr::arrange(bague)
  
  # Create a list specific to the targetted site and species
  liste <- data %>% 
    dplyr::filter(lieu %in% plot & espece == sp) %>% 
  
  # Create a dataframe containing only bird ID
    select(bague)
  
  # Create the name of the csv file
  lieu <- if (length(plot) > 1) { # case when the list involve more than one site
    substitute(plot) # the function "substitute()" gets the name of the object ; if you create an object such as corse <- c("mur", "pir", "ava"), the name will be "corse"
  } else { #case when there is only one site
    plot # take the name of the site
  }
  texte <- paste0("liste_sang_", sp, "_", lieu, ".csv")
  
  
write.csv(liste, paste0(output, texte), row.names = FALSE)
}

#-------------------------------------------------------------------------
# Extract files for blue tit
#-------------------------------------------------------------------------

#Blue tit from "Pirio" (Pirio & Tuarelli, as there were still nest boxes in Tuarelli in 2023)
pir <-  c("pir", "tua")
sang(pir, "ble")

# Blue tit from site "Avapessa"
sang("ava", "ble")

# Blue tit from site "Feliceto"
sang("fel", "ble")

#Blue tit from site "Muro"
sang("mur", "ble")

# Blue tit from sites "Arinelle" & "Filagna" (very close sites with potential exchanges)
ari_fil <- c("ari", "fil")
sang(ari_fil, "ble")

# Blue tit from site "Grassa"
sang("gra", "ble")

# Blue tit from site "Rouvière"
sang("rou", "ble")


#-------------------------------------------------------------------------
# Extract files for great tit
#-------------------------------------------------------------------------

# Great tit from site "Rouviere"
sang("rou", "cha")

# Great tit from sites "CEFE" or "Fac" (very close sites with potential exchanges)
cef_fac <- c("cef", "fac")
sang(cef_fac, "cha")

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

