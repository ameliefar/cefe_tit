#-----------------------------------------------------------------------------------------
# Working on the specific case of consulting a file within an ODK form
#-----------------------------------------------------------------------------------------

#' My goal here is to provide a file listing every individual associated with a blood sample
#' For people collecting data on the ODK app.
#' 
#' The idea(l) would be that information shows up when entering the ID on the ODK form
#' -- when mentioning ID XXXXXXX a pop-up shows up, with information (age, sex, site, blood quantity)
#' -- people can check if it makes sense (double check on the ID) and
#' -- decide to sample blood or not
#' 
library(tidyverse)
library(openxlsx)

#' object "repo" directs to the folder with the updated data. I am hiding the address to this repository
repo <- "//Maison/SIE/Projets/Donnees_Mesange/1-BDD  LA PLUS RECENTE/1- Données (Démo, Morpho,Pous, Obs)/8-BDD validé/"

output <- "C:/Users/FARGEVIEILLE/Documents/GitHub/cefe_tit/outputs/"

morpho <- openxlsx::read.xlsx(paste0(repo, "SIE MORPH 1976-2024.xlsx"), detectDates = TRUE)
cha_odk_alt <- morpho %>% 
  
  # Select great tit individuals associated with a blood sample within the last 8 years from urban sites
  dplyr::filter(an <= max(as.numeric(an)) & an >= (max(as.numeric(an)) - 7) & !is.na(quantite_sang) & 
                  espece == "cha" & lieu %in% c("bot", "cef", "fac", "font", "gram", "mas", "mos", "zoo")) %>% 
  
  # sort by ID number and quantity of blood (descending) to keep the largest amount as the first occurrence
  dplyr::arrange(bague, desc(quantite_sang)) %>% 
  
  # Keep only columns of interest
  dplyr::select(espece, lieu, an, bague, sex, age, quantite_sang) %>% 
  
  # Keep only one row for each ID
  dplyr::distinct(bague, .keep_all = TRUE) 

#' Rename values to make it more explicit + adapt column names to ODK form
cha_odk <- cha_odk_alt %>% 
  dplyr::mutate(sex = dplyr::case_when(sex == "1" ~ "M",
                                sex == "2" ~ "F",
                                TRUE ~ "?"),
                min_age = dplyr::case_when(stringr::str_detect(age, "A") ~ as.numeric(stringr::str_remove(age, "[AIJP]")) + 1,
                                           TRUE ~ as.numeric(stringr::str_remove(age, "[AIJP]"))),
                age_2025 = min_age + (2025 - as.numeric(an))) %>% 
  dplyr::select(bague, espece, lieu, an, sex, age_2025, quantite_sang)


# save file as a .csv file (necessary for ODK form)
write.csv(cha_odk, paste0(output, "bird_id.csv"), row.names = FALSE, na = "")