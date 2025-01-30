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
repo <- 

output <- "C:/Users/FARGEVIEILLE/Documents/GitHub/cefe_tit/outputs/"


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


# I can make some values more explicit (sex of individuals, add a minimal age)
# I also need to change column names to fit in the ODK form

# save file as a .csv file (necessary for ODK form)
write.csv(cha_odk, paste0(output, "liste_ID_ville.csv"), row.names = TRUE, na = "")