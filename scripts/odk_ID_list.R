#-----------------------------------------------------------------------------------------
#Bird ID and history
#' --------------------------------
#' 
#' The idea is to prepare a file that people can check in the field.
#' It is related to an ODK form - When entering the bird ID (metal band or Darvic)
#' in the form, this triggers data associated with the bird
#' It can help fieldworkers decide whether they sample blood, perform a behavior test,
#' Or also double check the ID if this does not fit the history of the bird 
#' (e.g. if they handle a blue tit in Corsica and the ring ID corresponds to a great tit in Montpellier)
#' 
library(tidyverse)
library(openxlsx)

#'
#' object "repo" directs to the folder with the updated (primary) data. 
#' object "repo1" directs to the folder with SPI-Birds formatted data. 
#' I am hiding the address to these repositories
repo <-
repo1 <- 

#' object "ouput" is the path where the list will be saved
output <- "C:/Users/FARGEVIEILLE/Documents/GitHub/cefe_tit/outputs/"


#' Call the formatted dataset containing each bird history
ind <- readRDS(paste0(repo1, "Individual_data_MON.rds"))

#' Filter to only keep targetted species and populations
#' Filter to keep birds tagged after 2016
#' Rename some columns to be more explicit in the app
ind1 <- ind %>% 
  dplyr::filter(speciesID %in% c("CYACAE", "PARMAJ") & siteID %in% c("MON", "MUR", "PIR", "ROU") & tagYear > 2016) %>% 
  dplyr::mutate(espece = dplyr::case_when(speciesID == "CYACAE" ~ "bleue",
                                          speciesID == "PARMAJ" ~ "charbo",
                                          TRUE ~ "autre"),
                LaidSite = stringr::str_split_i(broodIDLaid, "_", 2),
                LaidNest = stringr::str_split_i(broodIDLaid, "_", 3),
                LaidNestbox = paste(LaidSite, LaidNest, sep = "_"),
                nic_naiss = dplyr::case_when(LaidNestbox == "NA_NA" ~ NA_character_,
                                        TRUE ~ LaidNestbox),
                age_2025 = dplyr::case_when(tagStage == "chick" ~ 2025 - tagYear + 1,
                                            tagStage == "subadult" ~ 2025 - tagYear,
                                            TRUE ~ 2025 - tagYear -1)) %>% 
  dplyr::select(individualID, espece, lieu_naiss = "LaidSite", nic_naiss, age_2025, calculatedSex) 
  

#' Call the primary dataset containing information about (breeders) capture events
#' From this, I'd like to extract several information
#'  - the Darvic (or colour rings but we have to see if this gives info) -- DONE
#'  - the plot where the bird was first tagged (for birds tagged as breeder) -- DONE
#'  - the highest quantity of blood sampled -- DONE
#'  
#'  - the number of capture events - DONE
#'  - the number of OF performed (extract info "OF" from exp_ad) - DONE
#'  - the number of cognition tests performed (extract info "cog" from comments) - DONE

morpho <- openxlsx::read.xlsx(paste0(repo, "SIE MORPH 1976-2024.xlsx"), detectDates = TRUE)

#'Let's start by filter targetted species, targetted population, targetted years

morpho1 <- morpho %>% 
  dplyr::filter(espece %in% c("ble", "cha") & an > 2016 &
                  # sites from MON & ROU populations
                  lieu %in% c("bot", "cef", "fac", "font", "gram", "mas", "mos", "zoo", "rou",
                              # sites from MUR & PIR populations
                              "ava", "ari", "fel", "fil", "gra", "mur", "pir", "tua")) %>% 
  dplyr::mutate(date = format(as.Date(date_mesure, format = "%d/%m/%Y"), format = "%d/%m/%Y")) %>% 
  dplyr::select(lieu, an, espece, nic, date, fil, action, bague, quantite_sang, exp_ad, bague_coul, commentaire)

# getting the info where the bird was tagged 
first_tag <- morpho1 %>% 
  dplyr::filter(action == "bcj") %>% 
  dplyr::select(bague, site = "lieu") %>% 
  dplyr::distinct(bague, .keep_all = TRUE)

bird_id <- dplyr::left_join(ind1, first_tag, by = c("individualID" = "bague"))  %>% 
  dplyr::mutate(lieu_baguage = dplyr::case_when(is.na(lieu_naiss) ~ site,
                                         TRUE ~ lieu_naiss)) %>% 
  dplyr::select(bague = "individualID", espece, sexe = "calculatedSex", age_2025, lieu_bag = "lieu_baguage", nic_naiss)

# getting the info about Darvic or color ring combination
darvic <- morpho1 %>% 
  dplyr::filter(!is.na(bague_coul) & espece == "cha") %>% 
  dplyr::mutate(darvic = stringr::str_remove(bague_coul, "metal"),
                darvic = stringr::str_remove_all(darvic, "/"),
                darvic = stringr::str_to_upper(darvic)) %>% 
  dplyr::select(bague, darvic) %>% 
  dplyr::distinct(bague, .keep_all = TRUE)

bague_coul <- morpho1 %>% 
  dplyr::filter(!is.na(bague_coul) & espece == "ble") %>% 
  dplyr::mutate(bague_coul = stringr::str_to_lower(bague_coul)) %>% 
  dplyr::select(bague, bague_coul) %>% 
  dplyr::distinct(bague, .keep_all = TRUE)

bird_id_1 <- dplyr::left_join(bird_id, darvic, by = "bague") %>% 
  dplyr::left_join(bague_coul, by = "bague") %>% 
  dplyr::mutate(autre_bague = dplyr::case_when(espece == "charbo" ~ darvic,
                                               TRUE ~ bague_coul)) %>% 
  dplyr::select(bague, autre_bague, espece, sexe, age_2025, lieu_bag, nic_naiss)

# getting the info about blood quantity
blood <- morpho1 %>% 
  dplyr::filter(!is.na(quantite_sang)) %>% 
  dplyr::arrange(quantite_sang) %>% 
  dplyr::distinct(bague, .keep_all = TRUE) %>% 
  dplyr::select(bague, quantite_sang)

bird_id_2 <- dplyr::left_join(bird_id_1, blood, by = "bague") %>% 
  dplyr::mutate(sang_ok = dplyr::case_when(quantite_sang > 10 ~ "oui",
                                           quantite_sang < 10 ~ "non",
                                           TRUE ~ "non")) %>% 
  dplyr::select(bague, autre_bague, espece, sexe, age_2025, lieu_bag, nic_naiss, sang_ok)


# retrieving the info about the number of capture events
cap <- morpho1 %>% 
  dplyr::filter(fil != 5) %>% 
  dplyr::distinct(bague, date, .keep_all = TRUE) %>% 
  dplyr::group_by(bague) %>% 
  dplyr::summarise(number_captures = n())

bird_id_3 <- left_join(bird_id_2, cap, by = "bague") 

# retrieving the info about the number of OF record/Cognition record
of_cog <- morpho1 %>% 
  dplyr::mutate(of = dplyr::case_when(stringr::str_detect(exp_ad, "OF") ~ 1,
                                      TRUE ~ 0),
                cog = dplyr::case_when(stringr::str_detect(exp_ad, "COGNITION") ~ 1,
                                       TRUE ~ 0)) %>% 
  dplyr::group_by(bague) %>% 
  dplyr::summarise(nb_of = sum(of),
                   nb_cog = sum(cog))
  

bird_id_final <- dplyr::left_join(bird_id_3, of_cog, by = "bague")
 

# save file as a .csv file (necessary for ODK form)
write.csv(bird_id_final, paste0(output, "bird_id.csv"), row.names = FALSE, na = "")