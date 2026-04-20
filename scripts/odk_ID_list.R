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
#' object "repo2" directs to the temporary folder for for ongoing record of data
#' I am hiding the address to these repositories
repo <- 

repo1 <- 

repo2 <- 

#' object "ouput" is the path where the list will be saved
output <- "~/GitHub/cefe_tit/outputs/"


#' Call the formatted dataset containing each bird history
ind <- read_csv(paste0(repo1, "Individual_data_MON.csv"))


#'Indicate the current year and the historical year (ten years before current year)
current_year <- 2026
history_year <- current_year - 10

#' Filter to only keep targetted species and populations
#' Filter to keep birds tagged after history_year
#' Rename some columns to be more explicit in the app
ind1 <- ind %>% 
  dplyr::filter(speciesID %in% c("CYACAE", "PARMAJ") & siteID %in% c("MON", "MUR", "PIR", "ROU") & tagYear > history_year) %>% 
  dplyr::mutate(espece = dplyr::case_when(speciesID == "CYACAE" ~ "bleue",
                                          speciesID == "PARMAJ" ~ "charbo",
                                          TRUE ~ "autre"),
                LaidSite = stringr::str_split_i(broodIDLaid, "_", 2),
                LaidNest = stringr::str_split_i(broodIDLaid, "_", 3),
                LaidNestbox = paste(LaidSite, LaidNest, sep = "_"),
                nic_naiss = dplyr::case_when(LaidNestbox == "NA_NA" ~ NA_character_,
                                        TRUE ~ LaidNestbox),
                age_current = dplyr::case_when(tagStage == "chick" ~ current_year - tagYear,
                                            tagStage == "subadult" ~ current_year - (tagYear - 1),
                                            TRUE ~ current_year - (tagYear - 2))) %>% 
  dplyr::select(individualID, espece, lieu_naiss = "LaidSite", nic_naiss, age_capture = "tagStage", age_current, calculatedSex) 
  

#' Call the primary dataset containing information about (breeders) capture events
#' From this, I'd like to extract several information
#'  - the Darvic (or colour rings but we have to see if this gives info) -- DONE
#'  - the plot where the bird was first tagged (for birds tagged as breeder) -- DONE
#'  - the highest quantity of blood sampled -- DONE
#'  
#'  - the number of capture events - DONE
#'  - the number of OF performed (extract info "OF" from exp_ad) - DONE
#'  - the number of cognition tests performed (extract info "cog" from comments) - DONE

morpho_old <- openxlsx::read.xlsx(paste0(repo, "SIE MORPH 1976-2025_v1.xlsx"), detectDates = TRUE)
#morpho_new <- openxlsx::read.xlsx(paste0(repo2, "MORPH_2025.xlsx"), detectDates = TRUE)
#morpho <- rbind(morpho_old, morpho_new)

pous <- openxlsx::read.xlsx(paste0(repo, "SIE POUS 1976-2025_v1.xlsx"), detectDates = TRUE)
#'Let's start by filter targetted species, targetted population, targetted years

morpho1 <- morpho_old %>% 
  dplyr::filter(espece %in% c("ble", "cha") & an > history_year &
                  # sites from MON & ROU populations
                  lieu %in% c("bot", "cef", "fac", "font", "gram", "mas", "mos", "zoo", "rou",
                              # sites from MUR & PIR populations
                              "ava", "ari", "fel", "fil", "gra", "mur", "pir", "tua")) %>% 
  dplyr::mutate(date = format(as.Date(date_mesure, format = "%d/%m/%Y"), format = "%d/%m/%Y")) %>% 
  dplyr::select(lieu, an, espece, nic, date, fil, action, bague, quantite_sang, exp_ad, bague_coul, commentaire)

pous1 <- pous %>%
  dplyr::filter(espece %in% c("ble", "cha") & an > history_year &
                  # sites from MON & ROU populations
                  lieu %in% c("bot", "cef", "fac", "font", "gram", "mas", "mos", "zoo", "rou",
                              # sites from MUR & PIR populations
                              "ava", "ari", "fel", "fil", "gra", "mur", "pir", "tua")) %>%
  dplyr::mutate(date = format(as.Date(date_mesure, format = "%d/%m/%Y"), format = "%d/%m/%Y")) %>%
  dplyr::select(lieu, an, espece, nic, date, action, bague)


# getting the info where the bird was tagged 
first_tag <- morpho1 %>% 
  arrange(as.Date(date, format = "%d/%m/%Y")) %>% 
  dplyr::group_by(bague) %>% 
  dplyr::mutate(first(bague)) %>% 
  dplyr::select(bague, site = "lieu") %>% 
  dplyr::distinct(bague, .keep_all = TRUE)

bird_id <- dplyr::left_join(ind1, first_tag, by = c("individualID" = "bague"))  %>% 
  dplyr::mutate(lieu_baguage = dplyr::case_when(is.na(lieu_naiss) ~ site,
                                         TRUE ~ lieu_naiss)) %>% 
  dplyr::select(bague = "individualID", espece, sexe = "calculatedSex", age_capture, age_current, lieu_bag = "lieu_baguage", nic_naiss)

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
  dplyr::select(bague, autre_bague, espece, sexe, age_capture, age_current, lieu_bag, nic_naiss)

# getting the info about blood quantity
blood <- morpho1 %>% 
  dplyr::filter(!is.na(quantite_sang)) %>% 
  dplyr::arrange(quantite_sang) %>% 
  dplyr::distinct(bague, .keep_all = TRUE) %>% 
  dplyr::select(bague, quantite_sang)

bird_id_2 <- dplyr::left_join(bird_id_1, blood, by = "bague") %>% 
  dplyr::mutate(sang_ok = dplyr::case_when(quantite_sang >= 10 ~ "oui",
                                           TRUE ~ "non")) %>% 
  dplyr::select(bague, autre_bague, espece, sexe, age_capture, age_current, lieu_bag, nic_naiss, sang_ok)


# retrieving the info about the number of capture events
cap <- morpho1 %>% 
  dplyr::filter(fil != 5) %>% 
  dplyr::bind_rows(pous1) %>%
  dplyr::distinct(bague, date, .keep_all = TRUE) %>% 
  dplyr::group_by(bague) %>% 
  dplyr::summarise(nb_captures = n(),
                   last_mesure = last(date))

bird_id_3 <- left_join(bird_id_2, cap, by = "bague") 

# getting info about last season

# retrieving the info about the number of OF record/Cognition record
 of_cog <- morpho1 %>%
   dplyr::mutate(of = dplyr::case_when(stringr::str_detect(exp_ad, "OF") ~ 1,
                                     TRUE ~ 0),
                 cog = dplyr::case_when(stringr::str_detect(exp_ad, "COG") ~ 1,
                                        TRUE ~ 0)) %>%
   dplyr::group_by(bague) %>%
   dplyr::summarise(nb_of = sum(of),
                    nb_cog = sum(cog))


 bird_id_final <- dplyr::left_join(bird_id_3, of_cog, by = "bague") %>%
   dplyr::mutate(across(c("nb_captures", "nb_of", "nb_cog"), ~ dplyr::case_when(is.na(.) ~ 0,
                                                                                    TRUE ~ .)),
                 age_capture = dplyr::case_when(age_capture == "chick" ~ "poussin",
                                                age_capture == "subadult" ~ "juvénile",
                                                age_capture == "adult" ~ "adulte",
                                                TRUE ~ "indéterminé"))
 
# separate corsican birds from mainland birds

bird_id_corse <- bird_id_final %>% 
  dplyr::filter(lieu_bag %in% c("ava", "ari", "fel", "fil", "gra", "mur", "pir", "tua"))
bird_id_mtp <- bird_id_final %>% 
  dplyr::filter(lieu_bag %in% c("bot", "cef", "fac", "font", "gram", "mas", "mos", "rou", "zoo"))


# save file as a .csv file (necessary for ODK form)

write.csv(bird_id_corse, paste0(output, "bird_id_corse.csv"), row.names = FALSE, na = "")
write.csv(bird_id_mtp, paste0(output, "bird_id_mtp.csv"), row.names = FALSE, na = "")



# organize entity files (removing birds only captured as chicks)
cap_v2 <- morpho1 %>% 
  dplyr::filter(fil != 5) %>% 
  dplyr::distinct(bague, date, .keep_all = TRUE) %>% 
  dplyr::group_by(bague) %>% 
  dplyr::summarise(nb_captures = n(),
                   last_mesure = last(date))

bird_id_3_v2 <- left_join(bird_id_2, cap_v2, by = "bague") %>% 
  #Remove birds that were never captured as subadult or adult
  dplyr::filter(!is.na(nb_captures))

bird_id_final_v2 <- dplyr::left_join(bird_id_3_v2, of_cog, by = "bague") %>%
  dplyr::mutate(across(c("nb_captures", "nb_of", "nb_cog"), ~ dplyr::case_when(is.na(.) ~ 0,
                                                                               TRUE ~ .)),
                age_capture = dplyr::case_when(age_capture == "chick" ~ "poussin",
                                               age_capture == "subadult" ~ "juvénile",
                                               age_capture == "adult" ~ "adulte",
                                               TRUE ~ "indéterminé")) %>% 
  #Remove unbanded individuals who died during capture (ID starts with an O)
  dplyr::filter(!(stringr::str_detect(bague, "O"))) 
  



ind_ct_corse <- bird_id_final_v2 %>% 
  dplyr::filter(lieu_bag %in% c("ava", "ari", "fel", "fil", "gra", "mur", "pir", "tua")) %>% 
  select(label = "bague", last_mesure, sang_ok)


ind_ct_mtp <- bird_id_final_v2 %>% 
  dplyr::filter(lieu_bag %in% c("bot", "cef", "fac", "font", "gram", "mas", "mos", "rou", "zoo")) %>% 
  select(label = "bague", last_mesure, sang_ok)



# save file as a .csv file (necessary for ODK form)

write.csv(ind_ct_corse, paste0(output, "ind_ct_corse.csv"), row.names = FALSE, na = "")
write.csv(ind_ct_mtp, paste0(output, "ind_ct_mtp.csv"), row.names = FALSE, na = "")
