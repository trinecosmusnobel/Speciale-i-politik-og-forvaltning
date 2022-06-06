source("1.1 Indlæsning af data - SURVEY. R.R")
source("1.2 Tjek af studienumre.R") #Indlæs eksamensdata
source("1.3 Indlæsning af data - EKSAMENSDATA. R.R")
source("1.4 Indlæsning af data - MAILS OG STUDIENUMRE. R.R")

mail_data <- mail_data %>% rename(Studienr = studienr,
                                  ÅR = year)


#Datasæt:

full <- as_tibble(plyr::join(ny_eksamensdata, mail_data, type = "left", by = "Studienr", match = "first")) 
#full <- as_tibble(plyr::join(eksamen_data, mail_data, type = "left", by = "Studienr", match = "first")) 
### ^ den gamle eksamendata-fil
full <- as_tibble(plyr::join(full, karakter_data, type = "left", by = "Studienr", match = "first"))
full <- as_tibble(plyr::join(full, full_data, type = "left", by = c("mail", "year"), match = "first"))


navne <- readxl::read_xlsx("Navne og studienumre i 2018.xlsx") %>% distinct(Studienr, .keep_all = TRUE) 

full2018 <- full %>% filter(year == 2018) %>% distinct(Studienr, .keep_all = TRUE) %>% select(-name)
  
full2018 <- left_join(full2018, navne) 

full_not_2018 <- full %>% filter(year != 2018)

full <- bind_rows(full2018, full_not_2018)

full <- full %>% 
  mutate(name = coalesce(name, Fornavn), 
         gender_coded = eliter::code.gender(name)) %>%
  mutate(gender_coded = recode(gender_coded,
                                      "Women" = "Kvinde", 
                                      "Men" = "Mand")) 

full <- full %>%
  mutate(Bedømmernavn = case_when(
    Studienr %in% 
      
      c(ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET)
      ~ "ANONYMISERET",
    
    TRUE ~ Bedømmernavn)) %>% 
  
  
  mutate(Bedømmernavn = case_when(
    Eks.nr %in% 
      
      c(3,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET,
        ANONYMISERET)
    ~ "ANONYMISERET",
    
    TRUE ~ Bedømmernavn)) 

full <- full %>% group_by(Studienr) %>%
  mutate(group_id = cur_group_id()) %>% 
  ungroup() %>% 
  select(-c(Studienr, mail)) %>% 
  mutate(year = as.numeric(year))

full_valid        <- full %>% group_by(group_id) %>% filter(n() < 2)
full_invalid      <- full %>% group_by(group_id) %>% filter(n() > 1) %>% slice(which.min(year))

full <- bind_rows(full_valid, full_invalid) %>% select(-name, -Fornavn, -Efternavn, -Eks.nr)

full$gender_coded[full$gender_coded == "Binominal"] = NA

full <- full %>% mutate(KVINDE_dummy = as.character(case_when(gender_coded == "Kvinde" ~ 1, 
                                                              gender_coded == "Mand" ~ 0)))


full <- full %>% ungroup() %>%
  #  filter(type_eksamen == "alm.eksamen") %>% 
  mutate(eksamensform_dummy = as.numeric(recode(year,
                                                "2017" = 0,
                                                "2018" = 1,
                                                "2019" = 1)), 
         #        AndelPoint_vores = (SurveyOrdinaer + BeskrivOrdinaer + RegressionOrdinaer + SammenhaengOrdinaer)/4, 
         merit_dummy = recode(merit_dummy, 
                              "Ja" = 1, 
                              "Nej" = 0)) %>% 
  mutate(AndelPoint_vores = AndelPoint) %>% mutate(mindst_en_højtudd = 
                                                     en_forældre_højest_udd_dummy + 
                                                     begge_forældre_højest_udd_dummy, 
                                                   mindst_en_højtudd = recode(mindst_en_højtudd,
                                                                              "1" = 1, 
                                                                              "2" = 1,
                                                                              "0" = 0)) %>% mutate(eksamensform_dummy = as.numeric(recode(year,
                                                                                                                                          "2019" = 1,
                                                                                                                                          "2018" = 1,
                                                                                                                                          "2017" = 0)))  %>% mutate(Bedømmernavn = tolower(Bedømmernavn),
                                                                                                                                                                    bedømmer_anon = recode(Bedømmernavn,
                                                                                                                                                                                           "ANONYMISERET" = "Bedømmer_1",
                                                                                                                                                                                           "ANONYMISERET" = "Bedømmer_2",
                                                                                                                                                                                           "ANONYMISERET" = "Bedømmer_3",
                                                                                                                                                                                           "ANONYMISERET" = "Bedømmer_4",
                                                                                                                                                                                           "ANONYMISERET" = "Bedømmer_5",
                                                                                                                                                                                           "ANONYMISERET" = "Bedømmer_6",
                                                                                                                                                                                           "ANONYMISERET" = "Bedømmer_6",
                                                                                                                                                                                           "ANONYMISERET" = "Bedømmer_7",
                                                                                                                                                                                           "ANONYMISERET" = "Bedømmer_8"
                                                                                                                                                                                           
                                                                                                                                                                    ))

full$ingen_forældre_højest_udd_dummy <- ifelse(full$mor_højest_udd_dummy == 0 & full$far_højest_udd_dummy == 0, 1, 0)
