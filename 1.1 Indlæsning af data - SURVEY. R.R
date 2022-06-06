source("0.1 Libraries.R")
source("0.2 Custom functions.R")


#Import af data 
df17 <- readxl::read_xlsx("Datarens/data/2017.xlsx", sheet = 4) %>% 
  mutate(dummy_2017 = 1) %>% 
  mutate(across(starts_with("Hvilke instruktor"), ~na_if(., 0))) %>% 
  rename_at(vars(starts_with('Hvilke instruktor')), 
            funs(
              paste("instruktor", 
                    gsub('Hvilke instruktor har du haft til opfølgninger\\?* \\(*hvis flere, sæt flere krydser\\)* - ', "", .), sep = "_"))) %>% 
  mutate(antal_studiecafeer = NA) 

df18 <- readxl::read_xlsx("Datarens/data/2018.xlsx") %>% 
  mutate(dummy_2018 = 1) %>% 
  mutate(across(starts_with("Hvilke(n)"), ~na_if(., 0))) %>% 
  rename_at(vars(starts_with('Hvilke(n)')), 
            funs(
              gsub(" ", "_", paste("instruktor", 
                                   gsub("Hvilke\\(*n\\)* instruktor\\(*er\\)* har du haft til opfølgninger\\?* \\(*hvis flere, sæt flere krydser\\)* - ", "", .), sep = "_"))))  %>% 
  mutate(antal_studiecafeer = NA)

df19 <- readxl::read_xlsx("Datarens/data/2019.xlsx") %>% 
  mutate(dummy_2019 = 1) %>% 
  mutate(across(starts_with("Hvilke(n)"), ~na_if(., 0))) %>% 
  rename_at(vars(starts_with('Hvilke(n)')), 
            funs(
              gsub(" ", "_", paste("instruktor", 
                                   gsub("Hvilke\\(*n\\)* instruktor\\(*er\\)* har du haft til opfølgninger\\?* \\(*hvis flere, sæt flere krydser\\)* - ", "", .), sep = "_")))) %>% 
  mutate(antal_studiecafeer = `Hvor mange gange har du deltaget i en Studiecafé?`) %>% 
  select(!`Hvor mange gange har du deltaget i en Studiecafé?`)

#Tilføjer dummy-kolonner for de instruktorer som ikke har været der det givne år
for (i in setdiff(
  c(colnames(df18 %>% select(starts_with("instruktor"))),
    colnames(df19 %>% select(starts_with("instruktor")))),
  colnames(df17 %>% select(starts_with("instruktor")))
)) {
  df17[i] <- NA
}

for (i in setdiff(
  c(colnames(df17 %>% select(starts_with("instruktor"))),
    colnames(df19 %>% select(starts_with("instruktor")))),
  colnames(df18 %>% select(starts_with("instruktor")))
)) {
  df18[i] <- NA
}

for (i in setdiff(
  c(colnames(df17 %>% select(starts_with("instruktor"))),
    colnames(df18 %>% select(starts_with("instruktor")))),
  colnames(df19 %>% select(starts_with("instruktor")))
)) {
  df19[i] <- NA
}

df17 <- df17 %>% select(
  c(
    colnames(df17 %>% select(!starts_with("instruktor"))),
    colnames(df17 %>% select(starts_with("instruktor")))
  )
)

df18 <- df18 %>% select(
  c(
    colnames(df18 %>% select(!starts_with("instruktor"))),
    colnames(df18 %>% select(starts_with("instruktor")))
  )
)

df19 <- df19 %>% select(
  c(
    colnames(df19 %>% select(!starts_with("instruktor"))),
    colnames(df19 %>% select(starts_with("instruktor")))
  )
)

#Merge af data
df <- bind_rows(df17,
                df18,
                df19)

colnames(df17)
colnames(df18)

df$dummy_2017[is.na(df$dummy_2017)] <- 0
df$dummy_2018[is.na(df$dummy_2018)] <- 0
df$dummy_2019[is.na(df$dummy_2019)] <- 0





df <- df %>% 
  mutate(year = case_when(
    dummy_2017 == 1 ~ "2017",
    dummy_2018  == 1 ~ "2018",
    dummy_2019 == 1 ~"2019" )) %>% rename(studienr_selvangivet = `Hvad er dit studienummer?`)

colnames(df) <- c(
  "koen", 
  "alder",	
  "rap_studienr_survey",	
  "rap_gymgennemsnit_foer_vaegt",
  "rap_niveau_mat",	
  "rap_karakter_mat_survey",	
  "y_fag",	
  "mor_udd",	
  "far_udd",	
  "passer_vanskeligt_problem1",	
  "passer_karakter_betyder_meget1",
  "passer_loese_svaere_opgaver1",	
  "passer_mat_sjovt1",	
  "passer_blokade_tal1",	
  "passer_dygtig_kvantitativ_metode1",	
  "tilfreds_forelaesers_paedag_niveau",	
  "tilfreds_undervisers_paedag_niveau",	
  "enig_undervisning_god_forstaaelse",	
  "enig_undervisning_statistik_praksis",	
  "antal_forelaesninger",	
  "antal_opfoelgninger",	
  "antal_timer_forberedelse",	
  "andel_pensum_laest",	
  "vurdering_arbejdsbyrde",	
  "vurdering_rustet_eksamen",	
  "laeringsudbytte_obligatorisk_opg",	
  "vurdering_faget_relevant", 
  "grad_lyst_arb_kvantitativ_metode", 
  "kommentarer", 
  "status_ny",	
  "status_distribueret",	
  "status_nogen_svar",	
  "status_gennemfoert",	
  "status_frafaldet",	
  "mail",
  "dummy_2017", 
  "antal_studiecafeer", 
  "instruktor_1",
  "instruktor_2",
  "instruktor_3",
  "instruktor_4",
  "instruktor_5",
  "instruktor_6",
  "instruktor_7",
  "instruktor_8",
  "instruktor_9",
  "instruktor_10",
  "instruktor_11",
  "instruktor_12",
  "instruktor_13",
  "passer_vanskeligt_problem2",	
  "passer_karakter_betyder_meget2",	
  "passer_loese_svaere_opgaver2",	
  "passer_mat_sjovt2",	
  "passer_blokade_tal2",	
  "passer_dygtig_kvantitativ_metode2", 
  "dummy_2018",	
  "dummy_2019", 
  "year")

variable_omkodning <- df %>% dplyr::select(starts_with("passer_"), vurdering_rustet_eksamen, grad_lyst_arb_kvantitativ_metode) %>% colnames()
df <- df %>% mutate_at(vars(variable_omkodning), recode_fun)

#Merging af kolonner m. samme indhold
df <- df %>% mutate(passer_vanskeligt_problem = coalesce(passer_vanskeligt_problem1,passer_vanskeligt_problem2),
                    passer_karakter_betyder_meget = coalesce(passer_karakter_betyder_meget1, passer_karakter_betyder_meget2),
                    passer_loese_svaere_opgaver = coalesce(passer_loese_svaere_opgaver1, passer_loese_svaere_opgaver2),
                    passer_mat_sjovt = coalesce(passer_mat_sjovt1, passer_mat_sjovt2),
                    passer_blokade_tal = coalesce(passer_blokade_tal1, passer_blokade_tal2),
                    passer_dygtig_kvantitativ_metode = coalesce(passer_dygtig_kvantitativ_metode1, passer_dygtig_kvantitativ_metode2)) %>% 
  select(!c(passer_vanskeligt_problem1,
            passer_vanskeligt_problem2,
            passer_karakter_betyder_meget1,
            passer_karakter_betyder_meget2,
            passer_loese_svaere_opgaver1, passer_loese_svaere_opgaver2,
            passer_mat_sjovt1, passer_mat_sjovt2,
            passer_blokade_tal1, passer_blokade_tal2,
            passer_dygtig_kvantitativ_metode1, passer_dygtig_kvantitativ_metode2))

variable_omkodning <- df %>% dplyr::select(starts_with("tilfreds_")) %>% colnames()
df <- df %>% mutate_at(vars(variable_omkodning), recode_fun2)

variable_omkodning <- df %>% dplyr::select(starts_with("enig_")) %>% colnames()
df <- df %>% mutate_at(vars(variable_omkodning), recode_fun3)

table(df$vurdering_arbejdsbyrde)

df <- df %>% mutate(vurdering_arbejdsbyrde = recode(vurdering_arbejdsbyrde,
                                                    "1. For stor" = 5,
                                                    "2." = 4,
                                                    "3. Passende" = 3,
                                                    "4." = 2,
                                                    "5. For lille" = 1))

df <- df %>%
  mutate_at(vars(laeringsudbytte_obligatorisk_opg), ~recode(.,
                                                            "5. Meget lille udbytte" = 1,
                                                            "4." = 2,
                                                            "3." = 3,
                                                            "2." = 4,
                                                            "1. Meget stort udbytte" = 5)) %>%
  mutate_at(vars(vurdering_faget_relevant), ~recode(.,
                                                    "5. Slet ikke relevant" = 1,
                                                    "4." = 2,
                                                    "3." = 3,
                                                    "2." = 4,
                                                    "1. Yderst relevant" = 5))


df$antal_forelaesninger <- gsub(" forelæsninger", "", df$antal_forelaesninger)
df$antal_forelaesninger <- as.numeric(gsub(" forelæsning", "", df$antal_forelaesninger))
df$antal_opfoelgninger <- gsub(" opfølgninger", "", df$antal_opfoelgninger)
df$antal_opfoelgninger <- as.numeric(gsub(" opfølgning", "", df$antal_opfoelgninger))

#Selvrapporteret karakter
df <- df %>%
  mutate_at(vars(rap_karakter_mat_survey), ~ recode(., 
                                                    ANONYMISERET))

df$rap_karakter_mat_survey[df$rap_karakter_mat_survey == "NA"] <- NA
df$rap_karakter_mat_survey <- gsub(",", ".", df$rap_karakter_mat_survey)
df$rap_karakter_mat_survey <- as.numeric(df$rap_karakter_mat_survey)


df$rap_gymgennemsnit_foer_vaegt[df$rap_gymgennemsnit_foer_vaegt == "ANONYMISERET"]

df <- df %>%
  mutate_at(vars(rap_gymgennemsnit_foer_vaegt), ~ recode(., 
                                                         ANONYMISERET)) 

df$rap_gymgennemsnit_foer_vaegt <- gsub(",", ".", df$rap_gymgennemsnit_foer_vaegt)
df$rap_gymgennemsnit_foer_vaegt <- as.numeric(df$rap_gymgennemsnit_foer_vaegt)
df$rap_gymgennemsnit_foer_vaegt[df$rap_gymgennemsnit_foer_vaegt == "NA"] <- NA

#Socioøkonomisk baggrund

#These are 1) the reference group containing no education
#beyond compulsory schooling (obligatorisk uddannelse), 2) vocational or apprenticeship, 3) intermediate levels of
#education leading to white collar qualifications and, 4) higher levels of education like
#university. For women, there were very few observations for categories 3 and 4 so these
#were grouped with the vocational category. (James McIntosh & Martin D. Munk, s. 11) 

df$far_udd <- gsub(df$far_udd, pattern = "\177 ", replacement = "")
df$mor_udd <- gsub(df$mor_udd, pattern = "\177 ", replacement = "")

df <- df %>% mutate(y_fag_gym = recode(y_fag,
                                       "Historie" = "Hum/Sam",
                                       "Dansk" = "Hum/Sam",
                                       "Engelsk" = "Hum/Sam",
                                       "Fysik/kemi" = "Nat/Mat",
                                       "Matematik" = "Nat/Mat",
                                       "Naturfag/biologi" = "Nat/Mat"),
                    far_udd  = recode(far_udd, 
                                      "Folkeskole 9. eller 10. klasse" = "Folkeskole/Gym/ingen uddannelse",
                                      "Folkeskole mindre end 9. klasse" = "Folkeskole/Gym/ingen uddannelse",
                                      "Ingen uddannelse" = "Folkeskole/Gym/ingen uddannelse",
                                      "Bachelor" = "Mellemlang udd.",
                                      "Erhvervsuddannelse med lærlinge- eller elevtid (faglært, assistent m.v.)" = "Kort vid. udd./Erhvervsudd.",
                                      "Kort videregående uddannelse" = "Kort vid. udd./Erhvervsudd.",
                                      "Realeksamen/gymnasial uddannelse (f.eks. gymnasium, HTX, HHX, HH, HF)" = "Folkeskole/Gym/ingen uddannelse",
                                      "Mellemlang videregående uddannelse" = "Mellemlang udd.", 
                                      "Lang videregående uddannelse" = "Lang udd./Phd",
                                      "Forskeruddannelse (f.eks. ph.d.)" = "Lang udd./Phd"),
                    far_udd = na_if(far_udd, "Ved ikke"),
                    mor_udd  = recode(mor_udd, 
                                      "Folkeskole 9. eller 10. klasse" = "Folkeskole/Gym/ingen uddannelse",
                                      "Folkeskole mindre end 9. klasse" = "Folkeskole/Gym/ingen uddannelse",
                                      "Ingen udannelse" = "Folkeskole/Gym/ingen uddannelse",
                                      "Bachelor" = "Mellemlang udd.",
                                      "Erhvervsuddannelse med lærlinge- eller elevtid (faglært, assistent m.v.)" = "Kort vid. udd./Erhvervsudd.",
                                      "Kort videregående uddannelse" = "Kort vid. udd./Erhvervsudd.",
                                      "Realeksamen/gymnasial uddannelse (f.eks. gymnasium, HTX, HHX, HH, HF)" = "Folkeskole/Gym/ingen uddannelse",
                                      "Mellemlang videregående uddannelse" = "Mellemlang udd.", 
                                      "Lang videregående uddannelse" = "Lang udd./Phd",
                                      "Forskeruddannelse (f.eks. ph.d.)" = "Lang udd./Phd"),
                    mor_udd = na_if(mor_udd, "Ved ikke")) 


#Dummy - referencegruppe: Ingen kompetencegivende uddannelse (ingen, folkeskole, gym)

df$far_højest_udd_dummy <- ifelse(df$far_udd == "Lang udd./Phd", 1, 0 )
df$far_mellem_udd_dummy <- ifelse(df$far_udd == "Mellemlang udd.", 1, 0 )
df$far_kort_erhverv_udd_dummy <- ifelse(df$far_udd == "Kort vid. udd./Erhvervsudd.", 1, 0 )
df$far_ingen_udd_dummy <- ifelse(df$far_udd == "Folkeskole/Gym/ingen uddannelse", 1, 0 )

df$mor_højest_udd_dummy <- ifelse(df$mor_udd == "Lang udd./Phd", 1, 0 )
df$mor_mellem_udd_dummy <- ifelse(df$mor_udd == "Mellemlang udd.", 1, 0 )
df$mor_kort_erhverv_udd_dummy <- ifelse(df$mor_udd == "Kort vid. udd./Erhvervsudd.", 1, 0 )
df$mor_ingen_udd_dummy <- ifelse(df$far_udd == "Folkeskole/Gym/ingen uddannelse", 1, 0 )

#antal timer forberedelse
df <- df %>% mutate(antal_timer_forberedelse = recode(antal_timer_forberedelse,
                                                      ANONYMISERET))

df$antal_timer_forberedelse <- gsub(",", ".", df$antal_timer_forberedelse)
df$antal_timer_forberedelse <- as.numeric(df$antal_timer_forberedelse)


#Andel pensum læst

#LAV TIL FAKTOR(pensum læst)
df <- df %>% mutate(andel_pensum_laest = recode(andel_pensum_laest,
                                                "0-25 %" = "1",
                                                "26-50 %" = "2",
                                                "51-75 %" = "3",
                                                "76-100 %" = "4")) %>% mutate(andel_pensum_laest = as.numeric(andel_pensum_laest))



#Studiecafe-delagelse - OBS: Kun data for 2019

unique(df$antal_studiecafeer)

excel_numeric_to_date(c(44593, 44654, 44717, 44780))

df <- df %>% mutate(antal_studiecafeer = recode(antal_studiecafeer,
                                                "0" = "0",
                                                "44593" = "1", #1-2
                                                "44654" = "2", #3-4,
                                                "44717" = "3", #5-6
                                                "44780" = "4",
                                                "9 eller flere" = "5"))



#Omkodning til indeks, dummies m.m pba. bl.a. teori

df <- df %>% mutate(stress_dummy = case_when(
  vurdering_arbejdsbyrde == 5 ~ 1, # Meget stor arbejdsbyrde,
  vurdering_arbejdsbyrde == 1 ~ 0,
  vurdering_arbejdsbyrde == 2 ~ 0,
  vurdering_arbejdsbyrde == 3 ~ 0,
  vurdering_arbejdsbyrde == 4 ~ 0))#Reesten))

df <- df %>% mutate(intrinsisk_motivation = (passer_dygtig_kvantitativ_metode + 
                                               grad_lyst_arb_kvantitativ_metode)/2)
df <- df %>% mutate(self_efficacy = (passer_vanskeligt_problem + passer_loese_svaere_opgaver)/2)

#Øvrige af carinas omkodninger

#Positiv indstillet over for tal (modsat talblokade)
df <- df %>% mutate(positiv_tal = 8 - passer_blokade_tal)

#Matematik-evne-indeks
df <- df %>% mutate(matematik_evne = (positiv_tal + passer_mat_sjovt)/2)

#rustet til eksamen omvendt (ikke rustet til eksamen)
df <- df %>% mutate(rustet_eksamen_vendt =  8-vurdering_rustet_eksamen)

#udbytte af undervisning
df <- df %>% mutate(udbytte_undervisning = (rustet_eksamen_vendt + enig_undervisning_god_forstaaelse + enig_undervisning_statistik_praksis)/3)

#Deltagelse i studiecafe (dummy)
df <- df %>% mutate(dummy_studiecafe = recode(antal_studiecafeer, 
                                                "0" = "0",
                                                "1" = "1",
                                                "2" = "1",
                                                "3" = "1",
                                                "4" = "1",
                                                "5" = "1"))

#Mat niveau højt eller lavt
df <- df %>% mutate(matniveau = recode(rap_niveau_mat,
                                       "A" = 2,
                                       "B" = 1,
                                       "C" = 1))



#Matematik i gym (niveau ganget med karakter)
df <- df %>% mutate(matematik_gym = matniveau*rap_karakter_mat_survey)

df <- df %>% mutate(alder = as.numeric(alder))

df$alder[df$alder > 99] <- NA
df$alder[df$alder < 18] <- NA
#Sammenligning af socioøkonomisk baggrund

df <- df %>% mutate(en_forældre_højest_udd_dummy = case_when(mor_højest_udd_dummy == 1 & far_højest_udd_dummy == 0 ~ 1,
                                                             mor_højest_udd_dummy == 0 & far_højest_udd_dummy == 1 ~ 1,
                                                             mor_højest_udd_dummy == 1 & far_højest_udd_dummy == 1 ~ 0,
                                                             mor_højest_udd_dummy == 0 & far_højest_udd_dummy == 0 ~ 0))



df$begge_forældre_højest_udd_dummy <- ifelse(df$mor_højest_udd_dummy == 1 & df$far_højest_udd_dummy == 1, 1, 0)



full_data <- df

