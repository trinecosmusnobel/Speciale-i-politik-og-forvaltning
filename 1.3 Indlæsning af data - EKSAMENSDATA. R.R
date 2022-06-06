source("0.1 Libraries.R")

data_desc <- readxl::read_xlsx("Datarens/data/Karakterer Samlet.xlsx")

#Vi skal have én variabel med karakter til reeksamen og en med karakter til ordinær eksamen


data_desc <- data_desc %>%
  mutate(year = case_when(
    `2017_dummy` == 1 ~ "2017",
    `2018_dummy` == 1 ~ "2018",
    `2019_dummy` == 1 ~ "2019"
  )) %>% 
  mutate(har_været_til_reeksamen = case_when(
    is.na(ReekAndelPoint) ~ "Har ikke været til reeksamen",
    !is.na(ReekAndelPoint) ~ "Har været til reeksamen"
  ))

#alle eksamner som række? variable:
test_ordinær_ALLE <- data_desc %>% 
  dplyr::select(Studienr, 
                year, 
                Bestaaet_dummy, 
                Karakter, 
                Bedømmernavn, 
                AndelPoint, 
                SammenhaengOrdinaer, 
                BeskrivOrdinaer, 
                SurveyOrdinaer, 
                RegressionOrdinaer) %>% 
  mutate(type_eksamen = "alm.eksamen") %>% 
  filter(!is.na(AndelPoint)) %>% 
  mutate(id = row_number())

test_ordinær_reeksamen_næste_år <- test_ordinær_ALLE %>% 
  group_by(Studienr) %>% 
  mutate(n = n()) %>% 
  filter(n > 1,
         year == max(year)) %>% 
  mutate(type_eksamen = "reeksamen") %>% 
  dplyr::select(-n)

test_ordinær_ALLE #Alle ordinære eksamner "på papiret", dvs. det inkluderer dem, 
#der er gået til eksamen næste år OG dem der er bare er til alm. 
#ordinær eksamen

test_ordinær_reeksamen_næste_år <- test_ordinær_ALLE %>% 
  group_by(Studienr) %>% 
  mutate(n = n()) %>% 
  filter(n > 1,
         year == max(year)) %>% 
  mutate(type_eksamen = "reeksamen") %>% 
  dplyr::select(-n)

test_ordinær_reeksamen_næste_år #her har vi så dem UD af den forrige liste, som 
#FREMSTÅR som en ordinær eksam men som er nogle
#der har været til reeksamen - hvis et studienr
#optræder flere gange som ordinær eksamen,
#må det være fordi de er gået op næste år

test_ordinær_UDEN_reeksamen_næste_årtest <- test_ordinær_ALLE %>% 
  filter(!id %in% test_ordinær_reeksamen_næste_år$id) %>% 
  dplyr::select(-id)

test_ordinær_UDEN_reeksamen_næste_årtest  #her har vi KUN dem der er "almindelige"
#ordinære eksamner, altså IKKE dem der 
#er gået op næste år. 

alle_alm_eks_og_reeksamen_næste_år <- rbind(test_ordinær_UDEN_reeksamen_næste_årtest, test_ordinær_reeksamen_næste_år %>% dplyr::select(-id))

test_reeksamen <- data_desc %>%
  dplyr::select(Studienr, 
                year, 
                ReekBestået, 
                ReekKarakter, 
                ReekBedømmer, 
                ReekAndelPoint, 
                SammenhaengReek, 
                BeskrivReek, 
                SurveyReek, 
                RegressionReek) %>% 
  
  mutate(type_eksamen = "reeksamen") %>% 
  
  rename(
    Bestaaet_dummy = ReekBestået, 
    Karakter = ReekKarakter, 
    Bedømmernavn = ReekBedømmer, 
    AndelPoint = ReekAndelPoint, 
    SammenhaengOrdinaer = SammenhaengReek,
    BeskrivOrdinaer = BeskrivReek,
    SurveyOrdinaer = SurveyReek, 
    RegressionOrdinaer = RegressionReek
  ) %>% 
  
  filter(!is.na(AndelPoint))

NYESTE <- rbind(alle_alm_eks_og_reeksamen_næste_år, test_reeksamen) %>% mutate(id = row_number())

tre_eksamner <- NYESTE %>% 
  group_by(Studienr) %>% 
  mutate(n = n()) %>% 
  filter(n > 2) %>% 
  mutate(bestaa_antal = sum(Bestaaet_dummy)) %>% 
  filter(bestaa_antal <= 1)

tre_eksamner_BESTÅR <- tre_eksamner %>% 
  mutate(total_bestaa = sum(Bestaaet_dummy)) %>%
  filter(total_bestaa == 1) %>% 
  filter(Bestaaet_dummy == 1) %>%
  mutate(type_eksamen = "anden_reeksamen") %>% 
  dplyr::select(-n, -bestaa_antal, -total_bestaa)

tre_eksamner_BESTÅR_ALDRIG <- tre_eksamner %>% 
  mutate(total_bestaa = sum(Bestaaet_dummy)) %>% 
  filter(total_bestaa == 0, year == max(year)) %>% 
  mutate(type_eksamen = "anden_reeksamen") %>%
  dplyr::select(-n, -bestaa_antal, -total_bestaa)

NYESTE_uden_tredje <- NYESTE %>% 
  filter(!id %in% c(tre_eksamner_BESTÅR$id, tre_eksamner_BESTÅR_ALDRIG$id))

NYESTE_long <- rbind(NYESTE_uden_tredje, tre_eksamner_BESTÅR, tre_eksamner_BESTÅR_ALDRIG) %>% dplyr::select(-id)

NYESTE_long <- NYESTE_long %>% mutate(Studienr = as.numeric(Studienr))

NYESTE_long[ NYESTE_long == ANONYMISERET ] <- ANONYMISERET

eksamen_data <- NYESTE_long