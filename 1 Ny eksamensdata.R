#ny_eksamensdata <- readRDS("ny_eksamensdata.rds") #Mac version
ny_eksamensdata <- readRDS("ny_eksamensdata.rds")

ny_eksamensdata <- ny_eksamensdata %>% rename(
  "Studienr" = "studnr",
  "Bestaaet_dummy" = "bestaaet",
  "Karakter" = "karakter",
  "Bedømmernavn" = "bedømmer",
  "AndelPoint" = "AndelPoint_test",
)

ny_eksamensdata_karakter <- ny_eksamensdata %>%
  filter(year != 2017) %>% 
    mutate(Bestaaet_dummy = case_when(
      Karakter >= 2 ~ 1,
      Karakter <= 0 ~ 0
    ))

ny_eksamensdata_ikke_karakter <- ny_eksamensdata %>% filter(year == 2017)

ny_eksamensdata <- bind_rows(ny_eksamensdata_karakter, ny_eksamensdata_ikke_karakter)
