ny_eksamensdata <- readRDS("~/Downloads/ny_eksamensdata.rds")

ny_eksamensdata <- ny_eksamensdata %>% rename(
  "Studienr" = "studnr",
  "Bestaaet_dummy" = "bestaaet",
  "Karakter" = "karakter",
  "Bedømmernavn" = "bedømmer",
  "AndelPoint" = "AndelPoint_test",
)


