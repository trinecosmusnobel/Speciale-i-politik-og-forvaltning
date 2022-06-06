source("2.1 Join af dataset.R")

#######
df <- full %>% ungroup() %>%
  #  filter(type_eksamen == "alm.eksamen") %>% 
  mutate(eksamensform_dummy = as.numeric(recode(year,
                                                "2017" = 0,
                                                "2018" = 1,
                                                "2019" = 1)), 
         #        AndelPoint_vores = (SurveyOrdinaer + BeskrivOrdinaer + RegressionOrdinaer + SammenhaengOrdinaer)/4, 
         merit_dummy = recode(merit_dummy, 
                              "Ja" = 1, 
                              "Nej" = 0)) %>% 
  mutate(AndelPoint_vores = AndelPoint)

#######

df$gender_coded_num <- ifelse(df$gender_coded == "Kvinde", 1, 0 )
df$STX_d <- ifelse(df$Adg_type_dummy == "STX", 1, 0 )
df$HF_d <- ifelse(df$Adg_type_dummy == "HF", 1, 0 )
df$HHX_d <- ifelse(df$Adg_type_dummy == "HHX", 1, 0 )
df$HTX_d <- ifelse(df$Adg_type_dummy == "HTX", 1, 0 )
df$IB_d <- ifelse(df$Adg_type_dummy == "International", 1, 0 )

#######

vars <- readxl::read_xlsx("Variable_til_tabel.xlsx")

df %>% select(vars$Variabel)

get_desc <- function(x) {
  
  c(
    "Variabel" = x,
    "Min" = round(min(df %>% pull(x), na.rm = TRUE), digits = 2),
    "Max" = round(max(df %>% pull(x), na.rm = TRUE), digits = 2),
    "Mean" = round(mean(df %>% pull(x), na.rm = TRUE), digits = 2),
    "Median"= round(median(df %>% pull(x), na.rm = TRUE), digits = 2),
    "Standardafvigelse" = round(sd(df %>% pull(x), na.rm = TRUE), digits = 2)
  )
  
}

e <- as.data.frame(do.call(rbind, lapply(vars$Variabel, get_desc)))

e <- e %>% 
  as_tibble() 

e

andel <- function(x) {
  
  paste(round(prop.table(table(df %>% pull(x)))[2]*100, digits = 2), "%")
  
}

w <- left_join(vars, e)

w$Andel <- unlist(lapply(w$Variabel, andel))

w$Andel[w$Dummy == 0] <- ""
w$Min[w$Dummy == 1] <- ""
w$Max[w$Dummy == 1] <- ""
w$Mean[w$Dummy == 1] <- ""
w$Median[w$Dummy == 1] <- ""
w$Standardafvigelse[w$Dummy == 1] <- ""

w

desc_cron <- function(x) {
  
  i <- w %>% filter(Variabel == x)
  i
  c <- ltm::cronbach.alpha(df %>% select(i$VarInd1, i$VarInd2), na.rm = TRUE)
  c 
   c(i$Variabel,
   round(c$alpha, digits = 3))
}

e <- as.data.frame(do.call(rbind, lapply(vars %>% filter(Index == 1) %>% pull(Variabel), desc_cron)))




colnames(e) <- c("Variabel", "Cronbach's alpha")


e <- left_join(w, e)

e$Max <- as.character(round(as.numeric(e$Max), digits = 2))

e[is.na(e)] <- ""

e

e <- e %>% arrange(Dummy, Index) %>% select(Navn_på_variabel, Min, Max, Mean, Median, Standardafvigelse, Andel, `Cronbach's alpha`, Label)


e %>% 
  kable(format = "latex", 
        caption = "Variable i analysen", 
        label = "variabel", 
        booktabs = TRUE,
        col.names = c("Variabel", 
                      "Min.", 
                      "Max.", 
                      "Mean", 
                      "Median", 
                      "SD", 
                      "Andel",
                      "Cronbach", 
                      "Beskrivelse")) %>% 
  kable_classic_2(full_width = F) %>% 
  row_spec(0, bold = TRUE) %>% 
  column_spec(9, width = "6cm") %>% 
  column_spec(1, width = "3cm") %>% 
  column_spec(2:6, width = "0.75cm") %>% 
  column_spec(7, width = "1.2cm") %>% 
  column_spec(8, width = "3cm") %>% 
  kable_styling(font_size = 6, latex_options = "striped", position = "center") %>% 
  kableExtra::landscape()








