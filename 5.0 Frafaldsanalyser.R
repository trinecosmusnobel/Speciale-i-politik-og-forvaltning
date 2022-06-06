############

options(scipen = 100, digits = 4)
options(encoding = "UTF-8")
source("2.1 Join af dataset.R")

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

############

o <- df %>% 
  select(year, status_gennemfoert, AndelPoint) %>% 
  group_by(year, status_gennemfoert) %>% 
  mutate(mean = mean(AndelPoint, na.rm = TRUE), 
         n = n()) %>% 
  distinct(year, status_gennemfoert, .keep_all = TRUE) %>% 
  filter(status_gennemfoert %in% c(0, 1)) %>% 
  mutate(mean = mean * 100)

o17 <- df %>% filter(year == 2017)
o18 <- df %>% filter(year == 2018)
o19 <- df %>% filter(year == 2019)

t <- cbind( #Sammensæt tabel
  
  #fuldført
  paste(round(o %>% arrange(year) %>% filter(status_gennemfoert == 1) %>% pull(mean), digits = 3), " %",  ", n = ",  o %>% arrange(year) %>% filter(status_gennemfoert == 1) %>% pull(n), sep = ""),
  
  paste(round(o %>% arrange(year) %>% filter(status_gennemfoert == 0) %>% pull(mean), digits = 3), " %", ", n = ",  o %>% arrange(year) %>% filter(status_gennemfoert == 0) %>% pull(n), sep = ""), 
  
  #difference
  paste(
    round(c(o %>% arrange(year) %>% filter(status_gennemfoert == 1) %>% pull(mean)) - 
            c(o %>% arrange(year) %>% filter(status_gennemfoert == 0) %>% pull(mean)), digits = 3), 
    
    c(
      gtools::stars.pval(t.test(o17$status_gennemfoert, o17$AndelPoint)$p.value),
      gtools::stars.pval(t.test(o18$status_gennemfoert, o18$AndelPoint)$p.value),
      gtools::stars.pval(t.test(o19$status_gennemfoert, o19$AndelPoint)$p.value)
    ), sep = ""
  )  
  )

colnames(t) <- c("Fuldført", "Frafaldet", "Difference")
rownames(t) <- 2017:2019

t

t %>% 
  kable(format = "latex", caption = "Frafaldsanalyse med andel point som afhængig variabel. 
        P-værdien er udregnet med independent samples T-test, 
        og differencen er angivet i procenpoint.", label = "test", booktabs = TRUE) %>% 
  kable_classic_2(full_width = F, html_font = "Garamond") %>% 
  row_spec(0, bold = TRUE)
  


  
  
  