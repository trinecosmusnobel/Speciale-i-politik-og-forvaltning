options(encoding = "UTF-8")

source("2.1 Join af dataset.R")

library(lavaan)
library(semPlot)
library(glasso)
library(mosaic)
library(tidySEM)
library(semptools)
library(tikzDevice)
library(semTools)
library(semTable)


#SEM I VORES POJEKT
#Omkodning af variable til nummeriske

#Sem dataset
full.sem <- full %>% ungroup() %>% mutate(Kvinde = recode(gender_coded,
                                                        "Kvinde" = 1,
                                                        "Mand" = 0),
                                          Mand = recode(gender_coded,
                                                          "Mand" = 1,
                                                          "Kvinde" = 0),
                                          Bedømmelsesform  = recode(year, 
                                                                 "2017" = 0,
                                                                 "2018" = 1,
                                                                 "2019" = 1),
                                          merit  = recode(merit_dummy,
                                                          "Nej"  = 0,
                                                          "Ja"   = 1),
                                          Løse.Vanskelige.Problemer = passer_vanskeligt_problem,
                                          Løse.Svære.Opgaver = passer_loese_svaere_opgaver,
                                          Stress = stress_dummy,
                                          DygtigKvantMetode = passer_dygtig_kvantitativ_metode,
                                          LystArbejdeKvantMetode = grad_lyst_arb_kvantitativ_metode,
                                          Adg.karakter = Adg_kvotient,
                                          PositivTal = positiv_tal,
                                          MatematikSjovt = passer_mat_sjovt,
                                          MorLangUdd = mor_højest_udd_dummy,
                                          FarLangUdd = far_højest_udd_dummy,
                                          VurderingArbejdsbyrde = vurdering_arbejdsbyrde,
                                          BeggeForældreLangUdd = begge_forældre_højest_udd_dummy,
                                          EnForældreLangUdd = en_forældre_højest_udd_dummy,
                                          snitover7 = Adg_kvotient>7,
                                          snit7ellerunder = Adg_kvotient<=7,
                                          EkstrinsiskMotivation = passer_karakter_betyder_meget) %>% as_tibble()


full.sem$hf_dummy <- as.factor(ifelse(full.sem$Adg_type_dummy == "HF", 1, 0))
full.sem$hhx_dummy <- as.factor(ifelse(full.sem$Adg_type_dummy == "HHX", 1, 0))
full.sem$htx_dummy <- as.factor(ifelse(full.sem$Adg_type_dummy == "HTX", 1, 0))
full.sem$international_dummy <- as.factor(ifelse(full.sem$Adg_type_dummy == "International", 1, 0))

full.sem <- full.sem %>% mutate(mindst_en_højtudd = 
                      en_forældre_højest_udd_dummy + 
                      begge_forældre_højest_udd_dummy, 
                    mindst_en_højtudd = recode(mindst_en_højtudd,
                                               "1" = 1, 
                                               "2" = 1,
                                               "0" = 0)) 

df$ingen_forældre_højest_udd_dummy
#Køn interageret med bedømmelsesform på stress+præstation
pathmodel1 <- 
  'VurderingArbejdsbyrde ~ Adg.karakter + Kvinde + Bedømmelsesform + Kvinde:Bedømmelsesform + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy + matematik_gym
AndelPoint ~ VurderingArbejdsbyrde + Adg.karakter + Kvinde + Bedømmelsesform + Kvinde:Bedømmelsesform + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy + matematik_gym
Adg.karakter ~ Kvinde'

fit1 <- lavaan::sem(pathmodel1, full.sem)
summary(fit1, fit.measures = TRUE)

#tjek total effekt
fit1.1 <-
  '
  #direct effect
  AndelPoint ~ c1*Kvinde:Bedømmelsesform + c2*Kvinde + c3*Bedømmelsesform + c4*merit + c5*hhx_dummy + c6*htx_dummy + c7*international_dummy + c8*hf_dummy + c9*matematik_gym + b1*VurderingArbejdsbyrde + b3*Adg.karakter
  
  #mediators
  VurderingArbejdsbyrde ~ a1*Kvinde:Bedømmelsesform + a10*Kvinde + a11*Bedømmelsesform + a12*Adg.karakter + c13*merit + a14*hf_dummy + a15*hhx_dummy + a16*htx_dummy + a17*international_dummy + a18*matematik_gym
  Adg.karakter ~ a3*Kvinde
  
  #indirect effect
  indirect.effect := a1*b1 #KvindexBedømmelsesform>stress>AndelPoint

  #total effect
  total.effect := c1+(a1*b1)
'

fit1.1 <- lavaan::sem(fit1.1, full.sem)

residuals(fit1, type = "cor")


fitMeasures(fit1.1, c("chisq", "pvalue", "rmsea", "srmr", "cfi", "df"))
fitMeasures(fit1, c("chisq", "pvalue", "rmsea", "srmr", "cfi", "df"))


#afrapportering
semTable(fit1.1, file = NULL, paramSets = c("slopes", "loadings"),
         columns = c(est = "Estimate", se = "SE", z = "z", p = "p-value"),
         fits = c("chisq", "cfi", "rmsea", "srmr"), varLabels = NULL, groups = NULL,
         type = "latex", table.float = TRUE, caption = NULL,
         label = NULL, longtable = FALSE, print.results = TRUE,
         centering = "siunitx", alpha = c(0.05, 0.01, 0.001))



#SEM med Intrinsisk motivation

pathmodel2.3 <-  
  'Intrinsisk.Motivation =~ NA*DygtigKvantMetode + LystArbejdeKvantMetode
  Intrinsisk.Motivation ~~ 1*Intrinsisk.Motivation
Intrinsisk.Motivation ~ Kvinde + Bedømmelsesform + Adg.karakter + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy + matematik_gym
VurderingArbejdsbyrde ~ Intrinsisk.Motivation + Kvinde + Bedømmelsesform + Adg.karakter + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy  + matematik_gym
AndelPoint ~ VurderingArbejdsbyrde + Intrinsisk.Motivation + Kvinde + Adg.karakter + Bedømmelsesform + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy  + matematik_gym
Adg.karakter ~ Kvinde'

fit2 <- lavaan::sem(pathmodel2.3, full.sem)
summary(fit2)
#tjek residualer
residuals(fit2, type = "cor")

#tjek af total effekt
fit2.1 <-
  '#latent
 Intrinsisk.Motivation =~ NA*DygtigKvantMetode + LystArbejdeKvantMetode
  Intrinsisk.Motivation ~~ 1*Intrinsisk.Motivation
  
  #direct effect
  AndelPoint ~ c1*Kvinde + c2*Bedømmelsesform + c3*merit + c4*hhx_dummy + c5*htx_dummy + c6*international_dummy + c7*hf_dummy + c8*matematik_gym + b1*VurderingArbejdsbyrde + b2*Intrinsisk.Motivation + b3*Adg.karakter
  
  #mediators
  VurderingArbejdsbyrde ~ a1*Kvinde + b4*Intrinsisk.Motivation + a11*Bedømmelsesform + a12*Adg.karakter + a13*merit + a14*hf_dummy + a15*hhx_dummy + a16*htx_dummy + a17*international_dummy + a18*matematik_gym
  Intrinsisk.Motivation ~ a2*Kvinde + a22*Bedømmelsesform + b5*Adg.karakter + a24*merit + a25*hf_dummy + a26*hhx_dummy + a27*htx_dummy + a28*international_dummy + a29*matematik_gym
  Adg.karakter ~ a3*Kvinde
  
  #indirect effect
  indirect1 := a1*b1 #Kvinde>stress>AndelPoint
  indirect2 := a2*b2 #Kvinde>IM>AndelPoint
  indirect3 := a3*b3 #Kvinde>GYM>AndelPoint
  indirect4 := a2*b4*b1 #Kvinde>IM>Stress>Andelpoint
  indirect5 := a3*a12*b1 #Kvinde>GYM>Stress>AndelPoint
  indirect6 := a3*b5*b4*b1 #Kvinde>GYM>IM>Stress>AndelPoint
  
  indirect7_karakter_stress := a22*b4 #Karakter>IM>Stress
  indirect8_karakter_pres := a22*b4*b1 #Karakter>IM>Stress>Præstation
  
  #total effect
  total := c1+(a1*b1)+(a2*b2)+(a3*b3)+(a2*b4*b1)+(a3*a12*b1)+(a3*b5*b4*b1)

'


fit2.1 <- lavaan::sem(fit2.1, full.sem)

residuals(fit2.1, type = "cor")


#afrapportering

semTable(fit2.1, file = NULL, paramSets = c("slopes", "loadings"),
         columns = c(est = "Estimate", se = "SE", z = "z", p = "p-value"),
         fits = c("chisq", "cfi", "rmsea", "srmr"), varLabels = NULL, groups = NULL,
         type = "latex", table.float = TRUE, caption = NULL,
         label = NULL, longtable = FALSE, print.results = TRUE,
         centering = "siunitx", alpha = c(0.05, 0.01, 0.001))


#EKSTRINSISK MOTIVATION
pathmodel8 <- 
  'EkstrinsiskMotivation ~ Kvinde + Bedømmelsesform + Adg.karakter + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy + matematik_gym
VurderingArbejdsbyrde ~ EkstrinsiskMotivation + Kvinde + Bedømmelsesform + Adg.karakter + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy + matematik_gym
AndelPoint ~ VurderingArbejdsbyrde + EkstrinsiskMotivation + Kvinde + Adg.karakter + Bedømmelsesform + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy + matematik_gym
Adg.karakter ~ Kvinde'

fit8 <- lavaan::sem(pathmodel8, full.sem)

#tjek total efekt
pathmodel8.1 <-
  '#direct effect
  AndelPoint ~ c1*Kvinde + c2*Bedømmelsesform + c3*merit + c4*hhx_dummy + c5*htx_dummy + c6*international_dummy + c7*hf_dummy + c8*matematik_gym + b1*VurderingArbejdsbyrde + b2*EkstrinsiskMotivation + b3*Adg.karakter
  
  #mediators
  VurderingArbejdsbyrde ~ a1*Kvinde + b4*EkstrinsiskMotivation + a11*Bedømmelsesform + a12*Adg.karakter + a13*merit + a14*hf_dummy + a15*hhx_dummy + a16*htx_dummy + a17*international_dummy + a18*matematik_gym
  EkstrinsiskMotivation ~ a2*Kvinde + a22*Bedømmelsesform + b5*Adg.karakter + a24*merit + a25*hf_dummy + a26*hhx_dummy + a27*htx_dummy + a28*international_dummy + a29*matematik_gym
  Adg.karakter ~ a3*Kvinde
  
  #indirect effect
  indirect1 := a1*b1 #Kvinde>stress>AndelPoint
  indirect2 := a2*b2 #Kvinde>IM>AndelPoint
  indirect3 := a3*b3 #Kvinde>GYM>AndelPoint
  indirect4 := a2*b4*b1 #Kvinde>IM>Stress>Andelpoint
  indirect5 := a3*a12*b1 #Kvinde>GYM>Stress>AndelPoint
  indirect6 := a3*b5*b4*b1 #Kvinde>GYM>IM>Stress>AndelPoint
  
  #total effect
  total := c1+(a1*b1)+(a2*b2)+(a3*b3)+(a2*b4*b1)+(a3*a12*b1)+(a3*b5*b4*b1)

'

fit8.1 <- lavaan::sem(pathmodel8.1, full.sem)

#afrapportering
semTable(fit8.1, file = NULL, paramSets = c("slopes", "loadings"),
         columns = c(est = "Estimate", se = "SE", z = "z", p = "p-value"),
         fits = c("chisq", "cfi", "rmsea", "srmr"), varLabels = NULL, groups = NULL,
         type = "latex", table.float = TRUE, caption = NULL,
         label = NULL, longtable = FALSE, print.results = TRUE,
         centering = "siunitx", alpha = c(0.05, 0.01, 0.001))


#standardiserede residualer
residuals(fit8, type = "cor") 

##MESTRINGSTRO
pathmodel4 <- 
  'Self.Efficacy =~ NA*Løse.Vanskelige.Problemer + Løse.Svære.Opgaver
  Self.Efficacy ~~ 1*Self.Efficacy
Self.Efficacy ~ Kvinde + Bedømmelsesform + Adg.karakter + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy + matematik_gym
VurderingArbejdsbyrde ~ Self.Efficacy + Kvinde + Bedømmelsesform + Adg.karakter + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy + matematik_gym
AndelPoint ~ VurderingArbejdsbyrde + Self.Efficacy + Kvinde + Adg.karakter + Bedømmelsesform + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy + matematik_gym
Adg.karakter ~ Kvinde'

fit4 <- lavaan::sem(pathmodel4, full.sem)

#standardiserede residualer
residuals(fit4, type = "cor")

#tjek af total effekt
pathmodel4.1 <-
  '#latent
  Self.Efficacy =~ NA*Løse.Vanskelige.Problemer + Løse.Svære.Opgaver  
  Self.Efficacy ~~ 1*Self.Efficacy
  #direct effect
  AndelPoint ~ c1*Kvinde + c2*Bedømmelsesform + c3*merit + c4*hhx_dummy + c5*htx_dummy + c6*international_dummy + c7*matematik_gym + c8*hf_dummy + b1*VurderingArbejdsbyrde + b2*Self.Efficacy + b3*Adg.karakter
  
  #mediators
  VurderingArbejdsbyrde ~ a1*Kvinde + b4*Self.Efficacy + a11*Bedømmelsesform + a12*Adg.karakter + a13*merit + a14*hf_dummy + a15*hhx_dummy + a16*htx_dummy + a17*international_dummy + a18*matematik_gym
  Self.Efficacy ~ a2*Kvinde + a22*Bedømmelsesform + b5*Adg.karakter + a24*merit + a25*hf_dummy + a26*hhx_dummy + a27*htx_dummy + a28*international_dummy + a29*matematik_gym
  Adg.karakter ~ a3*Kvinde
  
  #indirect effect
  indirect1 := a1*b1 #Kvinde>stress>AndelPoint
  indirect2 := a2*b2 #Kvinde>IM>AndelPoint
  indirect3 := a3*b3 #Kvinde>GYM>AndelPoint
  indirect4 := a2*b4*b1 #Kvinde>IM>Stress>Andelpoint
  indirect5 := a3*a12*b1 #Kvinde>GYM>Stress>AndelPoint
  indirect6 := a3*b5*b4*b1 #Kvinde>GYM>IM>Stress>AndelPoint
  
  #total effect
  total := c1+(a1*b1)+(a2*b2)+(a3*b3)+(a2*b4*b1)+(a3*a12*b1)+(a3*b5*b4*b1)
'

fit4.1 <- lavaan::sem(pathmodel4.1, full.sem)


#afrapportering
semTable(fit4.1, file = NULL, paramSets = c("slopes", "loadings"),
         columns = c(est = "Estimate", se = "SE", z = "z", p = "p-value"),
         fits = c("chisq", "cfi", "rmsea", "srmr"), varLabels = NULL, groups = NULL,
         type = "latex", table.float = TRUE, caption = NULL,
         label = NULL, longtable = FALSE, print.results = TRUE,
         centering = "siunitx", alpha = c(0.05, 0.01, 0.001))




#FORÆLDRES UDDANNELSE 


#selfefficacy
pathmodel5.1 <- 
  'Self.Efficacy =~ NA*Løse.Vanskelige.Problemer + Løse.Svære.Opgaver
  Self.Efficacy ~~ 1*Self.Efficacy
Self.Efficacy ~ mindst_en_højtudd + Bedømmelsesform + Adg.karakter + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy
VurderingArbejdsbyrde ~ Self.Efficacy + mindst_en_højtudd + Bedømmelsesform + Adg.karakter + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy
AndelPoint ~ VurderingArbejdsbyrde + mindst_en_højtudd + Self.Efficacy + Adg.karakter + Bedømmelsesform + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy
Adg.karakter ~ mindst_en_højtudd
'

pathmodel5.2 <-
  '#latent
  Self.Efficacy =~ NA*Løse.Vanskelige.Problemer + Løse.Svære.Opgaver
  Self.Efficacy ~~ 1*Self.Efficacy
  
  #direct effect
  AndelPoint ~ c1*mindst_en_højtudd + c2*Bedømmelsesform + c3*merit + c4*hhx_dummy + c5*htx_dummy + c6*international_dummy + c7*hf_dummy + b1*VurderingArbejdsbyrde + b2*Self.Efficacy + b3*Adg.karakter
  
  #mediators
  VurderingArbejdsbyrde ~ a1*mindst_en_højtudd + b4*Self.Efficacy + a11*Bedømmelsesform + a12*Adg.karakter + a13*merit + a14*hf_dummy + a15*hhx_dummy + a16*htx_dummy + a17*international_dummy
  Self.Efficacy ~ a2*mindst_en_højtudd + a22*Bedømmelsesform + b5*Adg.karakter + a24*merit + a25*hf_dummy + a26*hhx_dummy + a27*htx_dummy + a28*international_dummy
  Adg.karakter ~ a3*mindst_en_højtudd
  
  #indirect effect
  indirect.effect1 := a1*b1 #SES>stress>AndelPoint
  indirect.effect2 := a2*b2 #SES>SE>AndelPoint
  indirect.effect3 := a3*b3 #SES>GYM>AndelPoint
  indirect.effect4 := a2*b4*b1 #SES>SE>Stress>Andelpoint
  indirect.effect5 := a3*a12*b1 #SES>GYM>Stress>AndelPoint
  indirect.effect6 := a3*b5*b4*b1 #SES>GYM>SES>Stress>AndelPoint
  
  #direct effect
  direct.effect := c1
  #total effect
  total.effect := c1+(a1*b1)+(a2*b2)+(a3*b3)+(a2*b4*b1)+(a3*a12*b1)+(a3*b5*b4*b1)
'

fit5.1 <- lavaan::sem(pathmodel5.1, full.sem)

fit5.2 <- lavaan::sem(pathmodel5.2, full.sem)
summary(fit5.2)
#afrapportering
  semTable(fit5.2, file = NULL, paramSets = c("slopes", "loadings"),
         columns = c(est = "Estimate", se = "SE", z = "z", p = "p-value"),
         fits = c("chisq", "cfi", "rmsea", "srmr"), varLabels = NULL, groups = NULL,
         type = "latex", table.float = TRUE, caption = NULL,
         label = NULL, longtable = FALSE, print.results = TRUE,
         centering = "siunitx", alpha = c(0.05, 0.01, 0.001))


#standardiserede residualer
residuals(fit5.2, type = "cor")

#vurdering arbejdsbyrde - forældres uddannelse
pathmodel5.4 <- 
  '
VurderingArbejdsbyrde ~ mindst_en_højtudd:Bedømmelsesform + mindst_en_højtudd + Bedømmelsesform + Adg.karakter + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy + matematik_gym
AndelPoint ~ VurderingArbejdsbyrde + mindst_en_højtudd:Bedømmelsesform + mindst_en_højtudd + Adg.karakter + Bedømmelsesform + merit + hf_dummy + hhx_dummy + htx_dummy + international_dummy + matematik_gym
Adg.karakter ~ mindst_en_højtudd'

fit6 <- lavaan::sem(pathmodel5.4, full.sem)
#tjek total effekt
pathmodel5.5 <-
  ' #direct effect
  AndelPoint ~ b1*VurderingArbejdsbyrde + c1*mindst_en_højtudd:Bedømmelsesform + c2*mindst_en_højtudd + c3*Adg.karakter + c4*Bedømmelsesform + c5*merit + c6*hf_dummy + c7*hhx_dummy + c8*htx_dummy + c9*international_dummy + c10*matematik_gym
    #mediators
VurderingArbejdsbyrde ~ a1*mindst_en_højtudd:Bedømmelsesform + a2*mindst_en_højtudd + a3*Bedømmelsesform + a4*Adg.karakter + a5*merit + a6*hf_dummy + a7*hhx_dummy + a8*htx_dummy + a9*international_dummy + a10*matematik_gym
Adg.karakter ~ a11*mindst_en_højtudd
    #indirect effect
indirect := a1*b1
    #direct effect
direct := c1
    #total effect
total.effect := c1+(a1*b1)

'
fit6.1 <- lavaan::sem(pathmodel5.5, full.sem)
#afrapportering

semTable(fit6.1, file = NULL, paramSets = c("slopes", "loadings"),
         columns = c(est = "Estimate", se = "SE", z = "z", p = "p-value"),
         fits = c("chisq", "cfi", "rmsea", "srmr"), varLabels = NULL, groups = NULL,
         type = "latex", table.float = TRUE, caption = NULL,
         label = NULL, longtable = FALSE, print.results = TRUE,
         centering = "siunitx", alpha = c(0.05, 0.01, 0.001))


#residualer
residuals(fit6.1, type = "cor")


