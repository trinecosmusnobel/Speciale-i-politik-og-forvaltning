source("2.1 Join af dataset.R")

options(encoding = "UTF-8")

til_faktor_analyse <- full %>% select(passer_vanskeligt_problem, passer_loese_svaere_opgaver) %>% mutate_all(as.numeric) # %>% mutate_all(scale)

fit <- factanal(na.omit(til_faktor_analyse), 1, rotation="varimax")
fit


load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(til_faktor_analyse),cex=.7) # add variable names 

#Cronbachs alpha

full <- full %>% ungroup(group_id)
full %>% select(passer_vanskeligt_problem, passer_loese_svaere_opgaver) %>% View

self.efficacy.CA.tjek <- full %>% dplyr::select(passer_vanskeligt_problem, passer_loese_svaere_opgaver)
intrinsisk.CA.tjek <- full %>% dplyr::select(passer_dygtig_kvantitativ_metode, grad_lyst_arb_kvantitativ_metode)
matematik.orientering <- full %>% dplyr::select(passer_mat_sjovt, positiv_tal)
akademisk.praksis <- full %>% dplyr::select(antal_opfoelgninger,
                                            antal_timer_forberedelse, antal_studiecafeer) %>% mutate_all(as.numeric) #%>% mutate_all(scale)



ltm::cronbach.alpha(na.omit(intrinsisk.CA.tjek)) # relativ høj cronbachs-alfa: 0.646
ltm::cronbach.alpha(na.omit(self.efficacy.CA.tjek)) # middel cronbachs-alfa: 0.674
ltm::cronbach.alpha(na.omit(matematik.orientering)) # høj cronbachs-alfa: 0.622
ltm::cronbach.alpha(na.omit(akademisk.praksis)) # meget lav cronbachs alfa. hmm.




corr.test(matematik.orientering)
