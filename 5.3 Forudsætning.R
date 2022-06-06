library(tidymodels)
library(performance)

check_ours <- function(x, y) {
  
  x <- check_model(x, check = c("normality", #normalfordelte fejlled
                           "linearity",  #linearitet
                           "homogeneity", #varianshomogenitet
                           "outliers",  #outliers
                           "vif", #multikollinearitet
                           "qq")) ##normalfordelte fejlled - 2
  
  x$name <- y
  
  x
  
}


m1
m3
m3.2
m1.1 
m1.2
m1.3

source("3.0 Samlede analyser.R")

hej <- list(m1,
            m3,
            m3.2,
            m1.1 ,
            m1.2,
            m1.3)

hej2 <- c("m1",
          "m3",
          "m3.2",
          "m1.1" ,
          "m1.2",
          "m1.3")

o <- lapply(hej, check_ours, y = hej2)

o[[1]]$name

f <- function(n) {
    png(paste0(o[[n]]$name[n], "_plot.png"), height = 2000, width = 2000, res = 200)
    print({o[[n]]})
    dev.off()    
}

lapply(1:length(o), f)

hej <- list(UDD1_i,
            UDD2_i,
            UDD3_i,
            UDD4_i,
            UDD1,
            UDD2,
            UDD3,
            UDD4)

hej2 <- c("UDD1_i",
          "UDD2_i",
          "UDD3_i",
          "UDD4_i",
          "UDD1",
          "UDD2",
          "UDD3",
          "UDD4")

o <- lapply(hej, check_ours, y = hej2)

f <- function(n) {
  png(paste0(o[[n]]$name[n], "_plot.png"), height = 2000, width = 2000, res = 200)
  print({o[[n]]})
  dev.off()    
}

lapply(1:length(o), f)

hej <- list(m8,
            m6,
            m7,
            m80,
            m92,
            m90,
            m19,
            m191,
            m193,
            m182,
            m181,
            m183)


hej2 <- c("m8",
          "m6",
          "m7",
          "m80",
          "m92",
          "m90",
          "m19",
          "m191",
          "m193",
          "m182",
          "m181",
          "m183")

o <- lapply(hej, check_ours, y = hej2)

f <- function(n) {
  png(paste0(o[[n]]$name[n], "_plot.png"), height = 2000, width = 2000, res = 200)
  print({o[[n]]})
  dev.off()    
}

lapply(1:length(o), f)



list.files(path = "FORUDSÆTNING/")
