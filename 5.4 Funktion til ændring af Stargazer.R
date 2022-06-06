source("3.0 Samlede analyser.R")

fix_stargazer_DK <- function(x) {
  
  x <- gsub("F Statistic", "F-test", x)
  x <- gsub("Observations", "N", x)
  x <- gsub("F Statistic", "F-test", x)
  x <- gsub("∗p<0,05; ∗∗p<0,01; ∗∗∗p<0,001", "", x)
  x <- writeLines(noquote(as.character(x)))
  x
}

z <- lapply(mget(ls(pattern = "STAR")), fix_stargazer_DK)

