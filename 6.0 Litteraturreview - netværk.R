library(bibliometrix)
library(formattable)
library(dplyr)

biblioshiny()

M <- convert2df(
  file = "Speciale_Scopus.bib", 
  dbsource = "scopus", 
  format = "bibtex")

NetMatrix <- biblioNetwork(M, 
                           analysis = "co-occurrences", 
                           network = "keywords", 
                           sep = ";")

e <- igraph::graph_from_adjacency_matrix(NetMatrix)

ggraph(e, layout = "stress", weights = edge_weights) + 
  geom_edge_link(aes(alpha = edge_weights)) + 
  geom_node_point() + 
  scale_edge_alpha_identity()


NetMatrix


devtools::install_github("schochastics/smglr")




co_occur_all<-networkPlot(NetMatrix, 
                          normalize="association", 
                          weighted=T, 
                          n = 30, 
                          Title = "Keyword Co-occurrences", 
                          type = "fruchterman", 
                          size=T,
                          edgesize = 5,
                          labelsize=0.7)







results <- biblioAnalysis(M, sep = ";")

q <- summary(results)

NetMatrix <- biblioNetwork(
  M,
  analysis = "co-occurrences",
  network = "keywords",
  n = NULL,
  sep = ";",
  short = FALSE,
  shortlabel = TRUE
)

net <- networkPlot(NetMatrix, type = "kamada", Title = "Co-Citation",labelsize=0.5) 

formattable(
  q$MostRelKeywords[,3:4],
  formatter = "format_table",
  preproc = NULL,
  postproc = NULL
)
