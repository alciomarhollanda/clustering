
## banco de dados iris
library(diceR)
library(dplyr)
library(ggplot2)
library(pander)

library(datasets)
data(iris)
summary(iris)




# faz um filtro na variavel para usar somente 4 atributos, retirando assim a classe
iris <- iris[, 1:4]



#agrupar os hgscdados em 3 ou 4 clusters, usando 80% de reimampling em 5 repetições,
#para esses algoritmos de cluster: Hierarchical Clustering, PAM e DIvisive
#ANAlysis Clustering (DIANA). 
#A distância euclidiana é utilizada para todos os algoritmos.
CC <- consensus_cluster(iris, nk = 3:4, p.item = 1, reps = 5,
                        algorithms = c("hc", "pam", "diana"))


co <- capture.output(str(CC))
strwrap(co, width = 80)

# CC <- apply(CC, 2:4, impute_knn, data = hgsc, seed = 1)

# Filtra somente PAM_Euclidean 4
pam.3 <- CC[, , "HC_Euclidean", "4", drop = FALSE]

cm <- consensus_matrix(pam.3)
dim(cm)


dev.off()

hm <- graph_heatmap(pam.3)








