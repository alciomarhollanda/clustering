## tutorial diceR() @alciomar

library(diceR)
library(dplyr)
library(ggplot2)
library(pander)
data(hgsc)

hgsc <- hgsc[1:100, 1:50]


#agrupar os hgscdados em 3 ou 4 clusters, usando 80% de reimampling em 5 repeti��es,
#para esses algoritmos de cluster: Hierarchical Clustering, PAM e DIvisive
#ANAlysis Clustering (DIANA). 
#A dist�ncia euclidiana � utilizada para todos os algoritmos.
CC <- consensus_cluster(hgsc, nk = 3:4, p.item = 0.8, reps = 5,
                        algorithms = c("hc", "pam", "diana"))

co <- capture.output(str(CC))
strwrap(co, width = 80)

CC <- apply(CC, 2:4, impute_knn, data = hgsc, seed = 1)

# Filtra somente PAM_Euclidean 4
pam.4 <- CC[, , "PAM_Euclidean", "4", drop = FALSE]

cm <- consensus_matrix(pam.4)
dim(cm)


dev.off()

hm <- graph_heatmap(pam.4)


