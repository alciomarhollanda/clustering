## tutorial diceR() @alciomar

library(diceR)
library(dplyr)
library(ggplot2)
library(pander)
data(hgsc)

hgsc <- hgsc[1:100, 1:50]


#agrupar os hgscdados em 3 ou 4 clusters, usando 80% de reimampling em 5 repeti??es,
#para esses algoritmos de cluster: Hierarchical Clustering, PAM e DIvisive
#ANAlysis Clustering (DIANA). 
#A dist?ncia euclidiana ? utilizada para todos os algoritmos.
CC <- consensus_cluster(hgsc, nk = 3:4, p.item = 0.8, reps = 5,
                        algorithms = c("hc", "pam", "diana"))

co <- capture.output(str(CC))
strwrap(co, width = 80)

CC <- apply(CC, 2:4, impute_knn, data = hgsc, seed = 1)

bigger = options()$max.print  # or add something larger
options("max.print" = bigger) # apparently RStudio sets max.print very low. 
CC

# Filtra somente PAM_Euclidean 4
pam.4 <- CC[, , "PAM_Euclidean", "4", drop = FALSE]

cm <- consensus_matrix(pam.4)
dim(cm)


dev.off()

hm <- graph_heatmap(pam.4)

ccomb_matrix <- consensus_combine(CC, element = "matrix")
ccomb_class <- consensus_combine(CC, element = "class")

str(ccomb_matrix, max.level  = 5)

cm_k3 <- consensus_matrix(ccomb_class$`3`)

cm_k4 <- consensus_matrix(ccomb_class$`4`)

# consensus matrix across subsamples and algorithms and k
cm_all <- consensus_matrix(ccomb_class)


CC2 <- consensus_cluster(hgsc, nk = 3:4, p.item = 0.8, reps = 5,
                         algorithms = "km")
ccomb_class2 <- consensus_combine(CC, CC2, element = "class")

ccomp <- consensus_evaluate(hgsc, CC, CC2, plot = FALSE)

ctrim <- consensus_evaluate(hgsc, CC, CC2, trim = TRUE, reweigh = FALSE, n = 2)

# https://cran.r-project.org/web/packages/diceR/diceR.pdf
#Example CSPA#

data(hgsc)
dat <- hgsc[1:100, 1:50]
x <- consensus_cluster(dat, nk = 4, reps = 4, algorithms = c("hc", "diana"),
                       progress = FALSE)
CSPA(x, k = 4)

############################################## PG 15
##Ploat

# Consensus clustering for 3 algorithms
library(ggplot2)
set.seed(911)
x <- matrix(rnorm(80), ncol = 10)
CC1 <- consensus_cluster(x, nk = 2:4, reps = 3,
                         algorithms = c("hc", "pam", "km"), progress = FALSE)
# Plot CDF
p <- graph_cdf(CC1)

# Change y label and add colours
p + labs(y = "Probability") + stat_ecdf(aes(colour = k)) +
  scale_color_brewer(palette = "Set2")

# Delta Area
p <- graph_delta_area(CC1)

# Heatmaps with column side colours corresponding to clusters
CC2 <- consensus_cluster(x, nk = 3, reps = 3, algorithms = "hc", progress =
                           FALSE)
graph_heatmap(CC2)

# Track how cluster assignments change between algorithms
p <- graph_tracking(CC1)


data(hgsc)
dat <- hgsc[1:100, 1:50]
cc <- consensus_cluster(dat, nk = 4, reps = 6, algorithms = "pam", progress =
                          FALSE)
table(majority_voting(cc[, , 1, 1, drop = FALSE], is.relabelled = FALSE))


##Testes

library(diceR)
library(ggplot2)
dat <- iris[, -5]
head(iris)
head(dat)



x <- consensus_cluster(dat, nk = 3, reps = 10, algorithms = c("km"),
                       progress = FALSE)



cspaData <- CSPA(x, k = 3)

mvData <- majority_voting(x,is.relabelled = FALSE)


ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + geom_point()


table(mvData, iris$Species)

table(cspaData, iris$Species)




irisClusterCSPA <- as.factor(cspaData)
irisClusterMV <- as.factor(mvData)

ggplot(iris, aes(Petal.Length, Petal.Width, color = irisClusterCSPA)) + geom_point()
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisClusterMV)) + geom_point()







