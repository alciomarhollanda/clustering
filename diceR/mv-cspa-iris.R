
##Testes Iris @alciomar

library(diceR)
library(ggplot2)
dat <- iris[, -5]
head(iris)
head(dat)



x <- consensus_cluster(dat, nk = 3:4, reps = 10, algorithms = c("km","pam","diana"),
                       progress = FALSE)

ccomb_matrix <- consensus_combine(x, element = "matrix")
ccomb_class <- consensus_combine(x, element = "class")

str(ccomb_matrix, max.level = 2)

options(max.print=1000000)
# consensus matrix across subsamples and algorithms and k
cm_all <- consensus_matrix(ccomb_class)

ccomp <- consensus_evaluate(dat, x, plot = FALSE)

knitr::kable(ccomp$ii$`4`)


cspaData <- CSPA(x, k = 3)

table(cspaData, iris$Species)

mvData <- majority_voting(x,is.relabelled = FALSE)

table(mvData, iris$Species)

ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + geom_point()



irisClusterCSPA <- as.factor(cspaData)
irisClusterMV <- as.factor(mvData)

ggplot(iris, aes(Petal.Length, Petal.Width, color = irisClusterCSPA)) + geom_point()
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisClusterMV)) + geom_point()

