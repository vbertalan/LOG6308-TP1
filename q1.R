library(Matrix)
u.data <- read.csv(file='Data/votes.csv', sep='|', header=T)
m <- as.matrix(sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3]))
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')

dim(m)

i <- m > 0              # index des valeurs non manquantes de m
length(i)               # nombre de cellules
sum(i)                  # seules les cellules TRUE sont non manquantes
i.ind <- which(i)       # index numérique plutôt que booléen
length(i.ind)
m[head(i.ind)]                          # les premiers votes
m[head(i.ind)][sample(6)]               # les premiers votes mélangés

err.quad.moy <- function(pred, reel) sqrt(mean((pred - reel)^2))
err.quad.moy(m[head(i.ind)][sample(6)], m[head(i.ind)])

err.quad.moy(m[i.ind][sample(length(i.ind))], m[i.ind]) # erreur quad. pour vote aléatoire
err.quad.moy(mean(m[i.ind]), m[i.ind])                  # erreur quad. pour vote moyen

head(apply(m, 1, function(ligne) mean(ligne[ligne > 0])))
mean(apply(m, 1, function(ligne) err.quad.moy(mean(ligne[ligne > 0]), ligne[ligne > 0]))) # approximation...
## Cette solution n'est pas tout à fait juste, car elle fait une moyenne d'erreurs quadratiques par utilisateur, alors qu'il faut faire l'erreur quadratique globale.
## Appliquons une méthode plus générale en créant une matrice de prédictions et en mesurant l'erreur sur les prédictions spécifiquement

## fonction utilitaire utile pour les validations croisées en replis
batch <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
batch(1:10, 3)

batch.ind <- batch(i.ind[sample(length(i.ind))], 10)   # 10 replis
str(batch.ind)

## Exemple pour un remplis
m.test.ind <- unlist(batch.ind[1])
letters[-c(3,5)]
m.train.ind <- unlist(batch.ind[seq(10)[-1]])
length(m.test.ind)
length(m.train.ind)
m.train <- m
m.train[m.test.ind] <- 0                # on élimine les valeurs de test pour l'entraînement
u.moy <- apply(m.train, 1, function(ligne) mean(ligne[ligne > 0]))
matrix(1:4,4) %*% rep(1,4)              # un moyen de créer une matrice de colonnes répétées par le produit dyadique
m.pred <- matrix(??, ncol=1) %*% rep(?, ncol(m)) # à compléter
err.quad.moy(m.pred[m.test.ind], m[m.test.ind])

## test d'hypothèse
hist(rnorm(100))
t.test(rnorm(100), rnorm(100, 0.1))
str(t.test(rnorm(100), rnorm(100, 0.1)))
sapply(1:10, function(i) t.test(rnorm(100), rnorm(100, 0.1))$p.value)

