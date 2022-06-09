#############################################################################
## Fonctions utilitaires
#############################################################################

## Divise le vecteur x en une liste de n segments de longeur relativement semblables
## (cette fonction est utile pour créer les replis d'une validation croisée)
batch <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
batch(1:10, 3)

## Cosinus entre les colonnes d'une matrice m
cosinus.m <- function(m) { n <- sqrt(colSums(m^2)); crossprod(m)/(n %o% n) }
## Cosinus entre un vecteur v colonne et les colonnes de la matrice m
cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2)); (v %*% m)/(n * sqrt(sum(v^2))) }

# Trouve les indexes des premières 'n' valeurs maximales d'une matrice
max.nindex <- function(m, n=5) {
    i <- order(m, decreasing=TRUE)
    return(i[1:n])
}
