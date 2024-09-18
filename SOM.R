# charger les packages
library(readxl)
library(tidyverse)
library(FactoMineR)
library(factoextra)
# charger la base de données et prétraitrement
dta = read_excel("data.xlsx")
df = dta[-1]
df = data.frame(df , row.names = dta$Reseaux)
attach(df)
## prétraitement
d = df
naniar::vis_miss(d)
d_mice = mice(d, meth="pmm")
d_imp = complete(d_mice)
naniar::vis_miss(d_imp)
# préparer la base de donnée pour som
d_imp %>%
  scale(.) %>%
  as.matrix(.) ->
  d_imp_mat
## Construction de la carte
library(kohonen) # Charge le package kohonen
#Paramètrage
set.seed(100)
carte_dmat <- som(d_imp_mat, 
                  grid = somgrid(xdim=3, ydim=3, topo = c("hexagonal")), rlen =300)
# Résumé de la carte
summary(carte_dmat)
# architecture de la grille
print(carte$grid)
# Evolution de l’apprentissage au fil des itérations
plot(carte_dmat, type = "changes")
# nombre d'individus dans chaque cellule
nb_d = table(carte_dmat$unit.classif)
nb_d 
#fonction de recuperation des individus de chaque neuronne
fonct_neur=function(don,cart_unit){
  Ens_Neuronne=list(neuronne1=vector(),neuronne2=vector(),
                    neuronne3=vector(),neuronne4=vector(),
                    neuronne5=vector(),neuronne6=vector(),
                    neuronne7=vector(),neuronne8=vector(),neuronne9=vector())
  for (i in 1:length(cart_unit)){
    compt=Ens_Neuronne[[cart_unit[i]]]
    Ens_Neuronne[[cart_unit[i]]][length(compt)+1]=don[i]
  }
  return(Ens_Neuronne)
}
# Regroupement des individus par neuronne
neur= fonct_neur(row.names(d_imp),carte_dmat$unit.classi)
neur
# jeu de couleurs pour les noeuds de la carte degrade.
degrade.bleu <- function(n){ return(rgb(0,0.4,1,alpha=seq(0,1,1/n))) }
# count plot
plot(carte_dmat,type="count",palette.name=degrade.bleu)
# distance au voisinage
plot(carte_dmat,type="dist.neighbours")
# profils des noeuds
plot(carte_dmat, type = "codes", codeRendering = "segments")
## un graphique pour chaque variable
code_dmat = carte_dmat$codes[[1]]
#jeu de couleurs pour le graphique
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6,alpha=alpha)[n:1]}
#graphique pour chaque variable
par(mfrow=c(5,5)) #Disposer en 5 lignes, 5 colonnes
for (j in 1:ncol(d_imp)){
plot(carte_dmat,type="property",property=code_dmat[,j],
     palette.name=coolBlueHotRed,main=colnames(d_imp)[j],cex=1) }
par(mfrow=c(1,1)) #Retour à 1 ligne et 1 colonne
#----------------------------- CAH
#matrice de distance entre les noeuds
dis_dmat <- dist(code_dmat)
#classification
cah_dmat <- hclust(dis_dmat,method="ward.D2",members=nb_d)
plot(cah_dmat, cex = 0.5)
abline(h=4.7, col = "red")
# découpage en 4 classes
groupes_dmat <- cutree(cah_dmat,k=4)
print(groupes_dmat)
# visualisation des classes dans la carte topologique
plot(carte_dmat,type="mapping",
     bgcol=c("steelblue1","sienna1","yellowgreen")[groupes_dmat])
add.cluster.boundaries(carte_dmat,clustering=groupes_dmat)
# affecter chaque individu à sa classe
ind.groupe <- groupes_dmat[carte_dmat$unit.classif]
ind.groupe
###----------------------- Dépassement aux normes
# creer un vecteur avec les neuronne comme indexe
ind.groupe2 = c(ind.groupe)
names(ind.groupe2)
# Obtenir les classe par individus
classe = unname(ind.groupe2)
classe
# affecter chaque individu à sa classe dans le tableau initiale
df.classe = data.frame(cluster = classe)
df.classe$cluster = as.factor(df.classe$cluster)
new_base = d_imp %>% mutate(Classe = df.classe$cluster)
new_base$Classe = as.factor(new_base$Classe)
# classe 1
som_classe1 = subset(new_base, Classe=="1")
## Turbidité
shapiro.test(som_classe1$Turbidité)
wilcox.test(som_classe1$Turbidité,mu=1,alternative = "two.sided")
wilcox.test(som_classe1$Turbidité,mu=1,alternative = "greater") 
## PH 
shapiro.test(som_classe1$PH)
wilcox.test(som_classe1$PH,mu=6.5,alternative = "two.sided")
wilcox.test(som_classe1$PH,mu=6.5,alternative = "greater") 
wilcox.test(som_classe1$PH,mu=8.5,alternative = "two.sided")
wilcox.test(som_classe1$PH,mu=8.5,alternative = "greater") 
## DHT 
shapiro.test(som_classe1$Dureté_Total) 
wilcox.test(som_classe1$Dureté_Total,mu=45,alternative = "two.sided")
wilcox.test(som_classe1$Dureté_Total,mu=45,alternative = "greater") 
# classe 2
som_classe2 = subset(new_base, Classe=="2")
## Nitrate 
shapiro.test(som_classe2$Nitrate) # = 0.1461
t.test(som_classe2$Nitrate,mu=50,alternative = "two.sided")
t.test(som_classe2$Nitrate,mu=50,alternative = "greater") 
## Ammonium 
shapiro.test(som_classe2$Ammonium) 
t.test(som_classe2$Ammonium,mu=1.5,alternative = "two.sided")
t.test(som_classe2$Ammonium,mu=1.5,alternative = "greater") 
## DHT 
shapiro.test(som_classe2$Dureté_Total) 
wilcox.test(som_classe2$Dureté_Total,mu=45,alternative = "two.sided")
wilcox.test(som_classe2$Dureté_Total,mu=45,alternative = "greater") 
## Chlore résiduel 
shapiro.test(som_classe2$Chlore_résiduel) 
wilcox.test(som_classe2$Chlore_résiduel,mu=0.2,alternative = "two.sided")
wilcox.test(som_classe2$Chlore_résiduel,mu=0.2,alternative = "greater")
wilcox.test(som_classe2$Chlore_résiduel,mu=0.5,alternative = "two.sided")
## Mn 
shapiro.test(som_classe2$Manganèse) 
wilcox.test(som_classe2$Manganèse,mu=0.1,alternative = "two.sided")
wilcox.test(som_classe2$Manganèse,mu=0.1,alternative = "greater") 
## Aluminium 
shapiro.test(som_classe2$Aluminium)
wilcox.test(som_classe2$Aluminium,mu=0.2,alternative = "two.sided")
wilcox.test(som_classe2$Aluminium,mu=0.2,alternative = "greater") 
## Température 
shapiro.test(som_classe2$Température)
wilcox.test(som_classe2$Température,mu=25,alternative = "two.sided")
wilcox.test(som_classe2$Température,mu=25,alternative = "greater") 





