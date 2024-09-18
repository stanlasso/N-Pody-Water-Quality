# charger les packages
library(readxl)
library(tidyverse)
library(FactoMineR)
library(factoextra)

# charger la base de données 
dta = read_excel("data.xlsx")
df = dta[-1]
df = data.frame(df , row.names = dta$Reseaux)
attach(df)
# selection des paramètres physicochimiques (ppc)
ppc = df[,1:17]
# visualiser les données manquantes
naniar::vis_miss(ppc)
# imputation des données manquantes par la moyenne
library(mice)
ppc_mice = mice(ppc, meth='mean')
ppc_imp = complete(ppc_mice)
naniar::vis_miss(ppc_imp)
#------------------------------ ACP
# calcule de l'ACP et obtention des valeurs propres
res.acp <- PCA(ppc_imp,ncp = 8,
               scale.unit = TRUE,
               graph = F)
eig.val_acp <- get_eigenvalue(res.acp)
eig.val_acp
# Visualiser l'éboulis des valeurs propres
fviz_eig(res.acp, addlabels = TRUE, ylim = c(0, 30))
# contribution et cos2 des variables
res.acp$var
# cercle de correlation des variables
fviz_pca_var(res.acp, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE) # Avoid text overlapping

# contribution et cos2 des individus
res.acp$ind
# représentation des individus
fviz_pca_ind(res.acp, repel = T)

#----------------------------------- CAH
### Trouver la méthode d'agglomération à utiliser
library(dplyr)
library(cluster)
#define linkage methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
#function to compute agglomerative coefficient
ac <- function(x) {
  agnes(ppc_imp, method = x)$ac
}
#calculate agglomerative coefficient for each clustering linkage method
link_ppc = sapply(m, ac)
link_ppc
#Déterminer le nombre optimal de clusters
fviz_nbclust(ppc_imp,FUNcluster = hcut, method = "wss")
### ACP avec 4 composantes principales puis clustering avec 3 classes
# 1. ACP 
pca <- PCA(ppc_imp, ncp = 4,graph = FALSE)
# 2. HCPC
hcpc.pca <- HCPC(pca,
                 nb.clust = 4,
                 metric = "euclidean", 
                 method = "ward",
                 graph = FALSE)

# dendrogramme
dendo.pca = fviz_dend(hcpc.pca, 
                      cex = 0.7,                     # Taille du text
                      palette = "jco",               # Palette de couleur ?ggpubr::ggpar
                      rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
                      rect_border = "jco",           # Couleur du rectangle
                      labels_track_height = 0.8)      # Augment l'espace pour le texte
dendo.pca
# Visualiser les clusters
clust.pca = fviz_cluster(hcpc.pca,
                         repel = TRUE,            # Evite le chevauchement des textes
                         show.clust.cent = TRUE, # Montre le centre des clusters
                         palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
                         ggtheme = theme_minimal(),
                         main = "Factor map")
clust.pca
# Caractérisations des classes par les variables
hcpc.pca$desc.var
# Caractérisations des classes par les individus
hcpc.pca$desc.ind


#------------------ Dépassements aux normes
# selection de la bd avec les cluster
bd1 = hcpc.pca$data.clust
bd1

## cluster 1
### selection de bd
cluster_1 = subset(bd1, clust=="1")
##### PH : seuil compris entre 6.5 et 8.5
shapiro.test(cluster_1$PH) # pas de normalité
wilcox.test(cluster_1$PH,mu=6.5,alternative = "two.sided")
wilcox.test(cluster_1$PH,mu=6.5,alternative = "greater") # mean(PH)>6.5
wilcox.test(cluster_1$PH,mu=8.5,alternative = "two.sided")
wilcox.test(cluster_1$PH,mu=8.5,alternative = "greater") # mean(PH)<8.5
##### Nitrite
shapiro.test(cluster_1$Nitrite) # pas de normalité
wilcox.test(cluster_1$Nitrite,mu=0.1,alternative = "two.sided")
wilcox.test(cluster_1$Nitrite,mu=0.1,alternative = "greater") # mean<0.1
##### Mineralisation
shapiro.test(cluster_1$Minéralisation) # pas de normalité
t.test(cluster_1$Minéralisation,mu=1000,alternative = "two.sided")
t.test(cluster_1$Minéralisation,mu=1000,alternative = "greater") # mean<
##### Fluorure
shapiro.test(cluster_1$Fluorure) # pas de normalité
wilcox.test(cluster_1$Fluorure,mu=1.5,alternative = "two.sided")
wilcox.test(cluster_1$Fluorure,mu=1.5,alternative = "greater") # <
##### Temperature
shapiro.test(cluster_1$Température) # pas de normalité
t.test(cluster_1$Température,mu=25,alternative = "two.sided")
t.test(cluster_1$Température,mu=25,alternative = "greater") # >
##### Manganèse
shapiro.test(cluster_1$Manganèse) # pas de normalité
t.test(cluster_1$Manganèse,mu=0.1,alternative = "two.sided")
t.test(cluster_1$Manganèse,mu=0.1,alternative = "greater") # <
##### Nitrate
shapiro.test(cluster_1$Nitrate) # pas de normalité
t.test(cluster_1$Nitrate,mu=50,alternative = "two.sided")
t.test(cluster_1$Nitrate,mu=50,alternative = "greater") # <
##### Chlorure
shapiro.test(cluster_1$Chlorure) # pas de normalité
wilcox.test(cluster_1$Chlorure,mu=250,alternative = "two.sided")
wilcox.test(cluster_1$Chlorure,mu=250,alternative = "greater") # <

## cluster 2
### selection de bd
cluster_2 = subset(bd1, clust=="2")
##### Ammonium
shapiro.test(cluster_2$Ammonium) #  normalité
t.test(cluster_2$Ammonium,mu=1.5,alternative = "two.sided")
t.test(cluster_2$Ammonium,mu=1.5,alternative = "greater")
##### Manganèse
shapiro.test(cluster_2$Manganèse) # pas de normalité
wilcox.test(cluster_2$Manganèse,mu=0.1,alternative = "two.sided")
wilcox.test(cluster_2$Manganèse,mu=0.1,alternative = "greater")
##### Dureté total
shapiro.test(cluster_2$Dureté_Total) # pas de normalité
wilcox.test(cluster_2$Dureté_Total,mu=45,alternative = "two.sided")
wilcox.test(cluster_2$Dureté_Total,mu=45,alternative = "greater")
##### Chlorure
shapiro.test(cluster_2$Chlorure) # pas de normalité
wilcox.test(cluster_2$Chlorure,mu=250,alternative = "two.sided")
wilcox.test(cluster_2$Chlorure,mu=250,alternative = "greater")

## cluster 3
### selection de bd
cluster_3 = subset(bd1, clust=="3")
##### Chlorure
shapiro.test(cluster_3$Chlorure) #  normalité
t.test(cluster_3$Chlorure,mu=250,alternative = "two.sided")
t.test(cluster_3$Chlorure,mu=250,alternative = "greater")
##### Chlore résiduel : seuil [0.2 ; 0.5]
shapiro.test(cluster_3$Chlore_résiduel) # pas de normalité
wilcox.test(cluster_3$Chlore_résiduel,mu=0.2,alternative = "two.sided")
wilcox.test(cluster_3$Chlore_résiduel,mu=0.5,alternative = "two.sided")
##### Température
shapiro.test(cluster_3$Température) # normalité
t.test(cluster_3$Température,mu=25,alternative = "two.sided")
t.test(cluster_3$Température,mu=25,alternative = "greater")
##### Nitrate
shapiro.test(cluster_3$Nitrate) # normalité
t.test(cluster_3$Nitrate,mu=50,alternative = "two.sided")
t.test(cluster_3$Nitrate,mu=50,alternative = "greater")


