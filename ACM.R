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
## Prétraitement
#Conception du jeu de données relatif aux paramètres microbiologique (pmb)
dechet = df[,18:21]
# Résumé du jeu de données relatif aux pmb
#summary(dechet)
# recodage des pmb : 0 pour absence et 1 pour présence
dechet$Coliformes_Totaux=as.factor(ifelse(dechet$Coliformes_Totaux=='0','0','1'))
dechet$Coli_Thermotolérant=as.factor(ifelse(dechet$Coli_Thermotolérant=='0','0','1'))
dechet$E_coli = as.factor(ifelse(dechet$E_coli=='0','0','1'))
dechet$Strepto_fécalis = as.factor(ifelse(dechet$Strepto_fécalis=='0','0','1'))
#Visualiser les données manquantes
naniar::vis_miss(dechet)
# Imputation des données manquantes par le mode
library(mice)
pmb_mice = mice(dechet)
pmb_imp = complete(pmb_mice)
naniar::vis_miss(pmb_imp)
#------------------------------- ACM
# Mise en oeuvre de l'ACM
acm <- pmb_imp %>% MCA (graph = F)
names (acm)
# Valeurs propres et inertie moyenne
acm$eig
# Eboulis des valeurs propres
fviz_screeplot (acm, addlabels = TRUE, ylim = c(0, 75))
# Qualité de représentation et visualisation des variables
## repel = TRUE pour éviter le chevauchement de texte
fviz_mca_var (acm, repel = TRUE)
#Visualiser les individus
fviz_mca_ind(acm,
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())
#------------------------------- CAH
## Trouver la méthode d'agglomération à utiliser
library(dplyr)
library(cluster)
#define linkage methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
#function to compute agglomerative coefficient
ac <- function(x) {
  agnes(pmb_imp, method = x)$ac
}
#calculate agglomerative coefficient for each clustering linkage method
link_pmb = sapply(m, ac)
link_pmb
## Déterminer le nombre optimal de clusters
fviz_nbclust(pmb_imp,FUNcluster = hcut, method = "wss")
## ACM avec 2 composantes principales puis clustering avec 5 classes
# Performing MCA
res.mca <- MCA(pmb_imp, 
               ncp = 2,            # Number of components kept
               graph=FALSE)
res.hcpc.mca <- HCPC (res.mca,
                      nb.clust = 5,
                      metric = "euclidean", 
                      method = "ward", 
                      graph = FALSE)

## Visualiser les clusters
clust.mca = fviz_cluster(res.hcpc.mca,
                         repel = TRUE,            # Evite le chevauchement des textes
                         show.clust.cent = TRUE, # Montre le centre des clusters
                         palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
                         ggtheme = theme_minimal(),
                         main = "Factor map")
clust.mca

## Description des classes par les variables
res.hcpc.mca$desc.var
# description des classes par les individus 
res.hcpc.mca$desc.ind



