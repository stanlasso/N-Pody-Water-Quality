# charger la bd
df = read_excel("Data_initial.Xlsx",sheet = "Data_cocody_Abob")
# inspecter la bd
View(df)
dim(df)
# retirer les indésirables
df2 = df[-c(1,2,3,5,27)]
View(df2)
dim(df2)
str(df2)
df2$Lieu_prél = as.factor(df2$Lieu_prél)
attach(df2)

# calcule de moyenne par lieu de pélevement

d1 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Couleur = mean(Color,na.rm = T)))

d2 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Turbidité = mean(Turbidity,na.rm = T)))

d3 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Minéralisation = mean(Minéralisation,na.rm = T)))

d4 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Température = mean(Temperature,na.rm = T)))

d5 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(PH = mean(Ph,na.rm = T)))

d6 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Nitrate = mean(Nitrates,na.rm = T)))

d7 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Nitrite = mean(Nitrites,na.rm = T)))

d8 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Ammonium = mean(Ammonium,na.rm = T)))

d9 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(M_O = mean(M_O,na.rm = T)))

d10 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Chlorure = mean(Chlorure,na.rm = T)))

d11 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Dureté_Total = mean(Dureté_Total,na.rm = T)))

d12 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Alcalinité = mean(Alcalinité,na.rm = T)))

d13 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Chlore_résiduel = mean(Chlore_résiduel,na.rm = T)))

d14 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Fer = mean(Fer,na.rm = T)))

d15 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Manganèse = mean(Mn,na.rm = T)))

d16 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Aluminium = mean(Aluminium,na.rm = T)))

d17 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Fluorure = mean(Fluorure,na.rm = T)))

d18 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Coliformes_Totaux = mean(Coliformes_Totaux,na.rm = T)))

d19 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Coli_Thermotolérant = mean(Coli_Thermotolérant,na.rm = T)))

d20 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(E_coli = mean(E_coli,na.rm = T)))

d21 = as.data.frame(df2 %>% group_by(Lieu_prél) %>%
  dplyr::summarise(Strepto_fécalis = mean(Strepto_fécalis,na.rm = T)))

df.fin = cbind(d1,d2[2],d3[2],d4[2],d5[2],d6[2],d7[2],d8[2],d9[2],d10[2],
               d11[2],d12[2],d13[2],d14[2],d15[2],d16[2],d17[2],d18[2],
               d19[2],d20[2],d21[2])

row_name_df.fin = paste("R", 1:nrow(df.fin), sep = "")

row.names(df.fin) = row_name_df.fin

df.fin

#readr::write_csv2(df.fin, file="Data.csv")
