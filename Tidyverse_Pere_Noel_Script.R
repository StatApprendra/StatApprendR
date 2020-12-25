
# --------------------------------------------------
# Gère la liste du Père Noël sous R avec Tidyverse
#
# --------------------------------------------------



# On prépare ses affaires
# --------------------------------------------------

# Chargement du package
library(tidyverse)
rm(list=ls())

# Chargement de la liste de cadeau du père Noël
# On précise que les case vides seront considérés comme des NA (= vide)
LISTE_PN1 <- read.csv2("Liste_Pere_Noel.csv", na.strings="")
TAB1918 <- read.csv2("Note_Gentillesse_2019-2018.csv", na.strings="")



# On travaille maintenant sur le liste du Père Noël
# --------------------------------------------------

# Choisir les colonnes selon le prénom, la note de gentillesse, et la ville
LISTE_PN2 <- LISTE_PN1 %>% select(Nom, Villle, Gentil2020)

# Garder les lignes dont la valeur de (colonne) gentillesse est supérieure à 10
LISTE_PN3 <- LISTE_PN2 %>% filter(Gentil2020>10)

# Renommer le nom de la colonne Ville par Ville
LISTE_PN3 <- LISTE_PN3 %>% rename(Ville = Villle)

# Garder les lignes dont la valeur de (colonne) Ville est NA
TAB_NA <- LISTE_PN3 %>% filter(is.na(Ville))

# Garder les lignes dont (colonne) Ville possède des infos
LISTE_PN4 <- LISTE_PN3 %>% filter(!is.na(Ville))

# Fusion de deux tableaux dont le colonne commune s'appelle Nom
LISTE_PN5 <- inner_join(LISTE_PN4, TAB1918, by="Nom")

# Créer une colonne qui s'appelle gentil3ans et la remplir avec 
# la moyenne de gentillesse sur les 3 ans
LISTE_PN6 <- LISTE_PN5 %>%  
  mutate(Gentil3ans=(Gentil2020 + Gentil2018 + Gentil2019)/3)

# Grouper par villes ensuite calculer la moyenne de gentillesse
TAB_VILLE <- LISTE_PN6 %>% 
  group_by(Ville) %>% 
  summarise(Moy_Ville = mean(Gentil3ans))


