
#--Renommer variables
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")

##Gestion des NA

##Suppression des ménages ne consommant pas de céréales
attach(cereales)
anyNA(Qtty_cons)

##Création d'une variable temporaire
cereales$t <- ifelse(is.na(Qtty_cons)==1,1,0)
table(cereales$t)
cereales_na <- cereales[cereales$t==1,] ##Sous la base cereales
View(cereales_na)

##Suppression des ménages n'ayant pas déclaré les qtités cons

cereales <- cereales[cereales$t==0,]
dim(cereales)

##Suppression de la variable temporaire
cereales$t <- NULL

##-Recodage de la variable cereales__id
cereales$cereales__id_recoded <- factor(cereales$cereales__id,
                                        levels = unname(attr(cereales$cereales__id,
                                                             "labels")),
                                        labels =names(attr(cereales$cereales__id,
                                                           "labels")))
edit(cereales$cereales__id_recoded)
View(cereales)

##-Recodage de la variable Unite_cons pour avoir les unites de mesures correspondantes
cereales$Unite_cons_recoded <- factor(cereales$Unite_cons,
                                      levels = unname(attr(cereales$Unite_cons,
                                                           "labels")),
                                      labels =names(attr(cereales$Unite_cons,
                                                         "labels")))
edit(cereales$Unite_cons_recoded)

##-Recodage de la variable Taille_cons pour avoir les unites de mesures correspondantes
cereales$Taille_cons_recoded <- factor(cereales$Taille_cons,
                                       levels = unname(attr(cereales$Taille_cons,
                                                            "labels")),
                                       labels =names(attr(cereales$Taille_cons,
                                                          "labels")))
edit(cereales$Taille_cons_recoded)

##-Changer le type d'une variable
attach(cereales)
DernierAchat <- as.factor(DernierAchat)
Taille_cons_recoded <- as.character(Taille_cons_recoded)


##-Decoupage en classe, identifier une cereale et une unite standard
cereales$classCereal_RizKg <- if_else(cereales$cereales__id==1 & cereales$Unite_cons==100,
                                      cut(cereales$Qtty_cons, labels = c("Tres faible","Faible", "Moyen" ,"Eleve"), 
                                          breaks = c(0,50,70,110,168)), NA)
edit(cereales$classCereal_RizKg)



###Créons dans la base cereales et la base table_de_conversion une variable real_id qui est la concaténation des valeurs de produitID, UniteID et TailleID
View(cereales)
cereales$real_id <- paste(cereales$cereales__id, cereales$Unite_cons, cereales$Taille_cons)
View(cereales)
table_de_conversion$real_id <- paste(table_de_conversion$produitID, table_de_conversion$uniteID, table_de_conversion$tailleID)
View(table_de_conversion)

cereales$real_id_achat <- paste(cereales$cereales__id, cereales$Unite_achat, cereales$Taille_achat)
View(cereales)
Table_de_conversion_phase_2$real_id_achat <- paste(Table_de_conversion_phase_2$produitID, Table_de_conversion_phase_2$uniteID, Table_de_conversion_phase_2$tailleID)
View(table_de_conversion)


###Nous pouvons maintenant fusionner la base cereales et la base table_de_conversion par la variable real_id
basefusionnee2 <- merge(cereales, Table_de_conversion_phase_2, by= "real_id_achat", all.x = TRUE, na.rm= TRUE)
View(basefusionnee2)
library(dplyr)

#Calculons le prix unitaire pour chaque unité de taille (pour chaque combinaison de produit, unité, taille)
basefusionnee2$prix_unitaire_real<- basefusionnee2$Value_achat/basefusionnee2$Qtty_achat


#Extraire les prix et autres dans une nouvelle base de données
prix_unitaire <- subset(basefusionnee2,!is.na(prix_unitaire_real),
                        select =c("cereales__id","Unite_achat", "Taille_achat", "prix_unitaire_real"))


prix_unitaire <- as.numeric(prix_unitaire)
prix_unitaire<- prix_unitaire %>%
  group_by(cereales__id,Unite_achat,Taille_achat) %>%
  mutate(pu_mean = mean(prix_unitaire_real, na.rm = T), pu_median=median(prix_unitaire_real,na.rm=T))

#' Vous aurez une base prixunitaire dont chque (p,u,t) aura 
#' un seul prix p ; 

prixunitaire2 <- prix_unitaire %>% select(cereales__id, Unite_achat,
                                         Taille_achat,pu_mean, pu_median) 


cereales2 <- merge(basefusionnee2, prixunitaire2, 
                   by.x=c("cereales__id","Unite_cons","Taille_cons"),
                   by.y = c("cereales__id", "Unite_achat", "Taille_achat"),
                   all.x = T)
#'Imputons les valeurs manquantes par la moyenne des prix pour une céréale et une unité donnée

cereales2 <- cereales2 %>%
  group_by(cereales__id)   %>% group_by(Unite_achat)   %>%
  mutate(mean_prix = mean(prix_unitaire_real, na.rm = TRUE),
         prix_unitaire_real = ifelse(is.na(prix_unitaire_real) ==TRUE, mean_prix, prix_unitaire_real))
cereales3 <-distinct(cereales2)
View(cereales3)
