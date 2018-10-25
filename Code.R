
# base$CatechoPPC <- as.character(base$Catechoindic)
# base$CatechoPPC[base$Catechoindic %in% c(	)] <- "0"
# base$CatechoPPC[base$Catechoindic %in% c("", "NA")] <- "NA"
# base$CatechoPPC[base$Catechoindic %in% c(	)] <- "1"
# base$CatechoPPC <- as.factor(base$CatechoPPC)


library(tidyverse)
library(questionr)
# library(epiDisplay)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)

setwd("/Users/antoinegaudetchardonnet/Documents/Sara/R")
base <- read.csv2("Base3.csv")

# TypeHED
# HED =  "HED","HED+ HSD","HED. HIP","HED. HSA","HED. HSD","HIV. HSA. HSD. HED","HSD. HED. ","HSD. HM. HED. contusion"

base$TypeHED <- as.character(base$TypeCL)
base$TypeHED[base$TypeCL %in% c("0","Anev Com post D","Dissec carot G ","HED","HED+ HSD","HED. HIP","HED. HSA","HED. HSD","HIP","HIP. HSA","HIP. HSA. HSD","HIV","HIV. HSA. HSD. HED","HM+contusion","HM. HIP","HM. HIV","HM. HSD","HSA","HSA ","HSA HIP","HSA+ HSD+ contusion","HSA. HIP. dissection carot","HSA. HSD","HSA. HSD. HIP","HSA. petechies","HSD","HSD ","HSD, HSA","HSD. HED. ","HSD. HIP","HSD. HM","HSD. HM. HED. contusion","HSD. HSA","HTIC","Hematome IC","Hematome fosse post","NA","Petechies","Petehies, HSD","TC","contusion","contusion front D. oedeme. H","contusion. HSA. DC","contusions ","embarrure ","hematome","hematome FT","oedeme","oedeme. HSA","petechies", "contusion front D. oedeme. HM")] <- "0"
base$TypeHED[base$TypeCL %in% c("", "NA")] <- "NA"
base$TypeHED[base$TypeCL %in% c("HED","HED+ HSD","HED. HIP","HED. HSA","HED. HSD","HIV. HSA. HSD. HED","HSD. HED. ","HSD. HM. HED. contusion")] <- "1"
base$TypeHED <- as.factor(base$TypeHED)

#Type Oedeme
Oedeme = 

base$TypeOedeme <- as.character(base$TypeCL)
base$TypeOedeme[base$TypeCL %in% c("0","Anev Com post D","Dissec carot G ","HED","HED+ HSD","HED. HIP","HED. HSA","HED. HSD","HIP","HIP. HSA","HIP. HSA. HSD","HIV","HIV. HSA. HSD. HED","HM+contusion","HM. HIP","HM. HIV","HM. HSD","HSA","HSA ","HSA HIP","HSA+ HSD+ contusion","HSA. HIP. dissection carot","HSA. HSD","HSA. HSD. HIP","HSA. petechies","HSD","HSD ","HSD, HSA","HSD. HED. ","HSD. HIP","HSD. HM","HSD. HM. HED. contusion","HSD. HSA","HTIC","Hematome IC","Hematome fosse post","NA","Petechies","Petehies, HSD","TC","contusion","contusion front D. oedeme. H","contusion. HSA. DC","contusions ","embarrure ","hematome","hematome FT","oedeme","oedeme. HSA","petechies", "contusion front D. oedeme. HM")] <- "0"
base$TypeOedeme[base$TypeCL %in% c("", "NA")] <- "NA"
base$TypeOedeme[base$TypeCL %in% c("HTIC","contusion front D. oedeme. H","oedeme","oedeme. HSA","contusion front D. oedeme. HM")] <- "1"
base$TypeOedeme <- as.factor(base$TypeOedeme)

# Type HSD
HSD = 

base$TypeHSD <- as.character(base$TypeCL)
base$TypeHSD[base$TypeCL %in% c("0","Anev Com post D","Dissec carot G ","HED","HED+ HSD","HED. HIP","HED. HSA","HED. HSD","HIP","HIP. HSA","HIP. HSA. HSD","HIV","HIV. HSA. HSD. HED","HM+contusion","HM. HIP","HM. HIV","HM. HSD","HSA","HSA ","HSA HIP","HSA+ HSD+ contusion","HSA. HIP. dissection carot","HSA. HSD","HSA. HSD. HIP","HSA. petechies","HSD","HSD ","HSD, HSA","HSD. HED. ","HSD. HIP","HSD. HM","HSD. HM. HED. contusion","HSD. HSA","HTIC","Hematome IC","Hematome fosse post","NA","Petechies","Petehies, HSD","TC","contusion","contusion front D. oedeme. H","contusion. HSA. DC","contusions ","embarrure ","hematome","hematome FT","oedeme","oedeme. HSA","petechies", "contusion front D. oedeme. HM")] <- "0"
base$TypeHSD[base$TypeCL %in% c("", "NA")] <- "NA"
base$TypeHSD[base$TypeCL %in% c("HED+ HSD","HED. HSD","HIP. HSA. HSD","HIV. HSA. HSD. HED","HM. HSD","HSA+ HSD+ contusion","HSA. HSD","HSD","HSD ","HSD, HSA","HSD. HED. ","HSD. HIP","HSD. HM","HSD. HM. HED. contusion","Petehies, HSD")] <- "1"
base$TypeHSD <- as.factor(base$TypeHSD)

# Type HSA
HSA = 

base$TypeHSA <- as.character(base$TypeCL)
base$TypeHSA[base$TypeCL %in% c("0","Anev Com post D","Dissec carot G ","HED","HED+ HSD","HED. HIP","HED. HSA","HED. HSD","HIP","HIP. HSA","HIP. HSA. HSD","HIV","HIV. HSA. HSD. HED","HM+contusion","HM. HIP","HM. HIV","HM. HSD","HSA","HSA ","HSA HIP","HSA+ HSD+ contusion","HSA. HIP. dissection carot","HSA. HSD","HSA. HSD. HIP","HSA. petechies","HSD","HSD ","HSD, HSA","HSD. HED. ","HSD. HIP","HSD. HM","HSD. HM. HED. contusion","HSD. HSA","HTIC","Hematome IC","Hematome fosse post","NA","Petechies","Petehies, HSD","TC","contusion","contusion front D. oedeme. H","contusion. HSA. DC","contusions ","embarrure ","hematome","hematome FT","oedeme","oedeme. HSA","petechies", "contusion front D. oedeme. HM")] <- "0"
base$TypeHSA[base$TypeCL %in% c("", "NA")] <- "NA"
base$TypeHSA[base$TypeCL %in% c("HIV","HIV. HSA. HSD. HED","HM. HIV","Anev Com post D","HED. HSA","HIP. HSA","HIP. HSA. HSD","HIV. HSA. HSD. HED","HM+contusion","HM. HIP","HM. HIV","HM. HSD","HSA","HSA ","HSA HIP","HSA+ HSD+ contusion","HSA. HIP. dissection carot","HSA. HSD","HSA. HSD. HIP","HSD, HSA","HSD. HM","HSD. HM. HED. contusion","HSD. HSA","contusion front D. oedeme. H","contusion. HSA. DC","oedeme. HSA","contusion front D. oedeme. HM")] <- "1"
base$TypeHSA <- as.factor(base$TypeHSA)

# Type Petechies
Petechies = 

base$TypePetechies <- as.character(base$TypeCL)
base$TypePetechies[base$TypeCL %in% c("0","Anev Com post D","Dissec carot G ","HED","HED+ HSD","HED. HIP","HED. HSA","HED. HSD","HIP","HIP. HSA","HIP. HSA. HSD","HIV","HIV. HSA. HSD. HED","HM+contusion","HM. HIP","HM. HIV","HM. HSD","HSA","HSA ","HSA HIP","HSA+ HSD+ contusion","HSA. HIP. dissection carot","HSA. HSD","HSA. HSD. HIP","HSA. petechies","HSD","HSD ","HSD, HSA","HSD. HED. ","HSD. HIP","HSD. HM","HSD. HM. HED. contusion","HSD. HSA","HTIC","Hematome IC","Hematome fosse post","NA","Petechies","Petehies, HSD","TC","contusion","contusion front D. oedeme. H","contusion. HSA. DC","contusions ","embarrure ","hematome","hematome FT","oedeme","oedeme. HSA","petechies", "contusion front D. oedeme. HM")] <- "0"
base$TypePetechies[base$TypeCL %in% c("", "NA")] <- "NA"
base$TypePetechies[base$TypeCL %in% c("HSA. petechies","Petechies","Petehies, HSD","petechies")] <- "1"
base$TypePetechies <- as.factor(base$TypePetechies)

# Types Hemaatome
Dissection = 

base$TypeHematome <- as.character(base$TypeCL)
base$TypeHematome[base$TypeCL %in% c("0","Anev Com post D","Dissec carot G ","HED","HED+ HSD","HED. HIP","HED. HSA","HED. HSD","HIP","HIP. HSA","HIP. HSA. HSD","HIV","HIV. HSA. HSD. HED","HM+contusion","HM. HIP","HM. HIV","HM. HSD","HSA","HSA ","HSA HIP","HSA+ HSD+ contusion","HSA. HIP. dissection carot","HSA. HSD","HSA. HSD. HIP","HSA. petechies","HSD","HSD ","HSD, HSA","HSD. HED. ","HSD. HIP","HSD. HM","HSD. HM. HED. contusion","HSD. HSA","HTIC","Hematome IC","Hematome fosse post","NA","Petechies","Petehies, HSD","TC","contusion","contusion front D. oedeme. H","contusion. HSA. DC","contusions ","embarrure ","hematome","hematome FT","oedeme","oedeme. HSA","petechies", "contusion front D. oedeme. HM")] <- "0"
base$TypeHematome[base$TypeCL %in% c("", "NA")] <- "NA"
base$TypeHematome[base$TypeCL %in% c("Dissec carot G ","HED. HIP","HIP","HIP. HSA","HIP. HSA. HSD","HM+contusion","HSA HIP","HSA+ HSD+ contusion","HSA. HIP. dissection carot","HSA. HSD. HIP","HSD. HIP","HSD. HM. HED. contusion","Hematome IC","Hematome fosse post","contusion","contusion front D. oedeme. H","contusion. HSA. DC","contusions ","embarrure ","hematome","hematome FT","contusion front D. oedeme. HM")] <- "1"
base$TypeHematome <- as.factor(base$TypeHematome)

# DefHD
# Def HD = "1", HD","HD, pneumo","HD. pneumo","HD. respi","multi","respi. HD"

base$DefHD <- as.character(base$Defaillance)
base$DefHD[base$Defaillance %in% c("0","1","HD","HD, pneumo","HD. pneumo","HD. respi","multi","respi","respi. HD")] <- "0"
base$DefHD[base$Defaillance %in% c("", "NA")] <- "NA"
base$DefHD[base$Defaillance %in% c("1","HD","HD, pneumo","HD. pneumo","HD. respi","multi","respi. HD")] <- "1"
base$DefHD <- as.factor(base$DefHD)

# DefRespi
# DefRespi = "HD, pneumo","HD. pneumo","HD. respi","multi","respi","respi. HD"

base$DefRespi <- as.character(base$Defaillance)
base$DefRespi[base$Defaillance %in% c("0","1","HD","HD, pneumo","HD. pneumo","HD. respi","multi","respi","respi. HD")] <- "0"
base$DefRespi[base$Defaillance %in% c("", "NA")] <- "NA"
base$DefRespi[base$Defaillance %in% c("HD, pneumo","HD. pneumo","HD. respi","multi","respi","respi. HD")] <- "1"
base$DefRespi <- as.factor(base$DefRespi)

# Atteinte Ortho

base$AtteinteOrtho <- as.character(base$AtteinteOrgane)
base$AtteinteOrtho[base$AtteinteOrgane %in% c("0","#face. PNO. rachis ","#face. femur. bassin","#face. ortho. bassin. PNO","#face. rein. PNO","0","PNM. PNO. face","PNO. bassin. rachis ","abdo","abdo. ortho","abdo. ortho. cotes. rachis","bassin. rate. rachis","face. abdo. orthp","face. ortho","face. ortho. abdo","face. rachis","face. thx. rachis. ","hemoPNO. bassin. #bassin. ","jambes","orho","ortho","ortho. PNO.","ortho. PNO. face","ortho. abdo","ortho. abdo. rachis","ortho. hemoPNO","pneumo","rachis. face. ortho","rachis. ortho. ","rechis. ORL. abdo","rechis. bassin. abdo","rein. respi","th", "","#face. PNO. rachis ","#face. femur. bassin", "#face. ortho. bassin. PNO", "#face. rein. PNO","0","PNM. PNO. face","PNO. bassin. rachis ","abdo","abdo, face, pneumo","abdo, face, pneumo, ortho","abdo, ortho","abdo, pneumo","abdo, pneumo, ortho","abdo. ortho","abdo. ortho. cotes. rachis","bassin. rate. rachis","face","face, pneumo","face, pneumo, ortho","face. abdo. orthp","face. ortho","face. ortho. abdo","face. rachis","face. thx. rachis. ","hemoPNO. bassin. #bassin. ","jambes","ortho","ortho, pneumo","ortho. PNO.","ortho. PNO. face","ortho. abdo","ortho. abdo. rachis","ortho. hemoPNO","pneumo","rachis. face. ortho","rachis. ortho. ","rechis. ORL. abdo","rechis. bassin. abdo","rein. respi","th")] <- "0"
base$AtteinteOrtho[base$AtteinteOrgane %in% c("", "NA")] <- "NA"
base$AtteinteOrtho[base$AtteinteOrgane %in% c("#face. PNO. rachis ","#face. femur. bassin", "#face. ortho. bassin. PNO", "PNO. bassin. rachis ","abdo, face, pneumo, ortho","abdo, ortho","abdo, pneumo, ortho","abdo. ortho. cotes. rachis","bassin. rate. rachis","face, pneumo, ortho","face. abdo. orthp","face. ortho","face. ortho. abdo","face. rachis","face. thx. rachis. ","hemoPNO. bassin. #bassin. ","jambes","ortho","ortho, pneumo","ortho. PNO.","ortho. PNO. face","ortho. abdo","ortho. abdo. rachis","ortho. hemoPNO","rachis. face. ortho","rachis. ortho. ","rechis. ORL. abdo","rechis. bassin. abdo","h\303\251moPNO. bassin. #bassin. ","#face. f\303\251mur. bassin","#face. PNO. rachis ","#face. femur. bassin","#face. ortho. bassin. PNO","PNO. bassin. rachis ","abdo. ortho","abdo. ortho. cotes. rachis","bassin. rate. rachis","face. abdo. orthp","face. ortho","face. ortho. abdo","face. rachis","face. thx. rachis. ","hemoPNO. bassin. #bassin. ","jambes","orho","ortho","ortho. PNO.","ortho. PNO. face","ortho. abdo","ortho. abdo. rachis","ortho. hemoPNO","rachis. face. ortho","rachis. ortho. ","rechis. ORL. abdo","rechis. bassin. abdo")] <- "1"
base$AtteinteOrtho <- as.factor(base$AtteinteOrtho)

# Atteinte Abdo

base$AtteinteAbdo <- as.character(base$AtteinteOrgane)
base$AtteinteAbdo[base$AtteinteOrgane %in% c("0","","#face. PNO. rachis ","#face. femur. bassin", "#face. ortho. bassin. PNO", "#face. rein. PNO","0","PNM. PNO. face","PNO. bassin. rachis ","abdo","abdo, face, pneumo","abdo, face, pneumo, ortho","abdo, ortho","abdo, pneumo","abdo, pneumo, ortho","abdo. ortho","abdo. ortho. cotes. rachis","bassin. rate. rachis","face","face, pneumo","face, pneumo, ortho","face. abdo. orthp","face. ortho","face. ortho. abdo","face. rachis","face. thx. rachis. ","hemoPNO. bassin. #bassin. ","jambes","ortho","ortho, pneumo","ortho. PNO.","ortho. PNO. face","ortho. abdo","ortho. abdo. rachis","ortho. hemoPNO","pneumo","rachis. face. ortho","rachis. ortho. ","rechis. ORL. abdo","rechis. bassin. abdo","rein. respi","th","h\303\251moPNO. bassin. #bassin. ","#face. f\303\251mur. bassin","#face. PNO. rachis ","#face. femur. bassin","#face. ortho. bassin. PNO","#face. rein. PNO","0","PNM. PNO. face","PNO. bassin. rachis ","abdo","abdo. ortho","abdo. ortho. cotes. rachis","bassin. rate. rachis","face. abdo. orthp","face. ortho","face. ortho. abdo","face. rachis","face. thx. rachis. ","hemoPNO. bassin. #bassin. ","jambes","orho","ortho","ortho. PNO.","ortho. PNO. face","ortho. abdo","ortho. abdo. rachis","ortho. hemoPNO","pneumo","rachis. face. ortho","rachis. ortho. ","rechis. ORL. abdo","rechis. bassin. abdo","rein. respi","th")] <- "0"
base$AtteinteAbdo[base$AtteinteOrgane %in% c("", "NA")] <- "NA"
base$AtteinteAbdo[base$AtteinteOrgane %in% c("abdo","abdo, face, pneumo","abdo, face, pneumo, ortho","abdo, ortho","abdo, pneumo","abdo, pneumo, ortho","abdo. ortho","abdo. ortho. cotes. rachis","bassin. rate. rachis","face. abdo. orthp","ortho. abdo","ortho. abdo. rachis","rechis. ORL. abdo","rechis. bassin. abdo","bassin. rate. rachis","abdo","abdo. ortho","abdo. ortho. cotes. rachis","face. abdo. orthp","face. ortho. abdo","ortho. abdo","ortho. abdo. rachis","rechis. ORL. abdo","rechis. bassin. abdo")] <- "1"
base$AtteinteAbdo <- as.factor(base$AtteinteAbdo)

# Atteinte Face

base$AtteinteFace <- as.character(base$AtteinteOrgane)
base$AtteinteFace[base$AtteinteOrgane %in% c("0","","#face. PNO. rachis ","#face. femur. bassin", "#face. ortho. bassin. PNO", "#face. rein. PNO","0","PNM. PNO. face","PNO. bassin. rachis ","abdo","abdo, face, pneumo","abdo, face, pneumo, ortho","abdo, ortho","abdo, pneumo","abdo, pneumo, ortho","abdo. ortho","abdo. ortho. cotes. rachis","bassin. rate. rachis","face","face, pneumo","face, pneumo, ortho","face. abdo. orthp","face. ortho","face. ortho. abdo","face. rachis","face. thx. rachis. ","hemoPNO. bassin. #bassin. ","jambes","ortho","ortho, pneumo","ortho. PNO.","ortho. PNO. face","ortho. abdo","ortho. abdo. rachis","ortho. hemoPNO","pneumo","rachis. face. ortho","rachis. ortho. ","rechis. ORL. abdo","rechis. bassin. abdo","rein. respi","th","h\303\251moPNO. bassin. #bassin. ", "","#face. PNO. rachis ","#face. femur. bassin","#face. ortho. bassin. PNO","#face. rein. PNO","0","PNM. PNO. face","PNO. bassin. rachis ","abdo","abdo. ortho","abdo. ortho. cotes. rachis","bassin. rate. rachis","face. abdo. orthp","face. ortho","face. ortho. abdo","face. rachis","face. thx. rachis. ","hemoPNO. bassin. #bassin. ","jambes","orho","ortho","ortho. PNO.","ortho. PNO. face","ortho. abdo","ortho. abdo. rachis","ortho. hemoPNO","pneumo","rachis. face. ortho","rachis. ortho. ","rechis. ORL. abdo","rechis. bassin. abdo","rein. respi","th")] <- "0"
base$AtteinteFace[base$AtteinteOrgane %in% c("", "NA")] <- "NA"
base$AtteinteFace[base$AtteinteOrgane %in% c("#face. PNO. rachis ","#face. femur. bassin", "#face. ortho. bassin. PNO", "#face. rein. PNO","PNM. PNO. face","abdo, face, pneumo","abdo, face, pneumo, ortho","face","face, pneumo","face, pneumo, ortho","face. abdo. orthp","face. ortho","face. ortho. abdo","face. rachis","face. thx. rachis. ","ortho. PNO. face","rachis. face. ortho","#face. f\303\251mur. bassin","#face. PNO. rachis ","#face. femur. bassin","#face. ortho. bassin. PNO","#face. rein. PNO","PNM. PNO. face","face. abdo. orthp","face. ortho","face. ortho. abdo","face. rachis","face. thx. rachis. ","ortho. PNO. face","rechis. ORL. abdo")] <- "1"
base$AtteinteFace <- as.factor(base$AtteinteFace)

# Atteinte Pneumo

base$AtteintePneumo <- as.character(base$AtteinteOrgane)
base$AtteintePneumo[base$AtteinteOrgane %in% c("face", "abdo, ortho","0","#face. f\303\251mur. bassin","#face. PNO. rachis ","#face. femur. bassin","#face. ortho. bassin. PNO","#face. rein. PNO","0","PNM. PNO. face","PNO. bassin. rachis ","abdo","abdo. ortho","abdo. ortho. cotes. rachis","bassin. rate. rachis","face. abdo. orthp","face. ortho","face. ortho. abdo","face. rachis","face. thx. rachis. ","hemoPNO. bassin. #bassin. ","jambes","orho","ortho","ortho. PNO.","ortho. PNO. face","ortho. abdo","ortho. abdo. rachis","ortho. hemoPNO","pneumo","rachis. face. ortho","rachis. ortho. ","rechis. ORL. abdo","rechis. bassin. abdo","rein. respi","th")] <- "0"
base$AtteintePneumo[base$AtteinteOrgane %in% c("", "NA")] <- "NA"
base$AtteintePneumo[base$AtteinteOrgane %in% c("#face. PNO. rachis ","#face. ortho. bassin. PNO", "#face. rein. PNO","PNM. PNO. face","PNO. bassin. rachis ","abdo, face, pneumo","abdo, face, pneumo, ortho","abdo, pneumo","abdo, pneumo, ortho","face, pneumo","face, pneumo, ortho","face. thx. rachis. ","hemoPNO. bassin. #bassin. ","ortho, pneumo","ortho. PNO.","ortho. PNO. face","ortho. hemoPNO","pneumo","rein. respi","th","h\303\251moPNO. bassin. #bassin. ","#face. PNO. rachis ","#face. ortho. bassin. PNO","#face. rein. PNO","PNM. PNO. face","PNO. bassin. rachis ","face. thx. rachis. ","hemoPNO. bassin. #bassin. ","ortho. PNO.","ortho. PNO. face","ortho. hemoPNO","pneumo","rein. respi","th")] <- "1"
base$AtteintePneumo <- as.factor(base$AtteintePneumo)






# Indication Catecho Choc
base$CatechoChoc <- as.character(base$Catechoindic)
base$CatechoChoc[base$Catechoindic %in% c("0","PPC","cho","choc","choc septique")] <- "0"
base$CatechoChoc[base$Catechoindic %in% c("", "NA")] <- "NA"
base$CatechoChoc[base$Catechoindic %in% c("cho","choc","choc septique")] <- "1"
base$CatechoChoc <- as.factor(base$CatechoChoc)

# Indication Catecho PPC
base$CatechoPPC <- as.character(base$Catechoindic)
base$CatechoPPC[base$Catechoindic %in% c("0","PPC","cho","choc","choc septique")] <- "0"
base$CatechoPPC[base$Catechoindic %in% c("", "NA")] <- "NA"
base$CatechoPPC[base$Catechoindic %in% c("PPC")] <- "1"
base$CatechoPPC <- as.factor(base$CatechoPPC)


# PAVM96Noso
base$PAVM96Noso <- as.character(base$PAVM96)
base$PAVM96Noso[base$PAVM96 %in% c("1","0")] <- "0"
base$PAVM96Noso[base$PAVM96 %in% c("", "NA")] <- "NA"
base$PAVM96Noso[base$PAVM96 %in% c("2")] <- "1"
base$PAVM96Noso <- as.factor(base$PAVM96Noso)

# PAVM96Comm
base$PAVM96Comm <- as.character(base$PAVM96)
base$PAVM96Comm[base$PAVM96 %in% c("2","0")] <- "0"
base$PAVM96Comm[base$PAVM96 %in% c("", "NA")] <- "NA"
base$PAVM96Comm[base$PAVM96 %in% c("1")] <- "1"
base$PAVM96Comm <- as.factor(base$PAVM96Comm)

# PAVM1Noso
base$PAVM1Noso <- as.character(base$PAVM1)
base$PAVM1Noso[base$PAVM1 %in% c("1","0")] <- "0"
base$PAVM1Noso[base$PAVM1 %in% c("", "NA")] <- "NA"
base$PAVM1Noso[base$PAVM1 %in% c("2")] <- "1"
base$PAVM1Noso <- as.factor(base$PAVM1Noso)

# PAVM1Comm
base$PAVM1Comm <- as.character(base$PAVM1)
base$PAVM1Comm[base$PAVM1 %in% c("2","0")] <- "0"
base$PAVM1Comm[base$PAVM1 %in% c("", "NA")] <- "NA"
base$PAVM1Comm[base$PAVM1 %in% c("1")] <- "1"
base$PAVM1Comm <- as.factor(base$PAVM1Comm)

# PAVM1Noso
base$PAVM17Noso <- as.character(base$PAVM17)
base$PAVM17Noso[base$PAVM17 %in% c("1","0")] <- "0"
base$PAVM17Noso[base$PAVM17 %in% c("", "NA")] <- "NA"
base$PAVM17Noso[base$PAVM17 %in% c("2")] <- "1"
base$PAVM17Noso <- as.factor(base$PAVM17Noso)

# PAVM1Comm
base$PAVM17Comm <- as.character(base$PAVM17)
base$PAVM17Comm[base$PAVM17 %in% c("2","0")] <- "0"
base$PAVM17Comm[base$PAVM17 %in% c("", "NA")] <- "NA"
base$PAVM17Comm[base$PAVM17 %in% c("1")] <- "1"
base$PAVM17Comm <- as.factor(base$PAVM17Comm)

# PAVM2Noso
base$PAVM2Noso <- as.character(base$PAVM2)
base$PAVM2Noso[base$PAVM2 %in% c("1","0")] <- "0"
base$PAVM2Noso[base$PAVM2 %in% c("", "NA")] <- "NA"
base$PAVM2Noso[base$PAVM2 %in% c("2")] <- "1"
base$PAVM2Noso <- as.factor(base$PAVM2Noso)

# PAVM2Comm
base$PAVM2Comm <- as.character(base$PAVM2)
base$PAVM2Comm[base$PAVM2 %in% c("2","0")] <- "0"
base$PAVM2Comm[base$PAVM2 %in% c("", "NA")] <- "NA"
base$PAVM2Comm[base$PAVM2 %in% c("1")] <- "1"
base$PAVM2Comm <- as.factor(base$PAVM2Comm)

# PAVM2Noso
base$PAVM27Noso <- as.character(base$PAVM27)
base$PAVM27Noso[base$PAVM27 %in% c("1","0")] <- "0"
base$PAVM27Noso[base$PAVM27 %in% c("", "NA")] <- "NA"
base$PAVM27Noso[base$PAVM27 %in% c("2")] <- "1"
base$PAVM27Noso <- as.factor(base$PAVM27Noso)

# PAVM2Comm
base$PAVM27Comm <- as.character(base$PAVM27)
base$PAVM27Comm[base$PAVM27 %in% c("2","0")] <- "0"
base$PAVM27Comm[base$PAVM27 %in% c("", "NA")] <- "NA"
base$PAVM27Comm[base$PAVM27 %in% c("1")] <- "1"
base$PAVM27Comm <- as.factor(base$PAVM27Comm)

# PAVM3Noso
base$PAVM3Noso <- as.character(base$PAVM3)
base$PAVM3Noso[base$PAVM3 %in% c("1","0")] <- "0"
base$PAVM3Noso[base$PAVM3 %in% c("", "NA")] <- "NA"
base$PAVM3Noso[base$PAVM3 %in% c("2")] <- "1"
base$PAVM3Noso <- as.factor(base$PAVM3Noso)

# PAVM3Comm
base$PAVM3Comm <- as.character(base$PAVM3)
base$PAVM3Comm[base$PAVM3 %in% c("2","0")] <- "0"
base$PAVM3Comm[base$PAVM3 %in% c("", "NA")] <- "NA"
base$PAVM3Comm[base$PAVM3 %in% c("1")] <- "1"
base$PAVM3Comm <- as.factor(base$PAVM3Comm)

# PAVM4Noso
base$PAVM4Noso <- as.character(base$PAVM4)
base$PAVM4Noso[base$PAVM4 %in% c("1","0")] <- "0"
base$PAVM4Noso[base$PAVM4 %in% c("", "NA")] <- "NA"
base$PAVM4Noso[base$PAVM4 %in% c("2")] <- "1"
base$PAVM4Noso <- as.factor(base$PAVM4Noso)

# PAVM4Comm
base$PAVM4Comm <- as.character(base$PAVM4)
base$PAVM4Comm[base$PAVM4 %in% c("2","0")] <- "0"
base$PAVM4Comm[base$PAVM4 %in% c("", "NA")] <- "NA"
base$PAVM4Comm[base$PAVM4 %in% c("1")] <- "1"
base$PAVM4Comm <- as.factor(base$PAVM4Comm)

# PAVM5Noso
base$PAVM5Noso <- as.character(base$PAVM5)
base$PAVM5Noso[base$PAVM5 %in% c("1","0")] <- "0"
base$PAVM5Noso[base$PAVM5 %in% c("", "NA")] <- "NA"
base$PAVM5Noso[base$PAVM5 %in% c("2")] <- "1"
base$PAVM5Noso <- as.factor(base$PAVM5Noso)

# PAVM5Comm
base$PAVM5Comm <- as.character(base$PAVM5)
base$PAVM5Comm[base$PAVM5 %in% c("2","0")] <- "0"
base$PAVM5Comm[base$PAVM5 %in% c("", "NA")] <- "NA"
base$PAVM5Comm[base$PAVM5 %in% c("1")] <- "1"
base$PAVM5Comm <- as.factor(base$PAVM5Comm)

# 
# "",
# "0",
# "Anev Com post D",
# "Dissec carot G ",
# "HED",
# "HED+ HSD",
# "HED. HIP",
# "HED. HSA",
# "HED. HSD",
# "HIP",
# "HIP. HSA",
# "HIP. HSA. HSD",
# "HIV",
# "HIV. HSA. HSD. HED",
# "HM+contusion",
# "HM. HIP",
# "HM. HIV",
# "HM. HSD",
# "HSA",
# "HSA ",
# "HSA HIP",
# "HSA+ HSD+ contusion",
# "HSA. HIP. dissection carot",
# "HSA. HSD",
# "HSA. HSD. HIP",
# "HSA. petechies",
# "HSD",
# "HSD ",
# "HSD, HSA",
# "HSD. HED. ",
# "HSD. HIP",
# "HSD. HM",
# "HSD. HM. HED. contusion",
# "HSD. HSA",
# "HTIC",
# "Hematome IC",
# "Hematome fosse post",
# "NA",
# "Petechies",
# "Petehies, HSD",
# "TC",
# "contusion",
# "contusion front D. oedeme. H",
# "contusion. HSA. DC",
# "contusions ",
# "embarrure ",
# "hematome",
# "hematome FT",
# "oedeme",
# "oedeme. HSA",
# "petechies",
# "contusion front D. oedeme. HM",
# 
# TC à verifier

write.csv(base, file = "MyData.csv", na="")

base2 <- base[base$Site == "1",]
base3 <- base[base$Site == "0",]

fitAGE <- aov(Age ~ Site, base)
summary (fitAGE)

AgeG<-c(
mean(base2$Age, na.rm = TRUE),
mean(base2$Age, na.rm = TRUE) - 1.96*sd(base2$Age, na.rm = TRUE)/sqrt(length(base2$Age[!is.na(base2$Age)])),
mean(base2$Age, na.rm = TRUE) + 1.96*sd(base2$Age, na.rm = TRUE)/sqrt(length(base2$Age[!is.na(base2$Age)]))
)
AgeG

AgeG<-c(
mean(base3$Age, na.rm = TRUE),
mean(base3$Age, na.rm = TRUE) - 1.96*sd(base3$Age, na.rm = TRUE)/sqrt(length(base3$Age[!is.na(base3$Age)])),
mean(base3$Age, na.rm = TRUE) + 1.96*sd(base3$Age, na.rm = TRUE)/sqrt(length(base3$Age[!is.na(base3$Age)]))
)
AgeG

# Tests popu #

tabASA <- table(base$ASA, base$Site)
tabASA
chisq.test(tabASA)
chisq.residuals(tabASA)





tabASA <- table(base$ASA, base$Site)
tabASA
chisq.test(tabASA)
chisq.residuals(tabASA)

tabASA1 <- table(base$ASA1, base$Site)
tabASA1
chisq.test(tabASA1)
chisq.residuals(tabASA1)
tabASA8 <- table(base$ASA8, base$Site)
tabASA8
chisq.test(tabASA8)
chisq.residuals(tabASA8)

tabASA9 <- table(base$ASA9, base$Site)
tabASA9
chisq.test(tabASA9)
chisq.residuals(tabASA9)

tabASA10 <- table(base$ASA10, base$Site)
tabASA10
chisq.test(tabASA10)
chisq.residuals(tabASA10)

tabASA11 <- table(base$ASA11, base$Site)
tabASA11
chisq.test(tabASA11)
chisq.residuals(tabASA11)

tabASA12 <- table(base$ASA12, base$Site)
tabASA12
chisq.test(tabASA12)
chisq.residuals(tabASA12)

tabASA13 <- table(base$ASA13, base$Site)
tabASA13
chisq.test(tabASA13)
chisq.residuals(tabASA13)

tabASA14 <- table(base$ASA14, base$Site)
tabASA14
chisq.test(tabASA14)
chisq.residuals(tabASA14)

tabASA15 <- table(base$ASA15, base$Site)
tabASA15
chisq.test(tabASA15)
chisq.residuals(tabASA15)

tabASA16 <- table(base$ASA16, base$Site)
tabASA16
chisq.test(tabASA16)
chisq.residuals(tabASA16)

tabASA17 <- table(base$ASA17, base$Site)
tabASA17
chisq.test(tabASA17)
chisq.residuals(tabASA17)

tabASA18 <- table(base$ASA18, base$Site)
tabASA18
chisq.test(tabASA18)
chisq.residuals(tabASA18)

