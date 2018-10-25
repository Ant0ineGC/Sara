

library(tidyverse)
library(questionr)
# library(epiDisplay)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)

setwd("/Users/antoinegaudetchardonnet/Git/Sara")

base <- read.csv2("Base3.csv")

base1 <- base[base$Site == "1",]
base0 <- base[base$Site == "0",]

base$grpASA <- cut(base$ASA, c(1, 2, 4), right = FALSE, include.lowest = TRUE)


# sink ("/Users/antoinegaudetchardonnet/Documents/Sara/R/popu2.csv", append = F)

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

t.test(Vmjours~Site,data=base)
VmjoursG<-c(
mean(base2$Vmjours, na.rm = TRUE),
mean(base2$Vmjours, na.rm = TRUE) - 1.96*sd(base2$Vmjours, na.rm = TRUE)/sqrt(length(base2$Vmjours[!is.na(base2$Vmjours)])),
mean(base2$Vmjours, na.rm = TRUE) + 1.96*sd(base2$Vmjours, na.rm = TRUE)/sqrt(length(base2$Vmjours[!is.na(base2$Vmjours)]))
)
VmjoursG

VmjoursG<-c(
mean(base3$Vmjours, na.rm = TRUE),
mean(base3$Vmjours, na.rm = TRUE) - 1.96*sd(base3$Vmjours, na.rm = TRUE)/sqrt(length(base3$Vmjours[!is.na(base3$Vmjours)])),
mean(base3$Vmjours, na.rm = TRUE) + 1.96*sd(base3$Vmjours, na.rm = TRUE)/sqrt(length(base3$Vmjours[!is.na(base3$Vmjours)]))
)
VmjoursG

t.test(Glasgow~Site,data=base)
GlasgowG<-c(
mean(base2$Glasgow, na.rm = TRUE),
mean(base2$Glasgow, na.rm = TRUE) - 1.96*sd(base2$Glasgow, na.rm = TRUE)/sqrt(length(base2$Glasgow[!is.na(base2$Glasgow)])),
mean(base2$Glasgow, na.rm = TRUE) + 1.96*sd(base2$Glasgow, na.rm = TRUE)/sqrt(length(base2$Glasgow[!is.na(base2$Glasgow)]))
)
GlasgowG

GlasgowG<-c(
mean(base3$Glasgow, na.rm = TRUE),
mean(base3$Glasgow, na.rm = TRUE) - 1.96*sd(base3$Glasgow, na.rm = TRUE)/sqrt(length(base3$Glasgow[!is.na(base3$Glasgow)])),
mean(base3$Glasgow, na.rm = TRUE) + 1.96*sd(base3$Glasgow, na.rm = TRUE)/sqrt(length(base3$Glasgow[!is.na(base3$Glasgow)]))
)
GlasgowG

t.test(Rankin~Site,data=base)
RankinG<-c(
mean(base2$Rankin, na.rm = TRUE),
mean(base2$Rankin, na.rm = TRUE) - 1.96*sd(base2$Rankin, na.rm = TRUE)/sqrt(length(base2$Rankin[!is.na(base2$Rankin)])),
mean(base2$Rankin, na.rm = TRUE) + 1.96*sd(base2$Rankin, na.rm = TRUE)/sqrt(length(base2$Rankin[!is.na(base2$Rankin)]))
)
RankinG

RankinG<-c(
mean(base3$Rankin, na.rm = TRUE),
mean(base3$Rankin, na.rm = TRUE) - 1.96*sd(base3$Rankin, na.rm = TRUE)/sqrt(length(base3$Rankin[!is.na(base3$Rankin)])),
mean(base3$Rankin, na.rm = TRUE) + 1.96*sd(base3$Rankin, na.rm = TRUE)/sqrt(length(base3$Rankin[!is.na(base3$Rankin)]))
)
RankinG

t.test(Duree.sejour~Site,data=base)
Duree.sejourG<-c(
mean(base2$Duree.sejour, na.rm = TRUE),
mean(base2$Duree.sejour, na.rm = TRUE) - 1.96*sd(base2$Duree.sejour, na.rm = TRUE)/sqrt(length(base2$Duree.sejour[!is.na(base2$Duree.sejour)])),
mean(base2$Duree.sejour, na.rm = TRUE) + 1.96*sd(base2$Duree.sejour, na.rm = TRUE)/sqrt(length(base2$Duree.sejour[!is.na(base2$Duree.sejour)]))
)
Duree.sejourG

Duree.sejourG<-c(
mean(base3$Duree.sejour, na.rm = TRUE),
mean(base3$Duree.sejour, na.rm = TRUE) - 1.96*sd(base3$Duree.sejour, na.rm = TRUE)/sqrt(length(base3$Duree.sejour[!is.na(base3$Duree.sejour)])),
mean(base3$Duree.sejour, na.rm = TRUE) + 1.96*sd(base3$Duree.sejour, na.rm = TRUE)/sqrt(length(base3$Duree.sejour[!is.na(base3$Duree.sejour)]))
)
Duree.sejourG

t.test(catechoDoseMax~Site,data=base)
catechoDoseMaxG<-c(
mean(base2$catechoDoseMax, na.rm = TRUE),
mean(base2$catechoDoseMax, na.rm = TRUE) - 1.96*sd(base2$catechoDoseMax, na.rm = TRUE)/sqrt(length(base2$catechoDoseMax[!is.na(base2$catechoDoseMax)])),
mean(base2$catechoDoseMax, na.rm = TRUE) + 1.96*sd(base2$catechoDoseMax, na.rm = TRUE)/sqrt(length(base2$catechoDoseMax[!is.na(base2$catechoDoseMax)]))
)
catechoDoseMaxG

catechoDoseMaxG<-c(
mean(base3$catechoDoseMax, na.rm = TRUE),
mean(base3$catechoDoseMax, na.rm = TRUE) - 1.96*sd(base3$catechoDoseMax, na.rm = TRUE)/sqrt(length(base3$catechoDoseMax[!is.na(base3$catechoDoseMax)])),
mean(base3$catechoDoseMax, na.rm = TRUE) + 1.96*sd(base3$catechoDoseMax, na.rm = TRUE)/sqrt(length(base3$catechoDoseMax[!is.na(base3$catechoDoseMax)]))
)
catechoDoseMaxG


fitGlasgow <- aov(Glasgow ~ Site, base)
summary (fitGlasgow)

GlasgowG<-c(
mean(base2$Glasgow, na.rm = TRUE),
mean(base2$Glasgow, na.rm = TRUE) - 1.96*sd(base2$Glasgow, na.rm = TRUE)/sqrt(length(base2$Glasgow[!is.na(base2$Glasgow)])),
mean(base2$Glasgow, na.rm = TRUE) + 1.96*sd(base2$Glasgow, na.rm = TRUE)/sqrt(length(base2$Glasgow[!is.na(base2$Glasgow)]))
)
GlasgowG

GlasgowG<-c(
mean(base3$Glasgow, na.rm = TRUE),
mean(base3$Glasgow, na.rm = TRUE) - 1.96*sd(base3$Glasgow, na.rm = TRUE)/sqrt(length(base3$Glasgow[!is.na(base3$Glasgow)])),
mean(base3$Glasgow, na.rm = TRUE) + 1.96*sd(base3$Glasgow, na.rm = TRUE)/sqrt(length(base3$Glasgow[!is.na(base3$Glasgow)]))
)
GlasgowG

fitIGS2 <- aov(IGS2 ~ Site, base)
summary (fitIGS2)

IGS2G<-c(
mean(base2$IGS2, na.rm = TRUE),
mean(base2$IGS2, na.rm = TRUE) - 1.96*sd(base2$IGS2, na.rm = TRUE)/sqrt(length(base2$IGS2[!is.na(base2$IGS2)])),
mean(base2$IGS2, na.rm = TRUE) + 1.96*sd(base2$IGS2, na.rm = TRUE)/sqrt(length(base2$IGS2[!is.na(base2$IGS2)]))
)
IGS2G

IGS2G<-c(
mean(base3$IGS2, na.rm = TRUE),
mean(base3$IGS2, na.rm = TRUE) - 1.96*sd(base3$IGS2, na.rm = TRUE)/sqrt(length(base3$IGS2[!is.na(base3$IGS2)])),
mean(base3$IGS2, na.rm = TRUE) + 1.96*sd(base3$IGS2, na.rm = TRUE)/sqrt(length(base3$IGS2[!is.na(base3$IGS2)]))
)
IGS2G

fitVmjours <- aov(Vmjours ~ Site, base)
summary (fitVmjours)

VmjoursG<-c(
mean(base2$Vmjours, na.rm = TRUE),
mean(base2$Vmjours, na.rm = TRUE) - 1.96*sd(base2$Vmjours, na.rm = TRUE)/sqrt(length(base2$Vmjours[!is.na(base2$Vmjours)])),
mean(base2$Vmjours, na.rm = TRUE) + 1.96*sd(base2$Vmjours, na.rm = TRUE)/sqrt(length(base2$Vmjours[!is.na(base2$Vmjours)]))
)
VmjoursG

VmjoursG<-c(
mean(base3$Vmjours, na.rm = TRUE),
mean(base3$Vmjours, na.rm = TRUE) - 1.96*sd(base3$Vmjours, na.rm = TRUE)/sqrt(length(base3$Vmjours[!is.na(base3$Vmjours)])),
mean(base3$Vmjours, na.rm = TRUE) + 1.96*sd(base3$Vmjours, na.rm = TRUE)/sqrt(length(base3$Vmjours[!is.na(base3$Vmjours)]))
)
VmjoursG


fitcatechoDoseMax <- aov(catechoDoseMax ~ Site, base)
summary (fitcatechoDoseMax)

catechoDoseMaxG<-c(
mean(base2$catechoDoseMax, na.rm = TRUE),
mean(base2$catechoDoseMax, na.rm = TRUE) - 1.96*sd(base2$catechoDoseMax, na.rm = TRUE)/sqrt(length(base2$catechoDoseMax[!is.na(base2$catechoDoseMax)])),
mean(base2$catechoDoseMax, na.rm = TRUE) + 1.96*sd(base2$catechoDoseMax, na.rm = TRUE)/sqrt(length(base2$catechoDoseMax[!is.na(base2$catechoDoseMax)]))
)
catechoDoseMaxG

catechoDoseMaxG<-c(
mean(base3$catechoDoseMax, na.rm = TRUE),
mean(base3$catechoDoseMax, na.rm = TRUE) - 1.96*sd(base3$catechoDoseMax, na.rm = TRUE)/sqrt(length(base3$catechoDoseMax[!is.na(base3$catechoDoseMax)])),
mean(base3$catechoDoseMax, na.rm = TRUE) + 1.96*sd(base3$catechoDoseMax, na.rm = TRUE)/sqrt(length(base3$catechoDoseMax[!is.na(base3$catechoDoseMax)]))
)
catechoDoseMaxG

fitCatechoDuree <- aov(CatechoDuree ~ Site, base)
summary (fitCatechoDuree)

CatechoDureeG<-c(
mean(base2$CatechoDuree, na.rm = TRUE),
mean(base2$CatechoDuree, na.rm = TRUE) - 1.96*sd(base2$CatechoDuree, na.rm = TRUE)/sqrt(length(base2$CatechoDuree[!is.na(base2$CatechoDuree)])),
mean(base2$CatechoDuree, na.rm = TRUE) + 1.96*sd(base2$CatechoDuree, na.rm = TRUE)/sqrt(length(base2$CatechoDuree[!is.na(base2$CatechoDuree)]))
)
CatechoDureeG

CatechoDureeG<-c(
mean(base3$CatechoDuree, na.rm = TRUE),
mean(base3$CatechoDuree, na.rm = TRUE) - 1.96*sd(base3$CatechoDuree, na.rm = TRUE)/sqrt(length(base3$CatechoDuree[!is.na(base3$CatechoDuree)])),
mean(base3$CatechoDuree, na.rm = TRUE) + 1.96*sd(base3$CatechoDuree, na.rm = TRUE)/sqrt(length(base3$CatechoDuree[!is.na(base3$CatechoDuree)]))
)
CatechoDureeG

fitDuree.sejour <- aov(Duree.sejour ~ Site, base)
summary (fitDuree.sejour)

Duree.sejourG<-c(
mean(base2$Duree.sejour, na.rm = TRUE),
mean(base2$Duree.sejour, na.rm = TRUE) - 1.96*sd(base2$Duree.sejour, na.rm = TRUE)/sqrt(length(base2$Duree.sejour[!is.na(base2$Duree.sejour)])),
mean(base2$Duree.sejour, na.rm = TRUE) + 1.96*sd(base2$Duree.sejour, na.rm = TRUE)/sqrt(length(base2$Duree.sejour[!is.na(base2$Duree.sejour)]))
)
Duree.sejourG

Duree.sejourG<-c(
mean(base3$Duree.sejour, na.rm = TRUE),
mean(base3$Duree.sejour, na.rm = TRUE) - 1.96*sd(base3$Duree.sejour, na.rm = TRUE)/sqrt(length(base3$Duree.sejour[!is.na(base3$Duree.sejour)])),
mean(base3$Duree.sejour, na.rm = TRUE) + 1.96*sd(base3$Duree.sejour, na.rm = TRUE)/sqrt(length(base3$Duree.sejour[!is.na(base3$Duree.sejour)]))
)
Duree.sejourG


fitRankin <- aov(Rankin ~ Site, base)
summary (fitRankin)

RankinG<-c(
mean(base2$Rankin, na.rm = TRUE),
mean(base2$Rankin, na.rm = TRUE) - 1.96*sd(base2$Rankin, na.rm = TRUE)/sqrt(length(base2$Rankin[!is.na(base2$Rankin)])),
mean(base2$Rankin, na.rm = TRUE) + 1.96*sd(base2$Rankin, na.rm = TRUE)/sqrt(length(base2$Rankin[!is.na(base2$Rankin)]))
)
RankinG

RankinG<-c(
mean(base3$Rankin, na.rm = TRUE),
mean(base3$Rankin, na.rm = TRUE) - 1.96*sd(base3$Rankin, na.rm = TRUE)/sqrt(length(base3$Rankin[!is.na(base3$Rankin)])),
mean(base3$Rankin, na.rm = TRUE) + 1.96*sd(base3$Rankin, na.rm = TRUE)/sqrt(length(base3$Rankin[!is.na(base3$Rankin)]))
)
RankinG

# sink()








sink ("/Users/antoinegaudetchardonnet/Git/Sara/popu.csv", append = F)

tabgrpASA <- table(base$grpASA, base$Site)
tabgrpASA
test <- chisq.test(tabgrpASA)
grpASA <- cbind (tabgrpASA, test$p.value)
print("grpASA")
write.csv (grpASA)

# Tests popu #

tabASA <- table(base$ASA, base$Site)
tabASA
test <- chisq.test(tabASA)
ASA <- cbind (tabASA, test$p.value)
print("ASA")
write.csv (ASA)
# test <- chisq.residuals(tabASA)


tabSexe.M..1 <- table(base$Sexe.M..1, base$Site)
tabSexe.M..1
test <- chisq.test(tabSexe.M..1)
Sexe.M..1 <- cbind (tabSexe.M..1, test$p.value)
print("Sexe.M..1")
write.csv (Sexe.M..1)
# test <- chisq.residuals(tabSexe.M..1)

tabAutonomie <- table(base$Autonomie, base$Site)
tabAutonomie
test <- chisq.test(tabAutonomie)
Autonomie <- cbind (tabAutonomie, test$p.value)
print("Autonomie")
write.csv (Autonomie)
# test <- chisq.residuals(tabAutonomie)

tabATCDPneumo <- table(base$ATCDPneumo, base$Site)
tabATCDPneumo
test <- chisq.test(tabATCDPneumo)
ATCDPneumo <- cbind (tabATCDPneumo, test$p.value)
print("ATCDPneumo")
write.csv (ATCDPneumo)
# test <- chisq.residuals(tabATCDPneumo)

tabATCDIS <- table(base$ATCDIS, base$Site)
tabATCDIS
test <- chisq.test(tabATCDIS)
ATCDIS <- cbind (tabATCDIS, test$p.value)
print("ATCDIS")
write.csv (ATCDIS)
# test <- chisq.residuals(tabATCDIS)

tabIPP <- table(base$IPP, base$Site)
tabIPP
test <- chisq.test(tabIPP)
IPP <- cbind (tabIPP, test$p.value)
print("IPP")
write.csv (IPP)
# test <- chisq.residuals(tabIPP)

tabATB <- table(base$ATB, base$Site)
tabATB
test <- chisq.test(tabATB)
ATB <- cbind (tabATB, test$p.value)
print("ATB")
write.csv (ATB)
# test <- chisq.residuals(tabATB)

tabHospit <- table(base$Hospit, base$Site)
tabHospit
test <- chisq.test(tabHospit)
Hospit <- cbind (tabHospit, test$p.value)
print("Hospit")
write.csv (Hospit)
# test <- chisq.residuals(tabHospit)

tabColoGerme <- table(base$ColoGerme, base$Site)
tabColoGerme
test <- chisq.test(tabColoGerme)
ColoGerme <- cbind (tabColoGerme, test$p.value)
print("ColoGerme")
write.csv (ColoGerme)
# test <- chisq.residuals(tabColoGerme)

tabColoAdAc <- table(base$ColoAdAc, base$Site)
tabColoAdAc
test <- chisq.test(tabColoAdAc)
ColoAdAc <- cbind (tabColoAdAc, test$p.value)
print("ColoAdAc")
write.csv (ColoAdAc)
# test <- chisq.residuals(tabColoAdAc)

tabGlasgow <- table(base$Glasgow, base$Site)
tabGlasgow
test <- chisq.test(tabGlasgow)
Glasgow <- cbind (tabGlasgow, test$p.value)
print("Glasgow")
write.csv (Glasgow)
# test <- chisq.residuals(tabGlasgow)

tabIGS2 <- table(base$IGS2, base$Site)
tabIGS2
test <- chisq.test(tabIGS2)
IGS2 <- cbind (tabIGS2, test$p.value)
print("IGS2")
write.csv (IGS2)
# test <- chisq.residuals(tabIGS2)

tabDefaillance.HD <- table(base$Defaillance.HD, base$Site)
tabDefaillance.HD
test <- chisq.test(tabDefaillance.HD)
Defaillance.HD <- cbind (tabDefaillance.HD, test$p.value)
print("Defaillance.HD")
write.csv (Defaillance.HD)
# test <- chisq.residuals(tabDefaillance.HD)

tabDefaillance.Respi <- table(base$Defaillance.Respi, base$Site)
tabDefaillance.Respi
test <- chisq.test(tabDefaillance.Respi)
Defaillance.Respi <- cbind (tabDefaillance.Respi, test$p.value)
print("Defaillance.Respi")
write.csv (Defaillance.Respi)
# test <- chisq.residuals(tabDefaillance.Respi)

tabAtteinte.Ortho <- table(base$Atteinte.Ortho, base$Site)
tabAtteinte.Ortho
test <- chisq.test(tabAtteinte.Ortho)
Atteinte.Ortho <- cbind (tabAtteinte.Ortho, test$p.value)
print("Atteinte.Ortho")
write.csv (Atteinte.Ortho)
# test <- chisq.residuals(tabAtteinte.Ortho)

tabAtteinte.Abdo <- table(base$Atteinte.Abdo, base$Site)
tabAtteinte.Abdo
test <- chisq.test(tabAtteinte.Abdo)
Atteinte.Abdo <- cbind (tabAtteinte.Abdo, test$p.value)
print("Atteinte.Abdo")
write.csv (Atteinte.Abdo)
# test <- chisq.residuals(tabAtteinte.Abdo)

tabAtteinte.Face <- table(base$Atteinte.Face, base$Site)
tabAtteinte.Face
test <- chisq.test(tabAtteinte.Face)
Atteinte.Face <- cbind (tabAtteinte.Face, test$p.value)
print("Atteinte.Face")
write.csv (Atteinte.Face)
# test <- chisq.residuals(tabAtteinte.Face)

tabAtteinte.Pneumo <- table(base$Atteinte.Pneumo, base$Site)
tabAtteinte.Pneumo
test <- chisq.test(tabAtteinte.Pneumo)
Atteinte.Pneumo <- cbind (tabAtteinte.Pneumo, test$p.value)
print("Atteinte.Pneumo")
write.csv (Atteinte.Pneumo)
# test <- chisq.residuals(tabAtteinte.Pneumo)

tabInhalation <- table(base$Inhalation, base$Site)
tabInhalation
test <- chisq.test(tabInhalation)
Inhalation <- cbind (tabInhalation, test$p.value)
print("Inhalation")
write.csv (Inhalation)
# test <- chisq.residuals(tabInhalation)

tabChir <- table(base$Chir, base$Site)
tabChir
test <- chisq.test(tabChir)
Chir <- cbind (tabChir, test$p.value)
print("Chir")
write.csv (Chir)
# test <- chisq.residuals(tabChir)

tabOsmotherapie <- table(base$Osmotherapie, base$Site)
tabOsmotherapie
test <- chisq.test(tabOsmotherapie)
Osmotherapie <- cbind (tabOsmotherapie, test$p.value)
print("Osmotherapie")
write.csv (Osmotherapie)
# test <- chisq.residuals(tabOsmotherapie)

tabCorticoides <- table(base$Corticoides, base$Site)
tabCorticoides
test <- chisq.test(tabCorticoides)
Corticoides <- cbind (tabCorticoides, test$p.value)
print("Corticoides")
write.csv (Corticoides)
# test <- chisq.residuals(tabCorticoides)

tabHypothermie <- table(base$Hypothermie, base$Site)
tabHypothermie
test <- chisq.test(tabHypothermie)
Hypothermie <- cbind (tabHypothermie, test$p.value)
print("Hypothermie")
write.csv (Hypothermie)
# test <- chisq.residuals(tabHypothermie)

tabVmjour <- table(base$Vmjour, base$Site)
tabVmjour
test <- chisq.test(tabVmjour)
Vmjour <- cbind (tabVmjour, test$p.value)
print("Vmjour")
write.csv (Vmjour)
# test <- chisq.residuals(tabVmjour)

tabVNI <- table(base$VNI, base$Site)
tabVNI
test <- chisq.test(tabVNI)
VNI <- cbind (tabVNI, test$p.value)
print("VNI")
write.csv (VNI)
# test <- chisq.residuals(tabVNI)

tabOptiflow <- table(base$Optiflow, base$Site)
tabOptiflow
test <- chisq.test(tabOptiflow)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabOptiflow)

tabReIOT <- table(base$ReIOT, base$Site)
tabReIOT
test <- chisq.test(tabReIOT)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabReIOT)

tabTracheo <- table(base$Tracheo, base$Site)
tabTracheo
test <- chisq.test(tabTracheo)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabTracheo)

tabHTIC <- table(base$HTIC, base$Site)
tabHTIC
test <- chisq.test(tabHTIC)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabHTIC)

tabX2.sedations <- table(base$X2.sedations, base$Site)
tabX2.sedations
test <- chisq.test(tabX2.sedations)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabX2.sedations)

tabX3.sedations <- table(base$X3.sedations, base$Site)
tabX3.sedations
test <- chisq.test(tabX3.sedations)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabX3.sedations)

tabPenthotal <- table(base$Penthotal, base$Site)
tabPenthotal
test <- chisq.test(tabPenthotal)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabPenthotal)

tabCurares <- table(base$Curares, base$Site)
tabCurares
test <- chisq.test(tabCurares)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabCurares)

tabCatecho <- table(base$Catecho, base$Site)
tabCatecho
test <- chisq.test(tabCatecho)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabCatecho)

tabcatechoDoseMax <- table(base$catechoDoseMax, base$Site)
tabcatechoDoseMax
test <- chisq.test(tabcatechoDoseMax)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabcatechoDoseMax)

tabCatechoDuree <- table(base$CatechoDuree, base$Site)
tabCatechoDuree
test <- chisq.test(tabCatechoDuree)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabCatechoDuree)

tabTransfusion <- table(base$Transfusion, base$Site)
tabTransfusion
test <- chisq.test(tabTransfusion)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabTransfusion)

tabDuree.sejour <- table(base$Duree.sejour, base$Site)
tabDuree.sejour
test <- chisq.test(tabDuree.sejour)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabDuree.sejour)

tabDecede <- table(base$Decede, base$Site)
tabDecede
test <- chisq.test(tabDecede)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabDecede)


tabRankin <- table(base$Rankin, base$Site)
tabRankin
test <- chisq.test(tabRankin)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabRankin)

tabTypeHED <- table(base$TypeHED, base$Site)
tabTypeHED
test <- chisq.test(tabTypeHED)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabTypeHED)

tabTypeOedeme <- table(base$TypeOedeme, base$Site)
tabTypeOedeme
test <- chisq.test(tabTypeOedeme)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabTypeOedeme)

tabTypeHSD <- table(base$TypeHSD, base$Site)
tabTypeHSD
test <- chisq.test(tabTypeHSD)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabTypeHSD)

tabTypeHSA <- table(base$TypeHSA, base$Site)
tabTypeHSA
test <- chisq.test(tabTypeHSA)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabTypeHSA)

tabTypePetechi <- table(base$TypePetechi, base$Site)
tabTypePetechi
test <- chisq.test(tabTypePetechi)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabTypePetechi)

tabTypeHematome <- table(base$TypeHematome, base$Site)
tabTypeHematome
test <- chisq.test(tabTypeHematome)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabTypeHematome)

tabCatechoChoc <- table(base$CatechoChoc, base$Site)
tabCatechoChoc
test <- chisq.test(tabCatechoChoc)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabCatechoChoc)

tabCatechoPPC <- table(base$CatechoPPC, base$Site)
tabCatechoPPC
test <- chisq.test(tabCatechoPPC)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabCatechoPPC)

tabPAVM.96h.Noso <- table(base$PAVM.96h.Noso, base$Site)
tabPAVM.96h.Noso
test <- chisq.test(tabPAVM.96h.Noso)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabPAVM.96h.Noso)

tabPAVM.96h.Comm <- table(base$PAVM.96h.Comm, base$Site)
tabPAVM.96h.Comm
test <- chisq.test(tabPAVM.96h.Comm)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabPAVM.96h.Comm)

tabPAVM1.7j.Noso <- table(base$PAVM1.7j.Noso, base$Site)
tabPAVM1.7j.Noso
test <- chisq.test(tabPAVM1.7j.Noso)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabPAVM1.7j.Noso)

tabPAVM1.7j.Comm <- table(base$PAVM1.7j.Comm, base$Site)
tabPAVM1.7j.Comm
test <- chisq.test(tabPAVM1.7j.Comm)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabPAVM1.7j.Comm)

tabPAVM1.7j.Noso.1 <- table(base$PAVM1.7j.Noso.1, base$Site)
tabPAVM1.7j.Noso.1
test <- chisq.test(tabPAVM1.7j.Noso.1)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabPAVM1.7j.Noso.1)

tabPAVM1.j.7Comm <- table(base$PAVM1.j.7Comm, base$Site)
tabPAVM1.j.7Comm
test <- chisq.test(tabPAVM1.j.7Comm)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabPAVM1.j.7Comm)

tabPAVM2...7j.Noso <- table(base$PAVM2...7j.Noso, base$Site)
tabPAVM2...7j.Noso
test <- chisq.test(tabPAVM2...7j.Noso)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabPAVM2...7j.Noso)

tabPAVM2...7jComm <- table(base$PAVM2...7jComm, base$Site)
tabPAVM2...7jComm
test <- chisq.test(tabPAVM2...7jComm)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabPAVM2...7jComm)

tabPAVM2.7j.Noso <- table(base$PAVM2.7j.Noso, base$Site)
tabPAVM2.7j.Noso
test <- chisq.test(tabPAVM2.7j.Noso)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabPAVM2.7j.Noso)

tabPAVM2..7j.Comm <- table(base$PAVM2..7j.Comm, base$Site)
tabPAVM2..7j.Comm
test <- chisq.test(tabPAVM2..7j.Comm)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabPAVM2..7j.Comm)

tabPAVM3Noso <- table(base$PAVM3Noso, base$Site)
tabPAVM3Noso
test <- chisq.test(tabPAVM3Noso)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabPAVM3Noso)

tabPAVM3Comm <- table(base$PAVM3Comm, base$Site)
tabPAVM3Comm
test <- chisq.test(tabPAVM3Comm)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabPAVM3Comm)

tabPAVM4Noso <- table(base$PAVM4Noso, base$Site)
tabPAVM4Noso
test <- chisq.test(tabPAVM4Noso)
XXX <- cbind (tabXXX, test$p.value)
print("XXX")
write.csv (XXX)
# test <- chisq.residuals(tabPAVM4Noso)



tabPAVM4Comm <- table(base$PAVM4Comm, base$Site)
test <- chisq.test(tabPAVM4Comm)
PAVM4Comm <- cbind (tabPAVM4Comm, test$p.value)
print("PAVM4Comm")
write.csv (PAVM4Comm)
# chisq.residuals(tabPAVM4Comm)


tabPAVM5Noso <- table(base$PAVM5Noso, base$Site)
test <- chisq.test(tabPAVM5Noso)
PAVM5Noso <- cbind (tabPAVM5Noso, test$p.value)
print("PAVM5Noso")
write.csv (PAVM5Noso)
# chisq.residuals(tabPAVM5Noso)

sink ()


# _________________________________________
# Analyse multivariée #
# ----------------------------------------


# multivariée CA vs autres
mod1<- glm(base$siteR ~ base$grpage2 + base$type + base$statutgg2 + base$figoIII, family="binomial")
logistic.display(mod1, decimal = 3)

library(sjPlot)
library(logistf)

plot_model(mod1, show.values = TRUE, show.p = TRUE, title = "CA vs autres")
ggsave(file = "/Users/antoinegaudetchardonnet/Documents/These/R/Résultats/mod1.pdf")

