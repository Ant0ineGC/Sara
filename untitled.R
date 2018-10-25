

library(tidyverse)
library(questionr)
# library(epiDisplay)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)

setwd("/Users/antoinegaudetchardonnet/Documents/Sara/R")
base <- read.csv2("Base3.csv")

base1 <- base[base$Site == "1",]
base0 <- base[base$Site == "0",]

base$grpASA <- cut(base$ASA, c(1, 2, 4), right = FALSE, include.lowest = TRUE)


sink ("/Users/antoinegaudetchardonnet/Documents/Sara/R/popu2.csv", append = F)

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

sink()








sink ("/Users/antoinegaudetchardonnet/Documents/Sara/R/popu.csv", append = F)


tabgrpASA <- table(base$grpASA, base$Site)
tabgrpASA
chisq.test(tabgrpASA)

# Tests popu #

tabASA <- table(base$ASA, base$Site)
tabASA
chisq.test(tabASA)
# chisq.residuals(tabASA)


tabSexe.M..1 <- table(base$Sexe.M..1, base$Site)
tabSexe.M..1
chisq.test(tabSexe.M..1)
# chisq.residuals(tabSexe.M..1)

tabAutonomie <- table(base$Autonomie, base$Site)
tabAutonomie
chisq.test(tabAutonomie)
# chisq.residuals(tabAutonomie)

tabATCDPneumo <- table(base$ATCDPneumo, base$Site)
tabATCDPneumo
chisq.test(tabATCDPneumo)
# chisq.residuals(tabATCDPneumo)

tabATCDIS <- table(base$ATCDIS, base$Site)
tabATCDIS
chisq.test(tabATCDIS)
# chisq.residuals(tabATCDIS)

tabIPP <- table(base$IPP, base$Site)
tabIPP
chisq.test(tabIPP)
# chisq.residuals(tabIPP)

tabATB <- table(base$ATB, base$Site)
tabATB
chisq.test(tabATB)
# chisq.residuals(tabATB)

tabHospit <- table(base$Hospit, base$Site)
tabHospit
chisq.test(tabHospit)
# chisq.residuals(tabHospit)

tabColoGerme <- table(base$ColoGerme, base$Site)
tabColoGerme
chisq.test(tabColoGerme)
# chisq.residuals(tabColoGerme)

tabColoAdAc <- table(base$ColoAdAc, base$Site)
tabColoAdAc
chisq.test(tabColoAdAc)
# chisq.residuals(tabColoAdAc)

tabGlasgow <- table(base$Glasgow, base$Site)
tabGlasgow
chisq.test(tabGlasgow)
# chisq.residuals(tabGlasgow)

tabIGS2 <- table(base$IGS2, base$Site)
tabIGS2
chisq.test(tabIGS2)
# chisq.residuals(tabIGS2)

tabDefaillance.HD <- table(base$Defaillance.HD, base$Site)
tabDefaillance.HD
chisq.test(tabDefaillance.HD)
# chisq.residuals(tabDefaillance.HD)

tabDefaillance.Respi <- table(base$Defaillance.Respi, base$Site)
tabDefaillance.Respi
chisq.test(tabDefaillance.Respi)
# chisq.residuals(tabDefaillance.Respi)

tabAtteinte.Ortho <- table(base$Atteinte.Ortho, base$Site)
tabAtteinte.Ortho
chisq.test(tabAtteinte.Ortho)
# chisq.residuals(tabAtteinte.Ortho)

tabAtteinte.Abdo <- table(base$Atteinte.Abdo, base$Site)
tabAtteinte.Abdo
chisq.test(tabAtteinte.Abdo)
# chisq.residuals(tabAtteinte.Abdo)

tabAtteinte.Face <- table(base$Atteinte.Face, base$Site)
tabAtteinte.Face
chisq.test(tabAtteinte.Face)
# chisq.residuals(tabAtteinte.Face)

tabAtteinte.Pneumo <- table(base$Atteinte.Pneumo, base$Site)
tabAtteinte.Pneumo
chisq.test(tabAtteinte.Pneumo)
# chisq.residuals(tabAtteinte.Pneumo)

tabInhalation <- table(base$Inhalation, base$Site)
tabInhalation
chisq.test(tabInhalation)
# chisq.residuals(tabInhalation)

tabChir <- table(base$Chir, base$Site)
tabChir
chisq.test(tabChir)
# chisq.residuals(tabChir)

tabOsmotherapie <- table(base$Osmotherapie, base$Site)
tabOsmotherapie
chisq.test(tabOsmotherapie)
# chisq.residuals(tabOsmotherapie)

tabCorticoides <- table(base$Corticoides, base$Site)
tabCorticoides
chisq.test(tabCorticoides)
# chisq.residuals(tabCorticoides)

tabHypothermie <- table(base$Hypothermie, base$Site)
tabHypothermie
chisq.test(tabHypothermie)
# chisq.residuals(tabHypothermie)

tabVmjour <- table(base$Vmjour, base$Site)
tabVmjour
chisq.test(tabVmjour)
# chisq.residuals(tabVmjour)

tabVNI <- table(base$VNI, base$Site)
tabVNI
chisq.test(tabVNI)
# chisq.residuals(tabVNI)

tabOptiflow <- table(base$Optiflow, base$Site)
tabOptiflow
chisq.test(tabOptiflow)
# chisq.residuals(tabOptiflow)

tabReIOT <- table(base$ReIOT, base$Site)
tabReIOT
chisq.test(tabReIOT)
# chisq.residuals(tabReIOT)

tabTracheo <- table(base$Tracheo, base$Site)
tabTracheo
chisq.test(tabTracheo)
# chisq.residuals(tabTracheo)

tabHTIC <- table(base$HTIC, base$Site)
tabHTIC
chisq.test(tabHTIC)
# chisq.residuals(tabHTIC)

tabX2.sedations <- table(base$X2.sedations, base$Site)
tabX2.sedations
chisq.test(tabX2.sedations)
# chisq.residuals(tabX2.sedations)

tabX3.sedations <- table(base$X3.sedations, base$Site)
tabX3.sedations
chisq.test(tabX3.sedations)
# chisq.residuals(tabX3.sedations)

tabPenthotal <- table(base$Penthotal, base$Site)
tabPenthotal
chisq.test(tabPenthotal)
# chisq.residuals(tabPenthotal)

tabCurares <- table(base$Curares, base$Site)
tabCurares
chisq.test(tabCurares)
# chisq.residuals(tabCurares)

tabCatecho <- table(base$Catecho, base$Site)
tabCatecho
chisq.test(tabCatecho)
# chisq.residuals(tabCatecho)

tabcatechoDoseMax <- table(base$catechoDoseMax, base$Site)
tabcatechoDoseMax
chisq.test(tabcatechoDoseMax)
# chisq.residuals(tabcatechoDoseMax)

tabCatechoDuree <- table(base$CatechoDuree, base$Site)
tabCatechoDuree
chisq.test(tabCatechoDuree)
# chisq.residuals(tabCatechoDuree)

tabTransfusion <- table(base$Transfusion, base$Site)
tabTransfusion
chisq.test(tabTransfusion)
# chisq.residuals(tabTransfusion)

tabDuree.sejour <- table(base$Duree.sejour, base$Site)
tabDuree.sejour
chisq.test(tabDuree.sejour)
# chisq.residuals(tabDuree.sejour)

tabDecede <- table(base$Decede, base$Site)
tabDecede
chisq.test(tabDecede)
# chisq.residuals(tabDecede)


tabRankin <- table(base$Rankin, base$Site)
tabRankin
chisq.test(tabRankin)
# chisq.residuals(tabRankin)

tabTypeHED <- table(base$TypeHED, base$Site)
tabTypeHED
chisq.test(tabTypeHED)
# chisq.residuals(tabTypeHED)

tabTypeOedeme <- table(base$TypeOedeme, base$Site)
tabTypeOedeme
chisq.test(tabTypeOedeme)
# chisq.residuals(tabTypeOedeme)

tabTypeHSD <- table(base$TypeHSD, base$Site)
tabTypeHSD
chisq.test(tabTypeHSD)
# chisq.residuals(tabTypeHSD)

tabTypeHSA <- table(base$TypeHSA, base$Site)
tabTypeHSA
chisq.test(tabTypeHSA)
# chisq.residuals(tabTypeHSA)

tabTypePetechi <- table(base$TypePetechi, base$Site)
tabTypePetechi
chisq.test(tabTypePetechi)
# chisq.residuals(tabTypePetechi)

tabTypeHematome <- table(base$TypeHematome, base$Site)
tabTypeHematome
chisq.test(tabTypeHematome)
# chisq.residuals(tabTypeHematome)

tabCatechoChoc <- table(base$CatechoChoc, base$Site)
tabCatechoChoc
chisq.test(tabCatechoChoc)
# chisq.residuals(tabCatechoChoc)

tabCatechoPPC <- table(base$CatechoPPC, base$Site)
tabCatechoPPC
chisq.test(tabCatechoPPC)
# chisq.residuals(tabCatechoPPC)

tabPAVM.96h.Noso <- table(base$PAVM.96h.Noso, base$Site)
tabPAVM.96h.Noso
chisq.test(tabPAVM.96h.Noso)
# chisq.residuals(tabPAVM.96h.Noso)

tabPAVM.96h.Comm <- table(base$PAVM.96h.Comm, base$Site)
tabPAVM.96h.Comm
chisq.test(tabPAVM.96h.Comm)
# chisq.residuals(tabPAVM.96h.Comm)

tabPAVM1.7j.Noso <- table(base$PAVM1.7j.Noso, base$Site)
tabPAVM1.7j.Noso
chisq.test(tabPAVM1.7j.Noso)
# chisq.residuals(tabPAVM1.7j.Noso)

tabPAVM1.7j.Comm <- table(base$PAVM1.7j.Comm, base$Site)
tabPAVM1.7j.Comm
chisq.test(tabPAVM1.7j.Comm)
# chisq.residuals(tabPAVM1.7j.Comm)

tabPAVM1.7j.Noso.1 <- table(base$PAVM1.7j.Noso.1, base$Site)
tabPAVM1.7j.Noso.1
chisq.test(tabPAVM1.7j.Noso.1)
# chisq.residuals(tabPAVM1.7j.Noso.1)

tabPAVM1.j.7Comm <- table(base$PAVM1.j.7Comm, base$Site)
tabPAVM1.j.7Comm
chisq.test(tabPAVM1.j.7Comm)
# chisq.residuals(tabPAVM1.j.7Comm)

tabPAVM2...7j.Noso <- table(base$PAVM2...7j.Noso, base$Site)
tabPAVM2...7j.Noso
chisq.test(tabPAVM2...7j.Noso)
# chisq.residuals(tabPAVM2...7j.Noso)

tabPAVM2...7jComm <- table(base$PAVM2...7jComm, base$Site)
tabPAVM2...7jComm
chisq.test(tabPAVM2...7jComm)
# chisq.residuals(tabPAVM2...7jComm)

tabPAVM2.7j.Noso <- table(base$PAVM2.7j.Noso, base$Site)
tabPAVM2.7j.Noso
chisq.test(tabPAVM2.7j.Noso)
# chisq.residuals(tabPAVM2.7j.Noso)

tabPAVM2..7j.Comm <- table(base$PAVM2..7j.Comm, base$Site)
tabPAVM2..7j.Comm
chisq.test(tabPAVM2..7j.Comm)
# chisq.residuals(tabPAVM2..7j.Comm)

tabPAVM3Noso <- table(base$PAVM3Noso, base$Site)
tabPAVM3Noso
chisq.test(tabPAVM3Noso)
# chisq.residuals(tabPAVM3Noso)

tabPAVM3Comm <- table(base$PAVM3Comm, base$Site)
tabPAVM3Comm
chisq.test(tabPAVM3Comm)
# chisq.residuals(tabPAVM3Comm)

tabPAVM4Noso <- table(base$PAVM4Noso, base$Site)
tabPAVM4Noso
chisq.test(tabPAVM4Noso)
# chisq.residuals(tabPAVM4Noso)

tabPAVM4Comm <- table(base$PAVM4Comm, base$Site)
tabPAVM4Comm
chisq.test(tabPAVM4Comm)
# chisq.residuals(tabPAVM4Comm)

tabPAVM5Noso <- table(base$PAVM5Noso, base$Site)
tabPAVM5Noso
chisq.test(tabPAVM5Noso)
# chisq.residuals(tabPAVM5Noso)

sink ()


