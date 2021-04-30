
# 1 Organisatorisches ----

# * 1.1 Laden der Pakete ----

library(car)
library(dplyr)
library(EMAtools)
library(ez)
library(ggplot2)
library(GPArotation)
library(ICC)
library(lme4)
library(lavaan)
library(MBESS)
library(nlme)
library(nortest)
library(psych)
library(reshape)
library(reshape2)
library(semTools)

# * 1.2 Laden des Datensatzes und der Datei mit den Zusatzvariablen ----

Alle_Daten <- read.csv2(file.choose()) # Datei: Alle_Daten_Stand 30.04.

Zusatzvariablen <- read.csv2(file.choose()) # Datei: Zusatzvariablen_neu

# * * 1.2.1 Kodierung fehlender Werte ----

Alle_Daten[Alle_Daten == -9 | Alle_Daten == -1 | Alle_Daten == ""] <- NA

# ** 1.2.2 neue Zusatzvariablen ----
    # 1) Feedback (Feedback = 1; Achtsamkeit = 0)
    # 2) FinishT1 (T1 Fragebogen ausgefüllt = 1; T1 Fragebogen nicht ausgefüllt = 0)
    # 3) FinishT2 (T2 Fragebogen ausgefüllt = 1; T2 Fragebogen nicht ausgefüllt = 0)
    # 4) Finish18 (mehr 17 Tagen Lernplaner ausgefüllt = 1; weniger als 17 Tage Lernplaner ausgefüllt = 0)

Alle_Daten_neu <- left_join(Alle_Daten, Zusatzvariablen, by = "SERIAL")

AD <- Alle_Daten_neu

# * 1.3 Dropout bestimmen ----
  # (Berechnen einer neuen Variable: dropout)

AD <- mutate(AD, dropout = ifelse(FinishT1 == 1 & FinishT2 == 1 & Finish18 == 1 & SERIAL != "NA", 0,1))

AD$dropout[is.na(AD$dropout)] <- 1

describeBy(data.frame(AD$Feedback), group = AD$dropout) # 569 Zeilen wurden als Dropout identifiziert.


# 2 Skalenbildung ---- 

# * 2.1 Skalenbildung T1 ----

AD$goalt1 <- (AD$T102_01 + AD$T102_02 + AD$T102_03 + AD$T102_04)/4

AD$mott1 <- (AD$T103_01 + AD$T103_02 + AD$T103_03)/3    

AD$volt1 <- (AD$T104_01 + AD$T104_02 + AD$T104_03 + AD$T104_04)/4

AD$reft1 <- (AD$T105_01 + AD$T105_02 + AD$T105_03)/3

AD$plant1 <- (AD$T106_01 + AD$T106_02 + AD$T106_03)/3

AD$prot1 <- (AD$T107_01 + AD$T107_02 + AD$T107_03 + AD$T107_04 + AD$T107_05 + AD$T107_06 + AD$T107_07)/7

AD$set1 <- (AD$T108_01 + AD$T108_02 + AD$T108_03 + AD$T108_04 + AD$T108_05 + AD$T108_06 + AD$T108_07+ AD$T108_08 + AD$T108_09)/9


# * 2.2 Skalenbildung T2 ----

AD$goalt2 <- (AD$T202_01 + AD$T202_02 + AD$T202_03 + AD$T202_04)/4

AD$mott2 <- (AD$T203_01 + AD$T203_02 + AD$T203_03)/3    

AD$volt2 <- (AD$T204_01 + AD$T204_02 + AD$T204_03 + AD$T204_04)/4

AD$reft2 <- (AD$T205_01 + AD$T205_02 + AD$T205_03)/3

AD$plant2 <- (AD$T206_01 + AD$T206_02 + AD$T206_03)/3

AD$prot2 <- (AD$T207_01 + AD$T207_02 + AD$T207_03 + AD$T207_04 + AD$T207_05 + AD$T207_06 + AD$T207_07)/7

AD$set2 <- (AD$T208_01 + AD$T208_02 + AD$T208_03 + AD$T208_04 + AD$T208_05 + AD$T208_06 + AD$T208_07+ AD$T208_08 + AD$T208_09)/9


# * 2.3 Skalenbildung CHIME ----

AD$chime1 <- (AD$FA02_01 + AD$FA02_05 + AD$FA02_14 + AD$FA02_29 + AD$FA02_34)/5

AD$chime2 <- (AD$FA02_09 + AD$FA02_18 + AD$FA02_21 + AD$FA02_27)/4

AD$chime3 <- (AD$FA02_10 + AD$FA02_12 + AD$FA02_17 + AD$FA02_26)/4

AD$chime4 <- (AD$FA02_02 + AD$FA02_07 + AD$FA02_11 + AD$FA02_32 + AD$FA02_36)/5

AD$chime5 <- (AD$FA02_08 + AD$FA02_13 + AD$FA02_16 + AD$FA02_20 + AD$FA02_25 + AD$FA02_28)/6

AD$chime6 <- (AD$FA02_19 + AD$FA02_22 + AD$FA02_30 + AD$FA02_33)/4

AD$chime7 <- (AD$FA02_04 + AD$FA02_23 + AD$FA02_31 + AD$FA02_35)/4

AD$chime8 <- (AD$FA02_03 + AD$FA02_06 + AD$FA02_15 + AD$FA02_24 + AD$FA02_37)/5

AD$chimeGesamt <- (AD$chime1 + AD$chime2 + AD$chime3 + AD$chime4 + AD$chime5 + AD$chime6 + AD$chime7 + AD$chime8)/8


# 3 Bildung der benötigten Subsets ----

# * 3.1 AD_ohne_Dropout ----

AD_ohne_Dropout <- filter(AD, FinishT1 == 1 & FinishT2 == 1 & Finish18 == 1 & SERIAL != "NA") # Es werden 574 Fälle raus gefiltert (5x SERIAL = NA)

# * 3.2 T1 ----

D_T1 <- subset(AD_ohne_Dropout, TIME=="T1")

# * 3.3 T2 ----

D_T2 <- subset(AD_ohne_Dropout, TIME=="T2")

# * 3.4 T1 & T2 ----

D_T1T2 <- rbind(D_T1, D_T2) # DZ883544 muss raus gefiltert werden, da im T2 Fragebogen nicht konzentriert 
D_T1T2 <- filter(D_T1T2, SERIAL != "DZ883544")

# * 3.5 T1 & T2 => CHIME ----

D_T1T2_CHIME <- D_T1T2 
D_T1T2_CHIME <- D_T1T2_CHIME %>% group_by(SERIAL) %>% filter(n()>1) %>% filter(chime1 != "NA", chimeGesamt != "NA")

# * 3.6 T1 & LP_T1-LP_T35 ----

D_LP <- filter(AD_ohne_Dropout, TIME != "T1", TIME != "T2", TIME != "T3")
D_LP <- filter(D_LP, TE16 != 2)

D_T1LP <- rbind(D_T1, D_LP)


# 4 Reliabilitätsanalysen: Berechnung McDonalds Omega ----

# * 4.1 Reliabilitäten: T1 ----

# Zielsetzung: alpha = .68; omega = .76 
omegagoalt1 <- AD_ohne_Dropout[c("T102_01", "T102_02", "T102_03", "T102_04")]
omega(omegagoalt1) 

# Selbstmotivierung: alpha = .79; omega = .82 
omegamott1 <- AD_ohne_Dropout[c("T103_01", "T103_02", "T103_03")]
omega(omegamott1) 

# Volition: alpha = .76; omega = .78 
omegavolt1 <- AD_ohne_Dropout[c("T104_01", "T104_02", "T104_03","T104_04")]
omega(omegavolt1) 

# Reflexion: alpha = .71; omega = .74
omegareft1 <- AD_ohne_Dropout[c("T105_01", "T105_02", "T105_03")]
omega(omegareft1) 

# Zeitplan: alpha = .86; omega = .88
omegaplant1 <- AD_ohne_Dropout[c("T106_01", "T106_02", "T106_03")]
omega(omegaplant1)

# Prokrastination: alpha = .95; omega = .96 
omegaprot1 <- AD_ohne_Dropout[c("T107_01", "T107_02", "T107_03","T107_04","T107_05","T107_06","T107_07")]
omega(omegaprot1) 

# Selbstwirksamkeit: alpha = .86; omega = .89 
omegaset1 <- AD_ohne_Dropout[c("T108_01", "T108_02", "T108_03","T108_04","T108_05","T108_06","T108_07","T108_08","T108_09")]
omega(omegaset1)

# Chime (T1_1) Gewahrsein gegenüber inneren Erfahrungen: alpha = .70; omega = .75
omegaChimeT1_1 <- D_T1[c("FA02_01", "FA02_05", "FA02_14", "FA02_29", "FA02_34")]
omega(omegaChimeT1_1)

# Chime (T1_2) Gewahrsein gegenüber äußeren Erfahrungen: alpha = .77; omega = .81
omegaChimeT1_2 <- D_T1[c("FA02_09", "FA02_18", "FA02_21", "FA02_27")]
omega(omegaChimeT1_2)

# Chime (T1_3) bewusstes Handeln, Gegenwärtigkeit: alpha = .60; omega = .67
omegaChimeT1_3 <- D_T1[c("FA02_10", "FA02_12", "FA02_17", "FA02_26")]
omega(omegaChimeT1_3)

# Chime (T1_4) annehmende, nicht-urteilende, mitfühlende Haltung: alpha = .88; omega = .90
omegaChimeT1_4 <- D_T1[c("FA02_02", "FA02_07", "FA02_11", "FA02_32", "FA02_36")]
omega(omegaChimeT1_4)

# Chime (T1_5) nicht-reaktive, dezentrierte Orientierung: alpha = .82; omega = .88
omegaChimeT1_5 <- D_T1[c("FA02_08", "FA02_13", "FA02_16", "FA02_20", "FA02_25", "FA02_28")]
omega(omegaChimeT1_5)

# Chime (T1_6) offene, nichtvermeidende Haltung: alpha = .53; omega = .56
omegaChimeT1_6 <- D_T1[c("FA02_19", "FA02_22", "FA02_30", "FA02_33")]
omega(omegaChimeT1_6)

# Chime (T1_7) Relativierung: alpha = .51; omega = .60
omegaChimeT1_7 <- D_T1[c("FA02_04", "FA02_23", "FA02_31", "FA02_35")]
omega(omegaChimeT1_7)

# Chime (T1_8) einsichtsvolles Verstehen: alpha = .73; omega = .80
omegaChimeT1_8 <- D_T1[c("FA02_03", "FA02_06", "FA02_15", "FA02_24", "FA02_37")]
omega(omegaChimeT1_8)

# Chime (T1_Gesamt): alpha = .89; omega = .91
omegaChimeT1_Gesamt <- D_T1[c("FA02_01", "FA02_05", "FA02_14", "FA02_29", "FA02_34", 
                           "FA02_09", "FA02_18", "FA02_21", "FA02_27",
                           "FA02_10", "FA02_12", "FA02_17", "FA02_26",
                           "FA02_02", "FA02_07", "FA02_11", "FA02_32", "FA02_36",
                           "FA02_08", "FA02_13", "FA02_16", "FA02_20", "FA02_25", "FA02_28",
                           "FA02_19", "FA02_22", "FA02_30", "FA02_33",
                           "FA02_04", "FA02_23", "FA02_31", "FA02_35",
                           "FA02_03", "FA02_06", "FA02_15", "FA02_24", "FA02_37")]
omega(omegaChimeT1_Gesamt)

# * 4.2 Reliabilitätsanalyse: T2 ----

# Zielsetzung: alpha = .68; omega = .75
omegagoalt2 <- AD_ohne_Dropout[c("T202_01", "T202_02", "T202_03", "T202_04")]
omega(omegagoalt2) 

# Selbstmotivierung: alpha = .83; omega = .84 
omegamott2 <- AD_ohne_Dropout[c("T203_01", "T203_02", "T203_03")]
omega(omegamott2)

# Volition: alpha = .85; omega = .87
omegavolt2 <- AD_ohne_Dropout[c("T204_01", "T204_02", "T204_03","T204_04")]
omega(omegavolt2) 

# Reflexion: alpha = .77; omega = .79
omegareft2 <- AD_ohne_Dropout[c("T205_01", "T205_02", "T205_03")]
omega(omegareft2) 

# Zeitplan: alpha = .85; omega = .85
omegaplant2 <- AD_ohne_Dropout[c("T206_01", "T206_02", "T206_03")]
omega(omegaplant2) 

# Prokrastination: alpha = .92; omega = .94 
omegaprot2 <- AD_ohne_Dropout[c("T207_01", "T207_02", "T207_03","T207_04","T207_05","T207_06","T207_07")]
omega(omegaprot2) 

# Selbstwirksamkeit: alpha = .88; omega = .91 
omegaset2 <- AD_ohne_Dropout[c("T208_01", "T208_02", "T208_03","T208_04","T208_05","T208_06","T208_07","T208_08","T208_09")]
omega(omegaset2)

# Chime (T2_1) Gewahrsein gegenüber inneren Erfahrungen: alpha = .75; omega = .81
omegaChimeT2_1 <- D_T2[c("FA02_01", "FA02_05", "FA02_14", "FA02_29", "FA02_34")]
omega(omegaChimeT2_1)

# Chime (T2_2) Gewahrsein gegenüber äußeren Erfahrungen: alpha = .81; omega = .82
omegaChimeT2_2 <- D_T2[c("FA02_09", "FA02_18", "FA02_21", "FA02_27")]
omega(omegaChimeT2_2)

# Chime (T2_3) bewusstes Handeln, Gegenwärtigkeit: alpha = .61; omega = .65
omegaChimeT2_3 <- D_T2[c("FA02_10", "FA02_12", "FA02_17", "FA02_26")]
omega(omegaChimeT2_3)

# Chime (T2_4) annehmende, nicht-urteilende, mitfühlende Haltung: alpha = .88; omega = .90
omegaChimeT2_4 <- D_T2[c("FA02_02", "FA02_07", "FA02_11", "FA02_32", "FA02_36")]
omega(omegaChimeT2_4)

# Chime (T2_5) nicht-reaktive, dezentrierte Orientierung: alpha = .82; omega = .86
omegaChimeT2_5 <- D_T2[c("FA02_08", "FA02_13", "FA02_16", "FA02_20", "FA02_25", "FA02_28")]
omega(omegaChimeT2_5)

# Chime (T2_6) offene, nichtvermeidende Haltung: alpha = .71; omega = .71
omegaChimeT2_6 <- D_T2[c("FA02_19", "FA02_22", "FA02_30", "FA02_33")]
omega(omegaChimeT2_6)

# Chime (T2_7) Relativierung: alpha = .72; omega = .77
omegaChimeT2_7 <- D_T2[c("FA02_04", "FA02_23", "FA02_31", "FA02_35")]
omega(omegaChimeT2_7)

# Chime (T2_8) einsichtsvolles Verstehen: alpha = .73; omega = .81
omegaChimeT2_8 <- D_T2[c("FA02_03", "FA02_06", "FA02_15", "FA02_24", "FA02_37")]
omega(omegaChimeT2_8)

# Chime (T2_Gesamt): alpha = .87; omega = .90
omegaChimeT2_Gesamt <- D_T2[c("FA02_01", "FA02_05", "FA02_14", "FA02_29", "FA02_34", 
                              "FA02_09", "FA02_18", "FA02_21", "FA02_27",
                              "FA02_10", "FA02_12", "FA02_17", "FA02_26",
                              "FA02_02", "FA02_07", "FA02_11", "FA02_32", "FA02_36",
                              "FA02_08", "FA02_13", "FA02_16", "FA02_20", "FA02_25", "FA02_28",
                              "FA02_19", "FA02_22", "FA02_30", "FA02_33",
                              "FA02_04", "FA02_23", "FA02_31", "FA02_35",
                              "FA02_03", "FA02_06", "FA02_15", "FA02_24", "FA02_37")]
omega(omegaChimeT2_Gesamt)


# 5 Deskriptive Analysen ----

# Haeufigkeitsverteilung Geschlecht aufgeteilt nach Bedingung
table(AD_ohne_Dropout$DD03, AD_ohne_Dropout$Feedback) 

# Alter aufgeteilt nach Bedingung
describeBy(AD_ohne_Dropout$DD02_01, AD_ohne_Dropout$Feedback, mat = TRUE) 

# vollständig ausgefüllte Lernplaner, aufgeteilt nach Bedingung
describeBy(D_T1$Anzahl_LP_vollstaendig, D_T1$Feedback, mat = TRUE) 
table(D_T1$Anzahl_LP_vollstaendig, D_T1$Feedback)

# Lernplaner nur abends, aufgeteilt nach Bedingung
describeBy(D_T1$Anzahl_LP_abends, D_T1$Feedback, mat = TRUE) 
table(D_T1$Anzahl_LP_abends , D_T1$Feedback) 

# Vorwissen Thema Achtsamkeit, aufgeteilt nach Bedingung
describeBy(AD_ohne_Dropout$DD10_01, AD_ohne_Dropout$Feedback, mat = TRUE) 

# Vorwissen Thema Feedback, aufgeteilt nach Bedingung
describeBy(AD_ohne_Dropout$DD10_08, AD_ohne_Dropout$Feedback, mat = TRUE) 


# 6 T-Tests (Gruppenvergleiche) ----

# * 6.1 Unterschiede zwischen Dropout (1) und kein Dropout (0) => T1 ----

AD$dropout.faktor <- factor(AD$dropout) # Dropout-Variable in einen Faktor konvertiert, um Levene Test rechnen zu können.

# Zielsetzung 
describeBy(AD$goalt1, AD$dropout, mat = TRUE)

leveneTest(AD$goalt1, AD$dropout.faktor) # n.s. => Varianzhomogenität gegeben

goalt1_drop <- t.test(AD$goalt1 ~ AD$dropout, var.equal = TRUE)
goalt1_drop # n.s.

ci.smd(ncp = -0.77618,
       n.1 = 151, n.2 = 24) # Cohens d = -.17 

# Selbstmotivierung
describeBy(AD$mott1, AD$dropout, mat = TRUE)

leveneTest(AD$mott1, AD$dropout.faktor) # n.s. => Varianzhomogenität gegeben

mott1_drop <- t.test(AD$mott1 ~ AD$dropout, var.equal = TRUE)
mott1_drop # n.s.

ci.smd(ncp = -0.78887,
       n.1 = 152, n.2 = 24) # Cohens d = -.17 

# Volition
describeBy(AD$volt1, AD$dropout, mat = TRUE)

leveneTest(AD$volt1, AD$dropout.faktor) # signifikant => Varianzhomogenität NICHT gegeben => Welch-Test

volt1_drop <- t.test(AD$volt1 ~ AD$dropout)
volt1_drop # n.s.

ci.smd(ncp = 0.16259,
       n.1 = 152, n.2 = 24) # Cohens d = .04 

# Reflexion
describeBy(AD$reft1, AD$dropout, mat = TRUE)

leveneTest(AD$reft1, AD$dropout.faktor) # n.s. => Varianzhomogenität gegeben

reft1_drop <- t.test(AD$reft1 ~ AD$dropout, var.equal = TRUE)
reft1_drop # n.s.

ci.smd(ncp = 0.67199,
       n.1 = 152, n.2 = 24) # Cohens d = .15 

# Zeitplan
describeBy(AD$plant1, AD$dropout, mat = TRUE)

leveneTest(AD$plant1, AD$dropout.faktor) # n.s. => Varianzhomogenität gegeben

plant1_drop <- t.test(AD$plant1 ~ AD$dropout, var.equal = TRUE)
plant1_drop # n.s.

ci.smd(ncp = 0.33985,
       n.1 = 152, n.2 = 24) # Cohens d = .07

# Prokrastination
describeBy(AD$prot1, AD$dropout, mat = TRUE)

leveneTest(AD$prot1, AD$dropout.faktor) # n.s. => Varianzhomogenität gegeben

prot1_drop <- t.test(AD$prot1 ~ AD$dropout, var.equal = TRUE)
prot1_drop # n.s.

ci.smd(ncp = -1.5985,
       n.1 = 151, n.2 = 24) # Cohens d = -.35

# Selbstwirksamkeit
describeBy(AD$set1, AD$dropout, mat = TRUE)

leveneTest(AD$set1, AD$dropout.faktor) # n.s. => Varianzhomogenität gegeben

set1_drop <- t.test(AD$set1 ~ AD$dropout, var.equal = TRUE)
set1_drop # n.s.

ci.smd(ncp = -1.0324,
       n.1 = 151, n.2 = 23) # Cohens d = -.23


# * 6.2 Unterschiede zwischen LPF (Feedback) und LPA (Achtsamkeit) => T1 ----

AD_ohne_Dropout$Feedback.Faktor <- factor(AD_ohne_Dropout$Feedback) # Feedback-Variable in einen Faktor konvertiert, um Levene Test rechnen zu können.

# * * 6.2.1 Trait Variablen ----

# Zielsetzung 
describeBy(AD_ohne_Dropout$goalt1, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$goalt1, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenität gegeben

goalt1_differences <- t.test(AD_ohne_Dropout$goalt1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
goalt1_differences # n.s. p = .07

ci.smd(ncp = -1.835,
       n.1 = 74, n.2 = 77) # Cohens d = -.30 

# Selbstmotivierung
describeBy(AD_ohne_Dropout$mott1, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$mott1, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenität gegeben

mott1_differences <- t.test(AD_ohne_Dropout$mott1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
mott1_differences # n.s. p = .13

ci.smd(ncp = -1.5245,
       n.1 = 74, n.2 = 78) # Cohens d = -.25 

# Volition 
describeBy(AD_ohne_Dropout$volt1, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$volt1, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenität gegeben

volt1_differences <- t.test(AD_ohne_Dropout$volt1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
volt1_differences # n.s. P = .72

ci.smd(ncp = -0.36545,
       n.1 = 74, n.2 = 78) # Cohens d = -.06

# Reflexion 
describeBy(AD_ohne_Dropout$reft1, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$reft1, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenität gegeben

reft1_differences <- t.test(AD_ohne_Dropout$reft1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
reft1_differences # n.s. P = .58

ci.smd(ncp = -0.56251,
       n.1 = 74, n.2 = 78) # Cohens d = -.09

# Zeitplan
describeBy(AD_ohne_Dropout$plant1, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$plant1, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenität gegeben

plant1_differences <- t.test(AD_ohne_Dropout$plant1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
plant1_differences # n.s. p = .13

ci.smd(ncp = -1.5088,
       n.1 = 74, n.2 = 78) # Cohens d = -.24

# Prokrastination
describeBy(AD_ohne_Dropout$prot1, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$prot1, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenität gegeben

prot1_differences <- t.test(AD_ohne_Dropout$prot1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
prot1_differences # n.s. p = .99

ci.smd(ncp = -0.016574,
       n.1 = 74, n.2 = 77) # Cohens d = -.003

# Selbstwirksamkeit
describeBy(AD_ohne_Dropout$set1, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$set1, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenität gegeben

set1_differences <- t.test(AD_ohne_Dropout$set1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
set1_differences # n.s. p = .86

ci.smd(ncp = 0.17729,
       n.1 = 74, n.2 = 77) # Cohens d = .03

# * * 6.2.2 Achtsamkeitsinventar (CHIME) ---- 

# CHIME Subskala 1 
D_T1$Achtsamkeit.Faktor <- factor(D_T1$Achtsamkeit) # Gruppe Achtsamkeit = 1; Gruppe Feedback = 0 

describeBy(D_T1$chime1, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime1, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenität gegeben

chime1_differences <- t.test(D_T1$chime1 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime1_differences # signifikant p = .0054

ci.smd(ncp = 2.8267,
       n.1 = 73, n.2 = 76) # Cohens d = .46

# CHIME Subskala 2
describeBy(D_T1$chime2, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime2, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenität gegeben

chime2_differences <- t.test(D_T1$chime2 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime2_differences # n.s. p = .28

ci.smd(ncp = 1.0873,
       n.1 = 73, n.2 = 77) # Cohens d = .18

# Chime Subskala 3
describeBy(D_T1$chime3, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime3, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenität gegeben

chime3_differences <- t.test(D_T1$chime3 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime3_differences # n.s. p = .10

ci.smd(ncp = -1.6512,
       n.1 = 73, n.2 = 77) # Cohens d = -.27

# Chime Subskala 4
describeBy(D_T1$chime4, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime4, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenität gegeben

chime4_differences <- t.test(D_T1$chime4 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime4_differences # n.s. p = .47

ci.smd(ncp = 0.71652,
       n.1 = 73, n.2 = 77) # Cohens d = .12

# Chime Subskala 5
describeBy(D_T1$chime5, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime5, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenität gegeben

chime5_differences <- t.test(D_T1$chime5 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime5_differences # n.s. p = .97

ci.smd(ncp = 0.038648,
       n.1 = 73, n.2 = 77) # Cohens d = .006

# Chime Subskala 6
describeBy(D_T1$chime6, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime6, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenität gegeben

chime6_differences <- t.test(D_T1$chime6 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime6_differences # signifikant p = .013

ci.smd(ncp = -2.5027,
       n.1 = 73, n.2 = 77) # Cohens d = -.41

# Chime Subskala 7
describeBy(D_T1$chime7, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime7, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenität gegeben

chime7_differences <- t.test(D_T1$chime7 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime7_differences # n.s. p = .86

ci.smd(ncp = 0.17361,
       n.1 = 73, n.2 = 77) # Cohens d = .03

# Chime Subskala 8
describeBy(D_T1$chime8, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime8, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenität gegeben

chime8_differences <- t.test(D_T1$chime8 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime8_differences # n.s. p = .36

ci.smd(ncp = 0.91834,
       n.1 = 73, n.2 = 77) # Cohens d = .15

# CHIME Gesamt (über alle 8 Subskalen hinweg)
describeBy(D_T1$chimeGesamt, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chimeGesamt, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenität gegeben

chimeGesamt_differences <- t.test(D_T1$chimeGesamt ~ D_T1$Achtsamkeit, var.equal = TRUE)
chimeGesamt_differences # n.s. p = 0.61

ci.smd(ncp = 0.51287,
       n.1 = 73, n.2 = 76) # Cohens d = .08



# 7 T1-T2 ANOVAs ----

# * 7.1 Trait Variablen ----

# lme für Zielsetzung

aggdata_long_GOAL <- melt(D_T1T2,id.vars=c("SERIAL", "Feedback"), measure.vars=c("goalt1", "goalt2"), variable.name="TIME",value.name="GOAL", na.rm = TRUE)

aggdata_long_GOAL <- aggdata_long_GOAL %>% group_by(SERIAL) %>% filter(n()>1) # Filtern einzelner Seriennummern, die nur T1 oder nur T2 ausgefüllt haben

describeBy(aggdata_long_GOAL$GOAL, list(aggdata_long_GOAL$TIME, aggdata_long_GOAL$Feedback), mat = TRUE) 

baseline_goal <- lme(GOAL ~ 1, random = ~1|TIME/Feedback, data = aggdata_long_GOAL, method = "ML")
goal <- lme(GOAL~TIME*Feedback, random=~TIME|SERIAL, data=aggdata_long_GOAL, method = "ML")

anova(baseline_goal)
anova(goal) # Feedback signifikant (p < .001, d = .30)
anova(baseline_goal, goal)

lme.dscore(goal,data=aggdata_long_GOAL,type="nlme")

summary(goal) # Warum werden die Koeffizienten für Feedback beim summary Befehl nicht signifikant, obwohl der anova Befehl, z.B. anova(goal) signifikante Ergebnisse anzeigt?

# erster Versuch Mittelwerte graphisch darzustellen
plot_GOAL <- error.bars.by(data.frame(D_T1T2$goalt1, D_T1T2$goalt2), D_T1T2$Feedback,
              lty = c(1,3), las =1,  by.var = TRUE, eyes = FALSE, 
              xlab = "Bedingung", 
              ylab = "Zielsetzung", ylim = c(1,6),
              main = "Zielsetzung Prä-Post",
              family(serif),
              )
# Versuch die Graphik schön darzustellen

aggdata_GOAL_plot <- melt(D_T1T2,id.vars=c("Feedback"), measure.vars=c("goalt1", "goalt2"), variable.name="TIME",value.name="GOAL", na.rm = TRUE)
cell1 <- subset(aggdata_GOAL_plot, Feedback == "0" & TIME == "goalt1")
cell2 <- subset(aggdata_GOAL_plot, Feedback == "0" & TIME == "goalt2")
cell3 <- subset(aggdata_GOAL_plot, Feedback == "1" & TIME == "goalt1")
cell4 <- subset(aggdata_GOAL_plot, Feedback == "1" & TIME == "goalt2")

a <- describeBy(cell1$GOAL)
b <- describeBy(cell2$GOAL)
c <- describeBy(cell3$GOAL)
d <- describeBy(cell4$GOAL)

a$mean
b$mean

plotdat <- data.frame(Feedback = c("0", "0", "1", "1"),
                      Messzeitpunkt = c("goalt1", "goalt2", "goalt1", "goalt2"),
                      Means = c(a$mean, b$mean, c$mean, d$mean),
                      Serrs = c(a$se, b$se, c$se, d$se))
                      
motorbar <- ggplot(data=plotdat, aes(x = Feedback, y=Means, fill = Messzeitpunkt))

motorbar <- motorbar + geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=Means-2*Serrs, ymax=Means+2*Serrs), width=.2,
                position=position_dodge(.9)) +
  labs(x="Feedbackgruppe", y = "Zielsetzung")+
  scale_fill_brewer(palette="Paired") + theme_minimal()
motorbar

# Alternative zum vorherigen Graphen

motorbar <- ggplot(data=plotdat, aes(x = Feedback, y=Means, fill = Messzeitpunkt))
motorbar <- motorbar + geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=Means-2*Serrs, ymax=Means+2*Serrs), width=.2,
                position=position_dodge(.9)) +
  labs(x="Gruppe", y = "Zielsetzung")+
  scale_fill_manual(values=c('darkgray','lightgray'))+
  theme_classic()

motorbar <- motorbar + theme(
  axis.title.x = element_text(size=14, face="bold"),
  axis.title.y = element_text(size=14, face="bold"),
  legend.title = element_text(size=14, face="bold"))
motorbar

# lme für Selbstmotivierung

aggdata_long_MOT <- melt(D_T1T2,id.vars=c("SERIAL", "Feedback"), measure.vars=c("mott1", "mott2"), variable.name="TIME",value.name="MOT", na.rm = TRUE)

aggdata_long_MOT <- aggdata_long_MOT %>% group_by(SERIAL) %>% filter(n()>1)

describeBy(aggdata_long_MOT$MOT, list(aggdata_long_MOT$TIME, aggdata_long_MOT$Feedback), mat = TRUE) 

baseline_mot <- lme(MOT ~ 1, random = ~1|TIME/Feedback, data = aggdata_long_MOT, method = "ML")
mot <- lme(MOT~TIME*Feedback, random=~TIME|SERIAL, data=aggdata_long_MOT, method = "ML")

anova(baseline_mot)
anova(mot) # TIME signifikant (p = .007, d = .51)
anova(baseline_mot, mot)

lme.dscore(mot,data=aggdata_long_MOT,type="nlme")

summary(mot)

# plot für Selbstmotivierung

plot_GOAL <- error.bars.by(data.frame(D_T1T2$mott1, D_T1T2$mott2), D_T1T2$Feedback,
                           lty = c(1,3), las =1,  by.var = TRUE, eyes = FALSE, 
                           xlab = "Bedingung", 
                           ylab = "Selbst-Motivation", ylim = c(1,6),
                           main = "Selbst-Motivation Prä-Post",
                           family(serif),
)

# lme für Volition

aggdata_long_VOL <- melt(D_T1T2,id.vars=c("SERIAL", "Feedback"), measure.vars=c("volt1", "volt2"), variable.name="TIME",value.name="VOL", na.rm = TRUE)

aggdata_long_VOL <- aggdata_long_VOL %>% group_by(SERIAL) %>% filter(n()>1)

describeBy(aggdata_long_VOL$VOL, list(aggdata_long_VOL$TIME, aggdata_long_VOL$Feedback), mat = TRUE) 

baseline_vol <- lme(VOL ~ 1, random = ~1|TIME/Feedback, data = aggdata_long_VOL, method = "ML")
vol <- lme(VOL~TIME*Feedback, random=~TIME|SERIAL, data=aggdata_long_VOL, method = "ML")

anova(baseline_vol)
anova(vol) # TIME signifikant (p < .001, d = .32)
anova(baseline_vol, vol)

lme.dscore(vol,data=aggdata_long_VOL,type="nlme")

summary(vol)

# weitere Graphik-Versuche (nicht zufrieden)

aggdata_long_VOL$Feedback.Faktor <- factor(aggdata_long_VOL$Feedback, level = c(0,1), label = c("Achtsamkeit", "Feedback"))

bar_Vol <- ggplot(aggdata_long_VOL, aes(TIME, VOL, fill = Feedback.Faktor))


bar_Vol + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2) +
  labs(x = "Messzeitpunkt", y = "Durchschnitt Volition", fill = "Bedingung")

  
# lme für Reflexion

aggdata_long_REF <- melt(D_T1T2,id.vars=c("SERIAL", "Feedback"), measure.vars=c("reft1", "reft2"), variable.name="TIME",value.name="REF", na.rm = TRUE)

aggdata_long_REF <- aggdata_long_REF %>% group_by(SERIAL) %>% filter(n()>1)

describeBy(aggdata_long_REF$REF, list(aggdata_long_REF$TIME, aggdata_long_REF$Feedback), mat = TRUE) 

baseline_ref <- lme(REF ~ 1, random = ~1|TIME/Feedback, data = aggdata_long_REF, method = "ML")
ref <- lme(REF~TIME*Feedback, random=~TIME|SERIAL, data=aggdata_long_REF, method = "ML")

anova(baseline_ref)
anova(ref) # n. s.
anova(baseline_ref, ref)

lme.dscore(ref,data=aggdata_long_REF,type="nlme")

summary(ref)


# lme für Zeitplan

aggdata_long_PLAN <- melt(D_T1T2,id.vars=c("SERIAL", "Feedback"), measure.vars=c("plant1", "plant2"), variable.name="TIME",value.name="PLAN", na.rm = TRUE)

aggdata_long_PLAN <- aggdata_long_PLAN %>% group_by(SERIAL) %>% filter(n()>1)

describeBy(aggdata_long_PLAN$PLAN, list(aggdata_long_PLAN$TIME, aggdata_long_PLAN$Feedback), mat = TRUE) 

baseline_plan <- lme(PLAN ~ 1, random = ~1|TIME/Feedback, data = aggdata_long_PLAN, method = "ML")
plan <- lme(PLAN~TIME*Feedback, random=~TIME|SERIAL, data=aggdata_long_PLAN, method = "ML")

anova(baseline_plan)
anova(plan) # TIME und FEEDBACK signifikant (TIME: p = .027, d = .20; Feedback: p = .022, d = .28)
anova(baseline_plan, plan)

lme.dscore(plan,data=aggdata_long_PLAN,type="nlme")

summary(plan)


# lme für Prokrastination

aggdata_long_PRO <- melt(D_T1T2,id.vars=c("SERIAL", "Feedback"), measure.vars=c("prot1", "prot2"), variable.name="TIME",value.name="PRO", na.rm = TRUE)

aggdata_long_PRO <- aggdata_long_PRO %>% group_by(SERIAL) %>% filter(n()>1)

describeBy(aggdata_long_PRO$PRO, list(aggdata_long_PRO$TIME, aggdata_long_PRO$Feedback), mat = TRUE) 

baseline_pro <- lme(PRO ~ 1, random = ~1|TIME/Feedback, data = aggdata_long_PRO, method = "ML")
pro <- lme(PRO~TIME*Feedback, random=~TIME|SERIAL, data=aggdata_long_PRO, method = "ML")

anova(baseline_pro)
anova(pro) # TIME signifikant (p < .001, d = -.33)
anova(baseline_pro, pro)

lme.dscore(pro,data=aggdata_long_PRO,type="nlme")

summary(pro)


# lme für Selbstwirksamkeit

aggdata_long_SE <- melt(D_T1T2,id.vars=c("SERIAL", "Feedback"), measure.vars=c("set1", "set2"), variable.name="TIME",value.name="SE", na.rm = TRUE)

aggdata_long_SE <- aggdata_long_SE %>% group_by(SERIAL) %>% filter(n()>1)

describeBy(aggdata_long_SE$SE, list(aggdata_long_SE$TIME, aggdata_long_SE$Feedback), mat = TRUE) 

baseline_se <- lme(SE ~ 1, random = ~1|TIME/Feedback, data = aggdata_long_SE, method = "ML")
se <- lme(SE~TIME*Feedback, random=~TIME|SERIAL, data=aggdata_long_SE, method = "ML")

anova(baseline_se)
anova(se) # TIME signifikant (p = < .001, d = .72)
anova(baseline_se, se)

lme.dscore(se,data=aggdata_long_SE,type="nlme")

summary(se)


# * 7.2 Achtsamkeitsinventar (CHIME) ----

D_T1T2_CHIME <- D_T1T2 
D_T1T2_CHIME <- D_T1T2_CHIME %>% group_by(SERIAL) %>% filter(n()>1) %>% filter(chime1 != "NA", chimeGesamt != "NA")

# lme für CHIME Subskala 1 (Achtsamkeit = 1; Feedback = 0)

describeBy(D_T1T2_CHIME$chime1, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Achtsamkeit), mat = TRUE) 

baseline_chime1 <- lme(chime1 ~ 1, random = ~1|TIME/Achtsamkeit, data = D_T1T2_CHIME, method = "ML")
CHIME1 <- lme(chime1~TIME*Achtsamkeit, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime1)
anova(CHIME1) # Achtsamkeit signifikant ( p = .0124, d = -.19)
anova(baseline_chime1, CHIME1)

lme.dscore(CHIME1,data=D_T1T2_CHIME,type="nlme")


# lme für CHIME Subskala 2 (Achtsamkeit = 1; Feedback = 0)

describeBy(D_T1T2_CHIME$chime2, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Achtsamkeit), mat = TRUE) 

baseline_chime2 <- lme(chime2 ~ 1, random = ~1|TIME/Achtsamkeit, data = D_T1T2_CHIME, method = "ML")
CHIME2 <- lme(chime2~TIME*Achtsamkeit, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime2)
anova(CHIME2) 
anova(baseline_chime2, CHIME2)

lme.dscore(CHIME2,data=D_T1T2_CHIME,type="nlme")


# lme für CHIME Subskala 3 (Achtsamkeit = 1; Feedback = 0)

describeBy(D_T1T2_CHIME$chime3, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Achtsamkeit), mat = TRUE) 

baseline_chime3 <- lme(chime3 ~ 1, random = ~1|TIME/Achtsamkeit, data = D_T1T2_CHIME, method = "ML")
CHIME3 <- lme(chime3~TIME*Achtsamkeit, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime3)
anova(CHIME3) 
anova(baseline_chime3, CHIME3)

lme.dscore(CHIME3,data=D_T1T2_CHIME,type="nlme")


# lme für CHIME Subskala 4 (Achtsamkeit = 1; Feedback = 0)

describeBy(D_T1T2_CHIME$chime4, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Achtsamkeit), mat = TRUE) 

baseline_chime4 <- lme(chime4 ~ 1, random = ~1|TIME/Achtsamkeit, data = D_T1T2_CHIME, method = "ML")
CHIME4 <- lme(chime4~TIME*Achtsamkeit, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime4)
anova(CHIME4) 
anova(baseline_chime4, CHIME4)

lme.dscore(CHIME4,data=D_T1T2_CHIME,type="nlme")


# lme für CHIME Subskala 5 (Achtsamkeit = 1; Feedback = 0)

describeBy(D_T1T2_CHIME$chime5, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Achtsamkeit), mat = TRUE) 

baseline_chime5 <- lme(chime5 ~ 1, random = ~1|TIME/Achtsamkeit, data = D_T1T2_CHIME, method = "ML")
CHIME5 <- lme(chime5~TIME*Achtsamkeit, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime5)
anova(CHIME5) # TIME signifikant (p = .0003, d = .54) 
anova(baseline_chime5, CHIME5)

lme.dscore(CHIME5,data=D_T1T2_CHIME,type="nlme")


# lme für CHIME Subskala 6 (Achtsamkeit = 1; Feedback = 0)

describeBy(D_T1T2_CHIME$chime6, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Achtsamkeit), mat = TRUE) 

baseline_chime6 <- lme(chime6 ~ 1, random = ~1|TIME/Achtsamkeit, data = D_T1T2_CHIME, method = "ML")
CHIME6 <- lme(chime6~TIME*Achtsamkeit, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime6)
anova(CHIME6) # Achtsamkeit signifikant (p = .0013, d = .40)
anova(baseline_chime6, CHIME6)

lme.dscore(CHIME6,data=D_T1T2_CHIME,type="nlme")


# lme für CHIME Subskala 7 (Achtsamkeit = 1; Feedback = 0)

describeBy(D_T1T2_CHIME$chime7, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Achtsamkeit), mat = TRUE) 

baseline_chime7 <- lme(chime7 ~ 1, random = ~1|TIME/Achtsamkeit, data = D_T1T2_CHIME, method = "ML")
CHIME7 <- lme(chime7~TIME*Achtsamkeit, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime7)
anova(CHIME7) 
anova(baseline_chime7, CHIME7)

lme.dscore(CHIME7,data=D_T1T2_CHIME,type="nlme")


# lme für CHIME Subskala 8 (Achtsamkeit = 1; Feedback = 0)

describeBy(D_T1T2_CHIME$chime8, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Achtsamkeit), mat = TRUE) 

baseline_chime8 <- lme(chime8 ~ 1, random = ~1|TIME/Achtsamkeit, data = D_T1T2_CHIME, method = "ML")
CHIME8 <- lme(chime8~TIME*Achtsamkeit, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime8)
anova(CHIME8) 
anova(baseline_chime8, CHIME8)

lme.dscore(CHIME8,data=D_T1T2_CHIME,type="nlme")


# lme für CHIME Gesamt (Achtsamkeit = 1; Feedback = 0)

describeBy(D_T1T2_CHIME$chimeGesamt, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Achtsamkeit), mat = TRUE)

baseline_chimeGesamt <- lme(chimeGesamt ~ 1, random = ~1|TIME/Achtsamkeit, data = D_T1T2_CHIME, method = "ML")
CHIMEgesamt <- lme(chimeGesamt~TIME*Achtsamkeit, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chimeGesamt)
anova(CHIMEgesamt) # TIME signifikant (p = .0265, d = .16)
anova(baseline_chimeGesamt, CHIMEgesamt)

lme.dscore(CHIMEgesamt,data=D_T1T2_CHIME,type="nlme")



# 8 Korrelation state und trait Variablen ----
    # => funzt noch nicht

KorrelationT1undLP <- cor(AD_ohne_Dropout[c("SE01_03","SM02_01","TE03_01","PL01_01","TE08_01","TE06_01","TE10_01","TE07_01","reft1","plant1","volt1","mott1","goalt1", "set1", "prot1")])

cortable <- KorrelationT1undLP %>% dplyr::select(SE01_03,SM02_01,TE03_01,PL01_01,TE08_01,TE06_01,TE10_01,TE07_01,reft1,plant1,volt1,mott1,goalt1, set1, prot1)

library("apaTables")

cortab <- apa.cor.table(cortable, filename = "table.doc", table.number = NA, landscape = TRUE)

# 9 Grand mean centering ----
    # => noch nicht erfolgreich #### --> M = 0 prüfen 

D_T1LPgmc <- D_T1LP %>%
  mutate(
    gmc_GOALt1 =goalt1- mean(goalt1, na.rm=TRUE),
    gmc_MOTt1 = mott1-mean(mott1, na.rm=TRUE),
    gmc_VOLt1 = volt1-mean(volt1, na.rm=TRUE),
    gmc_PLANt1 = plant1-mean(plant1, na.rm=TRUE),
    gmc_PROt1 = prot1-mean(prot1, na.rm=TRUE),
    gmc_SEt1 = set1-mean(set1, na.rm=TRUE)
    )

# 10 ICC berechnen ----

ICCgoal <- ICCbare(SERIAL, TE03_01, AD_ohne_Dropout)
ICCgoal
ICCse <- ICCbare(SERIAL, SE01_03, AD_ohne_Dropout)
ICCse
ICCmot <- ICCbare(SERIAL, SM02_01, AD_ohne_Dropout)
ICCmot
ICCplan <- ICCbare(SERIAL, PL01_01, AD_ohne_Dropout)
ICCplan
ICCef <- ICCbare(SERIAL, TE08_01, AD_ohne_Dropout)
ICCef
ICCtimeM <- ICCbare(SERIAL, TE06_01, AD_ohne_Dropout)
ICCtimeM 
ICCsat <- ICCbare(SERIAL, TE10_01, AD_ohne_Dropout)
ICCsat
ICCpro <- ICCbare(SERIAL, TE07_01, AD_ohne_Dropout)
ICCpro

# 11 der klägliche Versuch eines HLM ----
    # (funzt nicht)

# nach Field


intercept <-gls(TE03_01 ~ 1, data = D_T1LPgmc, method = "ML", na.action = na.exclude)

summary(intercept)

randomIntercept <-lme(TE03_01 ~ 1, data = D_T1LPgmc, random = ~1|SERIAL, method = "ML", na.action = na.exclude, control = list(opt="optim"))

summary(randomIntercept)

timeRI<-update(randomIntercept, .~. + DAYS)

summary(timeRI)

timeRS<-update(timeRI, random = ~Feedback +DAYS|SERIAL)

summary(timeRS)

ARModel<-update(timeRS, correlation = corAR1()) # kann hier keine Autokorrelation definieren

anova(intercept, randomIntercept, timeRI, timeRS, ARModel)

summary(ARModel); 
intervals(ARModel) 


# Wenn man DAYS zu character transformiert und damit rechnet (DAYSchar), kommen dieselben Ergebnisse wie für TIME raus

D_T1LPgmc$DAYSchar <- as.character(D_T1LPgmc$DAYS) 


## TIME statt DAYS (test-Rechnung)

timeRI2<-update(randomIntercept, .~. + TIME) 

summary(timeRI2)

timeRS2<-update(timeRI2, random = ~Feedback +TIME|SERIAL) # Mit TIME und dem D_T1LPgmc Datensatz funktioniert es ab hier nicht mehr

summary(timeRS2)


# braucht man einen aggregierten Datensatz oder können wir einfach mit D_T1LPgmc rechnen? z. B.:

HLMdata_GOAL <- melt(D_T1LPgmc,id.vars=c("SERIAL", "Feedback"), measure.vars=c("TE03_01", "gmc_GOALt1"), variable.name="TIME",value.name="GOAL", na.rm = TRUE) 


### Neuer Versuch mit lme4 Paket und lmer Funktion

modell.1 <- lmer(TE03_01 ~ 1 + (1|Feedback), data = D_T1LPgmc)
summary(modell.1)

0.04914/(0.04914+1.60414) # ICC

modell.2 <- lmer(TE03_01 ~ SERIAL+(1|Feedback), data = D_T1LPgmc)
summary(modell.2)

# nach Theobald

D_T1LPgmc$gmc_GOALt1fac <- factor(D_T1LPgmc$gmc_GOALt1) ### random Ausprobiern, weil die Fehlermeldung sagt die factors haben nicht 2 Level (SERIAL hat z. B. nur 1 level und die anderen sind keine factors)
D_T1LPgmc$TIMEfac <- factor(D_T1LPgmc$TIME)
D_T1LPgmc$SERIALfac <- factor(D_T1LPgmc$SERIAL)
D_T1LPgmc$Feedbackfac <- factor(D_T1LPgmc$Feedback)
D_T1LPgmc$TE03_01fac <- factor(D_T1LPgmc$TE03_01) ## Im Zweifel löschen

goalsetting_feedback <-lme(TE03_01 ~ Feedback+ gmc_GOALt1 ,  correlation = corAR1(),  random = ~ 1 + Feedback +DAYSchar | SERIAL, data = D_T1LPgmc, na.action=na.omit, method='ML')

selfefficacy_feedback <-lme(TASE ~ Feedback+ gmc_SEt1 , correlation = corAR1()  , random = ~ 1 + Feedback + days | idnr, data=data, na.action=na.omit, method='ML')



# vermutlich FINALE Lösung: ----


# (Zielerreichung => TE03_01;) => die Variable wird gar nicht ausgewertet :D Wir hatten Zielsetzung mit Zielerreichung verwechselt.

# Zielsetzung (anspruchsvoll) => LZ04_01; ### gmc_GOALt1
# Planung => PL01_01; ### gmc_PLANt1
# intrinsische Motivation => SM02_02; ### gmc_MOTt1 
# Selbstwirksamkeit => SE01_03; ### gmc_SEt1
# Zeitplan => TE06_01; ### gmc_PLANt1
# Zufriedenheit => TE10_01; ### KEIN mean centering
# Prokrastination => TE07_01; ### gmc_PROt1
# Anstrengung => TE08_01 ### KEIN mean centering


# neues Subset mit den relevanten Variablen für die Mehrebenenanalyse
D_T1LP_gmc2.0 <- subset(D_T1LPgmc, select = c(SERIAL, TIME2.0, Feedback, LZ04_01, PL01_01, SM02_02, SE01_03 ,TE06_01, TE10_01 ,TE07_01, TE08_01))

# zusätzliche csv-Datei mit den gmc-Variablen
gmc_Variablen <- read.csv2(file.choose()) # Datei: gmc_Variablen

# Zuordnung der gmc-Variablen im Subset D_T1LP_gmx2.0 zu den Seriennummern
D_T1LP_gmc2.0 <- left_join(D_T1LP_gmc2.0, gmc_Variablen, by = "SERIAL")


# Auswertung Zielsetzung (LZ04_01)
Zielsetzung.model <- lme(LZ04_01 ~ Feedback + gmc_GOALt1, random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Zielsetzung.model)

lme.dscore(Zielsetzung.model,data=D_T1LP_gmc2.0,type="nlme")

# Auswertung Planung (PL01_01)
Planung.model <- lme(PL01_01 ~ Feedback + gmc_PLANt1, random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Planung.model)

lme.dscore(Planung.model,data=D_T1LP_gmc2.0,type="nlme")

# Auswertung intrinsische Motivation (SM02_02)
Motivation.model <- lme(SM02_02 ~ Feedback + gmc_MOTt1 , random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Motivation.model)

lme.dscore(Motivation.model,data=D_T1LP_gmc2.0,type="nlme")

# Selbstwirksamkeit (SE01_03)
Selbstwirksamkeit.model <- lme(SE01_03 ~ Feedback + gmc_SEt1 , random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Selbstwirksamkeit.model)

lme.dscore(Selbstwirksamkeit.model,data=D_T1LP_gmc2.0,type="nlme")

# Zeitplan (TE06_01)
Zeitplan.model <- lme(TE06_01 ~ Feedback + gmc_PLANt1 , random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Zeitplan.model)

lme.dscore(Zeitplan.model,data=D_T1LP_gmc2.0,type="nlme")

# Zufriedenheit (TE10_01)
Zufriedenheit.model <- lme(TE10_01 ~ Feedback, random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Zufriedenheit.model)

lme.dscore(Zufriedenheit.model,data=D_T1LP_gmc2.0,type="nlme")

# Prokrastination (TE07_01)
Prokrastination.model <- lme(TE07_01 ~ Feedback + gmc_PROt1 , random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Prokrastination.model)

lme.dscore(Prokrastination.model,data=D_T1LP_gmc2.0,type="nlme")

# Anstrengung (TE08_01)
Anstrengung.model <- lme(TE08_01 ~ Feedback, random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Anstrengung.model)

lme.dscore(Anstrengung.model,data=D_T1LP_gmc2.0,type="nlme")
