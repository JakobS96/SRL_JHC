
# 1 Organisatorisches ----

# * 1.1 Laden der Pakete ----

library(apaTables)
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
library(misty)
library(nlme)
library(nortest)
library(psych)
library(reshape)
library(reshape2)
library(Rmisc)
library(semTools)
library(tidyr)
library(texreg)


# * 1.2 Laden des Datensatzes und der Datei mit den Zusatzvariablen ----

Alle_Daten <- read.csv2(file.choose()) # Datei: Alle_Daten_Stand 01.05.

Zusatzvariablen <- read.csv2(file.choose()) # Datei: Zusatzvariablen_neu

Achtsamkeit_Verweildauer <- read.csv2(file.choose()) # Datei: Daten_Achtsamkeit_Verweildauer

gmc_Variablen <- read.csv2(file.choose()) # Datei: gmc_Variablen

D_T1T3_final <- read.csv2(file.choose()) # Datei: D_T1T3_final

# * * 1.2.1 Kodierung fehlender Werte ----

Alle_Daten[Alle_Daten == -9 | Alle_Daten == -1 | Alle_Daten == ""] <- NA

Achtsamkeit_Verweildauer[Achtsamkeit_Verweildauer == -9 | Achtsamkeit_Verweildauer == -1 | Achtsamkeit_Verweildauer == ""] <- NA

# * * 1.2.2 neue Zusatzvariablen ----

    # 1) Feedback (Feedback = 1; Achtsamkeit = 0)
    # 2) FinishT1 (T1 Fragebogen ausgefuellt = 1; T1 Fragebogen nicht ausgefuellt = 0)
    # 3) FinishT2 (T2 Fragebogen ausgefuellt = 1; T2 Fragebogen nicht ausgefuellt = 0)
    # 4) Finish18 (mehr 17 Tagen Lernplaner ausgefuellt = 1; weniger als 17 Tage Lernplaner ausgefuellt = 0)

Alle_Daten_neu <- left_join(Alle_Daten, Zusatzvariablen, by = "SERIAL")

AD <- Alle_Daten_neu

Achtsamkeit_Verweildauer <- left_join(Achtsamkeit_Verweildauer, Zusatzvariablen, by = "SERIAL")

# * 1.3 Dropout bestimmen ----
  # (Berechnen einer neuen Variable: dropout)

AD <- mutate(AD, dropout = ifelse(FinishT1 == 1 & FinishT2 == 1 & Finish18 == 1 & SERIAL != "NA", 0,1))

AD$dropout[is.na(AD$dropout)] <- 1

describeBy(data.frame(AD$Feedback), group = AD$dropout) # 569 Zeilen wurden als Dropout identifiziert.

# Chi^2-Test: Unterschiedlich hoher Dropout zwischen den Gruppen? 

Dropout_nach_Bedingung <- table(AD$dropout, AD$Feedback)
addmargins(Dropout_nach_Bedingung)

chisq.test(Dropout_nach_Bedingung)


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


# * 2.4 Skalenbildung Evaluation Lernplaner ----

AD$EVLP <- (AD$EV02_01 + AD$EV02_02+ AD$EV02_03 + AD$EV02_04 + AD$EV02_05 + AD$EV02_06)/6

AD_ohne_Dropout$EVLP <- (AD_ohne_Dropout$EV02_01 + AD_ohne_Dropout$EV02_02+ AD_ohne_Dropout$EV02_03 + AD_ohne_Dropout$EV02_04 + AD_ohne_Dropout$EV02_05 + AD_ohne_Dropout$EV02_06)/6

 # relevant ob Dropout oder nicht? Deskriptiv sowieso kaum Unterschiede


# * 2.5 Skalenbildung Evaluation Feedback ----

AD_ohne_Dropout$EVFB <- (AD_ohne_Dropout$EV04_01 + AD_ohne_Dropout$EV04_02 + AD_ohne_Dropout$EV04_03 + AD_ohne_Dropout$EV04_04 + AD_ohne_Dropout$EV04_05)/5


# 3 Bildung der benoetigten Subsets ----

# * 3.1 AD_ohne_Dropout, Achtsamkeit_ohne_Dropout ----

AD_ohne_Dropout <- filter(AD, FinishT1 == 1 & FinishT2 == 1 & Finish18 == 1 & SERIAL != "NA") # Es werden 574 Fälle raus gefiltert (5x SERIAL = NA)

Achtsamkeit_ohne_Dropout <- filter(Achtsamkeit_Verweildauer, FinishT1 == 1 & FinishT2 == 1 & Finish18 == 1 & SERIAL != "NA")

# * 3.2 T1 ----

D_T1 <- subset(AD_ohne_Dropout, TIME=="T1")

# * 3.3 T2 ----

D_T2 <- subset(AD_ohne_Dropout, TIME=="T2")

# * 3.4 T3 ----

D_T3 <- subset(AD_ohne_Dropout, TIME=="T3")

# * 3.5 T1 & T2 ----

D_T1T2 <- rbind(D_T1, D_T2) # DZ883544 muss raus gefiltert werden, da im T2 Fragebogen nicht konzentriert 
D_T1T2 <- filter(D_T1T2, SERIAL != "DZ883544", SERIAL != "ZV438183")

table(D_T1T2$TIME, D_T1T2$Feedback)

table(AD_ohne_Dropout$TIME, AD_ohne_Dropout$Feedback)

# * 3.5 T1 & T2 => CHIME ----

D_T1T2_CHIME <- D_T1T2 
D_T1T2_CHIME <- D_T1T2_CHIME %>% group_by(SERIAL) %>% filter(n()>1) %>% filter(chime1 != "NA", chimeGesamt != "NA")

# * 3.6 T1 & LP_T1-LP_T35 ----

D_LP <- filter(AD_ohne_Dropout, TIME != "T1", TIME != "T2", TIME != "T3")
D_LP <- filter(D_LP, TE16 != 2)

D_T1LP <- rbind(D_T1, D_LP)

table(AD_ohne_Dropout$TE16)

# * 3.7 T1 & T3 => D_T1T3

D_T1T3 <- rbind(D_T1, D_T3) 

write.csv2(D_T1T3, "D_T1T3.csv")

# * 3.8 D_T1LP_gmc2.0 ----
# fuer die Mehrebenenanalyse

D_T1LP_gmc2.0 <- subset(D_T1LPgmc, select = c(SERIAL, TIME2.0, WEEK, Feedback, LZ04_01, PL01_01, SM02_02, SE01_03 ,TE06_01, TE10_01 ,TE07_01, TE08_01))

# Zuordnung der gmc-Variablen im Subset D_T1LP_gmx2.0 zu den Seriennummern
D_T1LP_gmc2.0 <- left_join(D_T1LP_gmc2.0, gmc_Variablen, by = "SERIAL")


# 4 Reliabilitaetsanalysen: Berechnung McDonalds Omega ----

# * 4.1 Reliabilitaeten: T1 ----

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

# Chime (T1_1) Gewahrsein gegenueber inneren Erfahrungen: alpha = .70; omega = .75
omegaChimeT1_1 <- D_T1[c("FA02_01", "FA02_05", "FA02_14", "FA02_29", "FA02_34")]
omega(omegaChimeT1_1)

# Chime (T1_2) Gewahrsein gegenueber aeu?eren Erfahrungen: alpha = .77; omega = .81
omegaChimeT1_2 <- D_T1[c("FA02_09", "FA02_18", "FA02_21", "FA02_27")]
omega(omegaChimeT1_2)

# Chime (T1_3) bewusstes Handeln, Gegenwaertigkeit: alpha = .60; omega = .67
omegaChimeT1_3 <- D_T1[c("FA02_10", "FA02_12", "FA02_17", "FA02_26")]
omega(omegaChimeT1_3)

# Chime (T1_4) annehmende, nicht-urteilende, mitfuehlende Haltung: alpha = .88; omega = .90
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

# * 4.2 Reliabilitaetsanalyse: T2 ----

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

# Chime (T2_1) Gewahrsein gegenueber inneren Erfahrungen: alpha = .75; omega = .81
omegaChimeT2_1 <- D_T2[c("FA02_01", "FA02_05", "FA02_14", "FA02_29", "FA02_34")]
omega(omegaChimeT2_1)

# Chime (T2_2) Gewahrsein gegenueber aeusseren Erfahrungen: alpha = .81; omega = .82
omegaChimeT2_2 <- D_T2[c("FA02_09", "FA02_18", "FA02_21", "FA02_27")]
omega(omegaChimeT2_2)

# Chime (T2_3) bewusstes Handeln, Gegenwaertigkeit: alpha = .61; omega = .65
omegaChimeT2_3 <- D_T2[c("FA02_10", "FA02_12", "FA02_17", "FA02_26")]
omega(omegaChimeT2_3)

# Chime (T2_4) annehmende, nicht-urteilende, mitfuehlende Haltung: alpha = .88; omega = .90
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

# * 5.1 Geschlecht ----

# Haeufigkeit und Anteil aufgeteilt nach Bedingung
table(AD_ohne_Dropout$DD03, AD_ohne_Dropout$Feedback) 
prop.table(table(AD_ohne_Dropout$DD03, AD_ohne_Dropout$Feedback))

# Haeufigkeit und Anteil gesamt
table(AD_ohne_Dropout$DD03) 
prop.table(table(AD_ohne_Dropout$DD03))


# * 5.2 Alter ----

# Alter aufgeteilt nach Bedingung
describeBy(AD_ohne_Dropout$DD02_01, AD_ohne_Dropout$Feedback, mat = TRUE) 

# Alter gesamt
describe(AD_ohne_Dropout$DD02_01) 

# * 5.3 Fachsemester ----

# Haeufigkeit und Anteil gesamt
table(AD_ohne_Dropout$DD04_01) 
prop.table(table(AD_ohne_Dropout$DD04_01))

# * 5.4 ausgefuellte Lernplaner ----

# vollstaendig ausgefuellte Lernplaner aufgeteilt nach Bedingung
describeBy(D_T1$Anzahl_LP_vollstaendig, D_T1$Feedback, mat = TRUE) 
table(D_T1$Anzahl_LP_vollstaendig, D_T1$Feedback)

# vollstaendig ausgefuellte Lernplaner gesamt
describe(D_T1$Anzahl_LP_vollstaendig) 

# Lernplaner nur abends aufgeteilt nach Bedingung
describeBy(D_T1$Anzahl_LP_abends, D_T1$Feedback, mat = TRUE) 
table(D_T1$Anzahl_LP_abends , D_T1$Feedback) 

# * 5.5 Vorwissen ----

# Vorwissen Thema Achtsamkeit, aufgeteilt nach Bedingung
describeBy(AD_ohne_Dropout$DD10_01, AD_ohne_Dropout$Feedback, mat = TRUE) 

# Vorwissen Thema Feedback, aufgeteilt nach Bedingung
describeBy(AD_ohne_Dropout$DD10_08, AD_ohne_Dropout$Feedback, mat = TRUE) 

# * 5.6 Evaluation Feedback ----

describeBy(AD_ohne_Dropout$EVFB, AD_ohne_Dropout$Feedback, mat = TRUE) # M = 3.15, SD = 1.08


# 6 T-Tests (Gruppenvergleiche) ----

# * 6.1 Unterschiede zwischen Dropout (1) und kein Dropout (0) => T1 ----

AD$dropout.faktor <- factor(AD$dropout) # Dropout-Variable in einen Faktor konvertiert, um Levene Test rechnen zu können.

# Zielsetzung 
describeBy(AD$goalt1, AD$dropout, mat = TRUE)

leveneTest(AD$goalt1, AD$dropout.faktor) # n.s. => Varianzhomogenitaet gegeben

goalt1_drop <- t.test(AD$goalt1 ~ AD$dropout, var.equal = TRUE)
goalt1_drop # n.s.

ci.smd(ncp = -0.77618,
       n.1 = 151, n.2 = 24) # Cohens d = -.17 

# Selbstmotivierung
describeBy(AD$mott1, AD$dropout, mat = TRUE)

leveneTest(AD$mott1, AD$dropout.faktor) # n.s. => Varianzhomogenitaet gegeben

mott1_drop <- t.test(AD$mott1 ~ AD$dropout, var.equal = TRUE)
mott1_drop # n.s.

ci.smd(ncp = -0.78887,
       n.1 = 152, n.2 = 24) # Cohens d = -.17 

# Volition
describeBy(AD$volt1, AD$dropout, mat = TRUE)

leveneTest(AD$volt1, AD$dropout.faktor) # signifikant => Varianzhomogenitaet NICHT gegeben => Welch-Test

volt1_drop <- t.test(AD$volt1 ~ AD$dropout)
volt1_drop # n.s.

ci.smd(ncp = 0.16259,
       n.1 = 152, n.2 = 24) # Cohens d = .04 

# Reflexion
describeBy(AD$reft1, AD$dropout, mat = TRUE)

leveneTest(AD$reft1, AD$dropout.faktor) # n.s. => Varianzhomogenitaet gegeben

reft1_drop <- t.test(AD$reft1 ~ AD$dropout, var.equal = TRUE)
reft1_drop # n.s.

ci.smd(ncp = 0.67199,
       n.1 = 152, n.2 = 24) # Cohens d = .15 

# Zeitplan
describeBy(AD$plant1, AD$dropout, mat = TRUE)

leveneTest(AD$plant1, AD$dropout.faktor) # n.s. => Varianzhomogenitaet gegeben

plant1_drop <- t.test(AD$plant1 ~ AD$dropout, var.equal = TRUE)
plant1_drop # n.s.

ci.smd(ncp = 0.33985,
       n.1 = 152, n.2 = 24) # Cohens d = .07

# Prokrastination
describeBy(AD$prot1, AD$dropout, mat = TRUE)

leveneTest(AD$prot1, AD$dropout.faktor) # n.s. => Varianzhomogenitaet gegeben

prot1_drop <- t.test(AD$prot1 ~ AD$dropout, var.equal = TRUE)
prot1_drop # n.s.

ci.smd(ncp = -1.5985,
       n.1 = 151, n.2 = 24) # Cohens d = -.35

# Selbstwirksamkeit
describeBy(AD$set1, AD$dropout, mat = TRUE)

leveneTest(AD$set1, AD$dropout.faktor) # n.s. => Varianzhomogenitaet gegeben

set1_drop <- t.test(AD$set1 ~ AD$dropout, var.equal = TRUE)
set1_drop # n.s.

ci.smd(ncp = -1.0324,
       n.1 = 151, n.2 = 23) # Cohens d = -.23


# * 6.2 Unterschiede zwischen LPF (Feedback) und LPA (Achtsamkeit) => T1 ----

AD_ohne_Dropout$Feedback.Faktor <- factor(AD_ohne_Dropout$Feedback) # Feedback-Variable in einen Faktor konvertiert, um Levene Test rechnen zu koennen.

# * * 6.2.1 Trait Variablen ----

# Zielsetzung 
describeBy(AD_ohne_Dropout$goalt1, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$goalt1, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenitaet gegeben

goalt1_differences <- t.test(AD_ohne_Dropout$goalt1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
goalt1_differences # n.s. p = .07

ci.smd(ncp = -1.835,
       n.1 = 74, n.2 = 77) # Cohens d = -.30 

# Selbstmotivierung
describeBy(AD_ohne_Dropout$mott1, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$mott1, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenitaet gegeben

mott1_differences <- t.test(AD_ohne_Dropout$mott1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
mott1_differences # n.s. p = .13

ci.smd(ncp = -1.5245,
       n.1 = 74, n.2 = 78) # Cohens d = -.25 

# Volition 
describeBy(AD_ohne_Dropout$volt1, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$volt1, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenitaet gegeben

volt1_differences <- t.test(AD_ohne_Dropout$volt1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
volt1_differences # n.s. P = .72

ci.smd(ncp = -0.36545,
       n.1 = 74, n.2 = 78) # Cohens d = -.06

# Reflexion 
describeBy(AD_ohne_Dropout$reft1, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$reft1, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenitaet gegeben

reft1_differences <- t.test(AD_ohne_Dropout$reft1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
reft1_differences # n.s. P = .58

ci.smd(ncp = -0.56251,
       n.1 = 74, n.2 = 78) # Cohens d = -.09

# Zeitplan
describeBy(AD_ohne_Dropout$plant1, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$plant1, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenitaet gegeben

plant1_differences <- t.test(AD_ohne_Dropout$plant1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
plant1_differences # n.s. p = .13

ci.smd(ncp = -1.5088,
       n.1 = 74, n.2 = 78) # Cohens d = -.24

# Prokrastination
describeBy(AD_ohne_Dropout$prot1, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$prot1, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenitaet gegeben

prot1_differences <- t.test(AD_ohne_Dropout$prot1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
prot1_differences # n.s. p = .99

ci.smd(ncp = -0.016574,
       n.1 = 74, n.2 = 77) # Cohens d = -.003

# Selbstwirksamkeit
describeBy(AD_ohne_Dropout$set1, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$set1, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenitaet gegeben

set1_differences <- t.test(AD_ohne_Dropout$set1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
set1_differences # n.s. p = .86

ci.smd(ncp = 0.17729,
       n.1 = 74, n.2 = 77) # Cohens d = .03

# vollstaendig ausgefuellte Lernplaner

describeBy(D_T1$Anzahl_LP_vollstaendig, D_T1$Feedback, mat = TRUE)

LP_vollstaendig <- t.test(D_T1$Anzahl_LP_vollstaendig ~ D_T1$Feedback, var.equal = TRUE)
LP_vollstaendig

ci.smd(ncp =  0.29203,
       n.1 = 73, n.2 = 77)


# * * 6.2.2 Achtsamkeitsinventar (CHIME) ---- 

# CHIME Subskala 1 
D_T1$Achtsamkeit.Faktor <- factor(D_T1$Achtsamkeit) # Gruppe Achtsamkeit = 1; Gruppe Feedback = 0 

describeBy(D_T1$chime1, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime1, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenitaet gegeben

chime1_differences <- t.test(D_T1$chime1 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime1_differences # signifikant p = .0054

ci.smd(ncp = 2.8267,
       n.1 = 73, n.2 = 76) # Cohens d = .46

# CHIME Subskala 2
describeBy(D_T1$chime2, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime2, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenitaet gegeben

chime2_differences <- t.test(D_T1$chime2 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime2_differences # n.s. p = .28

ci.smd(ncp = 1.0873,
       n.1 = 73, n.2 = 77) # Cohens d = .18

# Chime Subskala 3
describeBy(D_T1$chime3, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime3, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenitaet gegeben

chime3_differences <- t.test(D_T1$chime3 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime3_differences # n.s. p = .10

ci.smd(ncp = -1.6512,
       n.1 = 73, n.2 = 77) # Cohens d = -.27

# Chime Subskala 4
describeBy(D_T1$chime4, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime4, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenitaet gegeben

chime4_differences <- t.test(D_T1$chime4 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime4_differences # n.s. p = .47

ci.smd(ncp = 0.71652,
       n.1 = 73, n.2 = 77) # Cohens d = .12

# Chime Subskala 5
describeBy(D_T1$chime5, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime5, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenitaet gegeben

chime5_differences <- t.test(D_T1$chime5 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime5_differences # n.s. p = .97

ci.smd(ncp = 0.038648,
       n.1 = 73, n.2 = 77) # Cohens d = .006

# Chime Subskala 6
describeBy(D_T1$chime6, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime6, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenitaet gegeben

chime6_differences <- t.test(D_T1$chime6 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime6_differences # signifikant p = .013

ci.smd(ncp = -2.5027,
       n.1 = 73, n.2 = 77) # Cohens d = -.41

# Chime Subskala 7
describeBy(D_T1$chime7, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime7, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenitaet gegeben

chime7_differences <- t.test(D_T1$chime7 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime7_differences # n.s. p = .86

ci.smd(ncp = 0.17361,
       n.1 = 73, n.2 = 77) # Cohens d = .03

# Chime Subskala 8
describeBy(D_T1$chime8, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chime8, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenitaet gegeben

chime8_differences <- t.test(D_T1$chime8 ~ D_T1$Achtsamkeit, var.equal = TRUE)
chime8_differences # n.s. p = .36

ci.smd(ncp = 0.91834,
       n.1 = 73, n.2 = 77) # Cohens d = .15

# CHIME Gesamt (ueber alle 8 Subskalen hinweg)
describeBy(D_T1$chimeGesamt, D_T1$Achtsamkeit, mat = TRUE)

leveneTest(D_T1$chimeGesamt, D_T1$Achtsamkeit.Faktor) # n.s. => Varianzhomogenitaet gegeben

chimeGesamt_differences <- t.test(D_T1$chimeGesamt ~ D_T1$Achtsamkeit, var.equal = TRUE)
chimeGesamt_differences # n.s. p = 0.61

ci.smd(ncp = 0.51287,
       n.1 = 73, n.2 = 76) # Cohens d = .08

# * 6.3 Unterschiede Evaluation Lernplaner ----
describeBy(AD_ohne_Dropout$EVLP, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$EVLP, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenitaet gegeben

EVLP_differences <- t.test(AD_ohne_Dropout$EVLP ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
EVLP_differences # signfikant p < .001 --> LPF bewertet LP besser als LPA 

ci.smd(ncp = -3.5777,
       n.1 = 79, n.2 = 74) # d = -.58 # warum minus d?

# * * 6.3.1 Unterschiede Evaluation Lernplaneritem EV02_05 ----
# (Ich wuerde in Zukunft in bestimmten Situationen wieder einen Lernplaner nutzen.)

describeBy(AD_ohne_Dropout$EV02_05, AD_ohne_Dropout$Feedback, mat = TRUE)

leveneTest(AD_ohne_Dropout$EV02_05, AD_ohne_Dropout$Feedback.Faktor) # n.s. => Varianzhomogenitaet gegeben

EV02_05_differences <- t.test(AD_ohne_Dropout$EV02_05 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
EV02_05_differences # signfikant p < .001 --> LPF bewertet LP besser als LPA

ci.smd(ncp = -1.4414,
       n.1 = 79, n.2 = 74) # Cohens d = -.23


# 7 T1-T2 ANOVAs ----

# * 7.1 Trait Variablen ----


# lme fuer Zielsetzung 

aggdata_long_GOAL <- melt(D_T1T2,id.vars=c("SERIAL", "Feedback"), measure.vars=c("goalt1", "goalt2"), variable.name="TIME",value.name="GOAL", na.rm = TRUE)

aggdata_long_GOAL <- aggdata_long_GOAL %>% group_by(SERIAL) %>% filter(n()>1) # Filtern einzelner Seriennummern, die nur T1 oder nur T2 ausgefuellt haben

describeBy(aggdata_long_GOAL$GOAL, list(aggdata_long_GOAL$TIME, aggdata_long_GOAL$Feedback), mat = TRUE) 

baseline_goal <- lme(GOAL ~ 1, random = ~1|TIME/Feedback, data = aggdata_long_GOAL, method = "ML")
goal <- lme(GOAL~TIME*Feedback, random=~TIME|SERIAL, data=aggdata_long_GOAL, method = "ML")

anova(baseline_goal)
anova(goal) # Feedback signifikant (p < .001, d = .30)
anova(baseline_goal, goal)

lme.dscore(goal,data=aggdata_long_GOAL,type="nlme")

summary(goal) # Warum werden die Koeffizienten fuer Feedback beim summary Befehl nicht signifikant, obwohl der anova Befehl, z.B. anova(goal) signifikante Ergebnisse anzeigt?

# Plot fuer Zielsetzung

# erster Versuch Mittelwerte graphisch darzustellen
plot_GOAL <- error.bars.by(data.frame(D_T1T2$goalt1, D_T1T2$goalt2), D_T1T2$Feedback,
              lty = c(1,3), las =1,  by.var = TRUE, eyes = FALSE, 
              xlab = "Bedingung", 
              ylab = "Zielsetzung", ylim = c(1,6),
              main = "Zielsetzung Prä-Post",
              family(serif),
              )
# Versuch die Graphik schoen darzustellen

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


# lme fuer Selbstmotivierung

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


# lme fuer Volition

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

  
# lme fuer Reflexion

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


# lme fuer Zeitplan

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


# lme fuer Prokrastination

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


# lme fuer Selbstwirksamkeit

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

# lme fuer CHIME Subskala 1 

describeBy(D_T1T2_CHIME$chime1, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Feedback), mat = TRUE) 

baseline_chime1 <- lme(chime1 ~ 1, random = ~1|TIME/Feedback, data = D_T1T2_CHIME, method = "ML")
CHIME1 <- lme(chime1~TIME*Feedback, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime1)
anova(CHIME1) # Achtsamkeit signifikant ( p = .0124, d = -.19)
anova(baseline_chime1, CHIME1)

lme.dscore(CHIME1,data=D_T1T2_CHIME,type="nlme")


# lme fuer CHIME Subskala 2 

describeBy(D_T1T2_CHIME$chime2, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Feedback), mat = TRUE) 

baseline_chime2 <- lme(chime2 ~ 1, random = ~1|TIME/Feedback, data = D_T1T2_CHIME, method = "ML")
CHIME2 <- lme(chime2~TIME*Feedback, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime2)
anova(CHIME2) 
anova(baseline_chime2, CHIME2)

lme.dscore(CHIME2,data=D_T1T2_CHIME,type="nlme")


# lme fuer CHIME Subskala 3 

describeBy(D_T1T2_CHIME$chime3, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Feedback), mat = TRUE) 

baseline_chime3 <- lme(chime3 ~ 1, random = ~1|TIME/Feedback, data = D_T1T2_CHIME, method = "ML")
CHIME3 <- lme(chime3~TIME*Feedback, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime3)
anova(CHIME3) 
anova(baseline_chime3, CHIME3)

lme.dscore(CHIME3,data=D_T1T2_CHIME,type="nlme")


# lme fuer CHIME Subskala 4 

describeBy(D_T1T2_CHIME$chime4, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Feedback), mat = TRUE) 

baseline_chime4 <- lme(chime4 ~ 1, random = ~1|TIME/Feedback, data = D_T1T2_CHIME, method = "ML")
CHIME4 <- lme(chime4~TIME*Feedback, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime4)
anova(CHIME4) 
anova(baseline_chime4, CHIME4)

lme.dscore(CHIME4,data=D_T1T2_CHIME,type="nlme")


# lme fuer CHIME Subskala 5 

describeBy(D_T1T2_CHIME$chime5, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Feedback), mat = TRUE) 

baseline_chime5 <- lme(chime5 ~ 1, random = ~1|TIME/Feedback, data = D_T1T2_CHIME, method = "ML")
CHIME5 <- lme(chime5~TIME*Feedback, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime5)
anova(CHIME5) # TIME signifikant (p = .0003, d = .54) 
anova(baseline_chime5, CHIME5)

lme.dscore(CHIME5,data=D_T1T2_CHIME,type="nlme")


# lme fuer CHIME Subskala 6 

describeBy(D_T1T2_CHIME$chime6, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Feedback), mat = TRUE) 

baseline_chime6 <- lme(chime6 ~ 1, random = ~1|TIME/Feedback, data = D_T1T2_CHIME, method = "ML")
CHIME6 <- lme(chime6~TIME*Feedback, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime6)
anova(CHIME6) # Achtsamkeit signifikant (p = .0013, d = .40)
anova(baseline_chime6, CHIME6)

lme.dscore(CHIME6,data=D_T1T2_CHIME,type="nlme")


# lme fuer CHIME Subskala 7 

describeBy(D_T1T2_CHIME$chime7, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Feedback), mat = TRUE) 

baseline_chime7 <- lme(chime7 ~ 1, random = ~1|TIME/Feedback, data = D_T1T2_CHIME, method = "ML")
CHIME7 <- lme(chime7~TIME*Feedback, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime7)
anova(CHIME7) 
anova(baseline_chime7, CHIME7)

lme.dscore(CHIME7,data=D_T1T2_CHIME,type="nlme")


# lme fuer CHIME Subskala 8 

describeBy(D_T1T2_CHIME$chime8, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Feedback), mat = TRUE) 

baseline_chime8 <- lme(chime8 ~ 1, random = ~1|TIME/Feedback, data = D_T1T2_CHIME, method = "ML")
CHIME8 <- lme(chime8~TIME*Feedback, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chime8)
anova(CHIME8) 
anova(baseline_chime8, CHIME8)

lme.dscore(CHIME8,data=D_T1T2_CHIME,type="nlme")


# lme fuer CHIME Gesamt 

describeBy(D_T1T2_CHIME$chimeGesamt, list(D_T1T2_CHIME$TIME, D_T1T2_CHIME$Feedback), mat = TRUE)

baseline_chimeGesamt <- lme(chimeGesamt ~ 1, random = ~1|TIME/Feedback, data = D_T1T2_CHIME, method = "ML")
CHIMEgesamt <- lme(chimeGesamt~TIME*Feedback, random=~TIME|SERIAL, data=D_T1T2_CHIME, method = "ML")

anova(baseline_chimeGesamt)
anova(CHIMEgesamt) # TIME signifikant (p = .0265, d = .16)
anova(baseline_chimeGesamt, CHIMEgesamt)

lme.dscore(CHIMEgesamt,data=D_T1T2_CHIME,type="nlme")


# 8 Korrelation state und trait Variablen ----

aggdata_cor <-aggregate(D_T1LP, by =list(D_T1LP$SERIAL),
                    FUN=mean, na.rm=TRUE)


cor(aggdata_cor[c("SE01_03","SM02_02","LZ04_01","PL01_01","TE08_01","TE06_01","TE10_01","TE07_01","reft1","plant1","volt1","mott1","goalt1", "set1", "prot1")], use = "pairwise")


cortable <- aggdata_cor %>% dplyr::select(SE01_03,SM02_02,LZ04_01,PL01_01,TE08_01,TE06_01,TE10_01,TE07_01,reft1,plant1,volt1,mott1,goalt1, set1, prot1)

apa.cor.table(cortable, filename = "table.doc", table.number = NA,
              show.conf.interval = FALSE, landscape = TRUE)


# 9 Grand mean centering ----
    
D_T1LPgmc <- D_T1LP %>%
  mutate(
    gmc_LZ04_01 = LZ04_01- mean(LZ04_01, na.rm =TRUE),
    gmc_GOALt1 =goalt1- mean(goalt1, na.rm=TRUE),
    gmc_MOTt1 = mott1-mean(mott1, na.rm=TRUE),
    gmc_VOLt1 = volt1-mean(volt1, na.rm=TRUE),
    gmc_PLANt1 = plant1-mean(plant1, na.rm=TRUE),
    gmc_PROt1 = prot1-mean(prot1, na.rm=TRUE),
    gmc_SEt1 = set1-mean(set1, na.rm=TRUE)
    )

D_T1LP_gmc3.0 <- filter(D_T1LP_gmc2.0, TIME2.0 == "0")
mean(D_T1LP_gmc3.0$LZ04_01, na.rm=TRUE)


D_T1LP_gmc3.0 <- D_T1LP_gmc3.0 %>%
  mutate(
    gmc_LZ04_01 = LZ04_01 - mean(LZ04_01, na.rm = TRUE), # Zielsetzung
    gmc_PL01_01 = PL01_01 - mean(PL01_01, na.rm = TRUE), # Planung
    gmc_SM02_02 = SM02_02 - mean(SM02_02, na.rm = TRUE), # intrinsische Motivation
    gmc_SE01_03 = SE01_03 - mean(SE01_03, na.rm = TRUE), # Selbstwirksamkeit
    gmc_TE06_01 = TE06_01 - mean(TE06_01, na.rm = TRUE), # Zeitplan
    gmc_TE10_01 = TE10_01 - mean(TE10_01, na.rm = TRUE), # Zufriedenheit
    gmc_TE07_01 = TE07_01 - mean(TE07_01, na.rm = TRUE), # Prokrastination
    gmc_TE08_01 = TE08_01 - mean(TE08_01, na.rm = TRUE)  # Anstrengung
    )

write.csv2(D_T1LP_gmc3.0, "gmc3.0.csv")


# 10 ICC berechnen ----

ICCgoal <- ICCbare(SERIAL, LZ04_01, AD_ohne_Dropout)
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

# Brauchen wir die ICC nicht für den Datensatzt D_T1LP_gmc2.0, mit dem wir auch die Mehrebnenenanalyse gerehcnet haben?
# und warum für die einzelnen Personen und nicht nur für die Gruppenzugehörigkeit?

ICCgoal_Test <- ICCbare(SERIAL, LZ04_01, D_T1LP_gmc2.0)
ICCgoal_Test


# 11 Finale Loesung HLM: ----

# Zielsetzung (anspruchsvoll) => LZ04_01; ### gmc_GOALt1
# Planung => PL01_01; ### gmc_PLANt1
# intrinsische Motivation => SM02_02; ### gmc_MOTt1 
# Selbstwirksamkeit => SE01_03; ### gmc_SEt1
# Zeitplan => TE06_01; ### gmc_PLANt1
# Zufriedenheit => TE10_01; ### KEIN mean centering
# Prokrastination => TE07_01; ### gmc_PROt1
# Anstrengung => TE08_01 ### KEIN mean centering

D_T1LP_gmc2.0$Feedback <- factor(D_T1LP_gmc2.0$Feedback) # Feedback als Faktor, um mit Plots zu rechnen

# * 11.1 Zielsetzung (LZ04_01) ----

Zielsetzung.model <- lme(LZ04_01 ~ Feedback + TIME2.0 + gmc_GOALt1, random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Zielsetzung.model)

# Effektstaerke d
lme.dscore(Zielsetzung.model,data=D_T1LP_gmc2.0,type="nlme")


# * * 11.1.1 Plot Wochenverlauf_Zielsetzung ----

SummGoal <- summarySE(D_T1LP_gmc2.0, measurevar="LZ04_01", groupvars=c("Feedback","WEEK"), na.rm = TRUE)
SummGoal

pd <- position_dodge(0)

PlotGoal <- ggplot(SummGoal, aes(x=WEEK, y=LZ04_01, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=LZ04_01-ci, ymax=LZ04_01+ci), colour="black", width=.15, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Wochen") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Zielsetzung") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

PlotGoal

# * * 11.1.2 Plot Tagesverlauf_Zielsetzung ----

dayGoal <- summarySE(D_T1LP_gmc2.0, measurevar="LZ04_01", groupvars=c("Feedback","TIME2.0"), na.rm = TRUE)
dayGoal

pd <- position_dodge(0)
PlotGoalDay <- ggplot(dayGoal, aes(x=TIME2.0, y=LZ04_01, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=LZ04_01-ci, ymax=LZ04_01+ci), colour="black", width=.5, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Tage") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Zielsetzung") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

PlotGoalDay

# * 11.2 Planung (PL01_01) ----

Planung.model <- lme(PL01_01 ~ Feedback + TIME2.0 + gmc_PLANt1, random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Planung.model)

# Effektstaerke
lme.dscore(Planung.model,data=D_T1LP_gmc2.0,type="nlme")


# * * 11.2.1 Plot Wochenverlauf_Planung ----

SummPlan <- summarySE(D_T1LP_gmc2.0, measurevar="PL01_01", groupvars=c("Feedback","WEEK"), na.rm = TRUE)
SummPlan

pd <- position_dodge(0)

PlotPlan <- ggplot(SummPlan, aes(x=WEEK, y=PL01_01, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=PL01_01-ci, ymax=PL01_01+ci), colour="black", width=.15, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Wochen") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Planung") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))
PlotPlan

# * * 11.2.2 Plot Tagesverlauf_Planung ----

dayPlan <- summarySE(D_T1LP_gmc2.0, measurevar="PL01_01", groupvars=c("Feedback","TIME2.0"), na.rm = TRUE)
dayPlan

pd <- position_dodge(0)
PlotPlanDay <- ggplot(dayPlan, aes(x=TIME2.0, y=PL01_01, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=PL01_01-ci, ymax=PL01_01+ci), colour="black", width=.5, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Tage") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Planung") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

PlotPlanDay


# * 11.3 intrinsische Motivation (SM02_02) ----

Motivation.model <- lme(SM02_02 ~ Feedback + TIME2.0 + gmc_MOTt1 , random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Motivation.model)

# Effektstaerke
lme.dscore(Motivation.model,data=D_T1LP_gmc2.0,type="nlme")


# * * 11.3.1 Plot Wochenverlauf_intrinsische Motivation ----

SummMot <- summarySE(D_T1LP_gmc2.0, measurevar="SM02_02", groupvars=c("Feedback","WEEK"), na.rm = TRUE)
SummMot

pd <- position_dodge(0)

PlotMot <- ggplot(SummMot, aes(x=WEEK, y=SM02_02, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=SM02_02-ci, ymax=SM02_02+ci), colour="black", width=.15, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Wochen") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Motivation") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

PlotMot

# * * 11.3.2 Plot Tagesverlauf_intrinsische Motivation ----

dayMot <- summarySE(D_T1LP_gmc2.0, measurevar="SM02_02", groupvars=c("Feedback","TIME2.0"), na.rm = TRUE)
dayMot

pd <- position_dodge(0)
PlotMotDay <- ggplot(dayMot, aes(x=TIME2.0, y=SM02_02, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=SM02_02-ci, ymax=SM02_02+ci), colour="black", width=.5, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Tage") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Motivation") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

PlotMotDay


# * 11.4 Selbstwirksamkeit (SE01_03) ----

Selbstwirksamkeit.model <- lme(SE01_03 ~ Feedback + TIME2.0 + gmc_SEt1 , random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Selbstwirksamkeit.model)

# Effektstaerke
lme.dscore(Selbstwirksamkeit.model,data=D_T1LP_gmc2.0,type="nlme")


# * * 11.4.1 Plot Wochenverlauf_Selbstwirksamkeit ----

SummSe <- summarySE(D_T1LP_gmc2.0, measurevar="SE01_03", groupvars=c("Feedback","WEEK"), na.rm = TRUE)
SummSe

pd <- position_dodge(0)

PlotSe <- ggplot(SummSe, aes(x=WEEK, y=SE01_03, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=SE01_03-ci, ymax=SE01_03+ci), colour="black", width=.15, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Wochen") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Selbstwirksamkeit") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

PlotSe

# * * 11.4.2 Plot Tagesverlauf_Selbstwirksamkeit ----

daySe <- summarySE(D_T1LP_gmc2.0, measurevar="SE01_03", groupvars=c("Feedback","TIME2.0"), na.rm = TRUE)
daySe

pd <- position_dodge(0)
PlotSeDay <- ggplot(daySe, aes(x=TIME2.0, y=SE01_03, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=SE01_03-ci, ymax=SE01_03+ci), colour="black", width=.5, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Tage") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Selbstwirksamkeit") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

PlotSeDay


# * 11.5 Zeitplan (TE06_01) ----

Zeitplan.model <- lme(TE06_01 ~ Feedback + TIME2.0 + gmc_PLANt1 , random = ~ 1 + Feedback + TIME2.0 |SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Zeitplan.model)

# Effektstaerke
lme.dscore(Zeitplan.model,data=D_T1LP_gmc2.0,type="nlme")


# * * 11.5.1 Plot Wochenverlauf_Zeitplan ----

SummTime <- summarySE(D_T1LP_gmc2.0, measurevar="TE06_01", groupvars=c("Feedback","WEEK"), na.rm = TRUE)
SummTime

pd <- position_dodge(0)

PlotTime <- ggplot(SummTime, aes(x=WEEK, y=TE06_01, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=TE06_01-ci, ymax=TE06_01+ci), colour="black", width=.15, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Wochen") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Zeitplanung") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

PlotTime

# * * 11.5.2 Plot Tagesverlauf_Zeitplan ----

dayTime <- summarySE(D_T1LP_gmc2.0, measurevar="TE06_01", groupvars=c("Feedback","TIME2.0"), na.rm = TRUE)
dayTime

pd <- position_dodge(0)
PlotTimeDay <- ggplot(dayTime, aes(x=TIME2.0, y=TE06_01, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=TE06_01-ci, ymax=TE06_01+ci), colour="black", width=.5, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Tage") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Zeitplanung") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

PlotTimeDay


# * 11.6 Zufriedenheit (TE10_01) ----

Zufriedenheit.model <- lme(TE10_01 ~ Feedback + TIME2.0, random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Zufriedenheit.model)

# Effektstaerke
lme.dscore(Zufriedenheit.model,data=D_T1LP_gmc2.0,type="nlme")


# * * 11.6.1 Plot Wochenverlauf_Zufriedenheit ----

SummSat <- summarySE(D_T1LP_gmc2.0, measurevar="TE10_01", groupvars=c("Feedback","WEEK"), na.rm = TRUE)
SummSat

pd <- position_dodge(0)

PlotSat <- ggplot(SummSat, aes(x=WEEK, y=TE10_01, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=TE10_01-ci, ymax=TE10_01+ci), colour="black", width=.15, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Wochen") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Zufriedenheit") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

PlotSat

# * * 11.6.2 Plot Tagesverlauf_Zufriedenheit ----

daySat <- summarySE(D_T1LP_gmc2.0, measurevar="TE10_01", groupvars=c("Feedback","TIME2.0"), na.rm = TRUE)
daySat

pd <- position_dodge(0)
PlotSatDay <- ggplot(daySat, aes(x=TIME2.0, y=TE10_01, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=TE10_01-ci, ymax=TE10_01+ci), colour="black", width=.5, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Tage") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Zufriedenheit") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

PlotSatDay


# * 11.7 Prokrastination (TE07_01) ----

Prokrastination.model <- lme(TE07_01 ~ Feedback + TIME2.0 + gmc_PROt1 , random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Prokrastination.model)

# Effektstaerke
lme.dscore(Prokrastination.model,data=D_T1LP_gmc2.0,type="nlme")


# * * 11.7.1 Plot Wochenverlauf_Prokrastination ----

SummPro <- summarySE(D_T1LP_gmc2.0, measurevar="TE07_01", groupvars=c("Feedback","WEEK"), na.rm = TRUE)
SummPro

pd <- position_dodge(0)

PlotPro <- ggplot(SummPro, aes(x=WEEK, y=TE07_01, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=TE07_01-ci, ymax=TE07_01+ci), colour="black", width=.15, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Wochen") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Prokrastination") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

PlotPro

# * * 11.7.2 Plot Tagesverlauf_Prokrastination ----

dayPro <- summarySE(D_T1LP_gmc2.0, measurevar="TE07_01", groupvars=c("Feedback","TIME2.0"), na.rm = TRUE)
dayPro

pd <- position_dodge(0)
PlotProDay <- ggplot(dayPro, aes(x=TIME2.0, y=TE07_01, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=TE07_01-ci, ymax=TE07_01+ci), colour="black", width=.5, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Tage") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Prokrastination") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

PlotProDay


# * 11.8 Anstrengung/ Lernaufwand (TE08_01) ----

describeBy(AD_ohne_Dropout$TE08_01, AD_ohne_Dropout$Feedback, mat = TRUE)

Anstrengung.model <- lme(TE08_01 ~ Feedback + TIME2.0, random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)
summary(Anstrengung.model)

# Effektstaerke
lme.dscore(Anstrengung.model,data=D_T1LP_gmc2.0,type="nlme")


# * * 11.8.1 Plot Wochenverlauf_Anstrengung/ Lernaufwand ---- 

SummEff <- summarySE(D_T1LP_gmc2.0, measurevar="TE08_01", groupvars=c("Feedback","WEEK"), na.rm = TRUE)
SummEff

pd <- position_dodge(0)

PlotEff <- ggplot(SummEff, aes(x=WEEK, y=TE08_01, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=TE08_01-ci, ymax=TE08_01+ci), colour="black", width=.15, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Wochen") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Anstrengung") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

PlotEff

# * * 11.8.2 Plot Tagesverlauf_Anstrengung/ Lenraufwand ---- 

dayEff <- summarySE(D_T1LP_gmc2.0, measurevar="TE08_01", groupvars=c("Feedback","TIME2.0"), na.rm = TRUE)
dayEff

PlotEffDay <- ggplot(dayEff, aes(x=TIME2.0, y=TE08_01, colour=Feedback, group=Feedback)) + 
  geom_errorbar(aes(ymin=TE08_01-ci, ymax=TE08_01+ci), colour="black", width=.5, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill="white") + 
  xlab("Tage") +
  ylab("") +
  scale_colour_hue(name="Gruppe",    
                   breaks=c("0", "1"),
                   labels=c("Achtsamkeit", "Feedback"),
                   l=40) +                    
  ggtitle("Anstrengung") +
  expand_limits(y=2:6) +                        
  scale_y_continuous(breaks=2:6) +        
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

PlotEffDay


#### Genauere Betrachtung der Ergebnisse


# Modellvergleich für Prokrastination --> Sinnvoll, um Interaktionen nicht rechnen zu müssen, wenn Modelle dadurch schlechter werden
Prokrastination.model <- lme(TE07_01 ~ Feedback*TIME2.0 + gmc_PROt1 , random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)

Prokrastination.model2 <- lme(TE07_01 ~ Feedback + TIME2.0 + gmc_PROt1 , random = ~ 1 + Feedback + TIME2.0|SERIAL, correlation=corAR1(),na.action = na.omit, data = D_T1LP_gmc2.0)


anova(Prokrastination.model, Prokrastination.model2) # Modellfit wird schlechter, wenn Interaktion betrachtet wird 



# 12 Deskriptive Betrachtung der Verweildauer der Achtsamkeitsimpulse ----

# TA01 => Faust-Ballen-Thilini (ca. 140s)
# TA02 => Faust-Ballen-Jakob (ca. 120s)
# TA03 => Unsere Gedanken (240s)
# TA04 => Erfahrung beobachten (240s)
# TA05 => Seated-Mantra-Jakob (ca. 139s)
# TA06 => Seated-Mantra-Thilini (ca. 180s)
# TA07 => Bewusstes Atmen (240s)
# TA08 => OMP-Jakob (ca. 575s)
# TA09 => OMP-Thilini (ca.440s)
# TA10 => Wahrnehmung Atem (240s)

# Stichprobengroesse Achtsamkeit => 74

table(Achtsamkeit_ohne_Dropout$QUESTNNR) # Haeufigkeiten, wie oft die jeweiligen Lernplaner ausgefuellt wurden
table(Achtsamkeit_ohne_Dropout$SERIAL)

# * 12.1 TA01 - Faust-Ballen-Thilini (ca. 140s) ----
Achtsamkeit_false_TA01 <- filter(Achtsamkeit_ohne_Dropout, TIME013 < 140, QUESTNNR == "TA01") 
# Uebung "Faust-Ballen-Thilini" wurde an insgesamt an 3 Tagen angeboten, Planer 208mal ausgefuellt und davon insgesamt 34 mal die Uebung uebersprungen

Achtsamkeit_false_TA01$AI12_01 # angefuehrte Gruende, warum Uebung uebersprungen wurde

table(Achtsamkeit_false_TA01$SERIAL) # Haeufigkeiten, wie oft die Uebung von den jeweiligen Personen uebersprungen wurde


# * 12.2 TA02 - Faust-Ballen-Jakob (ca. 120s) ----
Achtsamkeit_false_TA02 <- filter(Achtsamkeit_ohne_Dropout, TIME013 < 120, QUESTNNR == "TA02")
# Uebung "Faust-Ballen-Jakob" wurde an insgesamt an 2 Tagen angeboten, Planer 142mal ausgefuellt und davon insgesamt 30 mal die Uebung uebersprungen

Achtsamkeit_false_TA02$AI12_01 # angefuehrte Gruende, warum Uebung uebersprungen wurde

table(Achtsamkeit_false_TA02$SERIAL) # insgesamt haben 23 Personen die ?bung einmal ?bersprungen; 7 Personen zweimal


# * 12.3 TA03 - Unsere Gedanken (240s) ----
Achtsamkeit_false_TA03 <- filter(Achtsamkeit_ohne_Dropout, TIME013 < 240, QUESTNNR == "TA03")
# Uebung "Unsere Gedanken" wurde an insgesamt an 5 Tagen angeboten, Planer 353mal ausgefuellt und davon insgesamt 47 mal die Uebung uebersprungen

Achtsamkeit_false_TA03$AI12_01 # angefuehrte Gruende, warum Uebung uebersprungen wurde

table(Achtsamkeit_false_TA03$SERIAL) # Haeufigkeiten, wie oft die Uebung von den jeweiligen Personen uebersprungen wurde


# * 12.4 TA04 - Erfahrung beobachten (240s) ----
Achtsamkeit_false_TA04 <- filter(Achtsamkeit_ohne_Dropout, TIME013 < 240, QUESTNNR == "TA04")
# Uebung "Erfahrung beobachten" wurde an insgesamt an 5 Tagen angeboten, Planer 350mal ausgefuellt und davon insgesamt 46 mal die Uebung uebersprungen

Achtsamkeit_false_TA04$AI12_01 # angefuehrte Gruende, warum Uebung uebersprungen wurde

table(Achtsamkeit_false_TA04$SERIAL) # Haeufigkeiten, wie oft die Uebung von den jeweiligen Personen uebersprungen wurde


# * 12.5 TA05 - Seated-Mantra-Jakob (ca. 139s) ----
Achtsamkeit_false_TA05 <- filter(Achtsamkeit_ohne_Dropout, TIME013 < 139, QUESTNNR == "TA05")
# Uebung "Seated-Mantra-Jakob" wurde an insgesamt an 3 Tagen angeboten, Planer 211mal ausgefuellt und davon insgesamt 30 mal die Uebung uebersprungen

Achtsamkeit_false_TA05$AI12_01 # angefuehrte Gruende, warum Uebung uebersprungen wurde

table(Achtsamkeit_false_TA05$SERIAL) # Haeufigkeiten, wie oft die Uebung von den jeweiligen Personen uebersprungen wurde


# * 12.6 TA06 - Seated-Mantra-Thilini (ca. 180s) ----
Achtsamkeit_false_TA06 <- filter(Achtsamkeit_ohne_Dropout, TIME013 < 180, QUESTNNR == "TA06")
# Uebung "Seated-Mantra-Thilini" wurde an insgesamt an 2 Tagen angeboten, Planer 145mal ausgefuellt und davon insgesamt 37 mal die Uebung uebersprungen

Achtsamkeit_false_TA06$AI12_01 # angefuehrte Gruende, warum Uebung uebersprungen wurde

table(Achtsamkeit_false_TA06$SERIAL) # Haeufigkeiten, wie oft die Uebung von den jeweiligen Personen uebersprungen wurde


# * 12.7 TA07 - Bewusstes Atmen (240s) ----
Achtsamkeit_false_TA07 <- filter(Achtsamkeit_ohne_Dropout, TIME013 < 240, QUESTNNR == "TA07")
# Uebung "Bewusstes Atmen" wurde an insgesamt an 6 Tagen angeboten, Planer 414mal ausgefuellt und davon insgesamt 58 mal die Uebung uebersprungen

Achtsamkeit_false_TA07$AI12_01 # angefuehrte Gruende, warum Uebung uebersprungen wurde

table(Achtsamkeit_false_TA07$SERIAL) # Haeufigkeiten, wie oft die Uebung von den jeweiligen Personen uebersprungen wurde


# * 12.8 TA08 - OMP-Jakob (ca. 575s) ----
Achtsamkeit_false_TA08 <- filter(Achtsamkeit_ohne_Dropout, TIME013 < 575, QUESTNNR == "TA08")
# Uebung "OMP-Jakob" wurde an insgesamt an 2 Tagen angeboten, Planer 132mal ausgefuellt und davon insgesamt 38 mal die Uebung uebersprungen

Achtsamkeit_false_TA08$AI12_01 # angefuehrte Gruende, warum Uebung uebersprungen wurde

table(Achtsamkeit_false_TA08$SERIAL) # Haeufigkeiten, wie oft die Uebung von den jeweiligen Personen uebersprungen wurde


# * 12.9 TA09 - OMP-Thilini (ca.440s) ----
Achtsamkeit_false_TA09 <- filter(Achtsamkeit_ohne_Dropout, TIME013 < 440, QUESTNNR == "TA09")
# Uebung "OMP-Thilini" wurde an insgesamt an 2 Tagen angeboten, Planer 136mal ausgefuellt und davon insgesamt 45 mal die Uebung uebersprungen

Achtsamkeit_false_TA09$AI12_01 # angefuehrte Gruende, warum Uebung uebersprungen wurde

table(Achtsamkeit_false_TA09$SERIAL) # Haeufigkeiten, wie oft die Uebung von den jeweiligen Personen uebersprungen wurde


# * 12.10 TA10 - Wahrnehmung Atem (240s) ----
Achtsamkeit_false_TA10 <- filter(Achtsamkeit_ohne_Dropout, TIME013 < 240, QUESTNNR == "TA10")
# Uebung "OMP-Thilini" wurde an insgesamt an 5 Tagen angeboten, Planer 347mal ausgefuellt und davon insgesamt 64 mal die Uebung uebersprungen

Achtsamkeit_false_TA10$AI12_01 # angefuehrte Gruende, warum Uebung uebersprungen wurde

table(Achtsamkeit_false_TA10$SERIAL) # Haeufigkeiten, wie oft die Uebung von den jeweiligen Personen uebersprungen wurde


# 13 Auswertung T3 ----

# * 13.1 Zufriedenheit mit den Noten ----

table(D_T3$Feedback)

# Angaben Datum Prüfung 1
table(D_T3$LP02_01)

# Angaben Datum Prüfung 2
table(D_T3$LP06_01)

# Angaben Datum Prüfung 3
table(D_T3$LP10_01)

## Angaben wie oft, welches Prüfungsdatum angegeben wurde, schwer zu interpretieren ... 
# Viele Personen haben mehrmals das gleiche Datum angegeben, anstatt die Felder einfach frei zu lassen.

# Zufriedenheit mit Note 1
describeBy(D_T3$PT03_01, D_T3$Feedback, mat = TRUE)

# Zufriedenheit mit Note 2
describeBy(D_T3$PT05_01, D_T3$Feedback, mat = TRUE)

# Zufriedenheit mit Note 3
describeBy(D_T3$PT07_01, D_T3$Feedback, mat = TRUE)

# Zufriedenheit mit Note gesamt
D_T3$ZufN_gesamt <- (D_T3$PT03_01 + D_T3$PT05_01 + D_T3$PT07_01)/ 3 # neue Variable: alle drei Noten zusammengerechnet & gemittelt

describeBy(D_T3$ZufN_gesamt, D_T3$Feedback, mat = TRUE)

leveneTest(D_T3$ZufN_gesamt, D_T3$Feedback.Faktor) # n.s. => Varianzhomogenitaet gegeben

ZufN_gesamt_differences <- t.test(D_T3$ZufN_gesamt ~ D_T3$Feedback, var.equal = TRUE)
ZufN_gesamt_differences # n.s. p = .730

ci.smd(ncp = 0.34638,
       n.1 = 45, n.2 = 69) # Cohens d = .07 

# * 13.2 erwartete vs. finale Noten ----

D_T1T3_final_2 <- D_T1T3_final %>% group_by(SERIAL) %>% filter(n()>1) # nur die Personen, die T1 & T3 ausgefüllt haben

table(D_T1T3_final_2$TIME == "T3", D_T1T3_final_2$Feedback)

addmargins(table(D_T1T3_final_2$TIME == "T3", D_T1T3_final_2$Feedback)) # 120 Personen haben T1 & T3 ausgefüllt

# erwartete Note 1
describeBy(D_T1T3_final_2$LP05, D_T1T3_final_2$Feedback, mat = TRUE)
# finale Note 1
describeBy(D_T1T3_final_2$PT02, D_T1T3_final_2$Feedback, mat = TRUE)


# erwartete Note 2
describeBy(D_T1T3_final_2$LP09, D_T1T3_final_2$Feedback, mat = TRUE)
# tatsächliche Note 2
describeBy(D_T1T3_final_2$PT04, D_T1T3_final_2$Feedback, mat = TRUE)


# erwartete Note 3
describeBy(D_T1T3_final_2$LP13, D_T1T3_final_2$Feedback, mat = TRUE)
# tatsächliche Note 3
describeBy(D_T1T3_final_2$PT06, D_T1T3_final_2$Feedback, mat = TRUE)

# erwartete Noten gesamt
D_T1T3_final_2$erwN_gesamt <- (D_T1T3_final_2$LP05 + D_T1T3_final_2$LP09 + D_T1T3_final_2$LP13)/ 3

describeBy(D_T1T3_final_2$erwN_gesamt, D_T1T3_final_2$Feedback, mat = TRUE)

# tatsächliche Noten gesamt
D_T1T3_final_2$tatsN_gesamt <- (D_T1T3_final_2$PT02 + D_T1T3_final_2$PT04 + D_T1T3_final_2$PT06)/ 3

describeBy(D_T1T3_final_2$tatsN_gesamt, D_T1T3_final_2$Feedback, mat = TRUE)


leveneTest(D_T1T3_final_2$tatsN_gesamt, D_T1T3_final_2$Feedback.Faktor) # n.s. => Varianzhomogenitaet gegeben

tatsN_gesamt_differences <- t.test(D_T1T3_final_2$tatsN_gesamt ~ D_T1T3_final_2$Feedback, var.equal = TRUE)
tatsN_gesamt_differences # n.s. p = .394

ci.smd(ncp = -0.85512,
       n.1 = 45, n.2 = 68) # Cohens d = -.16 



## Problem: Berechnung der neuen Variablen für die DIfferenz zwischen erwarteter und tatsächlich Note klappt nicht
## vmtl. müssten Daten dafür im Wide-Format vorliegen.
## Die Transformation ist mir bisher aber nicht gelungen. 

## vmtl. müsste man auch irgendwie noch ablgleichen, ob die Leute bei T1 & T3 auch die gleichen Prüfungen 1, 2 & 3 angegeben haben?!
## Sonst berechnet man ja Differenzwerte, die gar nicht zusammengehören, weil es sich um unterschiedliche Prüfungen handelt?!





