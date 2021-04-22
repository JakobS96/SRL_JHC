
# Laden der Pakete
library(car)
library(dplyr)
library(EMAtools)
library(ez)
library(GPArotation)
library(lme4)
library(lavaan)
library(nlme)
library(psych)
library(reshape)
library(reshape2)
library(semTools)


# Laden des Datensatzes und der Datei mit den Zusatzvariablen
Alle_Daten <- read.csv2(file.choose()) # Datei: Alle_Daten_Stand 15.04.

Zusatzvariablen <- read.csv2(file.choose()) # Datei: Zusatzvariablen_neu

# Kodierung fehlender Werte

Alle_Daten[Alle_Daten == -9 | Alle_Daten == -1 | Alle_Daten == ""] <- NA

# neue ZUsatzvariablen: 
    # 1) Feedback (Feedback = 1; Achtsamkeit = 0)
    # 2) FinishT1 (T1 Fragebogen ausgefüllt = 1; T1 Fragebogen nicht ausgefüllt = 0)
    # 3) FinishT2 (T2 Fragebogen ausgefüllt = 1; T2 Fragebogen nicht ausgefüllt = 0)
    # 4) Finish18 (mehr 17 Tagen Lernplaner ausgefüllt = 1; weniger als 17 Tage Lernplaner ausgefüllt = 0)

Alle_Daten_neu <- left_join(Alle_Daten, Zusatzvariablen, by = "SERIAL")

AD <- Alle_Daten_neu

# Berechnen einer neuen Variable: dropout
AD <- mutate(AD, dropout = ifelse(FinishT1 == 1 & FinishT2 == 1 & Finish18 == 1 & SERIAL != "NA", 0,1))

AD$dropout[is.na(AD$dropout)] <- 1

describeBy(data.frame(AD$Feedback), group = AD$dropout) # 569 Zeilen wurden als Dropout identifiziert.


# Dropout raus filtern:
AD_ohne_Dropout <- filter(AD, FinishT1 == 1 & FinishT2 == 1 & Finish18 == 1 & SERIAL != "NA") # Es werden 574 Fälle raus gefiltert (5x SERIAL = NA)


# Deskriptive Analysen 

table(AD_ohne_Dropout$DD03, AD_ohne_Dropout$Feedback) # H?ufigkeitsverteilung Geschlecht aufgeteilt nach Bedingung

describeBy(AD_ohne_Dropout$DD02_01, AD_ohne_Dropout$Feedback, mat = TRUE) # Alter aufgeteilt nach Bedingung

describeBy(AD_ohne_Dropout$DD10_01, AD_ohne_Dropout$Feedback, mat = TRUE) # Vorwissen Thema Achtsamkeit
describeBy(AD_ohne_Dropout$DD10_08, AD_ohne_Dropout$Feedback, mat = TRUE) # Vorwissen Thema Feedback


# Reliabilitätsanalysen T1: Berechnung McDonalds Omega

omegagoalt1 <- AD_ohne_Dropout[c("T102_01", "T102_02", "T102_03", "T102_04")]
omega(omegagoalt1) # alpha = .68; omega = .76

omegamott1 <- AD_ohne_Dropout[c("T103_01", "T103_02", "T103_03")]
omega(omegamott1) # alpha = .79; omega = .82

omegavolt1 <- AD_ohne_Dropout[c("T104_01", "T104_02", "T104_03","T104_04")]
omega(omegavolt1) # alpha = .76; omega = .78

omegareft1 <- AD_ohne_Dropout[c("T105_01", "T105_02", "T105_03")]
omega(omegareft1) # alpha = .71; omega = .74

omegaplant1 <- AD_ohne_Dropout[c("T106_01", "T106_02", "T106_03")]
omega(omegaplant1) # alpha = .86; omega = .88

omegaprot1 <- AD_ohne_Dropout[c("T107_01", "T107_02", "T107_03","T107_04","T107_05","T107_06","T107_07")]
omega(omegaprot1) # alpha = .95; omega = .96

omegaset1 <- AD_ohne_Dropout[c("T108_01", "T108_02", "T108_03","T108_04","T108_05","T108_06","T108_07","T108_08","T108_09")]
omega(omegaset1) # alpha = .86; omega = .89

# Reliabilitätsanalyse T2

omegagoalt2 <- AD_ohne_Dropout[c("T202_01", "T202_02", "T202_03", "T202_04")]
omega(omegagoalt2) # alpha = .68; omega = .75

omegamott2 <- AD_ohne_Dropout[c("T203_01", "T203_02", "T203_03")]
omega(omegamott2) # alpha = .83; omega = .84

omegavolt2 <- AD_ohne_Dropout[c("T204_01", "T204_02", "T204_03","T204_04")]
omega(omegavolt2) # alpha = .85; omega = .87

omegareft2 <- AD_ohne_Dropout[c("T205_01", "T205_02", "T205_03")]
omega(omegareft2) # alpha = .77; omega = .79

omegaplant2 <- AD_ohne_Dropout[c("T206_01", "T206_02", "T206_03")]
omega(omegaplant2) # alpha = .85; omega = .85

omegaprot2 <- AD_ohne_Dropout[c("T207_01", "T207_02", "T207_03","T207_04","T207_05","T207_06","T207_07")]
omega(omegaprot2) # alpha = .92; omega = .94

omegaset2 <- AD_ohne_Dropout[c("T208_01", "T208_02", "T208_03","T208_04","T208_05","T208_06","T208_07","T208_08","T208_09")]
omega(omegaset2) # alpha = .88; omega = .91


# Skalenbildung T1

AD$goalt1 <- (AD$T102_01 + AD$T102_02 + AD$T102_03 + AD$T102_04)/4

AD$mott1 <- (AD$T103_01 + AD$T103_02 + AD$T103_03)/3    

AD$volt1 <- (AD$T104_01 + AD$T104_02 + AD$T104_03 + AD$T104_04)/4

AD$reft1 <- (AD$T105_01 + AD$T105_02 + AD$T105_03)/3

AD$plant1 <- (AD$T106_01 + AD$T106_02 + AD$T106_03)/3

AD$prot1 <- (AD$T107_01 + AD$T107_02 + AD$T107_03 + AD$T107_04 + AD$T107_05 + AD$T107_06 + AD$T107_07)/7

AD$set1 <- (AD$T108_01 + AD$T108_02 + AD$T108_03 + AD$T108_04 + AD$T108_05 + AD$T108_06 + AD$T108_07+ AD$T108_08 + AD$T108_09)/9

# Skalenbildung T2

AD$goalt2 <- (AD$T202_01 + AD$T202_02 + AD$T202_03 + AD$T202_04)/4

AD$mott2 <- (AD$T203_01 + AD$T203_02 + AD$T203_03)/3    

AD$volt2 <- (AD$T204_01 + AD$T204_02 + AD$T204_03 + AD$T204_04)/4

AD$reft2 <- (AD$T205_01 + AD$T205_02 + AD$T205_03)/3

AD$plant2 <- (AD$T206_01 + AD$T206_02 + AD$T206_03)/3

AD$prot2 <- (AD$T207_01 + AD$T207_02 + AD$T207_03 + AD$T207_04 + AD$T207_05 + AD$T207_06 + AD$T207_07)/7

AD$set2 <- (AD$T208_01 + AD$T208_02 + AD$T208_03 + AD$T208_04 + AD$T208_05 + AD$T208_06 + AD$T208_07+ AD$T208_08 + AD$T208_09)/9


# t.tests für die Unterschiede zwischen Dropout (1) und kein Dropout (0)

goalt1_drop <- t.test(AD$goalt1 ~ AD$dropout, var.equal = TRUE)
goalt1_drop # n.s.

plant1_drop <- t.test(AD$plant1 ~ AD$dropout, var.equal = TRUE)
plant1_drop # n.s.

mott1_drop <- t.test(AD$mott1 ~ AD$dropout, var.equal = TRUE)
mott1_drop # n. s.

set1_drop <- t.test(AD$set1 ~ AD$dropout, var.equal = TRUE)
set1_drop # n.s.

prot1_drop <- t.test(AD$prot1 ~ AD$dropout, var.equal = TRUE)
prot1_drop # n.s.

volt1_drop <- t.test(AD$volt1 ~ AD$dropout, var.equal = TRUE)
volt1_drop # n.s.

reft1_drop <- t.test(AD$reft1 ~ AD$dropout, var.equal = TRUE)
reft1_drop # n.s. 


# t.tests für die Unterschiede zwischen LPF (Feedback) und LPA (Achtsamkeit) bei T1

goalt1_differences <- t.test(AD_ohne_Dropout$goalt1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
goalt1_differences # n.s. p = .07

plant1_differences <- t.test(AD_ohne_Dropout$plant1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
plant1_differences # n.s. p = .13

mott1_differences <- t.test(AD_ohne_Dropout$mott1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
mott1_differences # n.s. p = .13

set1_differences <- t.test(AD_ohne_Dropout$set1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
set1_differences # n.s. p = 86

prot1_differences <- t.test(AD_ohne_Dropout$prot1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
prot1_differences # n.s. p = .99

volt1_differences <- t.test(AD_ohne_Dropout$volt1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
volt1_differences # n.s. P = .72

reft1_differences <- t.test(AD_ohne_Dropout$reft1 ~ AD_ohne_Dropout$Feedback, var.equal = TRUE)
reft1_differences # n.s. P = .58


# Subsets bilden (T1)

D_T1 <- subset(AD_ohne_Dropout, TIME=="T1")

# Subsets bilden (T2)

D_T2 <- subset(AD_ohne_Dropout, TIME=="T2")

# Subsets bilden (T1 & T2)

D_T1T2 <- rbind(D_T1, D_T2) # DZ883544 muss raus gefiltert werden, da im T2 Fragebogen nicht konzentriert 
D_T1T2 <- filter(D_T1T2, SERIAL != "DZ883544")

# Subsets bilden (T1 & LP_T1-LP_T35)

D_T1LP <- filter(AD_ohne_Dropout, TIME != "T2", TIME != "T3") # hier müsste man vermutlich noch die Einträge der Tage raus filtern, an denen keine studienrelevanten Tätigkeiten absolviert wurden (TE16: 1 = ja, 2 = nein)
D_T1LP <- filter(D_T1LP, TE16 != 2)

# Korrelation zwischen Anzahl Lernplaner und Subset T1?


######## T1-T2 ANOVAs #######


# lme für Zielsetzung

aggdata_long_GOAL <- melt(D_T1T2,id.vars=c("SERIAL", "Feedback"), measure.vars=c("goalt1", "goalt2"), variable.name="TIME",value.name="GOAL", na.rm = TRUE)

aggdata_long_GOAL <- aggdata_long_GOAL %>% group_by(SERIAL) %>% filter(n()>1) # Filtern einzelner Seriennummern, die nur T1 oder nur T2 ausgefüllt haben

table(aggdata_long_GOAL$TIME, aggdata_long_GOAL$Feedback) # Ersatz für die describeBy Funktion

baseline_goal <- lme(GOAL ~ 1, random = ~1|TIME/Feedback, data = aggdata_long_GOAL, method = "ML")
goal <- lme(GOAL~TIME*Feedback, random=~TIME|SERIAL, data=aggdata_long_GOAL, method = "ML")

anova(baseline_goal)
anova(goal) # Feedback signifikant (p < .001, d = .31)
anova(baseline_goal, goal)

lme.dscore(goal,data=aggdata_long_GOAL,type="nlme")

summary(goal) # Warum werden die Koeffizienten für Feedback beim summary Befehl nicht signifikant, obwohl der anova Befehl, z.B. anova(goal) signifikante Ergebnisse anzeigt?


# lme für Planung

aggdata_long_PLAN <- melt(D_T1T2,id.vars=c("SERIAL", "Feedback"), measure.vars=c("plant1", "plant2"), variable.name="TIME",value.name="PLAN", na.rm = TRUE)

aggdata_long_PLAN <- aggdata_long_PLAN %>% group_by(SERIAL) %>% filter(n()>1)

table(aggdata_long_PLAN$TIME, aggdata_long_PLAN$Feedback)

baseline_plan <- lme(PLAN ~ 1, random = ~1|TIME/Feedback, data = aggdata_long_PLAN, method = "ML")
plan <- lme(PLAN~TIME*Feedback, random=~TIME|SERIAL, data=aggdata_long_PLAN, method = "ML")

anova(baseline_plan)
anova(plan) # TIME und FEEDBACK signifikant (TIME: p = .027, d = .20; Feedback: p = .022, d = .28)
anova(baseline_plan, plan)

lme.dscore(plan,data=aggdata_long_PLAN,type="nlme")

summary(plan)


# lme für Selbst-Motivation

aggdata_long_MOT <- melt(D_T1T2,id.vars=c("SERIAL", "Feedback"), measure.vars=c("mott1", "mott2"), variable.name="TIME",value.name="MOT", na.rm = TRUE)

aggdata_long_MOT <- aggdata_long_MOT %>% group_by(SERIAL) %>% filter(n()>1)

table(aggdata_long_MOT$TIME, aggdata_long_MOT$Feedback)

baseline_mot <- lme(MOT ~ 1, random = ~1|TIME/Feedback, data = aggdata_long_MOT, method = "ML")
mot <- lme(MOT~TIME*Feedback, random=~TIME|SERIAL, data=aggdata_long_MOT, method = "ML")

anova(baseline_mot)
anova(mot) # TIME signifikant (p = .009, d = .51)
anova(baseline_mot, mot)

lme.dscore(mot,data=aggdata_long_MOT,type="nlme")

summary(mot)


# lme für Selbstwirksamkeit

aggdata_long_SE <- melt(D_T1T2,id.vars=c("SERIAL", "Feedback"), measure.vars=c("set1", "set2"), variable.name="TIME",value.name="SE", na.rm = TRUE)

aggdata_long_SE <- aggdata_long_SE %>% group_by(SERIAL) %>% filter(n()>1)

table(aggdata_long_SE$TIME, aggdata_long_SE$Feedback)

baseline_se <- lme(SE ~ 1, random = ~1|TIME/Feedback, data = aggdata_long_SE, method = "ML")
se <- lme(SE~TIME*Feedback, random=~TIME|SERIAL, data=aggdata_long_SE, method = "ML")

anova(baseline_se)
anova(se) # TIME signifikant (p = < .001, d = .72)
anova(baseline_se, se)

lme.dscore(se,data=aggdata_long_SE,type="nlme")

summary(se)


# lme für Prokrastination

aggdata_long_PRO <- melt(D_T1T2,id.vars=c("SERIAL", "Feedback"), measure.vars=c("prot1", "prot2"), variable.name="TIME",value.name="PRO", na.rm = TRUE)

aggdata_long_PRO <- aggdata_long_PRO %>% group_by(SERIAL) %>% filter(n()>1)

table(aggdata_long_PRO$TIME, aggdata_long_PRO$Feedback)

baseline_pro <- lme(PRO ~ 1, random = ~1|TIME/Feedback, data = aggdata_long_PRO, method = "ML")
pro <- lme(PRO~TIME*Feedback, random=~TIME|SERIAL, data=aggdata_long_PRO, method = "ML")

anova(baseline_pro)
anova(pro) # TIME signifikant (p < .001, d = -.33)
anova(baseline_pro, pro)

lme.dscore(pro,data=aggdata_long_PRO,type="nlme")

summary(pro)

# lme für Volition

aggdata_long_VOL <- melt(D_T1T2,id.vars=c("SERIAL", "Feedback"), measure.vars=c("volt1", "volt2"), variable.name="TIME",value.name="VOL", na.rm = TRUE)

aggdata_long_VOL <- aggdata_long_VOL %>% group_by(SERIAL) %>% filter(n()>1)

table(aggdata_long_VOL$TIME, aggdata_long_VOL$Feedback)

baseline_vol <- lme(VOL ~ 1, random = ~1|TIME/Feedback, data = aggdata_long_VOL, method = "ML")
vol <- lme(VOL~TIME*Feedback, random=~TIME|SERIAL, data=aggdata_long_VOL, method = "ML")

anova(baseline_vol)
anova(vol) # TIME signifikant (p < .001, d = .32)
anova(baseline_vol, vol)

lme.dscore(vol,data=aggdata_long_VOL,type="nlme")

summary(vol)

# lme für Reflektion

aggdata_long_REF <- melt(D_T1T2,id.vars=c("SERIAL", "Feedback"), measure.vars=c("reft1", "reft2"), variable.name="TIME",value.name="REF", na.rm = TRUE)

aggdata_long_REF <- aggdata_long_REF %>% group_by(SERIAL) %>% filter(n()>1)

table(aggdata_long_REF$TIME, aggdata_long_REF$Feedback)

baseline_ref <- lme(REF ~ 1, random = ~1|TIME/Feedback, data = aggdata_long_REF, method = "ML")
ref <- lme(REF~TIME*Feedback, random=~TIME|SERIAL, data=aggdata_long_REF, method = "ML")

anova(baseline_ref)
anova(ref) # n. s.
anova(baseline_ref, ref)

lme.dscore(ref,data=aggdata_long_REF,type="nlme")

summary(ref)
