Alle_Daten <- read.csv2(file.choose())

Bedingung <- read.csv2(file.choose())

Bedingung <- Seriennummer_und_Bedingung

# Kodierung fehlender Werte

Alle_Daten[Alle_Daten == -9 | Alle_Daten == -1 | Alle_Daten == ""] <- NA

library("dplyr")

# neue Variable: GROUP (Feedback = 1; Achtsamkeit = 0)

Alle_Daten_neu <- left_join(Alle_Daten, Bedingung, by = "SERIAL")

AD <- Alle_Daten_neu

# Änderung von TIME character zu factor (TIME_neu)

class(AD$TIME)

AD$TIME_neu <- as.factor(AD$TIME)

class(AD$TIME_neu)

# Skalenbildung T1

AD$goalt1 <- (AD$T102_01 + AD$T102_02 + AD$T102_03 + AD$T102_04)/4

AD$mott1 <- (AD$T103_01 + AD$T103_02 + AD$T103_03)/3    

AD$volt1 <- (AD$T104_01 + AD$T104_02 + AD$T104_03 + AD$T104_04)/4

AD$reft1 <- (AD$T105_01 + AD$T105_02 + AD$T105_03)/3

AD$plant1 <- (AD$T106_01 + AD$T106_02 + AD$T106_03)/3

AD$prot1 <- (AD$T107_01 + AD$T107_02 + AD$T107_03 + AD$T107_04 + AD$T107_05 + AD$T107_06 + AD$T107_07)/7

AD$set1 <- (AD$T108_01 + AD$T108_02 + AD$T108_03 + AD$T108_04 + AD$T108_05 + AD$T108_06 + AD$T108_07+ AD$T108_08 + AD$T108_09)/9

#Skalenbildung T2

AD$goalt2 <- (AD$T202_01 + AD$T202_02 + AD$T202_03 + AD$T202_04)/4

AD$mott2 <- (AD$T203_01 + AD$T203_02 + AD$T203_03)/3    

AD$volt2 <- (AD$T204_01 + AD$T204_02 + AD$T204_03 + AD$T204_04)/4

AD$reft2 <- (AD$T205_01 + AD$T205_02 + AD$T205_03)/3

AD$plant2 <- (AD$T206_01 + AD$T206_02 + AD$T206_03)/3

AD$prot2 <- (AD$T207_01 + AD$T207_02 + AD$T207_03 + AD$T207_04 + AD$T207_05 + AD$T207_06 + AD$T207_07)/7

AD$set2 <- (AD$T208_01 + AD$T208_02 + AD$T208_03 + AD$T208_04 + AD$T208_05 + AD$T208_06 + AD$T208_07+ AD$T208_08 + AD$T208_09)/9

#ezANOVA

library(reshape2)
library(Hmisc)

aggdata_goal <- melt(aggdata,id.vars=c("SERIAL", "GROUP"), measure.vars=c("goalt1", "goalt2"), variable.name="TIME_neu", value.name="goal")
