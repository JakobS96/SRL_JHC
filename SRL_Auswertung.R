Alle_Daten <- read.csv2(file.choose())

Bedingung <- read.csv2(file.choose())

# Kodierung fehlender Werte

Alle_Daten[Alle_Daten == -9 | Alle_Daten == -1 | Alle_Daten == ""] <- NA

library("dplyr")

# neue Variable: GROUP (Feedback = 1; Achtsamkeit = 0)

Alle_Daten_neu <- left_join(Alle_Daten, Bedingung, by = "SERIAL")