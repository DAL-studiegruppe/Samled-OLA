library(tidyr)
library(devtools)
library(dkstat)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggcorrplot)

AlkoholGr <- dst_meta(table = "FU02")
AlkoholGrLi <- list(
  KONSUMGRP=c("02.1.1.1 Spiritus og likør", 
              "02.1.1.2 Alkoholiske læskedrikke",
              "02.1.2.1 Vin af druer",
              "02.1.2.2 Vin af andre frugter",
              "02.1.2.3 Hedvin",
              "02.1.2.4 Vinbaserede drikkevarer og alkoholfri vin",
              "02.1.3.1 Pilsnerøl, guldøl",
              "02.1.3.2 Andre alkoholholdige øl",
              "02.1.3.3 Øl med lavt alkoholindhold og alkoholfri øl",
              "02.1.3.4 Øl-baserede drikkevarer"
              ),
  PRISENHED="Faste priser",
  Tid="*"
)

AlkoholGrDf <- dst_get_data(table = "FU02",query = AlkoholGrLi, lang="da")

# Fjerne faste priser
AlkoholGrDf <- AlkoholGrDf[,-2]

# Gør navne pæne uden numre
AlkoholGrDf$KONSUMGRP <- gsub("[0-9.]+", "", AlkoholGrDf$KONSUMGRP)
AlkoholGrDf$KONSUMGRP <- trimws(AlkoholGrDf$KONSUMGRP)  # Fjern evt. unødvendige mellemrum

# Plot med alle grupper
ggplot(AlkoholGrDf, aes(x = TID, y = value, color = KONSUMGRP)) +
  geom_line(linewidth = 0.5) +  # Tegner linjer for hver kategori
  geom_point() +
  labs(title = "Danskernes fortrunke alkohol",
       x = "Tid",
       y = "Værdi",
       color = "Alkoholgruppe") +
  theme_minimal() +  # Et minimalistisk tema for et rent look
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Roter x-aksen for bedre læsbarhed


# Plot med 4 fremhævede grupper
ggplot(AlkoholGrDf, aes(x = TID, y = value, color = KONSUMGRP)) +
  geom_line(size = 0.5) +  # Tegner linjer for hver kategori
  geom_point() +
  scale_color_manual(values = c("Pilsnerøl, guldøl" = "blue", 
                                "Vin af druer" = "green", 
                                "Spiritus og likør" = "red", 
                                "Øl med lavt alkoholindhold og alkoholfri øl" = "purple",
                                "Other" = "gray")) +  # Sæt farver for hver kategori
  labs(title = "Danskerne har fortrukken alkohol",
       x = "Tid",
       y = "Værdi",
       color = "Alkoholgruppe") +
  theme_minimal() +  # Et minimalistisk tema for et rent look
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Roter x-aksen for bedre læsbarhed


# PIvot wider for mere overskuelig dataframe
AlkoholGrDfWi <- pivot_wider(data = AlkoholGrDf, names_from = "KONSUMGRP", values_from = "value")

# Fjern årstal
AlkoholGrDfWi <- AlkoholGrDfWi[-c(1:6),]





library(dplyr)

# Subset data frame for de ønskede alkoholgrupper
AlkoholGrDf_subset <- AlkoholGrDf %>%
  filter(KONSUMGRP %in% c("Pilsnerøl, guldøl", 
                          "Vin af druer", 
                          "Spiritus og likør", 
                          "Øl med lavt alkoholindhold og alkoholfri øl")) %>%
  filter(TID >= as.Date("2000-01-01") & TID <= as.Date("2022-12-31"))

AlkoholGrDf_subset <- as.data.frame(AlkoholGrDf_subset)


# Plot med 4 fremhævede grupper
ggplot(AlkoholGrDf_subset, aes(x = TID, y = value, color = KONSUMGRP)) +
  geom_line(size = 0.5) +  # Tegner linjer for hver kategori
  geom_point() +
  labs(title = "4 overordnede alkoholgrupper",
       x = "Tid",
       y = "Værdi",
       color = "Alkoholgruppe") +
  theme_minimal() +  # Et minimalistisk tema for et rent look
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Roter x-aksen for bedre læsbarhed




# 4.2
AlkoholGrDf_4.2 <- AlkoholGrDf %>%
  filter(KONSUMGRP %in% c("Pilsnerøl, guldøl", 
                          "Vin af druer", 
                          "Spiritus og likør")) %>%
  filter(TID >= as.Date("2000-01-01") & TID <= as.Date("2022-12-31"))

# Indlæs nødvendige pakker
library(tidyr)
library(dplyr)
library(ggcorrplot)

# Transformer data
AlkoholGrDf_4.2 <- AlkoholGrDf_4.2 %>%
  pivot_wider(names_from = KONSUMGRP, values_from = value)

# Beregn korrelationsmatrix
korrelationsmatrix <- cor(AlkoholGrDf_4.2[-1])  # Ekskluder TID-kolonnen

# Print korrelationsmatrixen
print(korrelationsmatrix)

# Visualisering af korrelationsmatrixen og plot af korrelationsmatrixen
ggcorrplot(korrelationsmatrix, method = "square",lab = TRUE, type = "lower")
















