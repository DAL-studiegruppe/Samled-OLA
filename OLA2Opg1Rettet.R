###############
### OPG 1.1 ###
###############

# Load devtools og dkstat
library("devtools")
library(dkstat)
# 
library(dplyr)
library(stringr)
library(tidyr)

# dst_search er en funktion som gør det muligt at søge efter tabeller baseret på søgeord
search<- dst_search(string = "postnr", field = "id")
print(search)

# Vi loader tabellen "POSTNR1"
postnummer<- dst_meta(table = "POSTNR1", lang = "da")

# Udvælg alle postnumre og alle tidsperioder
postnr_metafilters<- list(
  PNR20= "*",
  Tid= "*"
)
#Load data med valgte filtre
POSTNRdata<- dst_get_data(table= "POSTNR1", query= postnr_metafilters, lang="da")
View(POSTNRdata)

# lav en sum for hver unik by
POSTNRdata_summarized <- POSTNRdata %>%
  group_by(PNR20, TID) %>%
  summarise(value = sum(as.numeric(value), na.rm = TRUE), .groups = 'drop')

#Slet rækker med hele landet og ukendt
POSTNRdata_summarized<- POSTNRdata_summarized[-c(15931:15960),]

# Split postnummer og by
df1 <- POSTNRdata_summarized %>%
  # Brug separate() til at splitte 'by' i to kolonner ved første mellemrum
  separate(PNR20, into = c("Postnummer", "by"), sep = " ", extra = "merge", fill = "right")



###############
### OPG 1.2 ###
###############

#Fjern bykolonne for kun at have 3 variabler til pivot fuktionen
df1<- df1[,-2]
dfx<- pivot_wider(names_from = Postnummer, values_from = value, data = df1)
# fjern år fra rækker så vi kun har 2024
POSTNR_cat <- dfx[-c(1:14),]

# lav ny dataframe, så vi hele tiden har den originale dataframe
POSTNR_cat1 <- POSTNR_cat

# Transpose dataframe, så den bliver vendt
POSTNR_cat1 <- t(POSTNR_cat1)

# Fjern første række og behold som dataframe
POSTNR_cat1 <- as.data.frame(POSTNR_cat1)
POSTNR_cat1 <- POSTNR_cat1[POSTNR_cat1[, 1] != "2024-01-01", , drop = FALSE]

# Navngiv kolonne
names(POSTNR_cat1)[1] <- "Indbyggertal"



# Først flytter vi rækkeindekset til en kolonne
library(tidyverse)
POSTNR_cat1 <- rownames_to_column(POSTNR_cat1, var = "postnr")


# Brug for loop funktionen til at kontrollere indbygger tallet og tildele det en kategori
# Opret en tom kolonne til bykategorien
POSTNR_cat1$bycat <- NA  

# lav tallene til numeric, ellers virker koden ikke
POSTNR_cat1$Indbyggertal <- as.numeric(POSTNR_cat1$Indbyggertal)

# For-loop til at tildele bykategori baseret på indbyggertal
#for (i in 1:nrow(POSTNR_cat1)) {
 # if (POSTNR_cat1$Indbyggertal[i] <= 250) {
  #  POSTNR_cat1$bycat[i] <- "landsby"
  #} else if (POSTNR_cat1$Indbyggertal[i] > 250 & POSTNR_cat1$Indbyggertal[i] <= 1000) {
  #  POSTNR_cat1$bycat[i] <- "lille by"
  #} else if (POSTNR_cat1$Indbyggertal[i] > 1000 & POSTNR_cat1$Indbyggertal[i] <= 2500) {
  #  POSTNR_cat1$bycat[i] <- "almindelig by"
  #} else if (POSTNR_cat1$Indbyggertal[i] > 2500 & POSTNR_cat1$Indbyggertal[i] <= 10000) {
  #  POSTNR_cat1$bycat[i] <- "større by"
  #} else if (POSTNR_cat1$Indbyggertal[i] > 10000) {
  #  POSTNR_cat1$bycat[i] <- "storby"
  #} else {
  #  POSTNR_cat1$bycat[i] <- NA  # Håndterer eventuelle manglende værdier
  #}
#}

library(dplyr)

POSTNR_cat1 <- POSTNR_cat1 %>%
  mutate(bycat = case_when(
    Indbyggertal <= 250 ~ "landet",
    Indbyggertal > 250 & Indbyggertal <= 1000 ~ "landsby",
    Indbyggertal > 1000 & Indbyggertal <= 2500 ~ "lille by",
    Indbyggertal > 2500 & Indbyggertal <= 10000 ~ "almindelig by",
    Indbyggertal > 10000 & Indbyggertal <= 50000 ~ "større by",
    Indbyggertal > 50000 ~ "storby",
    TRUE ~ NA_character_  # Håndterer eventuelle manglende værdier
  ))



#############
# OPG 1.3 ###
#############

# Hent boligsiden data som RDS

boligsiden_clean_r <- readRDS("boligsiden_clean_r.rds") # Filen skal ligge i din lokale mappe

# Merge de 2 datasæt
merged_df <- merge(boligsiden_clean_r, POSTNR_cat1, by = "postnr")

# Tilpas kolonner så de passer med opgaven
merged_df <- merged_df[, c(1, 2, 13, 14, 3:12)]




###############
### OPG 1.4 ###
###############

# Lav plot
library(ggplot2)

# fjerne outliers i kvmpris på over 50.000 kr
boxplot(merged_df$kvmpris)

# Beregn gennemsnittet af kvmpris pr. bykategori
library(dplyr)
merged_df_avg <- merged_df %>%
  group_by(bycat) %>%
  summarise(mean_kvmpris = mean(kvmpris, na.rm = TRUE))

# Plot gennemsnits-kvmpris
ggplot(merged_df_avg, aes(x = bycat, y = mean_kvmpris, fill = bycat)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Bykat", y = "Gennemsnitlig kvmpris i tusinder", title = "Den gnmsnitlige kvmpris er højest på landet") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")




# Hvis der skal fjernes outliers
merged_df_OL <- merged_df[merged_df$kvmpris < 200.000,]

# Beregn gennemsnittet af kvmpris pr. bykategori
library(dplyr)
merged_df_avg <- merged_df_OL %>%
  group_by(bycat) %>%
  summarise(mean_kvmpris = mean(kvmpris, na.rm = TRUE))

# Plot gennemsnits-kvmpris
ggplot(merged_df_avg, aes(x = bycat, y = mean_kvmpris, fill = bycat)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Bykat", y = "Gennemsnitlig kvmpris i tusinder", title = "Den gnmsnitlige kvmpris er højest i større byer") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")
