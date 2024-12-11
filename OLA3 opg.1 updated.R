library(dkstat)
library(tidyverse)
library(ggplot2)
library(devtools)
library(usethis)
# opg.1 -  Kombinationsalgoritme i R
ialt=dst_get_tables(lang="da")
indi_meta <- dst_meta(table = "FORV1")
indi_list <- list(
  INDIKATOR="*",
  Tid="*"
)
Forbrugindi <- dst_get_data(table = "FORV1",query = indi_list, lang="da")
Forbrugindi <- pivot_wider(Forbrugindi, names_from = INDIKATOR, values_from = value)
opg1.1 <- Forbrugindi %>%
  filter(TID >= as.Date("2000-01-01") & TID <= as.Date("2024-12-31"))

# oprette nye dataframe og lave FORV1 til kvatalvis
opg1.1x <- data.frame(Indikator_dst = numeric(), FØ_idag = numeric(), FØ_år = numeric(), DØ_idag = numeric(), DØ_år = numeric(), ANS = numeric(), ANS12 = numeric(), A_løshed = numeric(), PI = numeric(), PØ = numeric(), A_nu = numeric(), R_12 = numeric(), F_mere = numeric())

for (i in 1:nrow(opg1.1)) {
  
  if (i %% 3 == 0) {
    gennemsnit_indi <- mean(opg1.1$Forbrugertillidsindikatoren[(i-2):i])
    gennemsnit_fø <- mean(opg1.1$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`[(i-2):i])
    gennemsnit_føå <- mean(opg1.1$`Familiens økonomiske  situation om et år, sammenlignet med i dag`[(i-2):i])
    gennemsnit_dø <- mean(opg1.1$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[(i-2):i])
    gennemsnit_døå <- mean(opg1.1$`Danmarks økonomiske situation om et år, sammenlignet med i dag`[(i-2):i])
    gennemsnit_an <- mean(opg1.1$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[(i-2):i])
    gennemsnit_an12 <- mean(opg1.1$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`[(i-2):i])
    gennemsnit_als <- mean(opg1.1$`Arbejdsløsheden om et år, sammenlignet med i dag`[(i-2):i])
    gennemsnit_p <- mean(opg1.1$`Priser i dag, sammenlignet med for et år siden`[(i-2):i])
    gennemsnit_på <- mean(opg1.1$`Priser om et år, sammenlignet med i dag`[(i-2):i])
    gennemsnit_anu <- mean(opg1.1$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation`[(i-2):i])
    gennemsnit_r <- mean(opg1.1$`Regner med at kunne spare op i de kommende 12 måneder`[(i-2):i])
    gennemsnit_f <- mean(opg1.1$`Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener`[(i-2):i])
    
    opg1.1x <- rbind(opg1.1x, data.frame(Indikator_dst = gennemsnit_indi, FØ_idag = gennemsnit_fø, FØ_år = gennemsnit_føå, DØ_idag = gennemsnit_dø, DØ_år = gennemsnit_døå, ANS = gennemsnit_an, ANS12 = gennemsnit_an12, A_løshed = gennemsnit_als, PI = gennemsnit_p, PØ = gennemsnit_på, A_nu = gennemsnit_anu, R_12 = gennemsnit_r, F_mere = gennemsnit_f))
  }
}


# combination af alle 12 spøgsmåler
opg1.1x$PI <- opg1.1x$PI*-1
opg1.1x$A_løshed <- opg1.1x$A_løshed*-1
spørgsmål_indices <- opg1.1x[2:13]
alle_kombinationer <- unlist(lapply(1:12, function(i) combn(spørgsmål_indices, i, simplify = FALSE)), recursive = FALSE)

bbdf <- data.frame(alle_kombinationer)

# regression

# Beregn R² for hver kombination
realvækst <- Real_v$Real_væskt
r_squared_values <- lapply(alle_kombinationer, function(combination) {
  # Beregn gennemsnit for kombinationen
  komb_mean <- rowMeans(as.data.frame(combination), na.rm = TRUE)
  
  # Fit linear model
  model <- lm(realvækst ~ komb_mean)
  
  # Returner R²-værdien
  summary(model)$r.squared
})

summary.af.comb <- lapply(alle_kombinationer, function(combination) {
  # Beregn gennemsnit for kombinationen
  komb_mean <- rowMeans(as.data.frame(combination), na.rm = TRUE)
  
  # Fit linear model
  model <- lm(realvækst ~ komb_mean)
  
  # Returner R²-værdien
  summary(model)
})

# Find kombination for højest r^2

r_squared_vector <- unlist(r_squared_values)

r.2df <- data.frame(unlist(r_squared_values))

# komb 1443(DØ_idag, ANS12, PØ, R_12, F_mere) r^2=0,447


# Find realvæksten og lave til en samlede dataframe

Forbrugv <- dst_meta(table = "NKHC021")
Forbrugvdf <- list(
  FORMAAAL="*",
  PRISENHED="*",
  SÆSON="*", 
  Tid="*"
)
Forbrugdata <- dst_get_data(table = "NKHC021",query = Forbrugvdf, lang="da")

Forbrugdata <- pivot_wider(Forbrugdata, names_from = FORMAAAL, values_from = value)

# subset colomn fra dataframen
Real_v <- Forbrugdata %>%
  filter(TID >= as.Date("1999-01-01") & TID <= as.Date("2024-12-31") & PRISENHED== "2020-priser, kædede værdier" & SÆSON=="Sæsonkorrigeret") %>%
  select(TID, PRISENHED, SÆSON, "I alt")

Real_v$Real_væskt <- NA

# diff mellem række 5 og reække 1 gange 100 for procent
forbrug_diff <- diff(log(as.numeric(Real_v$`I alt`)), lag = 4) * 100

forbrug_diff_full <- c(rep(NA, 4), forbrug_diff)

Real_v$Real_væskt <- forbrug_diff_full

Real_v <- Real_v[-c(1:4),]
df <- cbind(opg1.1x,Real_v)
df <- df[,-c(15:17)]

# forloop for r^2

# predict k.4 med NKHC021
Sdf <- cbind(Real_v,opg1.1x)
lm_dst <- lm(Real_væskt ~ Indikator_dst, data=Sdf)
summary(lm_dst)
q4_dst <- data.frame(Indikator_dst <- (-8.9+-9.3)/2)
predict(lm_dst,q4_dst)


# opg 1.5
spørgsmål_micro <- opg1.1x[c(2,3,6,7,11,12,13)]
micro_kombinationer <- unlist(lapply(1:7, function(i) combn(spørgsmål_micro, i, simplify = FALSE)), recursive = FALSE)

r_squared_micro <- lapply(micro_kombinationer, function(combination) {
  # Beregn gennemsnit for kombinationen
  micro_mean <- rowMeans(as.data.frame(combination), na.rm = TRUE)
  
  # Fit linear model
  m.model <- lm(realvækst ~ micro_mean)
  
  # Returner R²-værdien
  summary(m.model)$r.squared
})

micro.r.2 <- data.frame(unlist(r_squared_micro))
# komb.62 med 0.30 r^2

summary_lm <- sapply(alle_kombinationer, function(combination) {
  # Beregn gennemsnit for kombinationen
  cb_mean <- rowMeans(as.data.frame(combination), na.rm = TRUE)
  
  # Fit linear model
  cb.model <- lm(realvækst ~ cb_mean)
  
  # Returner R²-værdien
  summary(cb.model)
})
r_squared_list <- lapply(alle_kombinationer, function(combination) {
  # Beregn gennemsnit for kombinationen
  komb_mean <- rowMeans(as.data.frame(combination), na.rm = TRUE)
  
  # Fit linear model
  model <- lm(realvækst ~ komb_mean)
  
  # Returner R²-værdien
  summary(model)
})
