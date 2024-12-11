# Opgave 4.1 - 

library(devtools)
library(dkstat)
library(tidyverse)
library(ggplot2)
ialt=dst_get_tables(lang="da")

# API til forbrugindikator og dataframe
indi_meta <- dst_meta(table = "FORV1")
indi_list <- list(
  INDIKATOR="*",
  Tid="*"
)
Forbrugindi <- dst_get_data(table = "FORV1",query = indi_list, lang="da")
Forbrugindi <- pivot_wider(Forbrugindi, names_from = INDIKATOR, values_from = value)

# Træk de bestemt variabler fra dataframen
opg4.1 <- Forbrugindi %>%
  filter(TID >= as.Date("1996-01-01") & TID <= as.Date("2024-12-31")) %>%
  select(TID, 
         Forbrugertillidsindikatoren, 
         `Familiens økonomiske situation i dag, sammenlignet med for et år siden`, 
         `Familiens økonomiske  situation om et år, sammenlignet med i dag`,
         `Danmarks økonomiske situation i dag, sammenlignet med for et år siden`, 
         `Danmarks økonomiske situation om et år, sammenlignet med i dag`,
         `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`,
         `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`
  )

opg4.1x <- data.frame(Indikator = numeric(), FØ_idag = numeric(), FØ_år = numeric(), DØ_idag = numeric(), DØ_år = numeric(), ANS = numeric(), ANS12 = numeric())

for (i in 1:nrow(opg4.1)) {
  
  if (i %% 3 == 0) {
    gennemsnit_indi <- mean(opg4.1$Forbrugertillidsindikatoren[(i-2):i])
    gennemsnit_fø <- mean(opg4.1$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`[(i-2):i])
    gennemsnit_føå <- mean(opg4.1$`Familiens økonomiske  situation om et år, sammenlignet med i dag`[(i-2):i])
    gennemsnit_dø <- mean(opg4.1$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[(i-2):i])
    gennemsnit_døå <- mean(opg4.1$`Danmarks økonomiske situation om et år, sammenlignet med i dag`[(i-2):i])
    gennemsnit_an <- mean(opg4.1$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[(i-2):i])
    gennemsnit_an12 <- mean(opg4.1$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`[(i-2):i])
    
    opg4.1x <- rbind(opg4.1x, data.frame(Indikator = gennemsnit_indi, FØ_idag = gennemsnit_fø, FØ_år = gennemsnit_føå, DØ_idag = gennemsnit_dø, DØ_år = gennemsnit_døå, ANS = gennemsnit_an, ANS12 = gennemsnit_an12))
  }
}

# API af privatforbrug
Forbrugv <- dst_meta(table = "NKH1")
Forbrugvdf <- list(
  TRANSAKT="*",
  PRISENHED="*",
  SÆSON="*", 
  Tid="*"
)
Forbrugdata <- dst_get_data(table = "NKH1",query = Forbrugvdf, lang="da")

Forbrugdata <- pivot_wider(Forbrugdata, names_from = TRANSAKT, values_from = value)

# subset colomn fra dataframen
Real_v <- Forbrugdata %>%
  filter(TID >= as.Date("1995-01-01") & TID <= as.Date("2024-12-31") & PRISENHED== "2020-priser, kædede værdier" & SÆSON=="Sæsonkorrigeret") %>%
  select(TID, PRISENHED, SÆSON, `P.31 Husholdningernes forbrugsudgifter`)

Real_v$Real_væskt <- NA

# diff mellem række 5 og række 1 gange 100 for procent
forbrug_diff <- diff(log(as.numeric(Real_v$`P.31 Husholdningernes forbrugsudgifter`)), lag = 4) * 100

forbrug_diff_full <- c(rep(NA, 4), forbrug_diff)

Real_v$Real_væskt <- forbrug_diff_full

Real_v <- Real_v[-c(1:4),]

# Lave til samlede dataframen for lave grafer

Sdf <- cbind(Real_v,opg4.1x)
Sdf$fti <- rowMeans(Sdf[, 6, drop = FALSE], na.rm = TRUE)

# ggplot
min_value <- min(Sdf$Indikator, na.rm = TRUE)
max_value <- max(Sdf$Indikator, na.rm = TRUE)
min_time <- Sdf$TID[which.min(Sdf$Indikator)]
max_time <- Sdf$TID[which.max(Sdf$Indikator)]


# Opret grafen
ggplot(Sdf, aes(x = TID)) + 
  geom_bar(aes(y = Real_væskt), stat = "identity", color = "blue", alpha = 0.5, fill = "blue", show.legend = FALSE) +
  geom_line(aes(y = Indikator, color = "Forbrugertillidsindikator"), size = 1.5) +
  geom_point(aes(x = min_time, y = min_value), color = "red", size = 2) +  
  geom_point(aes(x = max_time, y = max_value), color = "green", size = 2) +
  geom_text(aes(x = min_time, y = min_value, label = round(min_value, 2)),
            vjust = -1, color = "red") +                                  
  geom_text(aes(x = max_time, y = max_value, label = round(max_value, 2)),
            vjust = -1, color = "green") + 
  scale_x_date(breaks = scales::date_breaks("5 years"), labels = scales::date_format("%Y")) + 
  ggtitle("Forbrugertilliden: Højdepunkter før Finanskrisen og Lavpunkter under Energikrisen") +
  xlab("Tid") +
  ylab("Indikator") +
  scale_color_manual(name = "Forklaring", values = c("Forbrugertillidsindikator" = "orange")) +
  theme_minimal() +
  theme(legend.position = "top")

# opgave 4.2
opg4.2 <- opg4.1x[c(17:115),]
gennemsnit4 <- mean(opg4.2$ANS) 
gennemsnit4
# opgave 4.3

meta_forbrug <- dst_meta(table = "NKHC021")
df_forbrug <- list(
  FORMAAAL="*",
  PRISENHED="*",
  SÆSON="*", 
  Tid="*"
)
df_fb <- dst_get_data(table = "NKHC021",query = df_forbrug, lang="da")

df_fb <- pivot_wider(df_fb, names_from = FORMAAAL, values_from = value)
df_fb <- df_fb %>%
  filter(TID >= as.Date("2000-01-01") & TID <= as.Date("2024-12-31") & 
           PRISENHED== "2020-priser, kædede værdier" & 
           SÆSON=="Sæsonkorrigeret")
names(df_fb)

# find grupper brugt flest penge i 2023 og højest stigning fra 2020 til 2023

funktionforbrug <- function(df_fb) {
  # data for 2020 og 2023
  df_fb_2020 <- subset(df_fb, format(TID, "%Y") == "2020")
  df_fb_2023 <- subset(df_fb, format(TID, "%Y") == "2023")
  
  # gennemsnitligt forbrug for hver kategori i 2020 og 2023
  mean_2020 <- colMeans(df_fb_2020[, 5:19], na.rm = TRUE)
  mean_2023 <- colMeans(df_fb_2023[, 5:19], na.rm = TRUE)
  
  # beregn ændringen i forbrug
  stigning <- mean_2023 - mean_2020
  
  # find kategorien med den største stigning og sørrest brugt
  max_stigning_kategori <- names(stigning)[which.max(stigning)]
  
  return(max_stigning_kategori)
}
funktionforbrug(df_fb)
max_forbrug_kategori <- names(mean_2023)[which.max(mean_2023)]
max_forbrug_kategori

# opg4.4
opg4.4 <- opg2.2x
opg4.4x <- cbind(opg4.4,df_fb)


# forloop til 15 realvækst
grp_subset <- opg4.4 %>%
  select(Indikator, di_fti)
grp_subset <- grp_subset[-c(1:4),]
grupper_real <- opg4.4x
grupper <- names(opg4.4x)[13:27]
# Realvækst
for (i in grupper) {
  
  forbrug_diff <- diff(log(as.numeric(opg4.4x[[i]])), lag = 4) * 100
  
  forbrug_diff_full <- c(rep(NA, 4), forbrug_diff)
  
  grupper_real[[paste0(i, "_Realvækst")]] <- forbrug_diff_full
}
grupper_real <- grupper_real[-c(1:4), (28:42)]
real_grupper <- data.frame(cbind(grupper_real,grp_subset))

# Lave for loop for 30 regressioner
forbrugskategorier <- names(real_grupper)[1:15]
resultater <- list()

for ( i in forbrugskategorier) {
  
  model_DST <- lm(real_grupper[[i]] ~ real_grupper$Indikator, data = real_grupper)
  
  model_DI <- lm(real_grupper[[i]] ~ real_grupper$di_fti, data = real_grupper)
  
  resultater[[paste(i, "vs DST")]] <- summary(model_DST)
  resultater[[paste(i, "vs DI")]] <- summary(model_DI)
}

2