library(devtools)
library(dkstat)
library(tidyverse)
library(ggplot2)
# Opg 2.1

# Hent data via API på indikator
indi_meta <- dst_meta(table = "FORV1")
indi_list <- list(
  INDIKATOR="*",
  Tid="*"
)
Forbrugindi <- dst_get_data(table = "FORV1",query = indi_list, lang="da")

# Tilret dataframen
Forbrugindi <- pivot_wider(Forbrugindi, names_from = INDIKATOR, values_from = value)
Forbrugindi <- Forbrugindi[-c(1:291),]
# Forbrugindi <- Forbrugindi[-c(310:311),]


# Hent privatforbrug via API
Forbrugv <- dst_meta(table = "NKHC021")
Forbrugvdf <- list(
  FORMAAAL="*",
  PRISENHED="*",
  SÆSON="*", 
  Tid="*"
)

Forbrugdata <- dst_get_data(table = "NKHC021",query = Forbrugvdf, lang="da")
Forbrugdata <- pivot_wider(Forbrugdata, names_from = FORMAAAL, values_from = value)

# Tilret dataframen med tid, kædede værdier og privatforbrug
Real_v <- Forbrugdata %>%
  filter(TID >= as.Date("1999-01-01") & TID <= as.Date("2024-12-31") & PRISENHED== "2020-priser, kædede værdier" & SÆSON=="Sæsonkorrigeret") %>%
  select(TID, PRISENHED, SÆSON, "I alt")

# Lav kolonne til realvækst
Real_v$Real_væskt <- NA

# Udregn realvækst
forbrug_diff <- diff(log(as.numeric(Real_v$`I alt`)), lag = 4) * 100

forbrug_diff_full <- c(rep(NA, 4), forbrug_diff)

Real_v$Real_væskt <- forbrug_diff_full

Real_v <- Real_v[-c(1:4),]

# Opret tom dataframe til beregning af kvartaler
Forbrugindi_kvartal <- data.frame(Forbrugertillidsindikatoren = numeric(), 
                                  `Familiens økonomiske situation i dag, sammenlignet med for et år siden` = numeric(),
                                  `Familiens økonomiske  situation om et år, sammenlignet med i dag` = numeric(),
                                  `Danmarks økonomiske situation i dag, sammenlignet med for et år siden` = numeric(),
                                  `Danmarks økonomiske situation om et år, sammenlignet med i dag` = numeric(),
                                  `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket` = numeric(),
                                  `Priser i dag, sammenlignet med for et år siden` = numeric(),
                                  `Priser om et år, sammenlignet med i dag` = numeric(),
                                  `Arbejdsløsheden om et år, sammenlignet med i dag` = numeric(),
                                  `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.` = numeric(),
                                  `Anser det som fornuftigt at spare op i den nuværende økonomiske situation` = numeric(),
                                  `Regner med at kunne spare op i de kommende 12 måneder` = numeric(),
                                  `Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener` = numeric())


# Lav loop for at beregne gennemsnit så det bliver kvartal

for (i in 1:nrow(Forbrugindi)) {
  
  if (i %% 3 == 0) {
    gennemsnit <- mean(Forbrugindi$Forbrugertillidsindikatoren[(i-2):i])
    gennemsnit1 <- mean(Forbrugindi$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`[(i-2):i])
    gennemsnit2 <- mean(Forbrugindi$`Familiens økonomiske  situation om et år, sammenlignet med i dag`[(i-2):i])
    gennemsnit3 <- mean(Forbrugindi$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[(i-2):i])
    gennemsnit4 <- mean(Forbrugindi$`Danmarks økonomiske situation om et år, sammenlignet med i dag`[(i-2):i])
    gennemsnit5 <- mean(Forbrugindi$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[(i-2):i])
    gennemsnit6 <- mean(Forbrugindi$`Priser i dag, sammenlignet med for et år siden`[(i-2):i])
    gennemsnit7 <- mean(Forbrugindi$`Priser om et år, sammenlignet med i dag`[(i-2):i])
    gennemsnit8 <- mean(Forbrugindi$`Arbejdsløsheden om et år, sammenlignet med i dag`[(i-2):i])
    gennemsnit9 <- mean(Forbrugindi$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`[(i-2):i])
    gennemsnit10 <- mean(Forbrugindi$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation`[(i-2):i])
    gennemsnit11 <- mean(Forbrugindi$`Regner med at kunne spare op i de kommende 12 måneder`[(i-2):i])
    gennemsnit12 <- mean(Forbrugindi$`Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener`[(i-2):i])
    # Opret nyt data frame til de beregnede gennemsnit og tilføj til Forbrugindi_kvartal
    ny_række <- data.frame(
      Forbrugertillidsindikatoren = gennemsnit,
      `Familiens økonomiske situation i dag, sammenlignet med for et år siden` = gennemsnit1,
      `Familiens økonomiske situation om et år, sammenlignet med i dag` = gennemsnit2,
      `Danmarks økonomiske situation i dag, sammenlignet med for et år siden` = gennemsnit3,
      `Danmarks økonomiske situation om et år, sammenlignet med i dag` = gennemsnit4,
      `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket` = gennemsnit5,
      `Priser i dag, sammenlignet med for et år siden` = gennemsnit6,
      `Priser om et år, sammenlignet med i dag` = gennemsnit7,
      `Arbejdsløsheden om et år, sammenlignet med i dag` = gennemsnit8,
      `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.` = gennemsnit9,
      `Anser det som fornuftigt at spare op i den nuværende økonomiske situation` = gennemsnit10,
      `Regner med at kunne spare op i de kommende 12 måneder` = gennemsnit11,
      `Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener` = gennemsnit12
      
    )
    
    Forbrugindi_kvartal <- rbind(Forbrugindi_kvartal, ny_række)
  }
}

# Fjern år 1999
Forbrugindi_kvartal <- Forbrugindi_kvartal[-c(1:4),]
### Forbrugindi_kvartal <- data.frame(Forbrugindi_kvartal) ###

Df_samlet <- cbind(Real_v,Forbrugindi_kvartal)

library(pls)
# PCA analyse
pcr.fit <- pcr(Real_væskt ~ 
                 `Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden` +
                 `Familiens.økonomiske.situation.om.et.år..sammenlignet.med.i.dag` +
                 `Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden` +
                 `Danmarks.økonomiske.situation.om.et.år..sammenlignet.med.i.dag` +
                 `Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket` +
                 `Priser.i.dag..sammenlignet.med.for.et.år.siden` +
                 `Priser.om.et.år..sammenlignet.med.i.dag` +
                 `Arbejdsløsheden.om.et.år..sammenlignet.med.i.dag` +
                 `Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.` +
                 `Anser.det.som.fornuftigt.at.spare.op.i.den.nuværende.økonomiske.situation` +
                 `Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder` +
                 `Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener`,
               data = Df_samlet, scale = TRUE,
               validation = "CV")

summary(pcr.fit)
loadings.pcr.fit <- pcr.fit$loadings
w.indicators.1 <- loadings.pcr.fit[1:12]^2
w.indicators.1
loadings(pcr.fit)

comp1 <- pcr.fit$loadings[,1]
comp1


# Korrelation
corrmaxtrix <- Df_samlet[,-c(1,2,3,4,6)]

# Beregn korrelationen mellem hver af X'erne og Y (real_væskt)
correlation_matrix <- cor(corrmaxtrix)  # Beregn korrelationen for hele datasættet

# Ekstraher korrelationen mellem real_væskt og hver af de øvrige variabler
correlation_with_Y <- correlation_matrix[, "Real_væskt"]

# Udskriv resultaterne
print(correlation_with_Y)





# Opg 2.3

# Hent data via API på indikator
Opg2.3 <- dst_get_data(table = "FORV1", query = indi_list, lang="da")

# Tilret dataframen
Opg2.3 <- pivot_wider(Opg2.3, names_from = INDIKATOR, values_from = value)
Opg2.3 <- Opg2.3[-c(1:291),]

# For at kunne forudsige realvæksten i 4. kvartal skal vi have data fra forbrugertillidsundersøgelsen
# i december måned

# Filtrér data for oktober og november 2024
okt_nov_data <- Opg2.3 %>%
  filter(TID == as.Date("2024-10-01") | TID == as.Date("2024-11-01"))

# Beregn gennemsnit for hver kolonne (bortset fra "TID")
december_values <- okt_nov_data %>%
  summarise(across(-TID, mean, na.rm = TRUE))

# Tilføj december 2024 til datasættet
Opg2.3 <- Opg2.3 %>%
  bind_rows(tibble(
    TID = as.Date("2024-12-01"),
    !!!december_values
  ))

# Opret tom dataframe til beregning af kvartaler
Opg2.3x <- data.frame(
  Forbrugertillidsindikatoren = numeric(), 
  `Familiens økonomiske situation i dag, sammenlignet med for et år siden` = numeric(),
  `Familiens økonomiske  situation om et år, sammenlignet med i dag` = numeric(),
  `Danmarks økonomiske situation i dag, sammenlignet med for et år siden` = numeric(),
  `Danmarks økonomiske situation om et år, sammenlignet med i dag` = numeric(),
  `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket` = numeric(),
  `Priser i dag, sammenlignet med for et år siden` = numeric(),
  `Priser om et år, sammenlignet med i dag` = numeric(),
  `Arbejdsløsheden om et år, sammenlignet med i dag` = numeric(),
  `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.` = numeric(),
  `Anser det som fornuftigt at spare op i den nuværende økonomiske situation` = numeric(),
  `Regner med at kunne spare op i de kommende 12 måneder` = numeric(),
  `Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener` = numeric()
)

# Lav loop for at beregne gennemsnit så det bliver kvartal
for (i in 1:nrow(Opg2.3)) {
  if (i %% 3 == 0) {
    avg <- mean(Opg2.3$Forbrugertillidsindikatoren[(i-2):i])
    avg1 <- mean(Opg2.3$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`[(i-2):i])
    avg2 <- mean(Opg2.3$`Familiens økonomiske  situation om et år, sammenlignet med i dag`[(i-2):i])
    avg3 <- mean(Opg2.3$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[(i-2):i])
    avg4 <- mean(Opg2.3$`Danmarks økonomiske situation om et år, sammenlignet med i dag`[(i-2):i])
    avg5 <- mean(Opg2.3$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[(i-2):i])
    avg6 <- mean(Opg2.3$`Priser i dag, sammenlignet med for et år siden`[(i-2):i])
    avg7 <- mean(Opg2.3$`Priser om et år, sammenlignet med i dag`[(i-2):i])
    avg8 <- mean(Opg2.3$`Arbejdsløsheden om et år, sammenlignet med i dag`[(i-2):i])
    avg9 <- mean(Opg2.3$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`[(i-2):i])
    avg10 <- mean(Opg2.3$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation`[(i-2):i])
    avg11 <- mean(Opg2.3$`Regner med at kunne spare op i de kommende 12 måneder`[(i-2):i])
    avg12 <- mean(Opg2.3$`Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener`[(i-2):i])
    
    # Opret nyt data frame til de beregnede gennemsnit og tilføj til Opg2.3x
    ny_række <- data.frame(
      Forbrugertillidsindikatoren = avg,
      `Familiens økonomiske situation i dag, sammenlignet med for et år siden` = avg1,
      `Familiens økonomiske situation om et år, sammenlignet med i dag` = avg2,
      `Danmarks økonomiske situation i dag, sammenlignet med for et år siden` = avg3,
      `Danmarks økonomiske situation om et år, sammenlignet med i dag` = avg4,
      `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket` = avg5,
      `Priser i dag, sammenlignet med for et år siden` = avg6,
      `Priser om et år, sammenlignet med i dag` = avg7,
      `Arbejdsløsheden om et år, sammenlignet med i dag` = avg8,
      `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.` = avg9,
      `Anser det som fornuftigt at spare op i den nuværende økonomiske situation` = avg10,
      `Regner med at kunne spare op i de kommende 12 måneder` = avg11,
      `Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener` = avg12
    )
    
    Opg2.3x <- rbind(Opg2.3x, ny_række)
  }
}

# Opret sekvens af kvartaler fra 1. kvartal 1999 til 3. kvartal 2024
tid_seq <- seq(from = as.Date("1999-01-01"), to = as.Date("2024-12-31"), by = "quarter")

# Indsæt tidsvariabel i dataframen
Opg2.3x$TID <- tid_seq

# Omorganiser kolonnerne for at flytte kolonnen TID (kolonne 14) til at være kolonne 1
Opg2.3x <- Opg2.3x[c(14, 1:13)]
Opg2.3x <- Opg2.3x[-c(1:4),]

# Nustil rækkeindeks
rownames(Opg2.3x) <- NULL

# Predict Q4
pred_q4 <- data.frame(Opg2.3x[100,-c(1:2)])

pca.p <- predict(pcr.fit, pred_q4, ncomp = 1)

pca.p

# PLSR
pls.fit <- plsr(Real_væskt ~ 
                  `Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden` +
                  `Familiens.økonomiske.situation.om.et.år..sammenlignet.med.i.dag` +
                  `Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden` +
                  `Danmarks.økonomiske.situation.om.et.år..sammenlignet.med.i.dag` +
                  `Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket` +
                  `Priser.i.dag..sammenlignet.med.for.et.år.siden` +
                  `Priser.om.et.år..sammenlignet.med.i.dag` +
                  `Arbejdsløsheden.om.et.år..sammenlignet.med.i.dag` +
                  `Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.` +
                  `Anser.det.som.fornuftigt.at.spare.op.i.den.nuværende.økonomiske.situation` +
                  `Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder` +
                  `Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener`,
                data = Df_samlet, scale = TRUE,
                validation = "CV")

pls.p <- predict(pls.fit, pred_q4, ncomp = 1)

pls.p


summary(pls.fit)
loadings.pls.fit <- pls.fit$loadings
pls.indicators.1 <- loadings.pls.fit[1:12]^2
pls.indicators.1
loadings(pls.fit)
pls.named_loadings <- setNames(pls.indicators.1, names(pls.fit$terms))
top_5 <- sort(abs(pls.named_loadings), decreasing = TRUE)[1:5]
print(top_5)
# SPG9, SPG1, SPG3, SPG5, SPG2

