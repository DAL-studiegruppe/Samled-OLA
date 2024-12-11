# Opgave 2.1 - Opdatering af DI’s forbrugertillidsindikator

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
opg2.1 <- Forbrugindi %>%
  filter(TID >= as.Date("2000-01-01") & TID <= as.Date("2023-12-31")) %>%
  select(TID, 
         Forbrugertillidsindikatoren, 
         `Familiens økonomiske situation i dag, sammenlignet med for et år siden`, 
         `Familiens økonomiske  situation om et år, sammenlignet med i dag`,
         `Danmarks økonomiske situation i dag, sammenlignet med for et år siden`, 
         `Danmarks økonomiske situation om et år, sammenlignet med i dag`,
         `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`,
         `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`
  )

opg2.1x <- data.frame(Indikator = numeric(), FØ_idag = numeric(), FØ_år = numeric(), DØ_idag = numeric(), DØ_år = numeric(), ANS = numeric(), ANS12 = numeric())

for (i in 1:nrow(opg2.1)) {
  
  if (i %% 3 == 0) {
    gennemsnit_indi <- mean(opg2.1$Forbrugertillidsindikatoren[(i-2):i])
    gennemsnit_fø <- mean(opg2.1$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`[(i-2):i])
    gennemsnit_føå <- mean(opg2.1$`Familiens økonomiske  situation om et år, sammenlignet med i dag`[(i-2):i])
    gennemsnit_dø <- mean(opg2.1$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[(i-2):i])
    gennemsnit_døå <- mean(opg2.1$`Danmarks økonomiske situation om et år, sammenlignet med i dag`[(i-2):i])
    gennemsnit_an <- mean(opg2.1$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[(i-2):i])
    gennemsnit_an12 <- mean(opg2.1$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`[(i-2):i])
    
    opg2.1x <- rbind(opg2.1x, data.frame(Indikator = gennemsnit_indi, FØ_idag = gennemsnit_fø, FØ_år = gennemsnit_føå, DØ_idag = gennemsnit_dø, DØ_år = gennemsnit_døå, ANS = gennemsnit_an, ANS12 = gennemsnit_an12))
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
  filter(TID >= as.Date("1999-01-01") & TID <= as.Date("2023-12-31") & PRISENHED== "2020-priser, kædede værdier" & SÆSON=="Sæsonkorrigeret") %>%
  select(TID, PRISENHED, SÆSON, `P.31 Husholdningernes forbrugsudgifter`)

Real_v$Real_væskt <- NA

# diff mellem række 5 og reække 1 gange 100 for procent
forbrug_diff <- diff(log(as.numeric(Real_v$`P.31 Husholdningernes forbrugsudgifter`)), lag = 4) * 100

forbrug_diff_full <- c(rep(NA, 4), forbrug_diff)

Real_v$Real_væskt <- forbrug_diff_full

Real_v <- Real_v[-c(1:4),]

# Lave til samlede dataframen for lave grafer
Sdf <- cbind(Real_v,opg2.1x)

Sdf$di_fti <- rowMeans(Sdf[ , c(7,9,11,12)])

ggplot(Sdf, aes(x = TID)) + 
  geom_bar(aes(y = Real_væskt), stat = "identity", color = "blue", alpha = 0.5) +
  geom_line(aes(y = di_fti, color = "DI's Forbrugertillidsindikator"), size = 1.5) 



# Opdater DI's tal til og med 2023
lm_di <- lm(Real_væskt ~ di_fti , data = Sdf)
Di_cor <- cor(Sdf$Real_væskt, Sdf$di_fti)
lm_dst <- lm(Real_væskt ~ Indikator, data=Sdf)
Dst_cor <- cor(Sdf$Real_væskt, Sdf$Indikator)
summary(lm_di)
Di_cor
summary(lm_dst)
Dst_cor
# Sammenligne med 2016
testdf <- Sdf[1:68,c(1,5,6,13)]
testlm <-  lm(Real_væskt ~ di_fti , data = testdf)
testlm2 <-  lm(Real_væskt ~ Indikator , data = testdf)
summary(testlm)
summary(testlm2)
cor(testdf$Real_væskt,testdf$di_fti)
cor(testdf$Real_væskt,testdf$Indikator)

## opg 2.2

opg2.2 <- Forbrugindi %>%
  filter(TID >= as.Date("2000-01-01") & TID <= as.Date("2024-12-31")) %>%
  select(TID, 
         Forbrugertillidsindikatoren, 
         `Familiens økonomiske situation i dag, sammenlignet med for et år siden`, 
         `Familiens økonomiske  situation om et år, sammenlignet med i dag`,
         `Danmarks økonomiske situation i dag, sammenlignet med for et år siden`, 
         `Danmarks økonomiske situation om et år, sammenlignet med i dag`,
         `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`,
         `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`
  )

opg2.2_1 <- opg2.2[1:297,]

opg2.2x <- data.frame(Indikator = numeric(), FØ_idag = numeric(), FØ_år = numeric(), DØ_idag = numeric(), DØ_år = numeric(), ANS = numeric(), ANS12 = numeric())

opg2.2di_fti <- data.frame(Indikator = numeric(), FØ_idag = numeric(), FØ_år = numeric(), DØ_idag = numeric(), DØ_år = numeric(), ANS = numeric(), ANS12 = numeric())

for (i in 1:nrow(opg2.2)) {
  
  if (i %% 3 == 0) {
    gennemsnit_indi <- mean(opg2.2$Forbrugertillidsindikatoren[(i-2):i])
    gennemsnit_fø <- mean(opg2.2$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`[(i-2):i])
    gennemsnit_føå <- mean(opg2.2$`Familiens økonomiske  situation om et år, sammenlignet med i dag`[(i-2):i])
    gennemsnit_dø <- mean(opg2.2$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[(i-2):i])
    gennemsnit_døå <- mean(opg2.2$`Danmarks økonomiske situation om et år, sammenlignet med i dag`[(i-2):i])
    gennemsnit_an <- mean(opg2.2$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[(i-2):i])
    gennemsnit_an12 <- mean(opg2.2$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`[(i-2):i])
    
    opg2.2x <- rbind(opg2.2x, data.frame(Indikator = gennemsnit_indi, FØ_idag = gennemsnit_fø, FØ_år = gennemsnit_føå, DØ_idag = gennemsnit_dø, DØ_år = gennemsnit_døå, ANS = gennemsnit_an, ANS12 = gennemsnit_an12))
  }
}

opg2.2x$di_fti <- rowMeans(opg2.2x[ , c(2,4,6,7)])

Real <- Forbrugdata %>%
  filter(TID >= as.Date("1999-01-01") & TID <= as.Date("2024-12-31") & PRISENHED== "2020-priser, kædede værdier" & SÆSON=="Sæsonkorrigeret") %>%
  select(TID, PRISENHED, SÆSON, `P.31 Husholdningernes forbrugsudgifter`)

Real$Real_væskt <- NA

forbrug_diff <- diff(log(as.numeric(Real$`P.31 Husholdningernes forbrugsudgifter`)), lag = 4) * 100

forbrug_diff_full <- c(rep(NA, 4), forbrug_diff)

Real$Real_væskt <- forbrug_diff_full

Real <- Real[-c(1:4),]

Df2.2 <- cbind(Real,opg2.2x)

#Df2.2$dummy <- NA
#Df2.2$direction <- NA
#for(i in 1:nrow(Df2.2)){  

#if(Df2.2$Real_væskt[i] >= 0){
#Df2.2$direction[i] <- "op"

#} else if(Df2.2$Real_væskt[i] < 0){
#Df2.2$direction[i] <- "ned"
#}
#}

#Df2.2$dummy <- ifelse(Df2.2$direction=="op",1,0)

# Linær regression af DI og DST's indikator

lmdi <- lm(Real_væskt~di_fti, data = Df2.2)
lmdst <- lm(Real_væskt~Indikator, data = Df2.2)
summary(lmdst)
summary(lmdi)
mean(Df2.2$Real_væskt)
# Forudsig med formel og funktion
q4_dst <- (-8.9+-9.3)/2
q4_di <- ((-8.9+-10.1)/2+(-9.8+-12.5)/2+(-16.1+-13.1)/2+(-8+-3.7)/2)/4

coef_intercept_di <- coef(lmdi)[1] 
coef_di_indi <- coef(lmdi)[2]
coef_intercept_dst <- coef(lmdst)[1] 
coef_dst_indi <- coef(lmdst)[2]

q4_real_dst <- coef_intercept_dst+coef_dst_indi*q4_dst

q4_real_di <- coef_intercept_di+coef_di_indi*q4_di

dq4dst <- data.frame(Indikator=q4_dst)
dq4di <- data.frame(di_fti=q4_di)
predict(lmdi,dq4di)
predict(lmdst,dq4dst)

q4_real_di
q4_real_dst


