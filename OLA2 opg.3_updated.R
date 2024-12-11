library(devtools)
library(dkstat)
library(tidyverse)
library(ggplot2)
library(caret)
library(pROC)
## Opg.3.1
Forbrugvv <- dst_meta(table = "NKH1")
Forbrugvdf <- list(
  TRANSAKT="*",
  PRISENHED="*",
  SÆSON="*", 
  Tid="*"
)
Forbrugdata <- dst_get_data(table = "NKH1",query = Forbrugvdf, lang="da")

Forbrugdata <- pivot_wider(Forbrugdata, names_from = TRANSAKT, values_from = value)

opg3.1 <- Forbrugdata %>%
  filter(TID >= as.Date("1997-01-01") & TID <= as.Date("2021-06-30") & PRISENHED== "2020-priser, kædede værdier" & SÆSON=="Sæsonkorrigeret") %>%
  select(TID, PRISENHED, SÆSON, `P.31 Husholdningernes forbrugsudgifter`)

opg3.1$Real_vækst <- NA

forbrug_diff <- diff(log(as.numeric(opg3.1$`P.31 Husholdningernes forbrugsudgifter`)), lag = 4) * 100

forbrug_diff_full <- c(rep(NA, 4), forbrug_diff)

opg3.1$Real_vækst <- forbrug_diff_full

opg3.1 <- opg3.1[-c(1:4),]

opg3.1$direction <- NA
opg3.1$dummy <- NA

# lave dummy_v

for(i in 1:nrow(opg3.1)){  
  
  if(opg3.1$Real_vækst[i] >= 0){
    opg3.1$direction[i] <- "op"
    
  } else if(opg3.1$Real_vækst[i] < 0){
    opg3.1$direction[i] <- "ned"
  }
}

opg3.1$dummy <- ifelse(opg3.1$direction=="op",1,0)

table(opg3.1$dummy)

# opg 3.2
# hent data fra DI
indi_meta <- dst_meta(table = "FORV1")
indi_list <- list(
  INDIKATOR="*",
  Tid="*"
)
Forbrugindi <- dst_get_data(table = "FORV1",query = indi_list, lang="da")
Forbrugindi <- pivot_wider(Forbrugindi, names_from = INDIKATOR, values_from = value)

opg3.2 <- Forbrugindi %>%
  filter(TID >= as.Date("1998-01-01") & TID <= as.Date("2024-12-30")) %>%
  select(TID, 
         Forbrugertillidsindikatoren,
         `Familiens økonomiske situation i dag, sammenlignet med for et år siden`, 
         `Danmarks økonomiske situation i dag, sammenlignet med for et år siden`, 
         `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`,
         `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`
  )


opg3.2x <- data.frame(FØ_idag = numeric(), DØ_idag = numeric(), ANS = numeric(), ANS12 = numeric(), Indikator=numeric())

for (i in 1:nrow(opg3.2)) {
  
  if (i %% 3 == 0) {
    gennemsnit_indi <- mean(opg3.2$Forbrugertillidsindikatoren[(i-2):i])
    gennemsnit_fø <- mean(opg3.2$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`[(i-2):i])
    gennemsnit_dø <- mean(opg3.2$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[(i-2):i])
    gennemsnit_an <- mean(opg3.2$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[(i-2):i])
    gennemsnit_an12 <- mean(opg3.2$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`[(i-2):i])
    
    opg3.2x <- rbind(opg3.2x, data.frame(FØ_idag = gennemsnit_fø, DØ_idag = gennemsnit_dø, ANS = gennemsnit_an, ANS12 = gennemsnit_an12, Indikator=gennemsnit_indi))
  }
}

# DI's indidkator

q4_dst <- (-8.9+-9.3)/2
q4_di <- ((-8.9+-10.1)/2+(-9.8+-12.5)/2+(-16.1+-13.1)/2+(-8+-3.7)/2)/4

opg3.2x$di_fti <- rowMeans(opg3.2x[,1:4])
opg3.2x <- opg3.2x[-c(95:107),]
opg3.2r <- cbind(opg3.1,opg3.2x) 
glm <- glm(dummy~di_fti, family = "binomial", data=opg3.2r)
glm2 <- glm(dummy~Indikator, family = "binomial", data=opg3.2r)

summary(glm)

pred24 <- data.frame(predict(glm, type = "response"))
view(pred24)

pred24_dst <- data.frame(predict(glm2, type = "response"))
view(pred24_dst)

opg3.2r$direction <- as.factor(opg3.2r$direction)
opg3.2r$direction <- relevel(opg3.2r$direction, ref = "ned")
contrasts(opg3.2r$direction)

# predictdata
fopg3.2 <- data.frame(di_fti=q4_di)
predict(glm, fopg3.2, type = "response")

dstopg3.2 <- data.frame(Indikator=q4_dst)
predict(glm2, dstopg3.2, type = "response")

##
pred24$prd_direction <- ifelse(pred24$predict.glm..type....response..>=0.5, 1,0)
pred24_dst$prd_direction <- ifelse(pred24_dst$predict.glm2..type....response..>=0.7, 1,0)
table(pred24_dst$prd_direction)
table(pred24$prd_direction)

# konfusion
di_konf <- data.frame(cbind(pred24$prd_direction,opg3.2r$dummy))
dst_konf <- data.frame(cbind(pred24_dst$prd_direction,opg3.2r$dummy))
di_konf$X1 <- factor(di_konf$X1, levels = c(0, 1))
di_konf$X2 <- factor(di_konf$X2, levels = c(0, 1))
dst_konf$X1 <- factor(dst_konf$X1, levels = c(0, 1))
dst_konf$X2 <- factor(dst_konf$X2, levels = c(0, 1))
konfusion_di <- confusionMatrix(data = di_konf$X1, reference = di_konf$X2)
konfusion_dst <- confusionMatrix(data = dst_konf$X1, reference = dst_konf$X2)
konfusion_dst
konfusion_di
table(di_konf$X2)
table(dst_konf$X1)

roc_curve <- roc(opg3.2r$dummy, pred24$predict.glm..type....response..)

# Plot ROC-kurven
roc_curve$sensitivities
roc_curve$specificities

fpr <- 1 - roc_curve$specificities
tpr <- roc_curve$sensitivities

# Plot TPR (y) mod FPR (x) manuelt
plot(fpr, tpr, type = "l", col = "blue", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate",
     xlim = c(0, 1), ylim = c(0, 1), main = "ROC Curve")

# Tilføj diagonal linje for tilfældig klassifikation (reference)
abline(a = 0, b = 1, col = "gray", lty = 2)
