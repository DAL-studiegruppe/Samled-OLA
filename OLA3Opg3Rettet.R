library(caret)
library(pROC)
# opg3.1

# Hent data via API på indikator
indi_meta <- dst_meta(table = "FORV1")
indi_list <- list(
  INDIKATOR="*",
  Tid="*"
)
Forbrugindi <- dst_get_data(table = "FORV1",query = indi_list, lang="da")
Forbrugindi <- pivot_wider(Forbrugindi, names_from = INDIKATOR, values_from = value)
opg3.1 <- Forbrugindi %>%
  filter(TID >= as.Date("2000-01-01") & TID <= as.Date("2024-12-31"))

# oprette nye dataframe og lave FORV1 til kvatalvis
opg3.1x <- data.frame(Indikator_dst = numeric())
for (i in 1:nrow(opg3.1)) {
  if (i %% 3 == 0) {
    gennemsnit_indi <- mean(opg3.1$Forbrugertillidsindikatoren[(i-2):i])
    opg3.1x <- rbind(opg3.1x, data.frame(Indikator_dst = gennemsnit_indi))
  }
}

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

# Tilret igen
Real_v <- Real_v[,-c(2:3)]

# Lav kolonne til realvækst
Real_v$Real_væskt <- NA

# Udregn realvækst
forbrug_diff <- diff(log(as.numeric(Real_v$`I alt`)), lag = 4) * 100

forbrug_diff_full <- c(rep(NA, 4), forbrug_diff)

Real_v$Real_væskt <- forbrug_diff_full

Real_v <- Real_v[-c(1:4),]




# hente realvækst
opg3.1p <- opg3.1x
df3.1 <- cbind(opg3.1p,Real_v)
df3.1$indikator <- df3.1$opg3.1x..99...
df3.1 <- df3.1[,-3]

# lave dummy variable
df3.1$direction <- NA
df3.1$dummy <- NA
for(i in 1:nrow(df3.1)){  
  
  if(df3.1$Real_væskt[i] >= 0){
    df3.1$direction[i] <- "op"
    
  } else if(df3.1$Real_væskt[i] < 0){
    df3.1$direction[i] <- "ned"
  }
}
df3.1$dummy <- ifelse(df3.1$direction=="op",1,0)
df3.1 <- df3.1[,-2]

# Logistisk regression og predict
glm3.1 <- glm(dummy~Indikator_dst, family = "binomial", data=df3.1)
summary(glm3.1)
fopg3.1 <- data.frame(Indikator_dst=-9.1) # Dette finder vi ud af opgave 2, hvor vi beregner gnm
predict(glm3.1, fopg3.1, type = "response")

# linear regression til k4 2024
tlmk4 <- lm(Real_væskt~Indikator_dst, data=df3.1)
ttlmk4 <- data.frame(Indikator_dst=-9.1) # Dette finder vi ud af opgave 2, hvor vi beregner gnm
predict(tlmk4, ttlmk4, type = "response")
summary(tlmk4)

# konfusionsmatrix
predk4 <- data.frame(predict(glm3.1, type = "response"))
predk4$prd_direction <- ifelse(predk4$predict.glm3.1..type....response..>=0.5,1,0)
table(predk4$prd_direction)
table(df3.1$dummy)
k4konf <- data.frame(cbind(predk4$prd_direction,df3.1$dummy))
k4konf$prd <- k4konf$X1
k4konf$real <- k4konf$X2
k4konf <- k4konf[,-c(1:2)]
k4konf$prd <- factor(k4konf$prd, levels = c(0, 1))
k4konf$real <- factor(k4konf$real, levels = c(0, 1))
konfusionk4 <- confusionMatrix(data = k4konf$prd, reference = k4konf$real)
konfusionk4

# Roc kurve
roc_curve <- roc(df3.1$dummy,predk4$predict.glm3.1..type....response..)

roc_curve$sensitivities
roc_curve$specificities

plot(roc_curve, col = "blue", main = "ROC Curve", print.auc = TRUE)

