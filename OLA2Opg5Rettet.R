###############
### Opg 5.1 ###
###############

# Load API fra Eurostat
library(eurostat)
library(restatapi)
library(dplyr)
library(tidyr)

# Hent data for husholdningernes forbrugsudgifter (løbende priser)
forbrugEU <- get_eurostat("tipsho41", 
                          filters = list(
                            geo = c("DK", "BE", "NL", "SE", "AT", "DE", "FR", "IT", "ES"), 
                            unit = "CP_MNAC"
                          ))

# Hent prisindeks (PD10_NAC) for samme lande, så vi kan beregne realvæksten (justeret for inflation)
prisindeksEU <- get_eurostat("tipsho41", 
                             filters = list(
                               geo = c("DK", "BE", "NL", "SE", "AT", "DE", "FR", "IT", "ES"), 
                               unit = "PD10_NAC"
                             ))

# Sørg for, at både forbrug og prisindeks har samme format
forbrugEU_cl <- forbrugEU %>%
  select(time, geo, values) %>%
  pivot_wider(names_from = geo, values_from = values)

prisindeksEU_cl <- prisindeksEU %>%
  select(time, geo, values) %>%
  pivot_wider(names_from = geo, values_from = values)

# Beregn realforbrug ved at justere for prisindeks
realforbrugEU <- forbrugEU_cl
lande <- names(forbrugEU_cl)[-1]

for (land in lande) {
  realforbrugEU[[land]] <- forbrugEU_cl[[land]] / prisindeksEU_cl[[land]] * 100
}

# Beregn kvartalsvis årlig realvækst
realvaekstEU <- realforbrugEU
for (land in lande) {
  forbrug_diff <- diff(log(as.numeric(realforbrugEU[[land]])), lag = 4) * 100
  realvaekstEU[[paste0(land, "_Realvækst")]] <- c(rep(NA, 4), forbrug_diff)
}

# Fjern de første 4 kvartaler, da der mangler vækstberegninger
realvaekstEU <- realvaekstEU[-c(1:20), ]

# Vis de første par rækker
head(realvaekstEU)



###############
### Opg 5.2 ###
###############

# Beregn gennemsnit for kolonne 11-19 (realvæksten)
# "2" = anvender funktionen, i dette tilfælde "mean" på alle kolonner. 1 = rækker
# "na.rm = TRUE" håndterer NA-værdier ved at ignorer dem. Kan udelades da der ikke er NA i data
mean_values <- apply(realvaekstEU[, 11:19], 2, mean, na.rm = TRUE)

# Print gennemsnittene
print("Landet med den højeste kvartalsvise årlige realvækst:")
print (which.max(mean_values))
print("Værdien for dette gennemsnit:")
print (max(mean_values))



###############
### Opg 5.3 ###
###############

# Start: Q1 2020 (officielt erklæret pandemi i marts 2020).
# Slut: Q2 2023 (WHO afsluttede den globale sundhedsnødsituation i maj 2023)
# subset coronakrisen fra

# Fjern Coronakrisen fra data: Q1 2020 (81. række) til Q1 2023 (93. række)
forbrugEU_fjerncorona <- realvaekstEU[-c(81:93), ]

# Beregn gennemsnit for kvartalsvis realvækst uden coronakrisen
mean_values_no_corona <- apply(forbrugEU_fjerncorona[, 11:19], 2, mean, na.rm = TRUE)

# Beregn gennemsnit for hele perioden (inkl. coronakrisen)
mean_values_with_corona <- apply(realvaekstEU[, 11:19], 2, mean, na.rm = TRUE)

# Beregn forskellen mellem gennemsnittene
difference <- mean_values_no_corona - mean_values_with_corona

# Find landet med den største effekt af coronakrisen
storsteffekt <- which.max(abs(difference))
storsteffekt_land <- names(difference)[storsteffekt]

# Output resultater
print("Land med den største effekt af coronakrisen:")
print(storsteffekt_land)
print("Forskellen i gennemsnitlig realvækst:")
print(difference[storsteffekt])






###############
### Opg 5.4 ###
###############

# Lav subset for perioden 2020Q1 til 2024Q3 (række 81-98)
forbrugEU_opg5_4 <- realvaekstEU[81:99, ]

# Beregn gennemsnitlig kvartalsvis realvækst for hvert land i perioden
lande <- names(realvaekstEU)[11:19]
mean_values_corona <- apply(forbrugEU_opg5_4[, lande], 2, mean, na.rm = TRUE)

# Identificer landet med det største fald i gennemsnitlig realvækst
mindste_gennemsnit <- which.min(mean_values_corona)
mindste_gennemsnit_land <- names(mean_values_corona)[mindste_gennemsnit]
mindste_vaerdi <- mean_values_corona[mindste_gennemsnit]

# Output resultater
print("Land med størst fald i gennemsnitlig kvartalsvis realvækst:")
print(mindste_gennemsnit_land)
print("Gennemsnitlig realvækst for dette land:")
print(mindste_vaerdi)






