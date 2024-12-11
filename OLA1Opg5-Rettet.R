##################
#### OPG 5.1 #####
##################

# Opret Klasse vektor
Klasse <- rep(c("A", "B", "C", "D"), each = 9)

# Opret Uge vektor
Uge <- rep(seq(1, 9), times = 4)

# Opret Score vektor
Score <- seq(10, 45)

# Lav dataframen
df <- data.frame(Klasse, Uge, Score)


##################
#### OPG 5.2 #####
##################

# Ny dataframe til resultatet
ny_df <- data.frame(Klasse = character(), Uge = integer(), Score = numeric())

# Loop igennem dataframen 'df'. Her gentager vi noget for hver række i tabellen. Det vi vil gentage er vha. modulo.
for (i in 1:nrow(df)) {
  # Brug modulo-operatoren for hver tredje iteration. 
  # For hver række tjekker vi, om rækkenummeret er deleligt med 3. Hvis ja → kør if funktion
  if (i %% 3 == 0) {
    # Når vi når hver tredje række, trækker vi to stykker information fra rækken: navnet på klassen og ugen.
    klasse_val <- df$Klasse[i]
    uge_val <- df$Uge[i]
    
    # Beregn gennemsnittet af Score for de tre forrige observationer
    score_avg <- mean(df$Score[(i-2):i])
    
    # Opret en ny 1x3 dataframe med værdierne fra de vektorer vi oprettede
    ny_række <- data.frame(Klasse = klasse_val, Uge = uge_val, Score = score_avg)
    
    # Tilføj den nye række til 'ny_df' vha. rbind der binder 2 dataframes sammen
    ny_df <- rbind(ny_df, ny_række)
  }
}

# Vis den nye dataframe
print(ny_df)



##################
#### OPG 5.1 #####
##################

# Aktiver packages
library(tidyr)

# Konverter dataframen til bredt format ved hjælp af pivot_wider()
# “Names_from = Klasse” betyder, at værdierne i kolonnen Klasse (A, B, C, D) bliver brugt til at oprette nye kolonnenavne.
# values_from = Score betyder, at værdierne i Score fylder de nye kolonner.

ny_df_bred <- pivot_wider(ny_df, 
                          names_from = Klasse, 
                          values_from = Score)

# Vis den nye dataframe
print(ny_df_bred)

