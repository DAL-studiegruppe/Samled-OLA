# opg 4
library(parallel)
library(combinat)

# Funktion til at beregne R² for hver kombination efter fjernelse af kvartaler
beregn_r2_stabilitet <- function(data, realvækst_col, kombinationer, n_kvartaler) {
  
  # Dataframe til at gemme resultater
  resultater <- data.frame(Fjernet_kvartaler = integer(), 
                           Kombination = character(), 
                           R2 = numeric(), 
                           stringsAsFactors = FALSE)
  
  # Hent Real vækst kolonnen
  realvækst <- data[[realvækst_col]]
  
  # Erstat mclapply med almindelig lapply
  resultater_list <- lapply(0:(n_kvartaler - 1), function(k) {
    # Fjern de seneste k kvartaler fra data
    data_subset <- data[1:(nrow(data) - k), ]
    realvækst_subset <- realvækst[1:(length(realvækst) - k)]
    
    # Beregn R² for hver kombination
    r2_values <- lapply(kombinationer, function(kombination) {
      komb_navne <- names(kombination)
      
      # Beregn gennemsnit for kombinationen
      if (length(kombination) > 1) {
        komb_mean <- rowMeans(data_subset[, komb_navne, drop = FALSE], na.rm = TRUE)
      } else {
        komb_mean <- data_subset[[komb_navne]]
      }
      
      # Fit linear model og beregn R2
      model <- lm(realvækst_subset ~ komb_mean)
      r_squared <- summary(model)$r.squared
      
      # Returner resultat for denne kombination og k
      data.frame(Fjernet_kvartaler = k, 
                 Kombination = paste(komb_navne, collapse = ", "), 
                 R2 = r_squared)
    })
    
    # Kombinér resultater for alle kombinationer
    do.call(rbind, r2_values)
  })
  
  # Kombinér alle resultater i én data frame
  resultater <- do.call(rbind, resultater_list)
  return(resultater)
}

# Generer alle kombinationer af de 12 spørgsmål
spørgsmål_indices <- opg1.1x[3:14]  # Justér til kolonnerne for dine spørgsmål
alle_kombinationer <- unlist(lapply(1:12, function(i) combn(spørgsmål_indices, i, simplify = FALSE)), recursive = FALSE)
df <- df[,-c(1,14)]
# Kør stabilitetsanalysen
stabilitet_resultater <- beregn_r2_stabilitet(df, "Real_væskt", alle_kombinationer, n_kvartaler = 36)

# Vis resultaterne
print(stabilitet_resultater)


r2_resultater <- pivot_wider(stabilitet_resultater, names_from = Fjernet_kvartaler, values_from = R2)

r2_res_mean <- data.frame(rowMeans(r2_resultater[,2:37]))

r2_res_mean$Kombination <- r2_resultater$Kombination

# Lave en til uden energikrisen i danmark og se forskel
edf <- df[-c(89:98),]
opg1.1e <- opg1.1x[-c(89:98),]
spørgsmål_e <- opg1.1e[2:13]  # Justér til kolonnerne for dine spørgsmål
e_komb <- unlist(lapply(1:12, function(i) combn(spørgsmål_e, i, simplify = FALSE)), recursive = FALSE)

e_stabilitet_resultater <- beregn_r2_stabilitet(edf, "Real_væskt", e_komb, n_kvartaler = 36)

e_r2_resultater <- pivot_wider(e_stabilitet_resultater, names_from = Fjernet_kvartaler, values_from = R2)

e_r2_mean <- data.frame(rowMeans(e_r2_resultater[,2:37]))

e_r2_mean$Kombination <- e_r2_resultater$Kombination
