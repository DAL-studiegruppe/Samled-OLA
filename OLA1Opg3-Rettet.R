###########################################
### OPGAVE 3.1 – Funktion til terninger ###
###########################################

# 25.000 kast med en terning 
# replace = TRUE tillader at få samme slag mere end 1 gang. 
kast25000 <- function() {
  kast <- sample(1:6, size = 25000, replace = TRUE)
  
  # vis antallet af 5'ere i kastet vha. sum()
  antal_5ere <- sum(kast == 5)
  
  # sandsynligheden for at slå en 5'er - simpel matematik
  sandsynlighed_5er <- antal_5ere / 25000
  
  # Udskriv resultater i console hvis ønsket
  cat("Antal 5'ere slået:", antal_5ere, "\n")
  cat("Sandsynligheden for at slå en 5'er:", sandsynlighed_5er, "\n")
}

# kør funktion
kast25000()


###########################
### OPGAVE 3.2 – Plot I ###
###########################

# kast med 6 terninger
kast_terninger <- function() {
  # Slå med 6 terninger
  kast_resultater <- sample(1:6, size = 6, replace = TRUE)
  
  # Beregn summen af kastene
  sum_kast <- sum(kast_resultater)
  
  # Udskriv resultaterne
  cat("Resultater af terningekast:", kast_resultater, "\n")
  cat("Summen af de 6 terninger:", sum_kast, "\n")
  
  # Returner summen af kastene (optional, kan gøres hvis der skal laves yderligere beregninger
  # med summen)
  return(sum_kast)
}

# kør funktion
kast_terninger()



# gør det nu 10.000 gange
antal_kast <- 10000
resultater10000 <- replicate(antal_kast, kast_terninger())

# lav barplot
barplot(table(resultater10000),
        main = "Frekvens af summer ved 10.000 kast med 6 terninger",
        xlab = "Sum af terningekast",
        ylab = "Frekvens(Antal kast)",
        col = "lightblue",
        border = "black")



############################
### OPGAVE 3.3 – Plot II ###
############################

# gør det nu 1.000.000 gange
antal_kast2 <- 1000000
resultater1.000.000 <- replicate(antal_kast2, kast_terninger())
barplot(table(resultater1.000.000),
        main = "Frekvens af summer ved 1.000.000 kast med 6 terninger",
        xlab = "Sum af terningekast",
        ylab = "Frekvens(Antal kast)",
        col = "lightblue",
        border = "black")




########################################
### OPGAVE 3.4 – Lav dine egne data ###
########################################

# tilfældigt opstillet række 
random <- sample(c(1, 2, 3, 5, 6))

# ikke tilfældig
notrandom <- 2:6

# lav matrix vha cbind - første kolonne skal være skal være 2:6 og anden kolonne skal være tilfældig
matrix <- cbind(notrandom, random)





