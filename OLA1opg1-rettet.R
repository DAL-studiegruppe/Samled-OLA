
data <- read.csv("boligsiden.csv", header = TRUE, skip=1, strip.white = TRUE, na.strings=c("NA",""))
data <- data[-1, ]

# Indlæser data
boligsiden <- read.csv("/Users/kristiankarlskov/Documents/DATAANALYSE/ola1/boligsiden.csv", sep = ",",
                       header = TRUE, stringsAsFactors = FALSE,
                       strip.white = TRUE, na.strings = c("NA",""))
install.packages("dplyr")
library(dplyr)

findhus1 <- data %>% filter(data$vej == "tousvej")
print(findhus1)

findhus2 <- data %>% filter(data$pris == "4.795.000 kr.")
print(findhus2)

findhus2igen <- data %>% filter(pris == "4.795.000 kr.", vej == "egevej")
print(findhus2igen)

sample(1:2551, )
udtrækning<- sample(boligsiden[,], replace = FALSE)
View(udtrækning)


n <- nrow(boligsiden)
udtrækning <- boligsiden[sample(1:n, 1), ]
print(udtrækning)

# Opret kolonnen 'Klasse' med 9 A'er, 9 B'er, 9 C'er og 9 D'er
klasse <- rep(c("A", "B", "C", "D"), each = 9)

# Opret kolonnen 'Uge' med gentagne tal fra 1 til 9
uge <- rep(seq(1, 9), times = 4)

# Opret kolonnen 'Score' med tilfældige observationer (du kan vælge dine egne værdier)
set.seed(123)  # Sørger for at resultaterne er reproducérbare
score <- sample(50:100, 36, replace = TRUE)

# Kombiner de tre kolonner i en data frame
data <- data.frame(Klasse = klasse, Uge = uge, Score = score)

# Udskriver data frame
print(data)


# Den oprindelige 36x3 data frame fra opgave 5.1
data <- data.frame(
  Klasse = rep(c("A", "B", "C", "D"), each = 9),
  Uge = rep(seq(1, 9), times = 4),
  Score = sample(50:100, 36, replace = TRUE)
)

# Opret en tom liste til at gemme de nye rækker
new_rows <- list()

# Loop igennem hver række i data
for (i in 1:nrow(data)) {
  
  # Tjek om rækkenummeret er deleligt med 3 ved hjælp af modulo
  if (i %% 3 == 0) {
    
    # Hent de foregående tre score-værdier
    previous_scores <- data$Score[(i-2):i]
    
    # Beregn gennemsnittet af de tre score-værdier
    avg_score <- mean(previous_scores)
    
    # Opret en ny 1x3 data frame med Klasse, Uge og gennemsnittet af Score
    new_row <- data.frame(
      Klasse = data$Klasse[i],   # 'Klasse' fra den tredje række i hver gruppe af 3
      Uge = data$Uge[i],         # 'Uge' fra den tredje række i hver gruppe af 3
      Score = avg_score          # Gennemsnit af de tre score observationer
    )
    
    # Tilføj den nye række til listen
    new_rows[[length(new_rows) + 1]] <- new_row
  }
}

# Kombiner de nye rækker til en samlet 9x3 data frame
new_data <- do.call(rbind, new_rows)

# Udskriv den nye data frame
print(new_data)

boligsiden_clean<- na.omit(boligsiden)
saveRDS(boligsiden_clean, file = "boligsiden_clean.rds")
