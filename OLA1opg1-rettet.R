
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

