#opgave 2.1#################################################################
############################################################################
#Lav beskrivende statistik for data på boliger til salg via boligsiden.dk fra opgave 1.
#Vær opmærksom på, hvilke variable I mener kan betragtes som x’er og yi en række simple lineære regressioner.
getwd()

# Indlæser data
boligsiden <- read.csv("/Users/kristiankarlskov/Documents/DATAANALYSE/ola1/boligsiden.csv", sep = ",",
                       header = TRUE, stringsAsFactors = FALSE,
                       strip.white = TRUE, na.strings = c("NA",""))
#rens df for NA værdier
boligsiden <- na.omit(boligsiden)

#Tjekke strukturen af dataen
str(boligsiden)

# Før vi kan plotte med kvadratmeterpris, skal den omdannes fra character til numeric ved at fjerne "." og "kr"
# Vi bruger derfor gsub til at fjerne punktum og "kr", og derved kunne lave det til numeric
#lave det til numeric
boligsiden$pris <- as.numeric(gsub("\\.|kr\\s*", "", boligsiden$pris))

#husk at rens data yderligere, tjek mdudg. 

# Resultat af str() for pris:
str(boligsiden$pris)

boxplot(boligsiden$kvmpris) 
#Dette boxplot viser en klar outlier ved 800, denne fjerens
#Nu fjernes denne outlier
boligsiden <- boligsiden[boligsiden$kvmpris <= 800, ]


#Nu bruges et histogram, for at danne samme overblik, men mere detaljeret for den lave ende af dataen
hist(boligsiden$kvmpris, breaks = 50, main = "Histogram over kvadratmeterpriser", xlab = "Kvm pris", ylab = "Frekvens") 
#Dette histogram viser en klar fejl i dataen, for kvmpriser <= 10

#Fjerne fejldataen under 10 i kvmpriser
boligsiden <- boligsiden[boligsiden$kvmpris >= 10, ] 

#opgave 2.2#################################################################
############################################################################
#Hvad er korrelationen mellem m2 og prisen for boliger lagt på boligsiden.dk?
# Giv en forklaring på begrebet korrelation.

#Korrelation mellem m^2 og prisen
Korrelation <- lm(boligsiden$størrelse ~ boligsiden$pris)
summary(Korrelation)

#opgave 2.3#################################################################
############################################################################
#Lav minimum 5 simple regressioner mellem pris pr. m2 og 5 andre variable

# Vi starter med simple lineære regressioner for at undersøge sammenhængen mellem kvmpris og de andre variabler
# 2.3.1 Kvmpris og Størrelse
hist(boligsiden$størrelse) #Histogram anvendes for at finde outliers
størrelse_test <- data.frame(boligsiden[boligsiden$størrelse <= 100, ]) # Fokusere ind på <100, for at finde de præcise punkt for datafejl
størrelse_test <- as.numeric(størrelse_test$størrelse)
hist(størrelse_test) #Her kan det ses at der er en outlier ved 10, så den fjernes grundet fejldata
boligsiden <- boligsiden[boligsiden$størrelse >= 10, ]

regPS <- lm(boligsiden$kvmpris ~ boligsiden$størrelse)
summary(regPS)
# Estimate: 0.058, P-værdi: 5.302e-08, Multiple R-squared: 0.024
# Der er en signifikant positiv sammenhæng mellem kvadratmeterpris og størrelsen af boligen

# 2.3.2 Kvmpris og Opført
unique(boligsiden$opført) #Her ses der 2 outliers fra årstal 1600 - 1699
#Efter vi fandt disse boliger via research, kan det ses at de er restaureret og derved retvisende at indkludere
regPO <- lm(boligsiden$kvmpris ~ boligsiden$opført)
summary(regPO)
# Estimate: -0.051, P-værdi: 9.943e-06, Multiple R-squared: 0.016
# Der er en signifikant negativ sammenhæng mellem byggeår og kvadratmeterpris

# 2.3.3 Kvmpris og Liggetid
# liggetid er unik, i og med at NA herunder er fra dem som ikke har været på siden i mere end 1 dag, så det ændres til 0
boligsiden$liggetid[is.na(boligsiden$liggetid)] <- 0
# Jeg fjerner "dag" fra liggetid, så det bliver numerisk
boligsiden$liggetid <- as.numeric(gsub("dag", "", boligsiden$liggetid))
regPL <- lm(boligsiden$kvmpris ~ boligsiden$liggetid)
summary(regPL)
# Estimate: 0.022, P-værdi: 0.7717, Multiple R-squared: 7.033e-05
# Der er ingen signifikant sammenhæng mellem liggetid og kvadratmeterpris, denne tages dog stadig for at perspektivere de andre til en ikke signifikant variable

# 2.3.4 Kvmpris og Postnummer
#Postnr
boxplot(boligsiden$postnr)
unique(boligsiden$postnr) #Her er det tydeligt at se at der er nogle klare outliers, eftersom et postnr ikke kan være <1000
boligsiden <- boligsiden[boligsiden$postnr >= 1000, ] 
#Fjerne alle rows med forkert postnr
boligsiden <- boligsiden[boligsiden$postnr <= 9990, ] # Samme som ovenstående, men over 9990
regPP <- lm(boligsiden$kvmpris ~ boligsiden$postnr)
summary(regPP)
# Estimate: -0.003, P-værdi: < 2.2e-16, Multiple R-squared: 0.2042
# Der er en signifikant negativ sammenhæng mellem postnummer og kvadratmeterpris

#2.3.5 Kvmpris og grund
#Grund
hist(boligsiden$grund)
grund_test <- data.frame(boligsiden[boligsiden$grund <= 100, ]) #For at se et fokuseret billede af <100
grund_test$grund <- as.numeric((grund_test$grund))
hist(grund_test$grund) #Her kan det ses at grunde under 10kvm udgør
boligsiden <- boligsiden[boligsiden$grund >= 10, ]
regPG <- lm(boligsiden$kvmpris ~ boligsiden$grund)
summary(regPG)
# Estimate: -0.006, P-værdi: < 0.00972, Multiple R-squared: 0.005567
# Der er en signifikant negativ sammenhæng mellem grundens størrelse og kvadratmeterpris

# Vi starter med simple lineære regressioner for at undersøge sammenhængen mellem kvmpris og de andre variabler
# 2.3.1 Kvmpris og Størrelse
hist(boligsiden$størrelse) #Histogram anvendes for at finde outliers
størrelse_test <- data.frame(boligsiden[boligsiden$størrelse <= 100, ]) # Fokusere ind på <100, for at finde de præcise punkt for datafejl
størrelse_test <- as.numeric(størrelse_test$størrelse)
hist(størrelse_test) #Her kan det ses at der er en outlier ved 10, så den fjernes grundet fejldata
boligsiden <- boligsiden[boligsiden$størrelse >= 10, ]

regPS <- lm(boligsiden$kvmpris ~ boligsiden$størrelse)
summary(regPS)
# Estimate: 0.058, P-værdi: 5.302e-08, Multiple R-squared: 0.024
# Der er en signifikant positiv sammenhæng mellem kvadratmeterpris og størrelsen af boligen

# 2.3.2 Kvmpris og Opført
unique(boligsiden$opført) #Her ses der 2 outliers fra årstal 1600 - 1699
#Efter vi fandt disse boliger via research, kan det ses at de er restaureret og derved retvisende at indkludere
regPO <- lm(boligsiden$kvmpris ~ boligsiden$opført)
summary(regPO)
# Estimate: -0.051, P-værdi: 9.943e-06, Multiple R-squared: 0.016
# Der er en signifikant negativ sammenhæng mellem byggeår og kvadratmeterpris

# 2.3.3 Kvmpris og Liggetid
# liggetid er unik, i og med at NA herunder er fra dem som ikke har været på siden i mere end 1 dag, så det ændres til 0
boligsiden$liggetid[is.na(boligsiden$liggetid)] <- 0
# Jeg fjerner "dag" fra liggetid, så det bliver numerisk
boligsiden$liggetid <- as.numeric(gsub("dag", "", boligsiden$liggetid))
regPL <- lm(boligsiden$kvmpris ~ boligsiden$liggetid)
summary(regPL)
# Estimate: 0.022, P-værdi: 0.7717, Multiple R-squared: 7.033e-05
# Der er ingen signifikant sammenhæng mellem liggetid og kvadratmeterpris, denne tages dog stadig for at perspektivere de andre til en ikke signifikant variable

# 2.3.4 Kvmpris og Postnummer
#Postnr
boxplot(boligsiden$postnr)
unique(boligsiden$postnr) #Her er det tydeligt at se at der er nogle klare outliers, eftersom et postnr ikke kan være <1000
boligsiden <- boligsiden[boligsiden$postnr >= 1000, ] 
#Fjerne alle rows med forkert postnr
boligsiden <- boligsiden[boligsiden$postnr <= 9990, ] # Samme som ovenstående, men over 9990
regPP <- lm(boligsiden$kvmpris ~ boligsiden$postnr)
summary(regPP)
# Estimate: -0.003, P-værdi: < 2.2e-16, Multiple R-squared: 0.2042
# Der er en signifikant negativ sammenhæng mellem postnummer og kvadratmeterpris

#2.3.5 Kvmpris og grund
#Grund
hist(boligsiden$grund)
grund_test <- data.frame(boligsiden[boligsiden$grund <= 100, ]) #For at se et fokuseret billede af <100
grund_test$grund <- as.numeric((grund_test$grund))
hist(grund_test$grund) #Her kan det ses at grunde under 10kvm udgør
boligsiden <- boligsiden[boligsiden$grund >= 10, ]
regPG <- lm(boligsiden$kvmpris ~ boligsiden$grund)
summary(regPG)
# Estimate: -0.006, P-værdi: < 0.00972, Multiple R-squared: 0.005567
# Der er en signifikant negativ sammenhæng mellem grundens størrelse og kvadratmeterpris
