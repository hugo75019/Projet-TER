#install.packages('FactoMineR',dep=TRUE)



library(FactoMineR)
library(dplyr)
library(ggplot2)

# Consumption data
Consumption = read.csv(file='C:/Users/Dell/Desktop/M1/TER/TER 9/Consumption.csv',sep=',',header = FALSE)
class(Consumption)
#Consumption = as.matrix(Consumption)
dim(as.matrix(Consumption))
names(Consumption)
Consumption[10]
plot(Consumption[10,],type='l')
plot(Consumption[20,], type='l')

# Weather data
Weather = read.table(file = 'C:/Users/Dell/Desktop/M1/TER/TER 9/weather.csv', sep = ',', header=TRUE)
Weather
dim(Weather)
names((Weather))
plot(Weather$Temperature, main='Temperature', type='l', col='red')
plot(Weather$Precipitation, main='Precipitation', type='l', col='blue')
summary(Weather)

# Type de jours
dayType = read.table(file='C:/Users/Dell/Desktop/M1/TER/TER 9/dayType.csv', sep=',', header=TRUE)

dim(dayType)
names(dayType)
head(dayType)
View(dayType)

# Dates
dayDates = read.table(file='C:/Users/Dell/Desktop/M1/TER/TER 9/dayDates.csv')
dates = read.table(file='C:/Users/Dell/Desktop/M1/TER/TER 9/dates.csv')

summary(Weather$Humidity)

datesweather<-bind_cols(dayDates,Weather)
View(dayDates)
View(dates)
View(Weather)
View(datesweather)

fullDatesInfos<-bind_cols(datesweather,dayType)
View(fullDatesInfos)
weekday<-filter(fullDatesInfos, Week==1)
weekday
summary(weekday[,1:4])

week_end<-filter(fullDatesInfos, Weekend==1)
week_end
summary(weekday[,1:4])


vacation<-filter(fullDatesInfos, Vacation==1)
vacation
summary(vacation[,1:4])

holidays<-filter(fullDatesInfos, Holiday==1)
holidays
summary(holidays[,1:4])

#different plot/boxplot
par(mfrow=c(2,2)) 
boxplot(fullDatesInfos$Temperature)
boxplot(fullDatesInfos$Precipitation) #trop de 0, jours sans pluie
boxplot(fullDatesInfos$Humidity)



#moyenne dayType
mean(fullDatesInfos$Temperature)
mean(fullDatesInfos$Humidity)
mean(fullDatesInfos$Precipitation)

# Affichage des histogrammes ET Quantiles
par(mfrow=c(2,2)) 

hist(fullDatesInfos$Temperature, col = "green")  # Pour tracer l'histogramme
abline(v = quantile(fullDatesInfos$Temperature), col = "red", lwd = 3) #Pour tracer la ligne 

hist(fullDatesInfos$Humidity, col = "green")  # Pour tracer l'histogramme
abline(v = quantile(fullDatesInfos$Humidity), col = "red", lwd = 3) #Pour tracer la ligne 

hist(fullDatesInfos$Precipitation, col = "green")  # Pour tracer l'histogramme
abline(v = quantile(fullDatesInfos$Precipitation), col = "red", lwd = 3) #Pour tracer la ligne 

## Pour weekday 
par(mfrow=c(2,2)) 
hist(weekday$Temperature, col = "green")  # Pour tracer l'histogramme
hist(weekday$Humidity, col = "green")  # Pour tracer l'histogramme
hist(weekday$Precipitation, col = "green")  # Pour tracer l'histogramme

##Pour le weekend
par(mfrow=c(2,2)) 
hist(week_end$Temperature, col = "green")  # Pour tracer l'histogramme
hist(week_end$Humidity, col = "green")  # Pour tracer l'histogramme
hist(week_end$Precipitation, col = "green")  # Pour tracer l'histogramme

## Holidays 
par(mfrow=c(2,2)) 
hist(holidays$Temperature, col = "green")  # Pour tracer l'histogramme
hist(holidays$Humidity, col = "green")  # Pour tracer l'histogramme
hist(holidays$Precipitation, col = "green")  # Pour tracer l'histogramme

## Vacation
par(mfrow=c(2,2)) 
hist(vacation$Temperature, col = "green")  # Pour tracer l'histogramme
hist(vacation$Humidity, col = "green")  # Pour tracer l'histogramme
hist(vacation$Precipitation, col = "green")  # Pour tracer l'histogramme

# Calcul et affichage du coefficient de cor?lation lin?aire
# ---
coeff <- cov(fullDatesInfos[,"Temperature"], fullDatesInfos[,"Vacation"]) / (sqrt(var(fullDatesInfos[,"Temperature"])*var(fullDatesInfos[,"Vacation"])))
paste("Coefficient de corr?lation lin?aire =", round(coeff,digits = 2))


#boxplot
par(mfrow=c(1,1)) 
boxplot(fullDatesInfos$Temperature ~ fullDatesInfos$V1)
boxplot(fullDatesInfos$Humidity ~ fullDatesInfos$V1)
boxplot(fullDatesInfos$Precipitation ~ fullDatesInfos$V1)

#Data consumption
View(Consumption)
names(Consumption)
dim(Consumption)
#Un tableau statistique 

## de minuit ? 6h du matin 
i<-24
matin_tot <- Consumption[,1:6]

while (i<8753){ 
  j<-i+1
  k<-i+6
  matin_tot <- rbind(matin_tot,Consumption[,j:k])
  i<-i+24 
  
}


View(matin_tot)
var.names <- names(Consumption)
# Initialisation de la table
caract.df <- data.frame()
# Pour chaque colonne, calcul de min, max, mean et ecart-type
for(strCol in var.names){
  caract.vect <- c(min(matin_tot[, strCol]), max(matin_tot[,strCol]), 
                   mean(matin_tot[,strCol]), sqrt(var(matin_tot[,strCol])))
  caract.df <- rbind.data.frame(caract.df, caract.vect)
}
# D?finition des row/colnames
rownames(caract.df) <- var.names
colnames(caract.df) <- c("Minimum", "Maximum", "Moyenne", "Ecart-type")
# Renvoyer la table
View(caract.df)

## de 6h ? 9h 
h1_pointe <- Consumption[,6:9]
View(h1_pointe)
var.names <- names(Consumption)
# Initialisation de la table
caract.df2 <- data.frame()
# Pour chaque colonne, calcul de min, max, mean et ecart-type
for(strCol in var.names){
  caract.vect2 <- c(min(h1_pointe[, strCol]), max(h1_pointe[,strCol]), 
                    mean(h1_pointe[,strCol]), sqrt(var(h1_pointe[,strCol])))
  caract.df2 <- rbind.data.frame(caract.df2, caract.vect2)
}
# D?finition des row/colnames
rownames(caract.df2) <- var.names
colnames(caract.df2) <- c("Minimum", "Maximum", "Moyenne", "Ecart-type")
# Renvoyer la table
View(caract.df2)

## de 9h ? 17h 
h2 <- Consumption[,9:17]
View(h2)
var.names <- names(Consumption)
# Initialisation de la table
caract.df3 <- data.frame()
# Pour chaque colonne, calcul de min, max, mean et ecart-type
for(strCol in var.names){
  caract.vect3 <- c(min(h2[, strCol]), max(h2[,strCol]), 
                    mean(h2[,strCol]), sqrt(var(h2[,strCol])))
  caract.df3 <- rbind.data.frame(caract.df3, caract.vect3)
}
# D?finition des row/colnames
rownames(caract.df3) <- var.names
colnames(caract.df3) <- c("Minimum", "Maximum", "Moyenne", "Ecart-type")
# Renvoyer la table
View(caract.df3)

## de 17h ? 21h 
h3 <- Consumption[,17:21]
var.names <- names(Consumption)
# Initialisation de la table
caract.df4 <- data.frame()
# Pour chaque colonne, calcul de min, max, mean et ecart-type
for(strCol in var.names){
  caract.vect4 <- c(min(h3[, strCol]), max(h3[,strCol]), 
                    mean(h3[,strCol]), sqrt(var(h3[,strCol])))
  caract.df4 <- rbind.data.frame(caract.df4, caract.vect4)
}
# D?finition des row/colnames
rownames(caract.df4) <- var.names
colnames(caract.df4) <- c("Minimum", "Maximum", "Moyenne", "Ecart-type")
# Renvoyer la table
View(caract.df4)


## de 21h ? minuit 

h4 <- Consumption[,21:24]
var.names <- names(Consumption)
# Initialisation de la table
caract.df5 <- data.frame()
# Pour chaque colonne, calcul de min, max, mean et ecart-type
for(strCol in var.names){
  caract.vect5 <- c(min(h4[, strCol]), max(h4[,strCol]), 
                    mean(h4[,strCol]), sqrt(var(h4[,strCol])))
  caract.df5 <- rbind.data.frame(caract.df5, caract.vect5)
}
# D?finition des row/colnames
rownames(caract.df5) <- var.names
colnames(caract.df5) <- c("Minimum", "Maximum", "Moyenne", "Ecart-type")
# Renvoyer la table
View(caract.df5)

View(Consumption)