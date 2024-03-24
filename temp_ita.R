setwd("C:\\UTENTE\\Desktop\\Tesi CLEBA\\R\\3BMeteo")

temp_ita.df <- read.csv("temp_ita.csv", sep=";")

attach(temp_ita.df)


library(dplyr) #%>%
library(tseries) #pacchetto analisi serie storiche
library(plotly) #zoom plot
library(lubridate) #seq
library(ggplot2) #violin plot
library(ggpubr) #ggarrange function
library(lmtest) #bptest
library(forecast) #previsioni

View(temp_ita.df)
names(temp_ita.df)
str(temp_ita.df)

temp_ita.df$data <- as.Date(temp_ita.df$data, format = "%d/%m/%Y")

summary(temp_ita.df)

capoluoghi <- data.frame(temp_ita.df %>% group_by(temp_ita.df$localita) %>% tally())[,1]
capoluoghi

giorni <- unique(temp_ita.df$data)
length(giorni)

Ancona <- temp_ita.df[which(temp_ita.df$localita=="Ancona"),][-c(1:2)]
Aosta <- temp_ita.df[which(temp_ita.df$localita=="Aosta"),][-c(1:2)]
Bari <- temp_ita.df[which(temp_ita.df$localita=="Bari"),][-c(1:2)]
Bologna <- temp_ita.df[which(temp_ita.df$localita=="Bologna"),][-c(1:2)]
Cagliari <- temp_ita.df[which(temp_ita.df$localita=="Cagliari"),][-c(1:2)]
Campobasso <- temp_ita.df[which(temp_ita.df$localita=="Campobasso"),][-c(1:2)]
Catanzaro <- temp_ita.df[which(temp_ita.df$localita=="Catanzaro"),][-c(1:2)]
Firenze <- temp_ita.df[which(temp_ita.df$localita=="Firenze"),][-c(1:2)]
Genova <- temp_ita.df[which(temp_ita.df$localita=="Genova"),][-c(1:2)]
L.Aquila <- temp_ita.df[which(temp_ita.df$localita=="L'Aquila"),][-c(1:2)]
Milano <- temp_ita.df[which(temp_ita.df$localita=="Milano"),][-c(1:2)]
Napoli <- temp_ita.df[which(temp_ita.df$localita=="Napoli"),][-c(1:2)]
Palermo <- temp_ita.df[which(temp_ita.df$localita=="Palermo"),][-c(1:2)]
Perugia <- temp_ita.df[which(temp_ita.df$localita=="Perugia"),][-c(1:2)]
Potenza <- temp_ita.df[which(temp_ita.df$localita=="Potenza"),][-c(1:2)]
Roma <- temp_ita.df[which(temp_ita.df$localita=="Roma"),][-c(1:2)]
Torino <- temp_ita.df[which(temp_ita.df$localita=="Torino"),][-c(1:2)]
Trento <- temp_ita.df[which(temp_ita.df$localita=="Trento"),][-c(1:2)]
Trieste <- temp_ita.df[which(temp_ita.df$localita=="Trieste"),][-c(1:2)]
Venezia <- temp_ita.df[which(temp_ita.df$localita=="Venezia"),][-c(1:2)]

################################################################################

temp_min.ita <- data.frame(giorni, Ancona$temperatura_min, Aosta$temperatura_min,
                       Bari$temperatura_min, Bologna$temperatura_min, Cagliari$temperatura_min,
                       Campobasso$temperatura_min, Catanzaro$temperatura_min, Firenze$temperatura_min,
                       Genova$temperatura_min, L.Aquila$temperatura_min, Milano$temperatura_min,
                       Napoli$temperatura_min, Palermo$temperatura_min, Perugia$temperatura_min,
                       Potenza$temperatura_min, Roma$temperatura_min, Torino$temperatura_min,
                       Trento$temperatura_min, Trieste$temperatura_min, Venezia$temperatura_min) 
  
temp_media.ita <- data.frame(giorni, Ancona$temperatura_media, Aosta$temperatura_media,
                             Bari$temperatura_media, Bologna$temperatura_media, Cagliari$temperatura_media,
                             Campobasso$temperatura_media, Catanzaro$temperatura_media, Firenze$temperatura_media,
                             Genova$temperatura_media, L.Aquila$temperatura_media, Milano$temperatura_media,
                             Napoli$temperatura_media, Palermo$temperatura_media, Perugia$temperatura_media,
                             Potenza$temperatura_media, Roma$temperatura_media, Torino$temperatura_media,
                             Trento$temperatura_media, Trieste$temperatura_media, Venezia$temperatura_media)
  
temp_max.ita <- data.frame(giorni, Ancona$temperatura_max, Aosta$temperatura_max,
                       Bari$temperatura_max, Bologna$temperatura_max, Cagliari$temperatura_max,
                       Campobasso$temperatura_max, Catanzaro$temperatura_max, Firenze$temperatura_max,
                       Genova$temperatura_max, L.Aquila$temperatura_max, Milano$temperatura_max,
                       Napoli$temperatura_max, Palermo$temperatura_max, Perugia$temperatura_max,
                       Potenza$temperatura_max, Roma$temperatura_max, Torino$temperatura_max,
                       Trento$temperatura_max, Trieste$temperatura_max, Venezia$temperatura_max)


names(temp_min.ita) <- c("Data",capoluoghi)
names(temp_max.ita) <- c("Data",capoluoghi)
names(temp_media.ita) <- c("Data",capoluoghi)


View(temp_min.ita)
View(temp_media.ita)
View(temp_max.ita)


################################################################################
#suddivisione nord - centro - sud

nord.min <- temp_min.ita[c("Data","Bologna","Trieste","Genova","Milano","Torino",
                           "Trento","Aosta","Venezia")]

nord.media <- temp_media.ita[c("Data","Bologna","Trieste","Genova","Milano","Torino",
                             "Trento","Aosta","Venezia")]

nord.max <- temp_max.ita[c("Data","Bologna","Trieste","Genova","Milano","Torino",
                             "Trento","Aosta","Venezia")]

centro.min <- temp_min.ita[c("Data","Roma","Ancona","Firenze","Perugia")]

centro.media <- temp_media.ita[c("Data","Roma","Ancona","Firenze","Perugia")]
  
centro.max <- temp_max.ita[c("Data","Roma","Ancona","Firenze","Perugia")]

sud.min <- temp_min.ita[c("Data","L'Aquila","Potenza","Catanzaro","Napoli",
                          "Campobasso","Bari","Cagliari","Palermo")]

sud.media <- temp_media.ita[c("Data","L'Aquila","Potenza","Catanzaro","Napoli",
                              "Campobasso","Bari","Cagliari","Palermo")]

sud.max <- temp_max.ita[c("Data","L'Aquila","Potenza","Catanzaro","Napoli",
                          "Campobasso","Bari","Cagliari","Palermo")]

#calcolo media per riga

mean.min.sud <- rowMeans(sud.min[c(2:9)])
mean.min.sud_df <- data.frame(sud.min$Data,mean.min.sud)
names(mean.min.sud_df) <- c("Data","Mean Min Sud")

  
mean.media.sud <- rowMeans(sud.media[c(2:9)])
mean.media.sud_df <- data.frame(sud.media$Data,mean.media.sud)
names(mean.media.sud_df) <- c("Data","Mean Media Sud")
  
mean.max.sud <- rowMeans(sud.max[c(2:9)])
mean.max.sud_df <- data.frame(sud.max$Data,mean.max.sud)
names(mean.max.sud_df) <- c("Data","Mean Max Sud")
  
mean.min.centro <- rowMeans(centro.min[c(2:5)])
mean.min.centro_df <- data.frame(centro.min$Data,mean.min.centro)
names(mean.min.centro_df) <- c("Data","Mean Min Centro")
  
mean.media.centro <- rowMeans(centro.media[c(2:5)])
mean.media.centro_df <- data.frame(centro.media$Data,mean.media.centro)
names(mean.media.centro_df) <- c("Data","Mean Media Centro")
  
mean.max.centro <- rowMeans(centro.max[c(2:5)])
mean.max.centro_df <- data.frame(centro.max$Data,mean.max.centro)
names(mean.max.centro_df) <- c("Data","Mean Max Centro")

mean.min.nord <- rowMeans(nord.min[c(2:9)])
mean.min.nord_df <- data.frame(nord.min$Data,mean.min.nord)
names(mean.min.nord_df) <- c("Data","Mean Min Nord")
  
mean.media.nord <- rowMeans(nord.media[c(2:9)])
mean.media.nord_df <- data.frame(nord.media$Data,mean.media.nord)
names(mean.media.nord_df) <- c("Data","Mean Media Nord")

mean.max.nord <- rowMeans(nord.max[c(2:9)])
mean.max.nord_df <- data.frame(nord.max$Data,mean.max.nord)
names(mean.max.nord_df) <- c("Data","Mean Max Nord")

#creazione df per temperature

mean.min.ita <- data.frame(temp_ita.df$data,mean.min.sud,mean.min.centro,mean.min.nord)
mean.media.ita <- data.frame(temp_ita.df$data,mean.media.sud,mean.media.centro,mean.media.nord)
mean.max.ita <- data.frame(temp_ita.df$data,mean.max.sud,mean.max.centro,mean.max.nord)

names(mean.min.ita) <- c("Giorno","Sud","Centro","Nord")
names(mean.media.ita) <- c("Giorno","Sud","Centro","Nord")
names(mean.max.ita) <- c("Giorno","Sud","Centro","Nord")

################################################################################

summary(mean.min.ita)
summary(mean.media.ita)
summary(mean.max.ita)

#troviamo le date dove, mediamente, si sono registrate le temperature minime e maggiorni
#sulle variabili di temperatura minima, media e massima


#giorno con temperatura minima più bassa per il Sud
giorno_min_sud <- mean.min.ita$Giorno[which.min(mean.min.ita$Sud)]

#giorno con temperatura minima più bassa per il Centro
giorno_min_centro <- mean.min.ita$Giorno[which.min(mean.min.ita$Centro)]

#giorno con temperatura minima più bassa per il Nord
giorno_min_nord <- mean.min.ita$Giorno[which.min(mean.min.ita$Nord)]


cat("Il giorno", as.character(giorno_min_sud), "il Sud ha registrato, in media, il valore più basso sulla temperatura minima pari a ", min(mean.min.ita$Sud))
cat("Il giorno", as.character(giorno_min_centro), "il Centro ha registrato, in media, il valore più basso sulla temperatura minima pari a ", min(mean.min.ita$Centro))
cat("Il giorno", as.character(giorno_min_nord), "il Nord ha registrato, in media, il valore più basso sulla temperatura minima pari a ", min(mean.min.ita$Nord))

#giorno con temperatura massima più alta per il Sud
giorno_max_sud <- mean.max.ita$Giorno[which.max(mean.max.ita$Sud)]

#giorno con temperatura massima più alta per il Centro
giorno_max_centro <- mean.max.ita$Giorno[which.max(mean.max.ita$Centro)]

#giorno con temperatura massima più alta per il Nord
giorno_max_nord <- mean.max.ita$Giorno[which.max(mean.max.ita$Nord)]

cat("Il giorno", as.character(giorno_max_sud), "il Sud ha registrato, in media, il valore più alta sulla temperatura massima pari a ", max(mean.max.ita$Sud))
cat("Il giorno", as.character(giorno_max_centro), "il Centro ha registrato, in media, il valore più alto sulla temperatura massima pari a ", max(mean.max.ita$Centro))
cat("Il giorno", as.character(giorno_max_nord), "il Nord ha registrato, in media, il valore più alta sulla temperatura massima pari a ", max(mean.max.ita$Nord))
###############################################################################

summary(mean.min.ita)
summary(mean.media.ita)
summary(mean.max.ita)


###############################################################################
#boxplot

par(mfrow=c(1,3))
boxplot(mean.min.ita$Sud, col="red", main="Temperatura Minima SUD", ylim=c(-10,30), ylab="Gradi(°C)")
boxplot(mean.min.ita$Centro, col="green",main="Temperatura Minima CENTRO", ylim=c(-10,30), ylab="Gradi(°C)")
boxplot(mean.min.ita$Nord, col="blue",main="Temperatura Minima NORD", ylim=c(-10,30), ylab="Gradi(°C)")

par(mfrow=c(1,3))
boxplot(mean.media.ita$Sud, col="red", main="Temperatura Media SUD", ylim=c(-5,35), ylab="Gradi(°C)")
boxplot(mean.media.ita$Centro, col="green",main="Temperatura Media CENTRO", ylim=c(-5,35), ylab="Gradi(°C)")
boxplot(mean.media.ita$Nord, col="blue",main="Temperatura Media NORD", ylim=c(-5,35), ylab="Gradi(°C)")

par(mfrow=c(1,3))
boxplot(mean.max.ita$Sud, col="red", main="Temperatura Massima SUD", ylim=c(-5,40), ylab="Gradi(°C)")
boxplot(mean.max.ita$Centro, col="green",main="Temperatura Massima CENTRO", ylim=c(-5,40), ylab="Gradi(°C)")
boxplot(mean.max.ita$Nord, col="blue",main="Temperatura Massima NORD", ylim=c(-5,40), ylab="Gradi(°C)")


################################################################################
#boxplot per year
par(mfrow=c(3,1))
boxplot(mean.min.ita$Sud ~ 
        reorder(format(mean.min.ita$Giorno, '%y'),
        mean.min.ita$Giorno), col="red",
        main ="Temperatura Minima SUD", xlab="Anni", ylab = "Gradi(°C)") 

boxplot(mean.min.ita$Centro ~ 
          reorder(format(mean.min.ita$Giorno, '%y'),
                  mean.min.ita$Giorno), col="green",
        main ="Temperatura Minima CENTRO", xlab="Anni", ylab = "Gradi(°C)") 

boxplot(mean.min.ita$Nord ~ 
          reorder(format(mean.min.ita$Giorno, '%y'),
                  mean.min.ita$Giorno), col="blue",
        main ="Temperatura Minima NORD", xlab="Anni", ylab = "Gradi(°C)") 



par(mfrow=c(3,1))
boxplot(mean.media.ita$Sud ~ 
          reorder(format(mean.media.ita$Giorno, '%y'),
                  mean.media.ita$Giorno), col="red",
        main ="Temperatura Media SUD", xlab="Anni", ylab = "Gradi(°C)") 

boxplot(mean.media.ita$Centro ~ 
          reorder(format(mean.media.ita$Giorno, '%y'),
                  mean.media.ita$Giorno), col="green",
        main ="Temperatura Media CENTRO", xlab="Anni", ylab = "Gradi(°C)") 

boxplot(mean.media.ita$Nord ~ 
          reorder(format(mean.media.ita$Giorno, '%y'),
                  mean.media.ita$Giorno), col="blue",
        main ="Temperatura Media NORD", xlab="Anni", ylab = "Gradi(°C)") 




par(mfrow=c(3,1))
boxplot(mean.max.ita$Sud ~ 
          reorder(format(mean.max.ita$Giorno, '%y'),
                  mean.max.ita$Giorno), col="red",
        main ="Temperatura Massima SUD", xlab="Anni", ylab = "Gradi(°C)") 

boxplot(mean.max.ita$Centro ~ 
          reorder(format(mean.max.ita$Giorno, '%y'),
                  mean.max.ita$Giorno), col="green",
        main ="Temperatura Massima CENTRO", xlab="Anni", ylab = "Gradi(°C)") 

boxplot(mean.max.ita$Nord ~ 
          reorder(format(mean.max.ita$Giorno, '%y'),
                  mean.max.ita$Giorno), col="blue",
        main ="Temperatura Massima NORD", xlab="Anni", ylab = "Gradi(°C)") 


################################################################################
#istogrammi


#istogrammi temp minime
par(mfrow=c(1,3))
hist(mean.min.sud_df$`Mean Min Sud`, breaks = 7, probability = T, col="red",
     ylim=c(0,0.06), main = "Temperature Minime SUD", xlab="Gradi(°C)")
lines(density(mean.min.sud_df$`Mean Min Sud`), lwd=2)


hist(mean.min.centro_df$`Mean Min Centro`, breaks = 7, probability = T, col="green",
     ylim=c(0,0.06), main = "Temperature Minime CENTRO", xlab="Gradi(°C)")
lines(density(mean.min.centro_df$`Mean Min Centro`), lwd=2)

hist(mean.min.nord_df$`Mean Min Nord`, breaks = 7, probability = T, col="blue",
     ylim=c(0,0.06), main = "Temperature Minime NORD", xlab="Gradi(°C)")
lines(density(mean.min.nord_df$`Mean Min Nord`), lwd=2)



#istogrammi temp massime
par(mfrow=c(1,3))
hist(mean.max.sud_df$`Mean Max Sud`, breaks = 7, probability = T, col="red",
     ylim=c(0,0.05), main = "Temperature Massime SUD", xlab="Gradi(°C)")
lines(density(mean.max.sud_df$`Mean Max Sud`), lwd=2)


hist(mean.max.centro_df$`Mean Max Centro`, breaks = 7, probability = T, col="green",
     ylim=c(0,0.05), main = "Temperature Massime CENTRO", xlab="Gradi(°C)")
lines(density(mean.max.centro_df$`Mean Max Centro`), lwd=2)

hist(mean.max.nord_df$`Mean Max Nord`, breaks = 7, probability = T, col="blue",
     ylim=c(0,0.05), main = "Temperature Massime NORD", xlab="Gradi(°C)")
lines(density(mean.max.nord_df$`Mean Max Nord`), lwd=2)

#########################################################



################################################################################
#Boxplot per anno (temperature minime)

par(mfrow=c(1,3))
#sud
boxplot(mean.min.sud_df$`Mean Min Sud` ~ reorder(format(mean.min.sud_df$Data,'%Y'), mean.min.sud_df$Data), outline = T, 
        main="BoxPlot Temperature Minime SUD", xlab = "Anno", ylab="Gradi (°C)", col=rainbow(16))

#centro
boxplot(mean.min.centro_df$`Mean Min Centro` ~ reorder(format(mean.min.centro_df$Data,'%Y'), mean.min.centro_df$Data), outline = T, 
        main="BoxPlot Temperature Minime CENTRO", xlab = "Anno", ylab="Gradi (°C)", col=rainbow(16))

#nord
boxplot(mean.min.nord_df$`Mean Min Nord` ~ reorder(format(mean.min.nord_df$Data,'%Y'), mean.min.nord_df$Data), outline = T, 
        main="BoxPlot Temperature Minime NORD", xlab = "Anno", ylab="Gradi (°C)", col=rainbow(16))

################################################################################
#Boxplot per anno (temperature medie)

par(mfrow=c(1,3))
#sud
boxplot(mean.media.sud_df$`Mean Media Sud` ~ reorder(format(mean.media.sud_df$Data,'%Y'), mean.media.sud_df$Data), outline = T, 
        main="BoxPlot Temperature Medie SUD", xlab = "Anno", ylab="Gradi (°C)", col=rainbow(16))

#centro
boxplot(mean.media.centro_df$`Mean Media Centro` ~ reorder(format(mean.media.centro_df$Data,'%Y'), mean.media.centro_df$Data), outline = T, 
        main="BoxPlot Temperature Medie CENTRO", xlab = "Anno", ylab="Gradi (°C)", col=rainbow(16))

#nord
boxplot(mean.media.nord_df$`Mean Media Nord` ~ reorder(format(mean.media.nord_df$Data,'%Y'), mean.media.nord_df$Data), outline = T, 
        main="BoxPlot Temperature Medie NORD", xlab = "Anno", ylab="Gradi (°C)", col=rainbow(16))

################################################################################
#Boxplot per anno (temperature massime)

par(mfrow=c(1,3))
#sud
boxplot(mean.max.sud_df$`Mean Max Sud` ~ reorder(format(mean.max.sud_df$Data,'%Y'), mean.max.sud_df$Data), outline = T, 
        main="BoxPlot Temperature Massime SUD", xlab = "Anno", ylab="Gradi (°C)", col=rainbow(16))

#centro
boxplot(mean.max.centro_df$`Mean Max Centro` ~ reorder(format(mean.max.centro_df$Data,'%Y'), mean.max.centro_df$Data), outline = T, 
        main="BoxPlot Temperature Massime CENTRO", xlab = "Anno", ylab="Gradi (°C)", col=rainbow(16))

#nord
boxplot(mean.max.nord_df$`Mean Max Nord` ~ reorder(format(mean.max.nord_df$Data,'%Y'), mean.max.nord_df$Data), outline = T, 
        main="BoxPlot Temperature Massime NORD", xlab = "Anno", ylab="Gradi (°C)", col=rainbow(16))

################################################################################
#violin plot per anno




# Estrai l'anno dalla colonna "Data"
Year <- year(as.Date(mean.min.sud_df$Data, format = "%Y-%m-%d"))

# temperature minime sud
violin.min.sud <-  ggplot(mean.min.sud_df, aes(x = factor(Year), y = `Mean Min Sud`)) +
                geom_violin(trim = FALSE, fill="red") +
                geom_boxplot(width = 0.2, fill = "white") +
                labs(x = "Anno", y = "Gradi (°C)", title = "Temperature Minime SUD") +
                theme_minimal()



# temperature minime centro
violin.min.centro <- ggplot(mean.min.centro_df, aes(x = factor(Year), y = `Mean Min Centro`)) +
                geom_violin(trim = FALSE, fill="green") +
                geom_boxplot(width = 0.2, fill = "white") +
                labs(x = "Anno", y = "Gradi (°C)", title = "Temperature Minime CENTRO") +
                theme_minimal()


# temperature minime nord
violin.min.nord <- ggplot(mean.min.nord_df, aes(x = factor(Year), y = `Mean Min Nord`)) +
  geom_violin(trim = FALSE, fill="blue") +
  geom_boxplot(width = 0.2, fill = "white") +
  labs(x = "Anno", y = "Gradi (°C)", title = "Temperature Minime NORD") +
  theme_minimal()


ggarrange(violin.min.sud,violin.min.centro, violin.min.nord)

##########################################################################

# Estrai l'anno dalla colonna "Data"

# temperature massime sud
violin.max.sud <-  ggplot(mean.max.sud_df, aes(x = factor(Year), y = `Mean Max Sud`)) +
  geom_violin(trim = FALSE, fill="red") +
  geom_boxplot(width = 0.2, fill = "white") +
  labs(x = "Anno", y = "Gradi (°C)", title = "Temperature Massime SUD") +
  theme_minimal()


# temperature massime centro
violin.max.centro <- ggplot(mean.max.centro_df, aes(x = factor(Year), y = `Mean Max Centro`)) +
  geom_violin(trim = FALSE, fill="green") +
  geom_boxplot(width = 0.2, fill = "white") +
  labs(x = "Anno", y = "Gradi (°C)", title = "Temperature Massime CENTRO") +
  theme_minimal()


# temperature massime nord
violin.max.nord <- ggplot(mean.max.nord_df, aes(x = factor(Year), y = `Mean Max Nord`)) +
  geom_violin(trim = FALSE, fill="blue") +
  geom_boxplot(width = 0.2, fill = "white") +
  labs(x = "Anno", y = "Gradi (°C)", title = "Temperature Massime NORD") +
  theme_minimal()


ggarrange(violin.max.sud,violin.max.centro, violin.max.nord)
########################################################################
#test normalita
mean.min.sud.sample <- mean.min.sud_df[sample(nrow(mean.min.sud_df), 5000),]$`Mean Min Sud`
mean.medie.sud.sample <- mean.media.sud_df[sample(nrow(mean.media.sud_df), 5000),]$`Mean Media Sud`
mean.max.sud.sample <- mean.max.sud_df[sample(nrow(mean.max.sud_df), 5000),]$`Mean Max Sud`

mean.min.centro.sample <- mean.min.centro_df[sample(nrow(mean.min.centro_df), 5000),]$`Mean Min Centro`
mean.medie.centro.sample <- mean.media.centro_df[sample(nrow(mean.media.centro_df), 5000),]$`Mean Media Centro`
mean.max.centro.sample <- mean.max.centro_df[sample(nrow(mean.max.centro_df), 5000),]$`Mean Max Centro`

mean.min.nord.sample <- mean.min.nord_df[sample(nrow(mean.min.nord_df), 5000),]$`Mean Min Nord`
mean.medie.nord.sample <- mean.media.nord_df[sample(nrow(mean.media.nord_df), 5000),]$`Mean Media Nord`
mean.max.nord.sample <- mean.max.nord_df[sample(nrow(mean.max.nord_df), 5000),]$`Mean Max Nord`

shapiro.test(mean.min.sud.sample)
shapiro.test(mean.medie.sud.sample)
shapiro.test(mean.max.sud.sample)

shapiro.test(mean.min.centro.sample)
shapiro.test(mean.medie.centro.sample)
shapiro.test(mean.max.centro.sample)

shapiro.test(mean.min.nord.sample)
shapiro.test(mean.medie.nord.sample)
shapiro.test(mean.max.nord.sample)


#########################################################################
par(mfrow=c(1,3))
qqnorm(mean.min.sud_df$`Mean Min Sud`, col="red", main="Temperature Minime SUD")
qqline(mean.min.sud_df$`Mean Min Sud`, lwd=2)

qqnorm(mean.min.centro_df$`Mean Min Centro`,col="green", main="Temperature Minime CENTRO")
qqline(mean.min.centro_df$`Mean Min Centro`, lwd=2)

qqnorm(mean.min.nord_df$`Mean Min Nord`,col="blue", main="Temperature Minime NORD")
qqline(mean.min.nord_df$`Mean Min Nord`, lwd=2)



par(mfrow=c(1,3))
qqnorm(mean.max.sud_df$`Mean Max Sud`, col="red", main="Temperature Massime SUD")
qqline(mean.max.sud_df$`Mean Max Sud`, lwd=2)

qqnorm(mean.max.centro_df$`Mean Max Centro`,col="green", main="Temperature Massime CENTRO")
qqline(mean.max.centro_df$`Mean Max Centro`, lwd=2)

qqnorm(mean.max.nord_df$`Mean Max Nord`,col="blue", main="Temperature Massime NORD")
qqline(mean.max.nord_df$`Mean Max Nord`, lwd=2)


################################################################################
#serie storica

mean.max.ita$Anno <- year(mean.max.ita$Giorno)

#temp max

#Medie annuali
media_annuale.max.sud <- mean.max.ita %>%
  group_by(Anno) %>%
  summarise(Media = mean(Sud, na.rm = TRUE))

media_annuale.max.centro <- mean.max.ita %>%
  group_by(Anno) %>%
  summarise(Media = mean(Centro, na.rm = TRUE))

media_annuale.max.nord <- mean.max.ita %>%
  group_by(Anno) %>%
  summarise(Media = mean(Nord, na.rm = TRUE))

View(media_annuale.max.nord)
View(media_annuale.max.centro)
View(media_annuale.max.sud)

media_annuale.max.ita <- cbind(media_annuale.max.nord,media_annuale.max.centro[-c(1)],media_annuale.max.sud[-c(1)])
names(media_annuale.max.ita) <- c("Anno","Nord","Centro","Sud")
View(media_annuale.max.ita)

par(mfrow=c(3,1))
plot(media_annuale.max.nord$Media, col="blue", main="Temperature massime NORD",
     type = "o", lwd=2, xlab="Anni", ylab="Gradi(°)", xaxt="n")
axis(1, at=1:16, labels=c(2008:2023))
plot(media_annuale.max.centro$Media, col="green", main="Temperature massime CENTRO", 
     type = "o", lwd=2, xlab="Anni", ylab="Gradi(°)", xaxt="n")
axis(1, at=1:16, labels=c(2008:2023))
plot(media_annuale.max.sud$Media, col="red", main="Temperature massime SUD", 
     type = "o", lwd=2, xlab="Anni", ylab="Gradi(°)", xaxt="n")
axis(1, at=1:16, labels=c(2008:2023))

###########################################################################
mean.min.ita$Anno <- year(mean.min.ita$Giorno)

#Medie annuali
media_annuale.min.sud <- mean.min.ita %>%
  group_by(Anno) %>%
  summarise(Media = mean(Sud, na.rm = TRUE))

media_annuale.min.centro <- mean.min.ita %>%
  group_by(Anno) %>%
  summarise(Media = mean(Centro, na.rm = TRUE))

media_annuale.min.nord <- mean.min.ita %>%
  group_by(Anno) %>%
  summarise(Media = mean(Nord, na.rm = TRUE))

View(media_annuale.min.nord)
View(media_annuale.min.centro)
View(media_annuale.min.sud)

media_annuale.min.ita <- cbind(media_annuale.min.nord,media_annuale.min.centro[-c(1)],media_annuale.min.sud[-c(1)])
names(media_annuale.min.ita) <- c("Anno","Nord","Centro","Sud")
View(media_annuale.min.ita)

par(mfrow=c(3,1))
plot(media_annuale.min.nord$Media, col="blue", main="Temperature minime NORD",
     type = "o", lwd=2, xlab="Anni", ylab="Gradi(°)", xaxt="n")
axis(1, at=1:16, labels=c(2008:2023))
plot(media_annuale.min.centro$Media, col="green", main="Temperature minime CENTRO", 
     type = "o", lwd=2, xlab="Anni", ylab="Gradi(°)", xaxt="n")
axis(1, at=1:16, labels=c(2008:2023))
plot(media_annuale.min.sud$Media, col="red", main="Temperature minime SUD", 
     type = "o", lwd=2, xlab="Anni", ylab="Gradi(°)", xaxt="n")
axis(1, at=1:16, labels=c(2008:2023))


################################################################################
#plot zoom medie nord-centro-sud per giorno


plot_ly(mean.max.ita, x = mean.max.ita$Giorno, type = "scatter", mode = "lines+text") %>%
  add_trace(y = mean.max.ita$Sud, name = 'Sud', line=list(color="red"))%>%
  add_trace(y = mean.max.ita$Centro, name = 'Centro', line=list(color="green"))%>%
  add_trace(y = mean.max.ita$Nord, name = 'Nord', line=list(color="blue"))%>%
  layout(xaxis = list(title = "Anni"), yaxis = list(title = "Gradi (°C)")) %>%
  config(displayModeBar = TRUE)



################################################################################
#calcolo media temperatura per anno

anni <- format(mean.min.ita.month$Mese, "%Y")
years_seq <- seq(from = ymd("2008-01-01"), to = ymd("2023-12-31"), by = "years")

mean.min.sud.year <- aggregate(Sud ~ anni, data = mean.min.ita.month, FUN = mean)
mean.min.centro.year <- aggregate(Centro ~ anni, data = mean.min.ita.month, FUN = mean)
mean.min.nord.year <- aggregate(Nord ~ anni, data = mean.min.ita.month, FUN = mean)

mean.min.ita.year <- data.frame(anni,mean.min.sud.year$Sud,mean.min.centro.year$Centro,mean.min.nord.year$Nord)
names(mean.min.ita.year) <- c("Anni","Sud","Centro","Nord")
mean.min.ita.year <- mean.min.ita.year[1:16,] #dal 2008 al 2023 (16)
mean.min.ita.year$Anni <- years_seq


mean.media.sud.year <- aggregate(Sud ~ anni, data = mean.media.ita.month, FUN = mean)
mean.media.centro.year <- aggregate(Centro ~ anni, data = mean.media.ita.month, FUN = mean)
mean.media.nord.year <- aggregate(Nord ~ anni, data = mean.media.ita.month, FUN = mean)

mean.media.ita.year <- data.frame(anni,mean.media.sud.year$Sud,mean.media.centro.year$Centro,mean.media.nord.year$Nord)
names(mean.media.ita.year) <- c("Anni","Sud","Centro","Nord")
mean.media.ita.year <- mean.media.ita.year[1:16,]
mean.media.ita.year$Anni <- years_seq


mean.max.sud.year <- aggregate(Sud ~ anni, data = mean.max.ita.month, FUN = mean)
mean.max.centro.year <- aggregate(Centro ~ anni, data = mean.max.ita.month, FUN = mean)
mean.max.nord.year <- aggregate(Nord ~ anni, data = mean.max.ita.month, FUN = mean)

mean.max.ita.year <- data.frame(anni,mean.max.sud.year$Sud,mean.max.centro.year$Centro,mean.max.nord.year$Nord)
names(mean.max.ita.year) <- c("Anni","Sud","Centro","Nord")
mean.max.ita.year <- mean.max.ita.year[1:16,] 
mean.max.ita.year$Anni <- years_seq


View(mean.min.ita.year)
View(mean.media.ita.year)
View(mean.max.ita.year)


################################################################################
#plot zoom medie nord-centro-sud per anno

plot_ly(mean.max.ita.year, x = mean.max.ita.year$Anni, type = "scatter", mode = "lines") %>%
  add_trace(y = mean.max.ita.year$Sud, name = 'Sud', line=list(color="red"))%>%
  add_trace(y = mean.max.ita.year$Centro, name = 'Centro', line=list(color="green"))%>%
  add_trace(y = mean.max.ita.year$Nord, name = 'Nord', line=list(color="blue"))%>%
  layout(xaxis = list(title = "Anni"), yaxis = list(title = "Gradi (°C)")) %>%
  config(displayModeBar = TRUE)


####################################################################
#studio correlazione
#scatter plots temp min

par(mfrow=c(1,3))
plot(mean.min.sud_df$`Mean Min Sud`,mean.min.centro_df$`Mean Min Centro`,
     col=c("red","green"), main ="Temperature Minime SUD-CENTRO",
     pch = 20,
     xlab="Gradi(°C) - SUD",
     ylab="Gradi(°C) - CENTRO")

plot(mean.min.centro_df$`Mean Min Centro`,mean.min.nord_df$`Mean Min Nord`,
     col=c("green","blue"), main ="Temperature Minime CENTRO-NORD",
     pch = 20,
     xlab="Gradi(°C) - CENTRO",
     ylab="Gradi(°C) - NORD")

plot(mean.min.sud_df$`Mean Min Sud`,mean.min.nord_df$`Mean Min Nord`,
     col=c("red","blue"), main ="Temperature Minime SUD-NORD",
     pch = 20,
     xlab="Gradi(°C) - SUD",
     ylab="Gradi(°C) - NORD")

cor.temp.min.sud.centro <- cor.test(mean.min.sud_df$`Mean Min Sud`,mean.min.centro_df$`Mean Min Centro`)
cor.temp.min.centro.nord <- cor.test(mean.min.centro_df$`Mean Min Centro`,mean.min.nord_df$`Mean Min Nord`)
cor.temp.min.sud.nord <- cor.test(mean.min.sud_df$`Mean Min Sud`,mean.min.nord_df$`Mean Min Nord`)

cor.temp.min.sud.centro$estimate
cor.temp.min.centro.nord$estimate
cor.temp.min.sud.nord$estimate

#scatter plots temp max
par(mfrow=c(1,3))
plot(mean.max.sud_df$`Mean Max Sud`,mean.max.centro_df$`Mean Max Centro`,
     col=c("red","green"), main ="Temperature Massime SUD-CENTRO",
     pch = 20,
     xlab="Gradi(°C) - SUD",
     ylab="Gradi(°C) - CENTRO")

plot(mean.max.centro_df$`Mean Max Centro`,mean.max.nord_df$`Mean Max Nord`,
     col=c("green","blue"), main ="Temperature Massime CENTRO-NORD",
     pch = 20,
     xlab="Gradi(°C) - CENTRO",
     ylab="Gradi(°C) - NORD")

plot(mean.max.sud_df$`Mean Max Sud`,mean.max.nord_df$`Mean Max Nord`,
     col=c("red","blue"), main ="Temperature Massime SUD-NORD",
     pch = 20,
     xlab="Gradi(°C) - SUD",
     ylab="Gradi(°C) - NORD")

cor.temp.max.sud.centro <- cor.test(mean.max.sud_df$`Mean Max Sud`,mean.max.centro_df$`Mean Max Centro`)
cor.temp.max.centro.nord <- cor.test(mean.max.centro_df$`Mean Max Centro`,mean.max.nord_df$`Mean Max Nord`)
cor.temp.max.sud.nord <- cor.test(mean.max.sud_df$`Mean Max Sud`,mean.max.nord_df$`Mean Max Nord`)

cor.temp.max.sud.centro$estimate
cor.temp.max.centro.nord$estimate
cor.temp.max.sud.nord$estimate

###############################################################################
#CREAZIONE MODELLO DI PREVISIONE

#temperature italiane minime
temp.min.ita <- rowMeans(temp_min.ita[-c(1)])
temp.min.ita.df <- as.data.frame(cbind(temp_min.ita[c(1)],temp.min.ita))
names(temp.min.ita.df) <- c("Data","Gradi")
View(temp.min.ita.df) 

#temperature italiane massime
temp.max.ita <- rowMeans(temp_max.ita[-c(1)])
temp.max.ita.df <- as.data.frame(cbind(temp_max.ita[c(1)],temp.max.ita))
names(temp.max.ita.df) <- c("Data","Gradi")
View(temp.max.ita.df)


#temperature italiane medie
temp.media.ita <- rowMeans(temp_media.ita[-c(1)])
temp.media.ita.df <- as.data.frame(cbind(temp_media.ita[c(1)],temp.media.ita))
names(temp.media.ita.df) <- c("Data","Gradi")
View(temp.media.ita.df)

#eliminazione 29 febbraio

check_is29Feb <- function(df) {
  # Estrae il giorno e il mese dalla data
  giorno <- day(df$Data)
  mese <- month(df$Data)
  
  # Controlla se la data è il 29 febbraio
  df$is29Feb <- giorno == 29 & mese == 2
  
  return(df)
}


temp.min.ita.df <- check_is29Feb(temp.min.ita.df)
View(temp.min.ita.df)

temp.max.ita.df <- check_is29Feb(temp.max.ita.df)
View(temp.max.ita.df)

temp.media.ita.df <- check_is29Feb(temp.media.ita.df)
View(temp.media.ita.df)

#eliminazione effettiva
temp.min.ita.df2 <- subset(temp.min.ita.df, is29Feb == FALSE)
temp.max.ita.df2 <- subset(temp.max.ita.df, is29Feb == FALSE)
temp.media.ita.df2 <- subset(temp.media.ita.df, is29Feb == FALSE)

temp.min.ita.df2 <- temp.min.ita.df2[-3]
temp.max.ita.df2 <- temp.max.ita.df2[-3]
temp.media.ita.df2 <- temp.media.ita.df2[-3]

#comparazione righe
nrow(temp.media.ita.df2) #5844
nrow(temp.media.ita.df2) #5840 

View(temp.media.ita.df2)
str(temp.media.ita.df2)

dim(temp.media.ita.df2)

#serie storica temperature minime
temp.min.ts <- ts(temp.min.ita.df2$Gradi, frequency = 365, start=2008)

start(temp.min.ts)
end(temp.min.ts)
frequency(temp.min.ts)

plot(temp.min.ts, xlab="Anni", ylab="Gradi(°C)", xaxt="n", col="blue")
axis(1, at=2008:2023, labels=c(2008:2023))


#serie storica temperature massime
temp.max.ts <- ts(temp.max.ita.df2$Gradi, frequency = 365, start=2008)

start(temp.max.ts)
end(temp.max.ts)
frequency(temp.max.ts)

plot(temp.max.ts, xlab="Anni", ylab="Gradi(°C)", xaxt="n", col="red")
axis(1, at=2008:2023, labels=c(2008:2023))

#serie storica temperature media
temp.media.ts <- ts(temp.media.ita.df2$Gradi, frequency = 365, start=2008)

start(temp.media.ts)
end(temp.media.ts)
frequency(temp.media.ts)

plot(temp.media.ts, xlab="Anni", ylab="Gradi(°C)", xaxt="n", col="green")
axis(1, at=2008:2023, labels=c(2008:2023))

#ANALISI CLASSICA

#Scelta modello additivo o moltiplicativo
#varianza costante nel tempo (omoschedasticità) -> modello additivo
#varianza aumenta o diminuisce nel tempo (eteroschedasticità) -> modello moltiplicativo

#TEST di Breusch-Pagan

# Il test di Breusch-Pagan è un test utilizzato per verificare l’omoschedasticità in un modello di regressione. 
# L’omoschedasticità si verifica quando la varianza degli errori è costante nel tempo, mentre l’eteroschedasticità si verifica quando la varianza degli errori varia nel tempo.

# Interpretare i risultati del test di Breusch-Pagan in R:
# 
#  - Ipotesi nulla (H0): L’ipotesi nulla del test di Breusch-Pagan è che gli errori siano omoschedastici, cioè la varianza degli errori è costante nel tempo.
#  - Ipotesi alternativa (H1): L’ipotesi alternativa è che gli errori siano eteroschedastici, cioè la varianza degli errori varia nel tempo.
#    p-value: Il p-value è la probabilità di ottenere un risultato almeno altrettanto estremo di quello osservato, supponendo che l’ipotesi nulla sia vera. 
#  - Se il p-value è inferiore al tuo livello di significatività (ad esempio, 0.05), allora rifiuti l’ipotesi nulla e concludi che la varianza degli errori varia nel tempo (eteroschedasticità).


model.temp.min <- lm(temp.min.ts ~ time(temp.min.ts))
model.temp.max <- lm(temp.max.ts ~ time(temp.max.ts))
model.temp.media <- lm(temp.media.ts ~ time(temp.media.ts))

bp.test.temp.min <- bptest(model.temp.min)
bp.test.temp.max <- bptest(model.temp.max)
bp.test.temp.media <- bptest(model.temp.media)

#risultati del test
bp.test.temp.min #omoschedasticità
bp.test.temp.max #omoschedasticità
bp.test.temp.media #omoschedasticità


#Modello Additivo temperature minime
classica.add.temp.min <- decompose(temp.min.ts, type = "additive")

plot.classica.add.temp.min <- plot(classica.add.temp.min)

#analisi residui 
residui.classica.add.temp.min <- scale(na.omit(classica.add.temp.min$random))

hist(residui.classica.add.temp.min)

qqnorm(residui.classica.add.temp.min)
qqline(residui.classica.add.temp.min)

acf(residui.classica.add.temp.min)
pacf (residui.classica.add.temp.min)


#analisi trend temperature minime
trend.add.temp.min <- classica.add.temp.min$trend

stima.trend.add.temp.min <- lm(trend.add.temp.min~time(trend.add.temp.min))
summary(stima.trend.add.temp.min)

#grafico trend
plot(trend.add.temp.min, xlab="Anni", ylab="Gradi(°C)",xaxt="n", main="Temperature Minime")
axis(1, at=2008:2023, labels=c(2008:2023))
abline(stima.trend.add.temp.min, col="red", lwd=2)



#Modello Additivo temperature massime
classica.add.temp.max <- decompose(temp.max.ts, type = "additive")

plot(classica.add.temp.max)


#analisi residui 
residui.classica.add.temp.max <- scale(na.omit(classica.add.temp.max$random))

hist(residui.classica.add.temp.max)

qqnorm(residui.classica.add.temp.max)
qqline(residui.classica.add.temp.max)

acf(residui.classica.add.temp.max)
pacf (residui.classica.add.temp.max)


#analisi trend temperature massime
trend.add.temp.max <- classica.add.temp.max$trend

stima.trend.add.temp.max <- lm(trend.add.temp.max~time(trend.add.temp.max))
summary(stima.trend.add.temp.max)

#grafico trend
plot(trend.add.temp.max, xlab="Anni", ylab="Gradi(°C)",xaxt="n", main="Temperature Massime")
axis(1, at=2008:2023, labels=c(2008:2023))
abline(stima.trend.add.temp.max, col="red", lwd=2)


#Modello Additivo temperature media
classica.add.temp.media <- decompose(temp.media.ts, type = "additive")
plot(classica.add.temp.media)


#analisi residui 
residui.classica.add.temp.media <- scale(na.omit(classica.add.temp.media$random))

hist(residui.classica.add.temp.media, col="green", xlab="Valore", ylab="Frequenza",
     main="Istogramma dei residui", xlim=c(-4,4))

qqnorm(residui.classica.add.temp.media)
qqline(residui.classica.add.temp.media)

acf(residui.classica.add.temp.media, main = "ACF Residui", ylab="Value")
pacf (residui.classica.add.temp.media, main = "PACF Residui", ylab="Value")


#analisi trend temperature media
trend.add.temp.media <- classica.add.temp.media$trend

stima.trend.add.temp.media <- lm(trend.add.temp.media~time(trend.add.temp.media))
summary(stima.trend.add.temp.media)

#grafico trend
plot(trend.add.temp.media, xlab="Anni", ylab="Gradi(°C)",xaxt="n", main="Temperature Media")
axis(1, at=2008:2023, labels=c(2008:2023))
abline(stima.trend.add.temp.media, col="red", lwd=2)


#previsione a tre anni temperature minime
forecast.classica.add.temp.min <- forecast(classica.add.temp.min$x, h=1825)
plot(forecast.classica.add.temp.min)

plot(temp.min.ts,xlim=c(2008,2029))
lines(forecast.classica.add.temp.min$mean, col="red")

summary(temp.min.ts)
summary(forecast.classica.add.temp.min$mean)

#previsione a tre anni temperature massime
forecast.classica.add.temp.max <- forecast(classica.add.temp.max$x, h=1825)
plot(forecast.classica.add.temp.max)

plot(temp.max.ts,xlim=c(2008,2029))
lines(forecast.classica.add.temp.max$mean, col="red")

summary(temp.max.ts)
summary(forecast.classica.add.temp.max$mean)

#previsione a tre anni temperature media
forecast.classica.add.temp.media <- forecast(classica.add.temp.media$x, h=1825)
plot(forecast.classica.add.temp.media, main = "Forecast Model Additive",
     xlab="Anni",ylab="Gradi (°C)")

plot(temp.media.ts,xlim=c(2008,2029), ylab="Gradi (°C)", xlab="Anni")
lines(forecast.classica.add.temp.media$mean, col="red")
legend("bottomright",           # Posizione della legenda
       legend=c("Dati storici", "Previsioni"),  # Etichette della legenda
       col=c("black", "red"),  # Colori delle linee
       lty=1,                  # Stili delle linee
       cex=0.7) 

summary(temp.media.ts)
summary(forecast.classica.add.temp.media$mean)

###########################################
#metodo HOLT-WINTERS temperatura media

#primo modello
exp.sm.temp.media <- HoltWinters (temp.media.ts, seasonal = "additive", alpha=1,
                                  gamma=1)                            
exp.sm.temp.media

plot(exp.sm.temp.media, col = c("black","red"), xlab="Anni",ylab="Gradi(°C)",
     main="Metodo Holt-Winters")
legend("bottomright", legend=c("Observed", "Fitted"),
       col=c("black", "red"),
       lty=1,                  # Stili delle linee
       cex=0.5) 

#analisi residui
res.hw.temp.media <- scale(na.omit(exp.sm.temp.media$fitted[,1]))
hist(res.hw.temp.media, main="Istogramma residui metodo HW", xlab="Residui",
     ylab="Frequenza", col="lightblue")

qqnorm(res.hw.temp.media)
qqline(res.hw.temp.media)


#previsione a 5 anni
forecast.exp.sm.temp.media <- forecast(exp.sm.temp.media$x, h = 1825)
plot(forecast.exp.sm.temp.media, main="Forecast modello HW",
     xlab = "Anni", ylab="Gradi (°C)")

plot(temp.media.ts, xlim=c(2008,2029), main="Forecast modello HW",
     xlab = "Anni", ylab="Gradi (°C)")
lines(forecast.exp.sm.temp.media$mean,col="red")

# Effettua previsioni
predicted_values <- predict(exp.sm.temp.media, n.ahead = 1825)

# Visualizza le previsioni
print(predicted_values)



plot(temp.media.ts, xlim=c(2008,2029), main = "Temperature medie italiane",
     xlab="Anni", ylab="Gradi (°C)",
     ylim=c(min(temp.media.ts),max(predicted_values)))
lines(predicted_values,col="red")
legend("bottomright",           # Posizione della legenda
       legend=c("Dati storici", "Previsioni"),  # Etichette della legenda
       col=c("black", "red"),  # Colori delle linee
       lty=1,                  # Stili delle linee
       cex=0.7) 

summary(temp.media.ts)
summary(predicted_values)


###########################################
#ANALISI MODERNA

View(temp.min.ita.df2)
View(temp.max.ita.df2)
View(temp.media.ita.df2)

#FUNZIONE DI AUTOCORRELAZIONE GLOBALE

acf(temp.min.ts, lag.max = 60)
acf(temp.max.ts, lag.max = 60)
acf(temp.media.ts, main = "ACF Temperature Medie", ylab="Value")


#FUNZIONE DI AUTOCORRELAZIONE PARZIALE
pacf(temp.min.ts, lag.max = 60)
pacf(temp.max.ts, lag.max = 60)
pacf(temp.media.ts, main = "PACF Temperature Medie", ylab="Value")


#Trasformazione Serie

#il log non accetta valori negagtivi, perciò dobbiamo passare per una standardizzazione 
#e riscalatura dei dati minimi.

summary(temp.min.ts)
summary(temp.media.ts)
summary(temp.max.ts)
#per le temperature massime non sarà necessario.

#standardizazzione

temp.min.ts.standard <- (temp.min.ts - mean(temp.min.ts)) / sd(temp.min.ts)
temp.media.ts.standard <- (temp.media.ts - mean(temp.media.ts)) / sd(temp.media.ts)


#riscalatura
temp.min.ts.rescale <- 0.01 + (temp.min.ts.standard - min(temp.min.ts.standard)) * (0.99 / (max(temp.min.ts.standard) - min(temp.min.ts.standard)))
summary(temp.min.ts.rescale)

temp.media.ts.rescale <- 0.01 + (temp.media.ts.standard - min(temp.media.ts.standard)) * (0.99 / (max(temp.media.ts.standard) - min(temp.media.ts.standard)))
summary(temp.min.ts.rescale)


#trasformazione logaritmica
temp.min.ts.log <- log(temp.min.ts.rescale)
temp.max.ts.log <- log(temp.max.ts)
temp.media.ts.log <- log(temp.min.ts.rescale)

summary(temp.min.ts.log)
summary(temp.max.ts.log)
summary(temp.media.ts.log)

plot(temp.min.ts.log)
plot(temp.max.ts.log)
plot(temp.media.ts.log, main = "Serie logaritmica: temperature medie italiane", ylab="Log", xlab="Anni")

#trasformazione per differenziazione
temp.min.ts.log.diff <- diff(temp.min.ts.log)
temp.max.ts.log.diff <- diff(temp.max.ts.log)
temp.media.ts.log.diff <- diff(temp.media.ts.log)

summary(temp.min.ts.log.diff)
summary(temp.max.ts.log.diff)
summary(temp.media.ts.log.diff)


plot(temp.min.ts.log.diff)
plot(temp.max.ts.log.diff)
plot(temp.media.ts.log.diff, main = "Serie logaritmica differenziata: temperature medie italiane",
     xlab="Anni",ylab="Log")


summary(temp.media.ts.log.diff)
summary(temp.media.ts)
var(temp.media.ts.log.diff)
var(temp.media.ts)

#FUNZIONE DI AUTOCORRELAZIONE GLOBALE
acf(temp.min.ts.log.diff, lag.max = 100)
acf(temp.max.ts.log.diff, lag.max = 100)
acf(temp.media.ts.log.diff, main = "ACF Serie logaritmica differenziata: temperature medie italiane",
    ylab = "Value")


#FUNZIONE DI AUTOCORRELAZIONE PARZIALE
pacf(temp.min.ts.log.diff, lag.max = 100)
pacf(temp.max.ts.log.diff, lag.max = 100)
pacf(temp.media.ts.log.diff, lag.max = 1000, main = "PACF Serie logaritmica differenziata: temperature medie italiane")





#Stima Modelli ARMA

arma22_temp.min <- Arima(temp.min.ts, order = c(2,0,2), lambda=0)
summary(arma22_temp.min)

summary(arma(temp.min.ts, order = c(2,2), include.intercept = F))

arma13_temp.max <- Arima(temp.max.ts, order = c(1,0,3), lambda = 0)
summary(arma13_temp.max)

summary(arma(temp.max.ts, order = c(1,3), include.intercept = F))


arma32_temp.media <- Arima(temp.media.ts, order = c(3,0,2), lambda = 0)
summary(arma32_temp.media)

summary(arma(temp.media.ts, order = c(3,2), include.intercept = F))

#serie + fitted
plot(temp.min.ts)
lines(arma22_temp.min$fitted, col="red")

plot(temp.max.ts)
lines(arma13_temp.max$fitted, col="red")

plot(temp.media.ts,  main = "Modello ARMA (3,2): Temperature medie italiane",
     xlab = "Anni", ylab="Gradi (°C)")
lines(arma32_temp.media$fitted, col="red")
legend("bottomright",           # Posizione della legenda
       legend=c("Observed", "Fitted"),  # Etichette della legenda
       col=c("black", "red"),  # Colori delle linee
       lty=1,                  # Stili delle linee
       cex=0.5) 

#anlisi residui modelli
res.arma22_temp.min <- ts(scale(na.omit(arma22_temp.min$residuals)))
res.arma13_temp.max <- ts(scale(na.omit(arma13_temp.max$residuals)))
res.prova <- as.double(arma32_temp.media$residuals)
res.arma32_temp.media <- ts(scale(na.omit(res.prova)))

#media residui
mean(res.arma22_temp.min)
mean(res.arma13_temp.max)
mean(res.arma32_temp.media)
var(res.arma32_temp.media)

#varianza residui
var(res.arma22_temp.min)
var(res.arma13_temp.max)
var(res.arma32_temp.media)

#normalità

#istogramma residui
hist(res.arma22_temp.min)
hist(res.arma13_temp.max)
hist(res.arma32_temp.media,breaks = 50, main ="Istogramma Residui ARMA(3,2)",
     xlab="Gradi (°C)", ylab="Frequenza", col="blue",
     xlim=c(-5,5))

#qqplot
qqnorm(res.arma22_temp.min)
qqline(res.arma22_temp.min)

qqnorm(res.arma13_temp.max)
qqline(res.arma13_temp.max)

qqnorm(res.arma32_temp.media)
qqline(res.arma32_temp.media,col="red")

shapiro.test(sample(nrow(res.arma22_temp.min), 5000)) #dati normali

shapiro.test(sample(nrow(res.arma13_temp.max), 5000)) #dati normali

shapiro.test(sample(nrow(res.arma32_temp.media), 5000)) #dati normali

#correlazione

#Scegliamo un ritardo di 1, perché vogliamo vedere se c'è autocorrelazione con ogni ritardo.

Box.test(res.arma22_temp.min, lag = 1, type = "Ljung-Box")
Box.test(res.arma13_temp.max, lag = 1, type = "Ljung-Box")
Box.test(res.arma32_temp.media, lag = 1, type = "Ljung-Box")

# vediamo un valore p molto grande, quindi possiamo accettare l'ipotesi nulla,
#indicando che la serie temporale no contiene un'autocorrelazione.
 

#PREVISIONI a 3 anni
temp.min.forecast <- forecast(arma22_temp.min$fitted, h=1825)

accuracy(temp.min.ts,temp.min.forecast$fitted)

plot(temp.min.forecast, main = "Forecast ARMA (3,2)",
     xlab="Anni",ylab="Temperature (°C)")

plot(temp.min.ts, xlim=c(2008,2029), main = "Previsioni modello ARMA(3,2)",
     xlab="Anni",ylab="Gradi(°C)")
lines(temp.min.forecast$mean, col="red")

summary(temp.min.ts)
summary(temp.min.forecast$mean)


temp.max.forecast <- forecast(arma13_temp.max$fitted, h=1825)

accuracy(temp.max.ts,temp.max.forecast$fitted)


plot(temp.max.forecast)

plot(temp.max.ts, xlim=c(2008,2029))
lines(temp.max.forecast$mean, col="red")

summary(temp.max.ts)
summary(temp.max.forecast$mean)


temp.media.forecast <- forecast(arma32_temp.media$fitted, h=1825)

accuracy(temp.media.ts,temp.media.forecast$fitted)

plot(temp.media.forecast)

var(temp.media.forecast$fitted)

plot(temp.media.ts, xlim=c(2008,2029))
lines(temp.media.forecast$mean, col="red")

summary(temp.max.ts)
summary(temp.max.forecast$mean)


write.csv(temp.min.ita.df2, "C:\\UTENTE\\Desktop\\Tesi CLEBA\\R\\3BMeteo\\DF Min Max\\df_temp_min.csv", row.names=FALSE)
write.csv(temp.max.ita.df2, "C:\\UTENTE\\Desktop\\Tesi CLEBA\\R\\3BMeteo\\DF Min Max\\df_temp_max.csv", row.names=FALSE)
write.csv(temp.media.ita.df2, "C:\\UTENTE\\Desktop\\Tesi CLEBA\\R\\3BMeteo\\DF Min Max\\df_temp_media.csv", row.names=FALSE)

write.csv(temp.media.ita.df, "C:\\UTENTE\\Desktop\\Tesi CLEBA\\R\\3BMeteo\\DF Min Max\\df_temp_media_bis.csv", row.names=FALSE)
