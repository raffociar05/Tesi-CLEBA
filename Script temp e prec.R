###CARICAMENTO DATI
library(readxl)
#set path
setwd("C:\\UTENTE\\Desktop\\Tesi CLEBA\\R\\Istat")
#dataframe temperature
df_temp <- read_excel("Temperatura media annua e precipitazione totale annua nei comuni capoluogo di provincia (Anni 2006-2021).xlsx", 
                      sheet = "Foglio1", col_names = F, skip=2)
#dataframe precipitazioni
df_prec <- read_excel("Temperatura media annua e precipitazione totale annua nei comuni capoluogo di provincia (Anni 2006-2021).xlsx", 
                      sheet = "Foglio2", col_names = F, skip=2)  

###TEMPERATURE MEDIE ANNUALI (°C)

colnames(df_temp) <- c("Comuni",2006:2021)

df_temp <- t(df_temp)

colnames(df_temp)<-c(df_temp[1,])
df_temp <- df_temp[-1,]

df_temp <- as.data.frame(df_temp)
df_temp <- as.data.frame(lapply(df_temp, as.numeric))

df_temp <- sapply(df_temp, as.numeric)
df_temp <- as.data.frame(df_temp)

str(df_temp) #df 16 x 109 (numeric) 

View(df_temp)

#count NA's
sum(is.na(df_temp)) #75

summary(df_temp)

#count NA's per comune
sapply(df_temp, function(x) sum(is.na(x)))


comuni.temp.nas <- sapply(df_temp, function(x) sum(is.na(x)))
comuni.temp.nas <- (as.data.frame(comuni.temp.nas))
colnames(comuni.temp.nas) <- "n"
righe.nas.temp <- which(comuni.temp.nas$n > 0)
comuni.nas.temp <- rownames(comuni.temp.nas)[righe.nas.temp]

df.comuni.temp.nas <- df_temp[c(comuni.nas.temp)]
rownames(df.comuni.temp.nas) <- c(2006:2021)


names(df_temp)[sapply(df_temp, function(x) sum(is.na(x)) == length(x))]
#Fermo e Nuoro hanno solo valori NA

#eliminazione Fermo, Nuoro e Siena dal dataframe
df.temp.new <- subset(df_temp, select = -c(Fermo,Nuoro,Siena))


####MODIFICHE DATASET
#sostituiamo i valori NA con la media della variabile

library(dplyr)
library(tidyr)

df.temp.new2 <- df.temp.new %>% 
  mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))

sum(is.na(df.temp.new2))
summary(df.temp.new2)
View(df.temp.new2)

###PRECIPITAZIONE TOTALE ANNUA (mm)

colnames(df_prec) <- c("Comuni",2006:2021)

df_prec <- t(df_prec)

colnames(df_prec)<-c(df_prec[1,])
df_prec <- df_prec[-1,]

df_prec <- as.data.frame(df_prec)
df_prec <- as.data.frame(lapply(df_prec, as.numeric)) 

df_prec <- sapply(df_prec, as.numeric)
df_prec <- as.data.frame(df_prec)

str(df_prec) #df 16 x 109 (numeric)

View(df_prec)

#count NA's
sum(is.na(df_prec)) #67

summary(df_prec)

#count NA's per comune
sapply(df_prec, function(x) sum(is.na(x)))

comuni.prec.nas <- sapply(df_prec, function(x) sum(is.na(x)))
comuni.prec.nas <- (as.data.frame(comuni.prec.nas))
colnames(comuni.prec.nas) <- "n"
righe.nas.prec <- which(comuni.prec.nas$n > 0)
comuni.nas.prec <- rownames(comuni.prec.nas)[righe.nas.prec]


df.comuni.prec.nas <- df_prec[c(comuni.nas.prec)] 
rownames(df.comuni.prec.nas) <- c(2006:2021)

names(df.comuni.prec.nas)[sapply(df.comuni.prec.nas, function(x) sum(is.na(x)) == length(x))]
#Gorizia ha solo valori NA

#eliminazione Gorizia dal dataframe
df.prec.new <- subset(df_prec, select = -Gorizia)


####MODIFICHE DATASET
#sostituiamo i valori NA con la media

df.prec.new2 <- df.prec.new %>% 
    mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))

sum(is.na(df.prec.new2))
summary(df.prec.new2)


############ DATASET PRONTO
rownames(df.temp.new2) <- c(2006:2021)
rownames(df.prec.new2) <- c(2006:2021)


View(df.temp.new2)
View(df.prec.new2)

########## SERIE STORICA
library(tseries)

ts.plot(df.prec.new2)
ts.plot(df.temp.new2)

## REGIONI PRECIPITAZIONI

Piemonte.prec <- df.prec.new2[1:8]
ValdAosta.prec <- df.prec.new2[9]
Lombardia.prec <- df.prec.new2[10:21]
TrentinoAltoAdige.prec <- df.prec.new2[22:23]
Veneto.prec <- df.prec.new2[24:30]
FriuliVeneziaGiulia.prec <- df.prec.new2[31:33]
Liguria.prec <- df.prec.new2[34:37]
EmiliaRomagna.prec <- df.prec.new2[38:46]
Toscana.prec <- df.prec.new2[47:56]
Umbria.prec <- df.prec.new2[57:58]
Marche.prec <- df.prec.new2[59:63]
Lazio.prec <- df.prec.new2[64:68]
Abruzzo.prec <- df.prec.new2[69:72]
Molise.prec <- df.prec.new2[73:74]
Campania.prec <-  df.prec.new2[75:79]
Puglia.prec <- df.prec.new2[80:87]
Basilicata.prec <- df.prec.new2[88:89]
Calabria.prec <- df.prec.new2[90:94]
Sicilia.prec <- df.prec.new2[95:103]
Sardegna.prec <- df.prec.new2[104:108]


nord.prec <- data.frame(Piemonte.prec,ValdAosta.prec,Lombardia.prec,TrentinoAltoAdige.prec,
                        Veneto.prec,FriuliVeneziaGiulia.prec,Liguria.prec,EmiliaRomagna.prec)
centro.prec <- data.frame(Toscana.prec,Umbria.prec,Marche.prec,Lazio.prec) 
sud.prec <- data.frame(Abruzzo.prec,Molise.prec,Campania.prec,Puglia.prec,Basilicata.prec,Calabria.prec,Sicilia.prec,Sardegna.prec) 

italia.prec <- data.frame(nord.prec,centro.prec,sud.prec)
  
summary(nord.prec)
summary(centro.prec)
summary(sud.prec) 

### REGIONI TEMPERATURE

Piemonte.temp <- df.temp.new2[1:8]
ValdAosta.temp <- df.temp.new2[9]
Lombardia.temp <- df.temp.new2[10:21]
TrentinoAltoAdige.temp <- df.temp.new2[22:23]
Veneto.temp <- df.temp.new2[24:30]
FriuliVeneziaGiulia.temp <- df.temp.new2[31:34]
Liguria.temp <- df.temp.new2[35:38]
EmiliaRomagna.temp <- df.temp.new2[39:47]
Toscana.temp <- df.temp.new2[48:56]
Umbria.temp <- df.temp.new2[57:58]
Marche.temp <- df.temp.new2[59:62]
Lazio.temp <- df.temp.new2[63:67]
Abruzzo.temp <- df.temp.new2[68:71]
Molise.temp <- df.temp.new2[72:73]
Campania.temp <-  df.temp.new2[74:78]
Puglia.temp <- df.temp.new2[79:86]
Basilicata.temp <- df.temp.new2[87:88]
Calabria.temp <- df.temp.new2[89:93]
Sicilia.temp <- df.temp.new2[94:102]
Sardegna.temp <- df.temp.new2[103:106]


nord.temp <- data.frame(Piemonte.temp,ValdAosta.temp,Lombardia.temp,TrentinoAltoAdige.temp,
                          Veneto.temp,FriuliVeneziaGiulia.temp,Liguria.temp,EmiliaRomagna.temp)
centro.temp <- data.frame(Toscana.temp,Umbria.temp,Marche.temp,Lazio.temp) 
sud.temp <- data.frame(Abruzzo.temp,Molise.temp,Campania.temp,Puglia.temp,Basilicata.temp,Calabria.temp,Sicilia.temp,Sardegna.temp) 

italia.temp <- data.frame(nord.temp,centro.temp,sud.temp)

summary(nord.temp)
summary(centro.temp)
summary(sud.temp)


###########################

#MEDIA PER GRUPPI DI REGIONI

#temperature

nord.temp.mean <- rowMeans(nord.temp)
centro.temp.mean <- rowMeans(centro.temp)
sud.temp.mean <- rowMeans(sud.temp)

df.temp.mean <- data.frame(nord.temp.mean,centro.temp.mean,sud.temp.mean)
colnames(df.temp.mean) <- c("Temp Nord mean","Temp Centro mean","Temp Sud mean")

#boxplot
boxplot(df.temp.mean$`Temp Nord mean`,df.temp.mean$`Temp Centro mean`,
        df.temp.mean$`Temp Sud mean`,names = c("Nord","Centro","Sud"),
        col = c("Blue","Green","Red"), ylab="Gradi (°C)")

#boxplot per anno
par(mfrow=c(3,1))
boxplot(t(nord.temp),col="blue",ylab="Gradi (°C)", main="Nord")
boxplot(t(centro.temp),col="green",ylab="Gradi (°C)", main="Centro")
boxplot(t(sud.temp),col="red",ylab="Gradi (°C)", main="Sud")


#istogramma
par(mfrow=c(1,3))
hist(df.temp.mean$`Temp Nord mean`, col="blue", xlab = "Temperatura(°C)",
     ylab = "Frequenza", main="Nord")
hist(df.temp.mean$`Temp Centro mean`, col="green", xlab = "Temperatura(°C)",
     ylab = "Frequenza", main="Centro")
hist(df.temp.mean$`Temp Sud mean`,  col="red", xlab = "Temperatura(°C)",
     ylab = "Frequenza",main="Sud")

#density
plot(density(df.temp.mean$`Temp Nord mean`), col="blue", xlab="Temperatura (°C)",
     xlim=c(13,18.5), ylim=c(0,1.1), main="Density Plot Teperature")
lines(density(df.temp.mean$`Temp Centro mean`), lwd=2, col="green")
lines(density(df.temp.mean$`Temp Sud mean`), lwd=2, col="red")
legend("topleft",legend=c("Nord", "Centro","Sud"), fill=c("blue","green","red"))


#serie storica
ts.plot(df.temp.mean, col=c("blue","green","red"), lwd=3, type="o",
        xlab="Anni", ylab="Gradi °C", gpars=list(xaxt="n"),
        main = "Andamento temperature medie 2006-2021")
axis(1, at=1:16, labels=c(2006:2021))
legend(x="topleft", legend=c("Nord", "Centro","Sud"), 
       fill = c("blue","green","red"), horiz=F)


#precipitazioni (mm)

nord.prec.mean <- rowMeans(nord.prec)
centro.prec.mean <- rowMeans(centro.prec)
sud.prec.mean <- rowMeans(sud.prec)

df.prec.mean <- data.frame(nord.prec.mean,centro.prec.mean,sud.prec.mean)
colnames(df.prec.mean) <- c("Prec Nord mean","Prec Centro mean","Prec Sud mean")

#boxplot
boxplot(df.prec.mean$`Prec Nord mean`,df.prec.mean$`Prec Centro mean`,
        main="Precipitazioni totali medie 2006-2021",
        df.prec.mean$`Prec Sud mean`,names = c("Nord","Centro","Sud"),
        col = c("Blue","Green","Red"), ylab="Millimetri (mm)")

#boxplot per anno
par(mfrow=c(3,1))
boxplot(t(nord.prec),col="blue",ylab="Millimetri (mm)", main="Nord")
boxplot(t(centro.prec),col="green",ylab="Millimetr (mm)", main="Centro")
boxplot(t(sud.prec),col="red",ylab="Millimetr (mm)", main="Sud")


#istogramma
par(mfrow=c(1,3))
hist(df.prec.mean$`Prec Nord mean`, col="blue", xlab = "Millimetri(mm)",
     ylab = "Frequenza", main="Nord")
hist(df.prec.mean$`Prec Centro mean`, col="green", xlab = "Millimetri(mm)",
     ylab = "Frequenza", main="Centro")
hist(df.prec.mean$`Prec Sud mean`,  col="red", xlab = "Millimetri(mm)",
     ylab = "Frequenza",main="Sud")

#density
plot(density(df.prec.mean$`Prec Nord mean`), col="blue", xlab="Millimetri(mm)",
     main="Density Plot Precipitazioni totali", ylim=c(0,0.005))
lines(density(df.prec.mean$`Prec Centro mean`), lwd=2, col="green")
lines(density(df.prec.mean$`Prec Sud mean`), lwd=2, col="red")
legend("topright",legend=c("Nord", "Centro","Sud"), fill=c("blue","green","red"))


mean(df.prec.mean$`Prec Sud mean`)
median(df.prec.mean$`Prec Sud mean`)


ts.plot(df.prec.mean, col=c("blue","green","red"), lwd=3, type="o",
        xlab="Anni", ylab="Millimetri (mm)", gpars=list(xaxt="n",yaxt="n"),
        main="Andatamento precipitazioni totali 2006-2021")
axis(1, at=1:16, labels=c(2006:2021))
axis(2, at = seq(from=600,to=1400,by=100))
legend(x="topright", legend=c("Nord", "Centro","Sud"), 
       fill = c("blue","green","red"), horiz=T, text.width = 0.1)




##############################################################################
#Tavola 9
#Indici estremi di temperatura calcolati su valori soglia nei comuni capoluogo di regione.
#Anomalie 2016-2021 dal valore climatico 1981-2010 e 1971-2000
#Anni 2016-2021 (valore in gradi Celsius e valori in numero di giorni)

#TNn: Minimo delle temperature minime (°C)							
#TNx: Massimo delle temperature minime (°C)							
#TXn: Minimo delle temperature massime (°C)							
#TXx: Massimo delle temperature massime (°C)							
#FD0: Giorni con gelo 							
#SU25: Giorni estivi	
#TR20: Notti tropicali
#VALORE CLIMATICO 1981-2010

df.indici.cap <- read_excel("Indici estremi temperatura Anno 2021.xlsx", 
                            sheet = "Foglio2",
                            col_types = c("text", 
                                          "numeric", "numeric", "numeric", "numeric",                                                              "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"))

df.indici.cap <- df.indici.cap[-(22:23),]

View(df.indici.cap)

summary(df.indici.cap)

#suddivisione nord centro sud

df.indici.cap.nord <- df.indici.cap[1:9,]
df.indici.cap.centro <- df.indici.cap[10:13,] 
df.indici.cap.sud <- df.indici.cap[14:21,] 

View(df.indici.cap.sud)

#suddivisione per indice

TNn.nord <- df.indici.cap.nord[c(1,2:8)]
TNn.centro <- df.indici.cap.centro[c(1,2:8)]
TNn.sud <- df.indici.cap.sud[c(1,2:8)]

TNn.nord.mean <- as.data.frame(colMeans(TNn.nord[-c(1)])) 
TNn.centro.mean <- as.data.frame(colMeans(TNn.centro[-c(1)]))
TNn.sud.mean <- as.data.frame(colMeans(TNn.sud[-c(1)]))

TNn.italy.mean <- cbind(TNn.nord.mean,TNn.centro.mean,TNn.sud.mean)
names(TNn.italy.mean) <- c("Nord","Centro","Sud")
View(round(TNn.italy.mean, digits = 2))



TNx.nord <- df.indici.cap.nord[c(1,9:15)]
TNx.centro <- df.indici.cap.centro[c(1,9:15)]
TNx.sud <- df.indici.cap.sud[c(1,9:15)]

TNx.nord.mean <- as.data.frame(colMeans(TNx.nord[-c(1)])) 
TNx.centro.mean <- as.data.frame(colMeans(TNx.centro[-c(1)]))
TNx.sud.mean <- as.data.frame(colMeans(TNx.sud[-c(1)]))

TNx.italy.mean <- cbind(TNx.nord.mean,TNx.centro.mean,TNx.sud.mean)
names(TNx.italy.mean) <- c("Nord","Centro","Sud")
View(round(TNx.italy.mean, digits = 2))



TXn.nord <- df.indici.cap.nord[c(1,16:22)]
TXn.centro <- df.indici.cap.centro[c(1,16:22)]
TXn.sud <- df.indici.cap.sud[c(1,16:22)]

TXn.nord.mean <- as.data.frame(colMeans(TXn.nord[-c(1)])) 
TXn.centro.mean <- as.data.frame(colMeans(TXn.centro[-c(1)]))
TXn.sud.mean <- as.data.frame(colMeans(TXn.sud[-c(1)]))

TXn.italy.mean <- cbind(TXn.nord.mean,TXn.centro.mean,TXn.sud.mean)
names(TXn.italy.mean) <- c("Nord","Centro","Sud")
View(round(TXn.italy.mean, digits = 2))



TXx.nord <- df.indici.cap.nord[c(1,23:29)]
TXx.centro <- df.indici.cap.centro[c(1,23:29)]
TXx.sud <- df.indici.cap.sud[c(1,23:29)]

TXx.nord.mean <- as.data.frame(colMeans(TXx.nord[-c(1)]))
TXx.centro.mean <- as.data.frame(colMeans(TXx.centro[-c(1)]))
TXx.sud.mean <- as.data.frame(colMeans(TXx.sud[-c(1)]))

TXx.italy.mean <- cbind(TXx.nord.mean,TXx.centro.mean,TXx.sud.mean)
names(TXx.italy.mean) <- c("Nord","Centro","Sud")
View(round(TXx.italy.mean, digits = 2))



FD0.nord <- df.indici.cap.nord[c(1,30:36)]
FD0.centro <- df.indici.cap.centro[c(1,30:36)]
FD0.sud <- df.indici.cap.sud[c(1,30:36)]

FD0.nord.mean <- as.data.frame(colMeans(FD0.nord[-c(1)]))
FD0.centro.mean <- as.data.frame(colMeans(FD0.centro[-c(1)]))
FD0.sud.mean <- as.data.frame(colMeans(FD0.sud[-c(1)]))

FD0.italy.mean <- cbind(FD0.nord.mean,FD0.centro.mean,FD0.sud.mean)
names(FD0.italy.mean) <- c("Nord","Centro","Sud")
View(round(FD0.italy.mean, digits = 2))



SU25.nord <- df.indici.cap.nord[c(1,37:43)]
SU25.centro <- df.indici.cap.centro[c(1,37:43)]
SU25.sud <- df.indici.cap.sud[c(1,37:43)]

SU25.nord.mean <- as.data.frame(colMeans(SU25.nord[-c(1)]))
SU25.centro.mean <- as.data.frame(colMeans(SU25.centro[-c(1)]))
SU25.sud.mean <- as.data.frame(colMeans(SU25.sud[-c(1)]))

SU25.italy.mean <- cbind(SU25.nord.mean,SU25.centro.mean,SU25.sud.mean)
names(SU25.italy.mean) <- c("Nord","Centro","Sud")
View(round(SU25.italy.mean, digits = 2))



TR20.nord <- df.indici.cap.nord[c(1,44:50)]
TR20.centro <- df.indici.cap.centro[c(1,44:50)]
TR20.sud <- df.indici.cap.sud[c(1,44:50)]

TR20.nord.mean <- as.data.frame(colMeans(TR20.nord[-c(1)]))
TR20.centro.mean <- as.data.frame(colMeans(TR20.centro[-c(1)]))
TR20.sud.mean <- as.data.frame(colMeans(TR20.sud[-c(1)]))

TR20.italy.mean <- cbind(TR20.nord.mean,TR20.centro.mean,TR20.sud.mean)
names(TR20.italy.mean) <- c("Nord","Centro","Sud")
View(round(TR20.italy.mean, digits = 2))




########################################################################
#Tavola 2 - Differenza della temperatura media annua per gli anni 2016-2021 
#dal valore medio del periodo 2006-2015 e rispettive anomalie(a) dal valore 
#climatico 1981-2010 e 1971-2000 nei comuni capoluogo di provincia 
#Anni 2016-2021, valore medio periodo 2006-2015,
#valore climatico 1981-2010 e 1971-2000 (valori assoluti in gradi Celsius) 						


df.diff.temp <- read_excel("Indici estremi temperatura Anno 2021.xlsx", 
                            sheet = "Foglio3")
df.diff.temp <- df.diff.temp[-c(22,23),]
View(df.diff.temp)

df.diff.temp.nord <- df.diff.temp[1:9,]
df.diff.temp.centro <- df.diff.temp[10:13,]
df.diff.temp.sud <- df.diff.temp[14:21,]


df.diff.temp.nord.mean <- as.data.frame(colMeans(df.diff.temp.nord[-c(1)]))
colnames(df.diff.temp.nord.mean) <- "Nord"
View(round(df.diff.temp.nord.mean, digits = 2))

df.diff.temp.centro.mean <- as.data.frame(colMeans(df.diff.temp.centro[-c(1)]))
colnames(df.diff.temp.centro.mean) <- "Centro"
View(round(df.diff.temp.centro.mean, digits = 2))

df.diff.temp.sud.mean <- as.data.frame(colMeans(df.diff.temp.sud[-c(1)]))
colnames(df.diff.temp.sud.mean) <- "Sud"
View(round(df.diff.temp.sud.mean, digits = 2))

df.diff.temp.italy.mean <- cbind(df.diff.temp.nord.mean,
                                 df.diff.temp.centro.mean,
                                 df.diff.temp.sud.mean)

View(round(df.diff.temp.italy.mean, digits = 2))


