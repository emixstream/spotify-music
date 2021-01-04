setwd("~/Desktop/FoS/Project Stat")
install.packages("car")
library(car)
install.packages("Hmisc")
library(Hmisc)

#*******************Sampling Dataset*******************
Spotify<-read.csv("data.csv")
attach(Spotify)
Spotify <- Spotify[order(Spotify$year),]
WODuplicated <- Spotify[!duplicated(Spotify[c("artists", "name")]),]
RemovedYEAR <- WODuplicated[(WODuplicated$year>1999 & WODuplicated$year<2019),]
RemovedTempo <- RemovedYEAR[RemovedYEAR$tempo >= 60, ]
RemovedPodcast <- RemovedTempo[RemovedTempo$speechiness <= 0.6,]
SpotifyCleaned <- RemovedPodcast
set.seed(1234)
SpotifySample <- SpotifyCleaned[sample(nrow(SpotifyCleaned), (0.2*(nrow(SpotifyCleaned))), replace = TRUE),]
write.csv(SpotifySample, "SampleSpotify.csv")
write.csv(SpotifyCleaned, "SpotifyNonCampionato.csv")
detach(Spotify)
detach()
rm(list=ls())

#*******************Load Dataset*******************
SpotifySample<-read.csv("SampleSpotify.csv")
SpotifyWOOutliers <- SpotifySample
SpotifyPopulation <- read.csv("SpotifyNonCampionato.csv")
attach(SpotifySample)

#*******************Function*******************
remove_outliers_IQR <- function(datas, variableName){
  indexColumn <- grep(variableName, colnames(datas))
  BPStat <- boxplot.stats(datas[,indexColumn,])
  datas <- datas[!(datas[,indexColumn,] %in% BPStat$out),]
  return (datas)
}
getmode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
remove_outliers_ZScore <- function(datas, variableName){
  indexColumn <- grep(variableName, colnames(datas))
  datas$zscoreDuration <- abs((datas[,indexColumn,]-mean(datas[,indexColumn,]))/(sd(datas[,indexColumn,])))
  datas <- datas[datas$zscoreDuration<3,]
  return (datas)
}
summary_time <- function(time){
  cat("  min", format( as.POSIXct(Sys.Date())+min(time)/1000, "%M:%S"))
  cat("  1st", format( as.POSIXct(Sys.Date())+quantile(time, 0.25)/1000, "%M:%S"))
  cat("  median", format( as.POSIXct(Sys.Date())+median(time)/1000, "%M:%S"))
  cat("  mean", format( as.POSIXct(Sys.Date())+mean(time)/1000, "%M:%S"))
  cat("  3rd", format( as.POSIXct(Sys.Date())+quantile(time, 0.75)/1000, "%M:%S"))
  cat("  max", format( as.POSIXct(Sys.Date())+max(time)/1000, "%M:%S"))
}

#*******************Outliers Removing and Analisys*******************
#-----------------Duartion_MS

#--Outliers deleting with BoxPlot *
summary_time(SpotifySample$duration_ms)
hist(as.double((duration_ms*(10^-3))/60), main="", ylab = "Frequenza", xlab = "Duration", las = 1, col = "darkred", border="white", freq=F)
SpotifySample<- remove_outliers_IQR(SpotifySample, "duration_ms")
summary_time(SpotifySample$duration_ms)
detach(SpotifySample)
attach(SpotifySample)
hist(as.double((duration_ms*(10^-3))/60), main="", ylab = "Frequenza", xlab = "Duration", las = 1, col = "darkred", border="white", freq=F)
qqnorm((duration_ms*(10^-3)/60), ylab = "Sample Qantiles", xlab = "Theoretical Quantiles", las = 1, col = "darkred")
qqline((duration_ms*(10^-3)/60), col="darkblue", lwd = 3)

#-----------------Tempo
summary(tempo)
hist(tempo)
SpotifySample<- remove_outliers_ZScore(SpotifySample, "tempo")
detach(SpotifySample)
attach(SpotifySample)
summary(SpotifySample$tempo)
hist(tempo, main="", ylab = "Frequenza", xlab = "Tempo", las = 1, col = "darkred", border="white", freq=F)
getmode(tempo)

#-----------------Loudness
summary(loudness)
hist(loudness)
getmode(loudness)
SpotifySample <- remove_outliers_ZScore(SpotifySample, "loudness")
detach(SpotifySample)
attach(SpotifySample)
summary(SpotifySample$loudness)
getmode(loudness)
hist(loudness, main="", ylab = "Frequenza", xlab = "Loudness", las = 1, col = "darkred", border="white", freq=F)

#-----------------Energy
hist(energy, main="", ylab = "Frequenza", xlab = "Energy", las = 1, col = "darkred", border="white", freq=F)
SpotifySample <- remove_outliers_IQR(SpotifySample, "energy")
detach(SpotifySample)
attach(SpotifySample)
hist(energy, main="", ylab = "Frequenza", xlab = "Energy", las = 1, col = "darkred", border="white", freq=F)
summary(energy)
getmode(energy)
plot(loudness, energy, xlab = "Loudness", ylab = "Energy", las = 1, col = "darkred")

#-----------------Acousticness
summary(acousticness)
getmode(acousticness)
SpotifySample <- remove_outliers_IQR(SpotifySample, "acousticness")
detach(SpotifySample)
attach(SpotifySample)
summary(acousticness)
getmode(acousticness)
hist(acousticness, main="", ylab = "Frequenza", xlab = "Acousticness", las = 1, col = "darkred", border="white", freq=F)

#-----------------Danceability
summary(danceability)
hist(danceability)
SpotifySample <- remove_outliers_IQR(SpotifySample, "danceability")
detach(SpotifySample)
attach(SpotifySample)
summary(SpotifySample$danceability)
hist(danceability, main="", ylab = "Frequenza", xlab = "Danceability", las = 1, col = "darkred", border="white", freq=F)
qqnorm(danceability, main="Danceability distribution", ylab = "Sample Qantiles", xlab = "Theoretical Quantiles", las = 1, col = "darkred")
qqline(danceability)
#plot(energy, dnorm(energy, mean = 0.5, sd = sd(c(0,1, by = 0.01))))


#-----------------Speechiness
summary(speechiness)
SpotifySample<-remove_outliers_ZScore(SpotifySample, "speechiness")
detach(SpotifySample)
attach(SpotifySample)
summary(SpotifySample$speechiness)
hist(speechiness, main="", ylab = "Frequenza", xlab = "Speechiness", las = 1, col = "darkred", border="white", freq=F)

#-----------------Instrumentalness
# % osservazioni che ricadono prima di 0.1
summary(instrumentalness)
(nrow(SpotifySample[SpotifySample$instrumentalness < 0.05,])/nrow(SpotifySample))*100
instrumentalnessDownTo05 <- SpotifySample[SpotifySample$instrumentalness < 0.05,]
(nrow(instrumentalnessDownTo05[instrumentalnessDownTo05$instrumentalness == 0,])/nrow(instrumentalnessDownTo05))*100
hist(instrumentalness, main="", ylab = "Frequenza", xlab = "Instrumentalness", las = 1, col = "darkred", border="white", freq=F)
rm(instrumentalnessDownTo05)
#-----------------Liveness
hist(liveness)
summary(liveness)
SpotifySample<- remove_outliers_IQR(SpotifySample, "liveness")
detach(SpotifySample)
attach(SpotifySample)
getmode(liveness)
summary(SpotifySample$liveness)
hist(liveness, main="", ylab = "Frequenza", xlab = "Liveness", las = 1, col = "darkred", border="white", freq=F)

#-----------------Valence
hist(valence, main="", ylab = "Frequenza", xlab = "Valence", las = 1, col = "darkred", border="white", freq=F)
#SpotifySample2 <- remove_outliers_IQR(SpotifySample, "valence")      #NO OUTLIERS

#-----------------Explicit
SpotifySample$explicit <- ifelse(SpotifySample$explicit == 0, 'Non Explicit', 'Explicit')
SpotifySample$explicit <- factor(SpotifySample$explicit)
table1 <- table(SpotifySample$explicit)
lbls <- paste(round((table1/margin.table(table1)*100),1),"%",sep="")
pie(table(SpotifySample$explicit), col = c("darkred", "deepskyblue"), labels = lbls, main='Explicit distribution')
legend(-0.9, 1.0, cex = 0.8, legend=c("TRUE", "FALSE"), fill = c("darkred", "deepskyblue"))
rm(lbls, table1)
detach(SpotifySample)
attach(SpotifySample)

#-----------------Key
SpotifySample$key <- factor(SpotifySample$key)
levels(SpotifySample$key) <- c("DO","DO#","RE","RE#","MI","FA","FA#","SOL","SOL#","LA","LA#", "SI")
plot(table(SpotifySample$key), ylab = "", col ="darkred")
getmode(key)
hist(key, breaks = seq(0, 11, 1))
nrow(SpotifySample[SpotifySample$key == "SOL",])
detach(SpotifySample)
attach(SpotifySample)

#-----------------Mode
SpotifySample$mode<-factor(SpotifySample$mode) 
levels(SpotifySample$mode) <- c("Minor", "Major")
detach(SpotifySample)
attach(SpotifySample)
table(SpotifySample$mode)
table1 <- table(SpotifySample$mode)
lbls <- paste(round((table1/margin.table(table1)*100),1),"%",sep="")
pie(table(SpotifySample$mode), col = c("darkred", "deepskyblue"), labels = lbls, main='Mode distribution')
legend(-0.9, 1.0, cex = 0.8, legend=c("Minor", "Major"), fill = c("darkred", "deepskyblue"))
rm(lbls, table1)

#-----------------Popularity
hist(popularity, main="", ylab = "Frequenza", xlab = "Popularity", las = 1, col = "darkred", border="white", freq=F)
summary(popularity)
getmode(popularity)

#-----------------Year
#La variabile year è stata riclassificata in 3 periodi di 5 anni e 1 da 4 anni
SpotifySample['periods']<-ifelse(SpotifySample$year<2005, 0, SpotifySample$year)
SpotifySample['periods']<-ifelse(SpotifySample$year<2010 & SpotifySample$year>2004, 1, SpotifySample$periods)
SpotifySample['periods']<-ifelse(SpotifySample$year<2015 & SpotifySample$year>2009, 2, SpotifySample$periods)
SpotifySample['periods']<-ifelse(SpotifySample$year>2014, 3, SpotifySample$periods)
SpotifySample$periods <- factor(SpotifySample$periods)
classdim <- c(5,5,5,4)
levels(SpotifySample$periods) <- c("00-04", "05-09", "10-14", "15-18")
detach(SpotifySample)
attach(SpotifySample)

bar <- table(periods)
bp<-barplot(bar, width = classdim, col = "darkred", main = "Songs for period")
abline(h=0)
text(bp, bar/2, labels = bar, font = 2, col = "white", cex = 1.6)
rm(bp, bar)


SpotifyWOOutliers <- SpotifySample
SpotifySample <- read.csv("SampleSpotify.csv")
rm(getmode, gini, remove_outliers_IQR, remove_outliers_ZScore, summary_time)
detach()
attach(SpotifySample)
#************************************Inferenza***************************************************

#pvalue per 0 non accetto per nessuna alpha per 1 per tute ; per p = 0.02 rifiuto H0 per ogni alpha >0.02
options(scipen=999)
n <- dim(SpotifySample)[1] # numero di righe

#-----------------Acousticness
# IC 90% per la media con varianza IGNOTA
xmedio_acoustic = mean(SpotifySample$acousticness) # media campionaria (xmedio)
v_camp_acoustic = var(SpotifySample$acousticness)*(n)/(n-1) # varianza campionaria corretta
# per TLC, per n grande lo stimatore Z=(xmedio-mu)/(S/radice(n)) converge in distribuzione a una N(0,1)
se_m_acoustic = sqrt(v_camp_acoustic/n)  # errore std di xmedio
se_m_acoustic
err_acoustic = qnorm(0.90)*se_m_acoustic
ic_acoustic = xmedio_acoustic + c(-err_acoustic,err_acoustic) #intervallo di confidenza per p
ic_acoustic
# Siamo confidenti al 90% che la media della popolazione sia all'interno dell'intervallo trovato
rm(ic_acoustic, err_acoustic, se_m_acoustic, v_camp_acoustic, xmedio_acoustic)

#-----------------Danceability
# Per poter semplificare le operazioni ed applicare dei metodi supponendo la varianza nota,
# ipotizziamo come popolazione completa di riferimento quella da cui abbiamo estratto il campione

# IC 99% per la media con varianza NOTA
xmedio_dance = mean(SpotifySample$danceability) # media campionaria (xmedio)
v_pop_dance = var(SpotifyPopulation$danceability) # varianza popolazione (nota)
# per TLC, per n grande lo stimatore Z=(xmedio-mu)/(sigma/radice(n)) converge in distribuzione a una N(0,1)
se_m_dance = sqrt(v_pop_dance/n)  # errore std di xmedio
se_m_dance
err_dance = qnorm(0.99)*se_m_dance
ic_dance = xmedio_dance + c(-err_dance,err_dance) #intervallo di confidenza per mu
ic_dance
# Siamo confidenti al 99% che il parametro mu della popolazione sia all'interno dell'intervallo trovato
rm(ic_dance, err_dance, se_m_dance, v_pop_dance, xmedio_dance)

#-----------------Duration_ms
# Per poter semplificare le operazioni ed applicare dei metodi supponendo la varianza nota,
# ipotizziamo come popolazione completa di riferimento quella da cui abbiamo estratto il campione

# VI 90% della media con varianza NOTA
xmedio_dur = mean(SpotifySample$duration_ms) # media campionaria (xmedio) circa 3:52 min
v_pop_dur = var(SpotifyPopulation$duration_ms) # varianza popolazione (nota)
# Facciamo una VI, con alfa = 0.1, che nella popolazione la durata media delle canzoni non sia superiore a 4 minuti (240000 ms) 
# H0: mu = 240000
# H1: mu > 240000
ip_mu_dur = 240000
# per TLC, per n grande lo stimatore Z=(xmedio-mu)/(sigma/radice(n)) converge in distribuzione a una N(0,1)
se_m_dur = sqrt(v_pop_dur/n)  # errore std di xmedio
se_m_dur

z_oss_dur = (xmedio_dur-ip_mu_dur)/se_m_dur
z_alfa90 = qnorm(1-0.1)
pvalue_dur = pnorm(z_oss_dur, lower.tail=FALSE)

z_oss_dur
z_alfa90
pvalue_dur
# z_oss_dur < z_alfa90 -> da evidenze empiriche NON si pu? rifiutare H0
# il p-value 1, quindi per qualsiasi alfa=(0.1, 0.05. 0.01) minori di 1 NON si rifuter? H0
rm(xmedio_dur, v_pop_dur, ip_mu_dur, se_m_dur, z_oss_dur, z_alfa90, pvalue_dur)

#-----------------Key
SpotifySample$key <- factor(SpotifySample$key)
levels(SpotifySample$key) <- c("DO","DO#","RE","RE#","MI","FA","FA#","SOL","SOL#","LA","LA#", "SI")
# IC 95% per la proporzione campionaria
table(SpotifySample$key)/length(SpotifySample$key)
p_key = 0.11412247 # proporzione campionaria della chiave SOL
q_key = 1-p_key
# per n molto grande e p molto piccolo (n*p>5 e n*q>5), per TLC la proporzione campionaria si distribuisce asintoticamente come una Z
se_p_key = sqrt(p_key*q_key/n)  # errore std di p
se_p_key
err_key = qnorm(0.975)*se_p_key
ic_key = p_key + c(-err_key,err_key) #intervallo di confidenza per p
ic_key
# Siamo confidenti al 95% che il parametro p sia all'interno dell'intervallo trovato
rm(p_key, q_key, se_p_key, err_key, ic_key)

#-----------------Loudness
# VI 95% della varianza
xmedio_loudness = mean(SpotifySample$loudness) # media campionaria (xmedio) circa -7 db
v_camp_loudness = var(SpotifySample$loudness)*(n)/(n-1) # varianza campionaria corretta
# Si pu? dire con un alfa = 0.05 che nella popolazione la varianza del volume medio delle canzoni sia maggiore a 20 decibel?
# H0: sigma >= 20
# H1: sigma < 20
ip_var_loudness = 20
# per TLC, per n grande lo stimatore Z=(n-1)*S^2/sigma^2 si distribuisce come a una Chisq con (n-1) gradi di libert?
gdl = n-1
stat_test_loudness = (n-1)*v_camp_loudness/ip_var_loudness

chi_alfa05mezzi = qchisq(0.05, gdl)
pvalue_loudness = pchisq(stat_test_loudness, gdl)

stat_test_loudness
chi_alfa05mezzi
pvalue_loudness

rm(stat_test_loudness, chi_alfa05mezzi, pvalue_loudness, gdl, ip_var_loudness, v_camp_loudness, xmedio_loudness)
#Rifiutiamo H0
# chi_alfa05mezzi < stat_test_loudness < -> da evidenze empiriche NON si rifiuta H0
# il p-value ? 0.15, quindi per qualsiasi alfa=(0.1, 0.05. 0.01) minori di 0.15 NON si rifiuter? H0

#-----------------Mode
SpotifySample$mode<-factor(SpotifySample$mode) 
levels(SpotifySample$mode) <- c("Minor", "Major")
# VI 95% della proporzione campionaria
table(SpotifySample$mode)/length(SpotifySample$mode)
p_mode = 0.3466159 # proporzione campionaria di mode 0
# Facciamo una VI, con alfa al 5%, che nella popolazione il 1/3 delle canzoni abbia scala melodica mminor (mode=0)
# H0: p = 1/3
# H1: p != 1/3
ip_p = 1/3
q_mode = 1-p_mode
# per n molto grande e p molto piccolo (n*p>5 e n*q>5), per TLC la proporzione campionaria si distribuisce asintoticamente come una Z
se_p_mode = sqrt(p_mode*q_mode/n)  # errore std di p
se_p_mode

z_oss_mode = (p_mode-ip_p)/se_p_mode
z_alfa95mezzi = qnorm(0.025)
pvalue_mode = 2*pnorm(z_oss_mode, lower.tail=FALSE)

z_oss_mode
-z_alfa95mezzi
z_alfa95mezzi
pvalue_mode
rm(pvalue_mode, z_alfa95mezzi, z_oss_mode, se_p_mode, q_mode, ip_p, p_mode)
# z_oss_mode > z_alfa95mezzi -> da evidenze empiriche si rifiuta H0
# il p-value ? circa 0.02, quindi per qualsiasi alfa=(0.1, 0.05) maggiori di 0.02 si rifiuter? H0


#-----------------Tempo
# VI 99% della media con varianza IGNOTA
xmedio_tempo = mean(SpotifySample$tempo) # media campionaria (xmedio) circa 121 bpm
v_camp_tempo = var(SpotifySample$tempo)*(n)/(n-1) # varianza campionaria corretta
# Si pu? dire con un alfa = 0.01 che nella popolazione la frequenza dei battiti media sia esattamente 120 bpm?
# H0: mu = 120
# H1: mu != 120
ip_mu_tempo = 120
# per TLC, per n grande lo stimatore Z=(xmedio-mu)/(S/radice(n)) converge in distribuzione a una N(0,1)
se_m_tempo = sqrt(v_camp_tempo/n)  # errore std di xmedio
se_m_tempo

z_oss_tempo = (xmedio_tempo-ip_mu_tempo)/se_m_tempo
z_alfa99 = qnorm(0.005)
pvalue_tempo = 2*(pnorm(z_oss_tempo, lower.tail=FALSE))

z_oss_tempo
-z_alfa99
z_alfa99
pvalue_tempo
rm(pvalue_tempo, z_alfa99, se_m_tempo, z_oss_tempo, ip_mu_tempo, xmedio_tempo, v_camp_tempo)
# z_oss_tempo > z_alfa99 -> da evidenze empiriche si rifiuta H0
# il p-value ? circa 0, quindi per qualsiasi alfa=(0.1, 0.05. 0.01) si rifiuter? H0

#************************************CorrTest***************************************************

#-----------------ACOUSTICNESS E LOUDNESS

cor.test(SpotifySample$liveness,SpotifySample$valence, 
         method = "pearson")

# UTILIZZANDO UNA T DI TUDENT CON (N - 2) GRADI DI LIBERTA' LA CORRELAZIONE E' PARI ALLO 0.0008
# il p-value ? vicino a uno, quindi non si pu? rifiutare l'ipotesi nulla che ro sia uguale a zero,
# cio? che ci sia incorrelazione anche nella popolazione

#-----------------LOUDNESS E ENERGY

cor.test(SpotifySample$energy,SpotifySample$loudness, 
         method = "pearson")

# UTILIZZANDO UNA T DI TUDENT CON (N - 2) GRADI DI LIBERTA' LA CORRELAZIONE E' PARI ALLO 0.75
# il p-value ? prossimo a zero, quindi rifiutiamo l'ipotesi nulla ossia e diciamo che ro ? diverso da zero,
# cio? che non ci sia incorrelazione nella popolazione

#************************************Chi Test***************************************************

#-----------------MODE E KEY
tbl1 = table(SpotifySample$mode, SpotifySample$key) 
tbl1                 # the contingency table 

chi=chisq.test(tbl1) 
chi
chi$statistic # chi quadro
chi$parameter # gradi di libert? (12-1)*(2-1)
chi$p.value # pvalue

# pvalue praticamente uguale a zero -> rifiuto H0, 
# quindi posso affermare che anche nella popolazione queste 2 variabili NON sono indipendenti
rm(chi, tbl1)
#************************************ANOVA***************************************************

SpotifySample['periods']<-ifelse(SpotifySample$year<2005, 0, SpotifySample$year)
SpotifySample['periods']<-ifelse(SpotifySample$year<2010 & SpotifySample$year>2004, 1, SpotifySample$periods)
SpotifySample['periods']<-ifelse(SpotifySample$year<2015 & SpotifySample$year>2009, 2, SpotifySample$periods)
SpotifySample['periods']<-ifelse(SpotifySample$year>2014, 3, SpotifySample$periods)
#CONTROLLO
class(SpotifySample$periods)
summary(SpotifySample$periods)
SpotifySample$periods <- factor(SpotifySample$periods)
levels(SpotifySample$periods) <- c("00-04", "05-09", "10-14", "15-18")

av1 = aov(popularity ~ periods, SpotifySample) 
summary(av1)
av1$coefficients # medie in ciascun livello di year

# test F con 3 gradi di libert? (4 categorie - 1)
# residuals ? pari a n - k (k=categorie)
# Dev. dovuta al fattore = 221585 (Dev Between)
# Dev. residua = 405943
# Mean Sq ? pari a SumSq/df
# F value ? il rapporto tra le 2 MeanSq
# pvalue: prob di osservare un valore di F con gli stessi gradi di libert? MAGGIORE di F Value
# questo significa che le medie sono signif, diverse anche nella popolazione perch? abbiamo rifiutato H0 (uguaglianza delle medie)
rm(av1)
rm(n)

#************************************Modelli***************************************************
SpotifySample <- SpotifyWOOutliers
SpotifySampleCopy <- SpotifySample
#CONTROLLO
class(SpotifySample$periods)
summary(SpotifySample$periods)
SpotifySample$periods <- factor(SpotifySample$periods)
levels(SpotifySample$periods) <- c("00-04", "05-09", "10-14", "15-18")

#MODELLO UNIVARIATO
modelloUniv <- lm(popularity~danceability, SpotifySample)
summary(modelloUniv) 
plot(modelloUniv)
#MODELLO MULTIVARIATO

model<-lm(popularity~valence+tempo+liveness+loudness+duration_ms+speechiness+mode+acousticness+danceability+key+periods, data=SpotifySample)
summary(model)

#non tutti i coefficienti sono significativi,quindi ricostruiamo modello con solo variabili significative
model2<-lm(popularity~loudness+speechiness+danceability+periods ,data=SpotifySample)
summary(model2)
vif(model2)

