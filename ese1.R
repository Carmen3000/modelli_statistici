library(tseries)

# Leggiamo i dati sul tasso di disoccupazione tra i maschi e visualizziamone il grafico
Dati<-read.ts(file="C:/Users/39328/Desktop/Universita/modelli statistici/Esercizi esame finale/Dati maschi disoccupati.txt",start=c(1992,4),frequency=4);
plot(Dati,main="Tasso di disoccupazione maschile,serie trimestrale")


# Iniziamo con lo stimare il trend della serie storica
# Il grafico suggerisce che la serie storica presenta una tendenza di fondo di tipo t^4

# Costruiamo la sequenza t=1,2,3,...T, dove T è pari alla lunghezza della serie
t<-seq(1,length(Dati),1)


# Stima ai minimi quadrati di un Trend polinomiale di ordine r=1
mod1<-lm(Dati~poly(t,1,raw=TRUE))
summary(mod1)

# Il test fi Fisher è positivo, il p-value < alpha
# R^2 corretto = 0.1614, molto basso 
# B0 = 6.7 B1 = 0.02, entrambe le componenti sono molto significative


# Confronto tra mod1 e i dati
fitmod1<-fitted(mod1)
ts.plot(Dati,fitmod1,gpars=list(col=c(2,3))) 

# Il modello non si adatta bene ai dati 
# Secondo il criterio basato sull'indice di determinazione corretto R^2 
# proseguiamo nella stima di modelli con ordine superiore


# Stima ai minimi quadrati di un Trend polinomiale di ordine 2 
mod2<-lm(Dati~poly(t,2,raw=TRUE))
summary(mod2)

# Il test di Fisher è positivo , il p-value < alpha
# R^2 corretto = 0.3247 , molto basso 
# B0 = 8.5791463 B1 = -0.0706567 B2 = 0.0008338  , tutte molto significative

# Confronto tra mod2 e i dati
fitmod2<-fitted(mod2)
ts.plot(Dati,fitmod2,gpars=list(col=c(2,3)))
 
# R^2 è aumenta, proseguiamo con r=3


# Stima ai minimi quadrati di un Trend polinomiale di ordine 3
mod3<-lm(Dati~poly(t,3,raw=TRUE))
summary(mod3)

## Il test di Fisher è positivo , il p-value < alpha
## R^2 corretto = 0.3692 , aumento lieve

# Confronto tra mod3 e i dati
fitmod3<-fitted(mod3)
ts.plot(Dati,fitmod3,gpars=list(col=c(2,3))) 

# si adatta meglio ai dati, ma proseguiamo con r=4


# Stima ai minimi quadrati di un Trend polinomiale di ordine 4 
mod4<-lm(Dati~poly(t,4,raw=TRUE))
summary(mod4)

# Il test di Fisher è positivo , il p-value < alpha
# tutte le componenti sono statisticamente significative
# R^2 corretto = 0.815 , aumento significativo

# Confronto tra mod4 e i dati
fitmod4<-fitted(mod4)
ts.plot(Dati,fitmod4,gpars=list(col=c(2,3))) 

# proseguiamo con r=5

# Stima ai minimi quadrati di un Trend polinomiale di ordine 5
mod5<-lm(Dati~poly(t,5,raw=TRUE))
summary(mod5)

# R^2 corretto = 0.8138 , diminuisce, quindi scegliamo il polinomio di ordine r=4 
# il modello stimato è quindi yt = 5.591e+00 + 5.125e-01 t  -2.408e-02 t^2 +  3.599e-04 t^3 -1.648e-06 t^4  



# Stimiamo la stagioinalità della serie storica

# Costruzione delle variabili dummy
yy<-rep(c(4,1,2,3),29);  # ripetiamo il vettore 4,1,2,3 per 29 anni completi
length(yy);              # ha 116 elimenti
n<-length(Dati);         # ha 113 elementi, bisogna eliminare gli ultimi tre trimestri

xx<-yy[1:length(Dati)];
length(xx);

dati1<-cbind(xx,Dati);
dati1;


d1<-ifelse(xx==1,1,0); # assegnazione condizionata: se xx=1 allora assegna a d1=1 altrimenti 0
d2<-ifelse(xx==2,1,0); # assegnazione condizionata: se xx=2 allora assegna a d2=1 altrimenti 0
d3<-ifelse(xx==3,1,0); # assegnazione condizionata: se xx=3 allora assegna a d3=1 altrimenti 0
d4<-ifelse(xx==4,1,0); # assegnazione condizionata: se xx=4 allora assegna a d4=1 altrimenti 0


# Nuova organizzazione dei dati
dati2<-cbind(xx,Dati,d1,d2,d3,d4);
dati2


# Stima ai minimi quadrati della componente stagionale
stag<-lm(Dati~d1+d2+d3+d4-1); # -1 per eliminatre l'intercetta e evitare la trappola delle dummy
summary(stag);

# tutte le d fortemente signifacative
# R^2 corretto molto alto

# Confronto tra i dati osservati e quelli stimati
fitdatistag<-fitted(stag)
ts.plot(Dati,fitdatistag,gpars=list(col=c(2,3)),main="Valori osservati e valori stimati della componente stagionale")

# l'andamento della componente stagionale periodicità, ovviamente i nostri dati hanno anche la componente del trend


# stima del trend e della componente stagionale 
modFinale<-lm(Dati~d1+d2+d3+d4+poly(t,4,raw=TRUE)-1)
summary(modFinale)

# plot finale
fit_modFinale<-fitted(modFinale)
ts.plot(Dati,fit_modFinale,gpars=list(col=c(2,3)),main ="valori osservati e valori stimati");










