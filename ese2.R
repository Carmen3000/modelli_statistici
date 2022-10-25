library(tseries)

## Leggiamo i dati sull'indice della produzione industriale , mensile
Dati<-read.ts(file="C:/Users/39328/Desktop/Universita/modelli statistici/Esercizi esame finale/Dati indice di produzione industriale.txt",start=c(1990,1),frequency=12); 

##Stimiamo il trend
plot(Dati,main="Indice di produzione industriale, serie mensile");

# Il grafico non suggerisce una particolare tendenza di fondo, sembra periodica

## Costriamo della sequenza t=1,2,3,...T, dove T è pari alla lunghezza della serie
t<-seq(1,length(Dati),1);

## Stima ai minimi quadrati di un TREND polinomiale di ordine 1 con intercetta
mod1<-lm(Dati~poly(t,1,raw=TRUE));
summary(mod1);


## Dall'analisi dei residui notiamo che la currva normale dei residui è leggermente asimmetrica a sinistra
## Il test di Fisher è negativo B1 = 0 , il test di Fisher coincide con il test marginale 
## R^2 corretto = -0.0003496, negativo 
## B1 non significativo

## Confronto tra mod1 e i dati
fitmod1<-fitted(mod1)
ts.plot(Dati,fitmod1,gpars=list(col=c(2,3))) 

##secondo il criterio basato sull'indice di determinazione corretto R^2 proseguiamo nella stima di modelli con ordine superiore 2

## Stima ai minimi quadrati di un TREND polinomiale di ordine 2 con intercetta
mod2<-lm(Dati~poly(t,2,raw=TRUE));
summary(mod2);

## Il test di Fisher è positivo , il p-value < alpha
## R^2 corretto = 0.08973 , molto basso 

## Confronto tra mod2 e i dati
fitmod2<-fitted(mod2)
ts.plot(Dati,fitmod2,gpars=list(col=c(2,3))) 
## B2 negativo, curva rivolta verso il basso

## R^2 è aumenta, proseguiamo con r=3

## Stima ai minimi quadrati di un TREND polinomiale di ordine 3 con intercetta
mod3<-lm(Dati~poly(t,3,raw=TRUE));
summary(mod3);

## Il test di Fisher è positivo , il p-value < alpha
## R^2 corretto = 0.1137 , aumento lieve

## Confronto tra mod3 e i dati
fitmod3<-fitted(mod3)
ts.plot(Dati,fitmod3,gpars=list(col=c(2,3))) 
## si adatta meglio ai dati ma proseguiamo con r=4


## Stima ai minimi quadrati di un TREND polinomiale di ordine 4 con intercetta
mod4<-lm(Dati~poly(t,4,raw=TRUE));
summary(mod4);

## Il test di Fisher è positivo , il p-value < alpha
## R^2 corretto = 0.1108 , diminuisce

## Confronto tra mod4 e i dati
fitmod4<-fitted(mod4)
ts.plot(Dati,fitmod4,gpars=list(col=c(2,3))) 
# R^2 corretto diminuisce, quindi r=3





## Strimiamo la stagioinalità 

 

## Costruzione delle dummy
yy<-rep(c(1,2,3,4,5,6,7,8,9,10,11,12),22); 

length(yy);       
n<-length(Dati);     
n #cancella

xx<-yy[1:length(Dati)];
length(xx);
xx # cancella

dati1<-cbind(xx,Dati);

dati1;

# Costruzione delle variabili dummy
d1<-ifelse(xx==1,1,0); # assegnazione condizionata: se xx=1 allora assegna a d1=1 altrimenti 0
d2<-ifelse(xx==2,1,0); # assegnazione condizionata: se xx=2 allora assegna a d2=1 altrimenti 0
d3<-ifelse(xx==3,1,0); # assegnazione condizionata: se xx=3 allora assegna a d3=1 altrimenti 0
d4<-ifelse(xx==4,1,0); # assegnazione condizionata: se xx=4 allora assegna a d4=1 altrimenti 0
d5<-ifelse(xx==5,1,0); # assegnazione condizionata: se xx=1 allora assegna a d1=1 altrimenti 0
d6<-ifelse(xx==6,1,0); # assegnazione condizionata: se xx=2 allora assegna a d2=1 altrimenti 0
d7<-ifelse(xx==7,1,0); # assegnazione condizionata: se xx=3 allora assegna a d3=1 altrimenti 0
d8<-ifelse(xx==8,1,0); # assegnazione condizionata: se xx=4 allora assegna a d4=1 altrimenti 0
d9<-ifelse(xx==9,1,0); # assegnazione condizionata: se xx=1 allora assegna a d1=1 altrimenti 0
d10<-ifelse(xx==10,1,0); # assegnazione condizionata: se xx=2 allora assegna a d2=1 altrimenti 0
d11<-ifelse(xx==11,1,0); # assegnazione condizionata: se xx=3 allora assegna a d3=1 altrimenti 0
d12<-ifelse(xx==12,1,0); # assegnazione condizionata: se xx=4 allora assegna a d4=1 altrimenti 0

# controlla commenti

# Nuova organizzazione
dati2<-cbind(xx,Dati,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12);

dati2

# stima ai minimi quadrati della componente stagionale
stag<-lm(Dati~d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12-1); # -1 per eliminatre l'intercetta e evivatre la trappola dell dummy
summary(stag);

# tutte le d fortemente signifacative
# R^2 corretto molto alto

fitdatistag<-fitted(stag)
ts.plot(Dati,fitdatistag,gpars=list(col=c(2,3)),main="Valori osservati e valori stimati della componente stagionale")
# l'andamento della componente stagionale è più o meno conforme ai dati, studia bene la 
# periodicità, ovviamente i nostri dati hanno anche la componente del trend
# ATTENZIONE: nel caso detrendizzare i nostri dati per poter verificare se la componente stagionale va bene



# stima del trend e della componente stagionale 
modFinale<-lm(Dati~d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+poly(t,3,raw=TRUE)-1)
summary(modFinale)

# R^2 corretto 0.9965 
# tutto ok 

# plot finale
fit_modFinale<-fitted(modFinale)
ts.plot(Dati,fit_modFinale,gpars=list(col=c(2,3)),main ="valori osservati e valori stimati");

# il modello si adatta più o meno bene ai dati, tuttavia non riusciamo a stimare bene i picchi a 








# stima del trend e della componente stagionale senza il grado 1
polisenza1<- t^2 + t^3  # to do : trovare  il modo di togliere l'ordine 1

modFinale2<-lm(Dati~d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+poly(t+t^2+t^3)-1)
summary(modFinale2)


# R^2 corretto 0.9965 
# tutto ok 

# plot finale
fit_modFinale2<-fitted(modFinale2)
ts.plot(Dati,fit_modFinale2,gpars=list(col=c(2,3)),main ="valori osservati e valori stimati");




# Attenzione ai punti e virgola e ai nomi delle variabili
# manca : analisi esporativa dei dati , analisi dei residui, intervalli predittivi


# residui<-resid(modFinale);
# summary(residui);
# residuiStand<-rstandard(modFinale);
# hist(residuiStand,freq=F);
# curve(dnorm(x),add=T);
# plot(residuiStand);