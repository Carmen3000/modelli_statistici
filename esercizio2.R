#Leggiamo i dati
WW<-read.csv(file.choose(),sep=";",header=TRUE)

dim(WW)
head(WW)

library(glmnet)
names(WW)

# WW contiene sia la matrice X dei regressori che la variabile indipendente y, suddivisiamo 

xx<-WW[,-12] # prendto tutte le colonne tranne l'ultima
x<- as.matrix(xx);dim(x);names(x);
y<-WW$quality  # estraggo l'ultima colonna, cioè y

# Costruiamo la discretizzazione di lambda
# seguenza di numeri decrescente da 10 a -2, con decremento 0.1212121

qq<-seq(10,-2,length=100)
qq

# creaiamo una sequenza decrescente di lambda
griglia=10^seq(10,-2,length=100)
griglia

#stima della reidge regression
ridgeRegression=glmnet(x,y,alpha=0,lambda=griglia)

# otteniamo le stime dei Bi al variare di  lambda
dim(coef(ridgeRegression))
coef(ridgeRegression)

plot(ridgeRegression,main="Reidge Regression, regressori standardizzati",xvar="lambda",label=TRUE)


#ogni curva descrive la variazione del coefficiente Bi al variare di lambda , > lamda più le stime dei coeff tendono a zero
# infatti la RR opera un restringimento delle stime dei coeff. Quando lambda -> 0 si ottengono le stime ai minimi quadrati


# applichiamo la cross validation
cv.outK10=cv.glmnet(x,y,lambda=griglia,alpha=0)
plot(cv.outK10)

# estraiamo il valore di lambda che rende minima la stima degli MSE calcolati sul test set
lmin<-cv.outK10$lambda.min
lmin

# otteniamo un lambda minimo di 0.01

# usiamo tale lambda per stimare il modello finale 
ridge.mod=glmnet(x,y,alpha=0,lambda=lmin)
coef(ridge.mod)[,1]











