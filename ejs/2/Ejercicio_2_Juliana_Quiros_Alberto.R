




data <- read.table("C:\\GDrive1\\Uni\\Master\\datos_perdidos\\ejs\\1\\Ejercicio2.dat", header = T)

set.seed(1)
## MAR

U = runif(500, 0, 1)
MedIQ = median(data$IQ)
M = rep(MedIQ,500)
data$RENDmar = data$REND
data$RENDmar[U < 0.80 & data$IQ < M]<-NA
data$RENDmar[U < 0.20 & data$IQ >= M]<-NA
table(is.na(data$RENDmar))
n = dim(data)[1]



## Media


MediaY = mean(data$RENDmar, na.rm = TRUE)


for (i in 1:n){
	if (is.na(data$RENDmar[i])){
		data$RENDmar[i] = MediaY;
	}
}


M_imputacionporlamedia = mean(data$RENDmar,na.rm=TRUE);
DT_imputacionesporlamedia = sd(data$RENDmar,na.rm=TRUE);
R_imputacionporlamedia = cor(data$IQ,data$RENDmar,use="complete.obs")

M_imputacionporlamedia
DT_imputacionesporlamedia
R_imputacionporlamedia




set.seed(1)
## MAR

U = runif(500, 0, 1)
MedIQ = median(data$IQ)
M = rep(MedIQ,500)
data$RENDmar = data$REND
data$RENDmar[U < 0.80 & data$IQ < M]<-NA
data$RENDmar[U < 0.20 & data$IQ >= M]<-NA
table(is.na(data$RENDmar))
n = dim(data)[1]


## Hot-deck


MediaY = mean(data$RENDmar, na.rm = TRUE)
DTY = sd(data$RENDmar, na.rm = TRUE)

#vamos a utilizar un bucle for para imputar
for (i in 1:n){
	if (is.na(data$RENDmar[i])){
		data$RENDmar[i] = MediaY + rnorm(1,0,1)*DTY;
	}
}

#calculamos los estad?sticos media, D.T. y correlaci?n con estudios
M_imputacionhotdeck = mean(data$RENDmar,na.rm=TRUE);
DT_imputacionhotdeck = sd(data$RENDmar,na.rm=TRUE);
R_imputacionhotdeck = cor(data$IQ,data$RENDmar,use="complete.obs")


M_imputacionhotdeck
DT_imputacionhotdeck
R_imputacionhotdeck



set.seed(1)
## MAR

U = runif(500, 0, 1)
MedIQ = median(data$IQ)
M = rep(MedIQ,500)
data$RENDmar = data$REND
data$RENDmar[U < 0.80 & data$IQ < M]<-NA
data$RENDmar[U < 0.20 & data$IQ >= M]<-NA
table(is.na(data$RENDmar))
n = dim(data)[1]

## Regresion

fit <- lm(RENDmar ~ IQ,data)
summary(fit)
#vamos a utilizar un bucle for para imputar
for (i in 1:n){
	if (is.na(data$RENDmar[i])){
		data$RENDmar[i] = fit$coefficients[1] + fit$coefficients[2]*data$IQ[i];
	}
}

#calculamos los estad?sticos media, D.T. y correlaci?n con estudios
M_imputacionregresion = mean(data$RENDmar,na.rm=TRUE);
DT_imputacionregresion = sd(data$RENDmar,na.rm=TRUE);
R_imputacionregresion = cor(data$IQ,data$RENDmar,use="complete.obs")

M_imputacionregresion
DT_imputacionregresion
R_imputacionregresion







set.seed(1)
## MAR

U = runif(500, 0, 1)
MedIQ = median(data$IQ)
M = rep(MedIQ,500)
data$RENDmar = data$REND
data$RENDmar[U < 0.80 & data$IQ < M]<-NA
data$RENDmar[U < 0.20 & data$IQ >= M]<-NA
table(is.na(data$RENDmar))
n = dim(data)[1]

## Regresion estocastica
fit <- lm(RENDmar ~ IQ,data)
summary(fit)
DT = sd(residuals(fit));#obtengo la desviaci?n t?pica residual

#vamos a utilizar un bucle for para imputar
for (i in 1:n){
	if (is.na(data$RENDmar[i])){
		data$RENDmar[i] = fit$coefficients[1] + fit$coefficients[2]*data$IQ[i] + rnorm(1,0,1)*DT;
	}
}

#calculamos los estad?sticos media, D.T. y correlaci?n con estudios
M_imputacionregresionest = mean(data$RENDmar,na.rm=TRUE);
DT_imputacionregresionest = sd(data$RENDmar,na.rm=TRUE);
R_imputacionregresionest = cor(data$IQ,data$RENDmar,use="complete.obs")

M_imputacionregresionest
DT_imputacionregresionest
R_imputacionregresionest