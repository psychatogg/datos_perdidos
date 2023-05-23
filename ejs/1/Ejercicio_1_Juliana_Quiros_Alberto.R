
library(mice)
library(naniar)



## 1.1
data <- read.table("C:\\GDrive1\\Uni\\Master\\datos_perdidos\\ejs\\1\\Ejercicio1.dat", header = T)
md.pattern(data)

## 1.2
data_noMOT <- data[,-4]
md.pattern(data_noMOT)


## 1.3
REND <- ifelse(is.na(data$REND), 0, 1)
data$REND <- REND

t.test(data$HESTUDIO ~ data$REND)

## 1.5

vis_miss(data)


## 2.2
data2 <- read.table("C:\\GDrive1\\Uni\\Master\\datos_perdidos\\ejs\\1\\Ejercicio2.dat", header = T)

## MCAR
m1 <- vector(length = 5)
s1 <- vector(length = 5)
c1 <- vector(length = 5)
set.seed(1)
for(i in 1:5) {
U = runif(500, 0, 1) 
data2$RENDmcar = data2$REND 
head(data2,20)
data2$RENDmcar[U < 0.50]<-NA 
head(data2,20)
table(is.na(data2$RENDmcar))


m1[i] <- mean(data2$RENDmcar,na.rm=TRUE)
s1[i] <- sd(data2$RENDmcar,na.rm=TRUE)
c1[i] <- cor(data2$IQ,data2$RENDmcar,use="complete.obs")

}
m1
s1
c1


## MAR
m2 <- vector(length = 5)
s2 <- vector(length = 5)
c2 <- vector(length = 5)
set.seed(1)

qIQ <- quantile(data2$IQ)

for(i in 1:5) {
U = runif(500, 0, 1)
data2$RENDmar = data2$REND
data2$RENDmar[U < 0.80 & data2$IQ <= qIQ[[1]]]<-NA
data2$RENDmar[U < 0.60 &  data2$IQ > qIQ[[1]]  & data2$IQ <= qIQ[[2]]]<-NA
data2$RENDmar[U < 0.40 &  data2$IQ > qIQ[[2]]  & data2$IQ <= qIQ[[3]]]<-NA
data2$RENDmar[U < 0.20 &  data2$IQ > qIQ[[3]]  & data2$IQ <= qIQ[[4]]]<-NA
table(is.na(data2$RENDmar))

m2[i] <- mean(data2$RENDmar,na.rm=TRUE) 
s2[i] <- sd(data2$RENDmar,na.rm=TRUE) 
c2[i] <- cor(data2$IQ,data2$RENDmar,use="complete.obs")
}
m2
s2
c2


## MNAR

m3 <- vector(length = 5)
s3 <- vector(length = 5)
c3 <- vector(length = 5)
set.seed(1)

qREND <- quantile(data2$REND)

for(i in 1:5) {
	U = runif(500, 0, 1)
	data2$RENDmnar = data2$REND
	data2$RENDmnar[U < 0.80 & data2$REND <= qREND[[1]]]<-NA
	data2$RENDmnar[U < 0.60 &  data2$REND > qREND[[1]]  & data2$REND <= qREND[[2]]]<-NA
	data2$RENDmnar[U < 0.40 &  data2$REND > qREND[[2]]  & data2$REND <= qREND[[3]]]<-NA
	data2$RENDmnar[U < 0.20 &  data2$REND > qREND[[3]]  & data2$REND <= qREND[[4]]]<-NA
	table(is.na(data2$RENDmnar))
	
	m3[i] <- mean(data2$RENDmnar,na.rm=TRUE) 
	s3[i] <- sd(data2$RENDmnar,na.rm=TRUE) 
	c3[i] <- cor(data2$IQ,data2$RENDmnar,use="complete.obs")
}
m3
s3
c3