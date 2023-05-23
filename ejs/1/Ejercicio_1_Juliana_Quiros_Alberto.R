
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
