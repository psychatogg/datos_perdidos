library(mice)
library(QuantPsyc)
library(naniar)
library(lavaan)

## lm
data <- read.table("C:\\GDrive1\\Uni\\Master\\datos_perdidos\\ejs\\3\\Regresion estudiantes TDAH.txt", header = T)

fit_lm <- lm(aprendizaje ~ escala_mot + horas_estudio + salario_padres + estudio_padres + programa, data = data)

summary(fit_lm)

lm.beta(fit_lm)


## mice pmm

md.pattern(data)

imp <- mice(data, seed = 20000,m=50) 
print(imp)
fit_mice <- with(imp, lm(aprendizaje ~ escala_mot + horas_estudio + salario_padres + estudio_padres + programa))
summary(pool(fit_mice))
pool.r.squared(fit_mice, adjusted = FALSE)
pool.r.squared(fit_mice, adjusted = TRUE)

data_imp <- complete(imp,1)


## mice norm


imp_norm <- mice(data, seed = 20000,m=50,"norm") 
print(imp_norm)
fit_mice_norm <- with(imp_norm, lm(aprendizaje ~ escala_mot + horas_estudio + salario_padres + estudio_padres + programa))
summary(pool(fit_mice_norm))
pool.r.squared(fit_mice_norm, adjusted = FALSE)
pool.r.squared(fit_mice_norm, adjusted = TRUE)

data_imp_norm <- complete(imp_norm,1)


## mice orden
vis_miss(data)
ini <- mice(data = data, maxit = 0)
ini$visitSequence

visita = ini$visitSequence
visita = c("programa","salario_padres","estudio_padres","aprendizaje","horas_estudio","escala_mot")

pred=ini$predictorMatrix
pred[,"id"] <- 0
pred

imp_orden <- mice(data, m=50,pred=pred, seed = 20000,vis=visita)

fit_orden <- with(imp_orden, lm(aprendizaje ~ escala_mot+horas_estudio+salario_padres+estudio_padres+programa))
summary(pool(fit_orden))
pool.r.squared(fit_orden, adjusted = FALSE)
pool.r.squared(fit_orden, adjusted = TRUE)
data_imp_orden <- complete(imp_orden,1)

## ML

model <- ' 
aprendizaje ~ escala_mot + horas_estudio + salario_padres + estudio_padres + programa
' 
fit_ml <- sem(model,data = data,meanstructure=TRUE,
						 missing="ML", fixed.x=FALSE) 
summary(fit_ml,fit.measures=F,standardized=TRUE) 


lavInspect(fit_ml, "rsquare")

residuals <- residuals(fit_ml, type = "standardized")

sigma_e <- sd(residuals$mean)
sigma_e

		 