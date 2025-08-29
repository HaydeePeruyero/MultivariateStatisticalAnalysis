# 
datos <- data.frame(
  Observacion = 1:25,
  y = c(16.68, 11.50, 12.03, 14.88, 13.75,
        18.11, 8.00, 17.83, 79.24, 21.50,
        40.33, 21.00, 13.50, 19.75, 24.00,
        29.00, 15.35, 19.00, 9.50, 35.10,
        17.90, 52.32, 18.75, 19.83, 10.75),
  x1 = c(7, 3, 3, 4, 6,
         7, 2, 7, 30, 5,
         16, 10, 4, 6, 9,
         10, 6, 7, 3, 17,
         10, 26, 9, 8, 4),
  x2 = c(560, 220, 340, 80, 150,
         330, 110, 210, 1460, 605,
         688, 215, 255, 462, 448,
         776, 200, 132, 36, 770,
         140, 810, 450, 635, 150)
)

datos

pairs(datos[-1])


idv <- rep(1, nrow(datos))

X <- matrix(c(idv, datos$x1, datos$x2), nrow = 25, ncol = 3)

y <- matrix(datos$y, nrow=25, ncol=1)


## El estimador \hat{\beta} = (X'X)^{-1}X'y

beta <- solve(t(X) %*% X) %*% t(X) %*% y

beta

## \hat{y} = beta[1] + beta[2]X1 + beta[3]X2


M1 <- lm(y ~ x1 + x2, datos)

M1


## Estimación de sigma^2


SSE <- t(y) %*% y - t(beta) %*% t(X) %*% y
SSE  

#grados de libertad = n-p-1

varest <- SSE / (nrow(y) - nrow(beta))
varest

summary(M1)

sum(residuals(M1)^2) / df.residual(M1)

write.csv(datos, file="ruta/nombre.csv", row.names = FALSE)

##instalar paquetes
install.packages("datasets")


## Pruebas de Hipótesis

SCT <- t(y) %*% y - sum(y)**2 / nrow(datos)
SCT

SCE <- t(beta) %*% t(X) %*% y - sum(y)**2 /nrow(datos)
SCE

SSE <- SCT - SCE
SSE


# El estadístico F_0

## \alpha = 0.05


F0 <- (SCE / (ncol(X) -1 )) / (SSE / ( nrow(X) - (ncol(X)-1) -1  ) )
F0

## F_{0.05, 2, 22} = 3.44

# F0 > F_{0.05, 2, 22}

# Se rechaza H_0

# Usando el Modelo M1 


SCT.m <- sum( (datos$y - mean(datos$y))**2)
SCT.m

SCE.m <- sum( (M1$fitted - mean(datos$y))**2)
SCE.m

SSE.m <- sum(M1$residuals**2)
SSE.m

n <- nrow(y)
n

GLT <- n-1
GLT


GLRes <- df.residual(M1)
GLRes


GLR <- GLT - GLRes
GLR


CMR <- SCE / GLR
CMR

CMRes <- SSE / GLRes
CMRes


F0 <- CMR / CMRes
F0


alpha <- 0.05
df1 <- GLR
df2 <- GLRes

F_crit <- qf(1-alpha, df1, df2)
F_crit


pv <- 1 - pf(F0, GLR, GLRes)
pv


## Pruebas sobre coeficientes individuales


SCT <- t(y) %*% y -sum(y)**2/ nrow(datos)
SCT  


SCE <- t(beta) %*% t(X) %*% y - sum(y)**2 / nrow(datos)
SCE  
  
SSE <- SCT -SCE
SSE  


## (X'X)^{-1}
C22 <-  solve(t(X)%*%X)[3,3]
C22  

t0 <- beta[3] / sqrt(varest * C22)
t0

### Se rechaza la H_0

tt <- qt(p=0.95+0.05/2, df=22, lower.tail = TRUE)
tt


M1 <- lm(y~ x1 + x2, data = datos)


summary(M1)

### Intervalo de confianza de los coef de regresión

beta1 <- beta[2]
beta1


C11 <- solve(t(X) %*% X)[2,2]
C11

izq <- beta1 - tt * sqrt(varest * C11)
izq  

der <- beta1 + tt * sqrt(varest *C11)
der

### x1 = 8 cajas y x2 = 275 pies de distancia


X0 <- matrix( c(1, 8, 275), nrow = 3)
X0

y0 <- t(X0) %*% beta 
y0

var_y0 <- varest * t(X0) %*% solve(t(X) %*% X) %*% X0
var_y0


l_izq <- y0 - tt *sqrt(var_y0)
l_izq

l_der <- y0 + tt *sqrt(var_y0)
l_der



## Ejemplo marketing

library(datarium)

data("marketing")

str(marketing)

modelo1 <- lm(sales ~ youtube + facebook + newspaper, data = marketing)

summary(modelo1)


modelo2 <- lm(sales ~ youtube + facebook, data = marketing)
summary(modelo2)


## Validación de Supuestos

yp <-c(6.40, 15.05, 18.75, 30.25, 44.85, 48.85, 51.55, 61.50, 100.44, 111.42)
x1 <-c(1.32, 2.69, 3.56, 4.41, 5.35, 6.20, 7.12, 8.87, 9.80, 10.65)
x2 <-c(1.15, 3.40, 4.10, 8.75, 14.82, 15.15, 15.32, 18.18, 35.19, 40.40)
datos<-data.frame(yp, x1, x2)


plot(datos)

library(ggplot2)

g1 <- ggplot(data = datos, mapping=aes(x=x1, y=yp) )+
  geom_point(color="blue", size=2)+
  labs(title = 'yp~x1', x='x1')+
  geom_smooth(method = "lm", se= FALSE, color="black") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5))
g1


## Multicolinealidad

variables <- data.frame(x1,x2)
m_cor <- cor(variables,method = "pearson" )
m_cor


library(corrplot)

corrplot(m_cor)

modelo1 <- lm(formula = yp~x1+x2, data = datos)
summary(modelo1)


modelo2 <- lm(formula = yp ~ x2, data = datos)
summary(modelo2)


#ANOVA

# H_0: las variables que eliminamos no tienen significancia
# H_1: las variables son significativas

# \alpha = 0.05
anova(modelo1, modelo2)


modelo3 <- lm(formula = yp ~ x1 + x2 -1, data = datos)
summary(modelo3)

## Normalidad

residuales <- modelo3$residuals

qqnorm(residuales)
qqline(residuales)


plot(modelo3)

# H_0: la distribución normal
# H_1: la distribución no es normal

shapiro.test(residuales)
# alpha = 0.05


# Homocedasticidad

par(mfrow=c(1,1))
plot(modelo3)

# H_0: los residuos tienen varianza constante (homocedasticidad)
# H_1: Hay heterocedasticidad en los residuos

library(lmtest)

# Breusch-Pagan
# alpha=0.05
bptest(modelo3)

modelo4 <- lm(formula = yp ~ log(x1) + x2 - 1, data = datos)
summary(modelo4)


bptest(modelo4)

plot(modelo4)



## No autocorrelación

# Durbin-Watson

# H_0: No hay autocorrelación en los errores
# H_1: Hay autocorrelación

# alpha = 0.05
dwtest(modelo4)


## Predicción 

# ancho del horno = 2.10
# temperatura = 3.10

# ¿Cuál sería el tiempo de cocción?

nuevo.dato <- data.frame(x1= 2.10, x2= 3.10)

modelo4$coefficients

prediccion <- predict(modelo4, newdata = nuevo.dato)
prediccion


### Análisis de Varianza

datos <- data.frame(
  Fuerza_enlace = c(
    2158.70, 1678.15, 2316.00, 2061.30, 2207.50,
    1708.30, 1784.70, 2575.00, 2357.90, 2256.70,
    2165.20, 2399.55, 1779.80, 2336.75, 1765.30,
    2053.50, 2414.40, 2200.50, 2654.20, 1753.70
  ),
  Edad_lote = c(
    15.50, 23.75, 8.00, 17.00, 5.50,
    19.00, 24.00, 2.50, 7.50, 11.00,
    13.00, 3.75, 25.00, 9.75, 22.00,
    18.00, 6.00, 12.50, 2.00, 21.50
  )
)

datos

modelo_completo <- lm(Fuerza_enlace~ Edad_lote, data = datos)
plot(datos$Edad_lote, datos$Fuerza_enlace)

abline(modelo_completo, col="red", lwd=2)


modelo_reducido <- lm(Fuerza_enlace ~ 1, data= datos)
plot(datos$Edad_lote, datos$Fuerza_enlace)
abline(modelo_reducido, col="red", lwd=2)

anova(modelo_completo)


datos <- data.frame(
  Paciente = 1:32,
  Infarc = c(
    0.119, 0.190, 0.395, 0.469, 0.130, 0.311, 0.418, 0.480,
    0.687, 0.847, 0.062, 0.122, 0.033, 0.102, 0.206, 0.249,
    0.220, 0.299, 0.350, 0.350, 0.588, 0.379, 0.149, 0.316,
    0.390, 0.429, 0.477, 0.439, 0.446, 0.538, 0.625, 0.974
  ),
  Area = c(
    0.34, 0.64, 0.76, 0.83, 0.73, 0.82, 0.95, 1.06,
    1.20, 1.47, 0.44, 0.77, 0.90, 1.07, 1.01, 1.03,
    1.16, 1.21, 1.20, 1.22, 0.99, 0.77, 1.05, 1.06,
    1.02, 0.99, 0.97, 1.12, 1.23, 1.19, 1.22, 1.40
  ),
  X2 = c(
    0,0,0,0,0,0,0,0,
    0,0,1,1,1,1,1,1,
    1,1,1,1,1,0,0,0,
    0,0,0,0,0,0,0,0
  ),
  X3 = c(
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,1,1,
    1,1,1,1,1,1,1,1
  )
)
datos


modelo_completo <- lm(Infarc ~ Area + X2 + X3, data = datos)
summary(modelo_completo)


anov <- anova(modelo_completo)

SCE_X1 <- anov$`Sum Sq`[1]
SSE_C <- anov$`Sum Sq`[4]
df2 <- anov$Df[4]
df1 <- nrow(datos) - 3 - df2
F_g <- (SCE_X1 / df1) / (SSE_C / df2)
F_g 


SCE_X1X2X3 <- sum(anov$`Sum Sq`[1:3])
df1 <- nrow(datos) - 1 - df2

F_all <- (SCE_X1X2X3 /df1) / (SSE_C / df2)
F_all


pf(F_all, df= 3, df2 = 28, lower.tail = FALSE)
summary(modelo_completo)
anov

SCE_X2X3 <- sum(anov$`Sum Sq`[2:3])
df1 <- nrow(datos) - 2 -df2

F_dos <-(SCE_X2X3 / df1) / (SSE_C / df2)
F_dos

pf(F_dos, df= 2, df2 = 28, lower.tail = FALSE)
anov

summary(modelo_completo)
