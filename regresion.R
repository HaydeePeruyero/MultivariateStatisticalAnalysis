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
