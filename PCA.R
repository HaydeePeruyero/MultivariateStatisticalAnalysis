# Análisis de Componentes Principales

## Ejemplo 1: usar la matriz de covarianza de 3 variables X1,
## X2 y X3

Sigma <- matrix(c(1, -2, 0,
                  -2, 5, 0,
                  0,  0, 2),
                nrow = 3, byrow = TRUE)

Sigma


## ¿A que elementos de Sigma están asociadas las CP?
## Necesitamos calcular los valores propios y vectores propios


eig <- eigen(Sigma)
## por defecto eigen devuelve los valores propios ordenados

lamda <- eig$values
lamda

E <- eig$vectors
E

#Verifiquemos que E'\Sigma E tiene en la diagonal los valores
# propios

D <- t(P) %*% Sigma %*% P

print(round(D,8))

# comprobar la varianza y cov de las componentes

vars_pc <- sapply(1:3, function(i) 
  as.numeric( t(E[,i]) %*% Sigma %*% E[,i]))

vars_pc

covs_pc <- matrix(0,3,3)

for(i in 1:3) for(j in 1:3) covs_pc[i,j] <- 
  as.numeric( t(E[,i]) %*% Sigma %*% E[,j])

round(covs_pc,8)

## Prop: \sigma_11 + \sigma_22 + ... + \sigma_pp = suma(var(X_i))
## =\lamda_1 + \lamda_2 + ...+\lambda_p = suma(var(Y_i))

traza_sigma <- sum(diag(Sigma))
traza_sigma

suma_autovalores <- sum(lamda)
suma_autovalores

## ¿Cómo escribimos las Componentes Principales?

## Proporción de la varianza explicada por la k-ésima CP

total_var <- sum(diag(Sigma))

prop_explicada <- lamda / total_var
prop_explicada

cum_prop <- cumsum(prop_explicada)
cum_prop


## Correlaciones

sigma_diag <- diag(Sigma)

ncomp <- length(lamda)

rho <- matrix(NA, nrow=ncomp, ncol=ncol(E),
              dimnames = list(paste0("Y",1:ncomp), 
                              paste0("X", 1:ncol(E))))

for(i in 1:ncomp){
  for(j in 1:ncol(E)){
    rho[i,j] <- E[j,i] *sqrt(lamda[i]) / sqrt(sigma_diag[j])
  }
}

rho

## Ejemplo 2: 

# matriz de covarianza

Sigma <- matrix(c(1, 4,
                  4, 100), nrow = 2, byrow = TRUE)

Sigma


sd_vec <- sqrt(diag(Sigma))
sd_vec

#matriz de correlaciones
R_mat <- diag( 1 /sd_vec ) %*% Sigma %*% diag(1/sd_vec)
R_mat


## Calcular valores y vectores propios de Sigma y de R

eig_Sigma <- eigen(Sigma)
lambda_S <- eig_Sigma$values
lambda_S
P_S <- eig_Sigma$vectors
P_S

eig_R <- eigen(R_mat)
lambda_R <-eig_R$values
lambda_R
P_R <- eig_R$vectors
P_R

# Proporción de la varianza explicada de Sigma

total_var_S <- sum(diag((Sigma)))
prop_S <- lambda_S/total_var_S

prop_S

# Proporción de la varianza explicada de R

total_var_R <- sum(diag(R_mat))
prop_R <- lambda_R / total_var_R

prop_R

# Correlaciones entre las Y_i y las X_i

sigma_dia <- diag(Sigma)
sigma_dia
rho_YX_Sigma <- matrix(NA, 2,2, 
                       dimnames = list(paste0("Y",1:2), 
                                       paste0("X",1:2)))

for(i in 1:2) for (j in 1:2){
  rho_YX_Sigma[i,j] <- P_S[j,i] * sqrt(lambda_S[i])/ sqrt(sigma_dia[j])
}

rho_YX_Sigma
P_S
lambda_S
sigma_dia


## Correlaciones entre las Y_i y las Z_i

R_mat

rho_YZ_R <- matrix(NA, 2,2, 
                       dimnames = list(paste0("Y",1:2), 
                                       paste0("Z",1:2)))

for(i in 1:2) for (j in 1:2){
  rho_YZ_R[i,j] <- P_R[j,i] * sqrt(lambda_R[i])
}
rho_YZ_R


# t(P)*Sigma*P = diag(\lambda)

round(diag(t(P_S) %*% Sigma %*% P_S),3) == round(lambda_S,3)
lambda_S

lambda_R

# t(P_R) * R * P_R

diag(t(P_R) %*% R_mat %*% P_R)

lambda_R


## Ejemplos con bases de datos

library(factoextra)
library(readxl)

records <- read_excel("D:/Users/hayde/Documents/R_sites/MultivariateStatisticalAnalysis/data/NationalTrackRecords2.xlsx")

View(records)

records2 <- records[,2:8]
rownames(records2) <- records$...1

var(records2)
cor(records2)

library(corrplot)

cor.mat <- cor(records2, use = "complete.obs")
corrplot(cor.mat)


# CP

cp1 <- princomp(records2, cor = TRUE)
summary(cp1)


cp1$loadings



## Tarea: Investigar porque no dan los mismos signos

cp1$sdev

varianza.cp1 <- (cp1$sdev)^2
varianza.cp1

summary(cp1)


proporcion.varianza.cp1 <- (cp1$sdev)^2 / sum((cp1$sdev)^2)
proporcion.varianza.cp1

porcentaje.varianza.cp1 <- 100*(cp1$sdev)^2 / sum((cp1$sdev)^2)
porcentaje.varianza.cp1

proporcion.acumulada_varianza <- cumsum(proporcion.varianza.cp1)
proporcion.acumulada_varianza


## Tarea1: Investigar porque no dan los mismos signos
## Tarea2: ¿Cómo decidimos con cuantas componentes trabajar?