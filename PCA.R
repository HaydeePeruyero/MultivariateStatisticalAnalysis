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


#----------- 
## Aplicaciones a datos reales
library(factoextra)

library(readxl)
records <- read_excel("D:/Users/hayde/Documents/R_sites/MultivariateStatisticalAnalysis/data/NationalTrackRecords2.xlsx")

records2 <- records[,2:8]

rownames(records2) <- records$...1

var(records2)

cor(records2)

library(corrplot)

res1 <- cor.mtest(records2, conf.level=0.95)

res2 <- cor.mtest(records2, conf.level=0.99)

cor.mat <- cor(records2, use="complete.obs")

corrplot(cor.mat)

cp1 <- princomp(records2, cor = TRUE)

summary(cp1)

cp1$loadings

#Y_1 = 0.368*(100m -mean(100m)) + 0.365 *(200m - mean(200m)) +....

cp1$sdev

varianza.cp1 <- (cp1$sdev)^2
varianza.cp1

proporcion.varianza.cp1 <- varianza.cp1 / sum(varianza.cp1)
proporcion.varianza.cp1

porcentaje.varianza <- varianza.cp1*100 / sum(varianza.cp1)
porcentaje.varianza

proporcion.acumulada_varianza <- cumsum(proporcion.varianza.cp1)
proporcion.acumulada_varianza


screeplot(cp1, type = "lines")


##Kaiser
# Necesitamos

pca_obj <- prcomp(records2, scale. = TRUE)

eigenvalues <- pca_obj$sdev^2
eigenvalues


fviz_eig(pca_obj, addlabels = TRUE, ylim=c(0,100))

componentes_kaiser <- which(eigenvalues > 1)
componentes_kaiser


### Horn

library(psych)

set.seed(123)

fa.parallel(records2,
            fa = "pc",
            n.iter = 100,
            show.legend = TRUE,
            main = "Análisis Paralelo - Procedimiento de Horn")


## eigenvalores de la matriz de correlación

ev_cor <- eigen(cor.mat)
ev_cor


library(nFactors)


ap <- parallel(subject = nrow(records2),
               var = ncol(records2),
               rep = 100,
               cent = 0.05)

ap


num_cp <- nScree(x = ev_cor$values,
                 aparallel = ap$eigen$qevpea,
                 cor = TRUE)

num_cp


##Plot de visualización

biplot(pca_obj, scale = 0, cex =0.8)





fviz_pca_biplot(pca_obj,
                repel = TRUE,
                col.ind = "gray30",
                col.var = "firebrick",
                geom.ind = "point",
                label = "var",
                title = "Biplot de PCA")



biplot(cp1)

#Interpretarlo



