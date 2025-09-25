### Plots multivariados


library(aplpack)

data(iris)

faces(iris[c(1,51,101),1:4],
      nrow.plot = 1,
      ncol.plot = 3,
      main = "La primer flor de cada especie",
      print.info = TRUE)

faces(iris[c(1:10,51:60, 101:110), 1:4],
      nrow.plot = 3,
      ncol.plot = 10)


## Curvas de Andrew

library(andrews)

andrews(iris, type=1, clr = 5, ymax = 3, 
        main= "Curva tipo 1")


# Gráficos de Paralelas

library(lattice)

data("mtcars")

parallelplot(~mtcars|mtcars$cyl, main="Plot de paralelas por Cilindros")

parallelplot(~mtcars[, c(1,6,7,3,4,5)], col=as.numeric(mtcars$cyl)-3)


parallelplot(~iris|iris$Species)

parallelplot(~iris, col=as.numeric(iris$Species))


# Estrellas

palette(rainbow(12, s=0.6, v=0.75))

stars(mtcars[,1:7], len = 0.8, key.loc = c(12,15), draw.segments = TRUE, main="Base de motores")


stars(mtcars[,1:7], locations = c(0,0), radius=FALSE, 
      key.loc = c(0,0), lty=2, main="Plot de radar")

# ej: replicar el ejemplo con la base de datos iris


stars(iris[,1:4], locations = c(0,0), radius=FALSE, 
      key.loc = c(0,0), lty=2)


stars(iris[,1:4], len = 0.8, key.loc = c(0,0), draw.segments = TRUE)





















