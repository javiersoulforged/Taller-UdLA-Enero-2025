####################################
### Analisis Componentes Principales
####################################

library(psych)
library(devtools)
library(ggbiplot)
#install.packages("devtools")
#install_github("vqv/ggbiplot")

data(iris)
str(iris)
summary(iris)
dim(iris)

pairs.panels(iris[,-5], gap = 0, pch=21, bg = c("red", "yellow", "blue")[iris$Species])

pc <- prcomp(iris[,-5], center = TRUE, scale. = TRUE)
attributes(pc)
print(pc)
summary(pc)

pairs.panels(pc$x, gap=0, pch=21, bg = c("red", "yellow", "blue")[iris$Species])

g <- ggbiplot(pc, obs.scale = 1, var.scale = 1, groups = iris$Species,
              ellipse = TRUE, circle = TRUE, ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)


################
### tSNE
################

library(Rtsne)
#install.packages("Rtsne")
library(ggplot2)

iris_matrix <- as.matrix(iris[, -5])
iris_matrix <- unique(iris_matrix)    # quitamos duplicidad
dim(iris_matrix)
set.seed(42)

#dims = 2: Reduce a 2 dimensiones.
#perplexity = 30: Balancea aspectos locales y globales de la data.
#verbose = TRUE: Imprime mensajes de progreso.
#max_iter = 500: Configura el numero de iteraciones.

tsne_result <- Rtsne(iris_matrix, dims=2, perplexity=30, 
                     verbose=TRUE, max_iter=500)
tsne_result 
tsne_result$Y

tsne_data <- data.frame(tsne_result$Y)
tsne_data$Species <- iris$Species[!duplicated(as.matrix(iris[, -5]))]
colnames(tsne_data) <- c("Dim1", "Dim2", "Species")
tsne_data

ggplot(tsne_data, aes(x = Dim1, y = Dim2, color = Species)) +
  geom_point(size = 3) +
  labs(title = "t-SNE Visualization of Iris Dataset", x = "Dimension 1", 
       y = "Dimension 2") + theme_minimal()

tsne_result2 <- Rtsne(iris_matrix, dims=2, perplexity=49, 
                     verbose=TRUE, max_iter=500)
tsne_data2 <- data.frame(tsne_result2$Y)
tsne_data2$Species <- iris$Species[!duplicated(as.matrix(iris[, -5]))]
colnames(tsne_data2) <- c("Dim1", "Dim2", "Species")

ggplot(tsne_data2, aes(x = Dim1, y = Dim2, color = Species)) +
  geom_point(size = 3) +
  labs(title = "t-SNE Visualization of Iris Dataset", x = "Dimension 1", 
       y = "Dimension 2") + theme_minimal()


