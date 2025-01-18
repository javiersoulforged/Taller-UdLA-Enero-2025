#install.packages("factoextra")
#install.packages("fpc")
library(fpc)
library(factoextra)

################################
### Comparando kmeans vs. Dbscan
################################

data("multishapes")
df <- multishapes[, 1:2]
head(df)
plot(df)
set.seed(123)

km.res <- kmeans(df, 5, nstart = 25) # Método Kmeans
fviz_cluster(km.res, df, frame = FALSE, geom = "point")

data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]
set.seed(123)

db <- fpc::dbscan(df, eps = 0.15, MinPts = 5) # Método Dbscan
print(db)
db$cluster[sample(1:1089, 50)]

# Plot DBSCAN results
plot(db, df, main = "DBSCAN", frame = FALSE)
fviz_cluster(db, df, stand = FALSE, frame = FALSE, geom = "point")

# Como encontrar eps
dbscan::kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)

#######################################
### Kmeans para Tratamiento de imagenes
#######################################

library(ggplot2)
library(jpeg)
#install.packages("jpeg")

### Imagen "Color Bird"

plotTheme <- function() {
  theme(
    panel.background = element_rect(
      size = 3,
      colour = "black",
      fill = "white"),
    axis.ticks = element_line(
      size = 2),
    panel.grid.major = element_line(
      colour = "gray80",
      linetype = "dotted"),
    panel.grid.minor = element_line(
      colour = "gray90",
      linetype = "dashed"),
    axis.title.x = element_text(
      size = rel(1.2),
      face = "bold"),
    axis.title.y = element_text(
      size = rel(1.2),
      face = "bold"),
    plot.title = element_text(
      size = 20,
      face = "bold",
      vjust = 1.5)
  )
}

image <- readJPEG("ColorfulBird.jpg")
imgDm <- dim(image)

imgRGB <- data.frame(
  x = rep(1:imgDm[2], each = imgDm[1]),
  y = rep(imgDm[1]:1, imgDm[2]),
  R = as.vector(image[,,1]),
  G = as.vector(image[,,2]),
  B = as.vector(image[,,3])
  )

ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) +
  labs(title = "Original Image: Colorful Bird") +
  xlab("x") +
  ylab("y") +
  plotTheme()

kClusters = 5
kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
kColours <- rgb(kMeans$centers[kMeans$cluster,])

ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
  xlab("x") +
  ylab("y") + 
  plotTheme()


