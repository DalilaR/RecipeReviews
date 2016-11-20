library(kohonen)
set.seed(106)
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')
sommap <- som(normalizeWords,grid = somgrid(5, 5, "rectangular"))
plot(sommap, type = "property", property = sommap$codes[,1],main = colnames(sommap$codes)[1])
plot(sommap, type="dist.neighbours", main = "SOM neighbour distances")
som.hc <- cutree(hclust(dist(sommap$codes)), 5)
add.cluster.boundaries(sommap, som.hc)

plot(sommap, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters")
#For Reviews
wmatrix = as.matrix(WordMatrix)
sommap <- som(wmatrix[1:233,1:108],grid = somgrid(5, 5, "hexagonal"))
plot(sommap, type = "property", property = sommap$codes[,35],main = colnames(sommap$codes)[85])
plot(sommap, type="dist.neighbours", main = "SOM neighbour distances")
plot(sommap, type="changes")
plot(sommap, type="count")
plot(sommap, type = "mapping",property = sommap$codes[,35])
mydata <- sommap$codes
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)
som_cluster <- cutree(hclust(dist(sommap$codes)), 6)
# plot these results:
plot(sommap, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(sommap, som_cluster)
#Get the clusters
sommap$unit.classif



