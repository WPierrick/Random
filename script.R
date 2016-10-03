install.packages("kohonen")
libray("kohonen")
setwd("...")
dataset <- read.csv("diabetic_data.csv")
dataset2<-data.matrix(dataset)
idmap <- read.csv("IDs_mapping.csv")


#Self organizing maps
result.som <- som(dataset2, grid=somgrid(10,10, "rectangular"))

plot(result.som, type="codes", main = c("Codes X", "Codes Y"))
X11()
plot(result.som, type="changes")
X11()
plot(result.som, type="counts")

coolBlueHotRed <- function(n, alpha = 1) {
	rainbow(n, end=4/6, alpha=alpha)[n:1]
}
plot(result.som, type="quality", palette.name = coolBlueHotRed)


result.som <- som(dataset2, grid=somgrid(10,10, "hexagonal"), rlen = 50,
				toroidal=TRUE)


#Biclustering
res <- biclust(dataset2, method=BCCC(), delta=1.5,  alpha=1, number=10)
plotclust(res, dataset2)

#Spectral clustering
res <- biclust(dataset2, method=BCSpectral(), numberOfEigenvalues=3)
