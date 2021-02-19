library(MASS)
library(klaR)


#Method 1 : LDA
iris= read.csv(file = 'iris.csv')
#Performing EDA Initially to check how the Dataset Looks like
colnames(iris) <- c("ID","Sepal.Length", "Sepal.Width","Petal.Length","Petal.Width","Species")
#Renaming the species tso that it is distinctly visible via partimat
iris$Species[iris$Species=='Iris-setosa']="setosa"
iris$Species[iris$Species=='Iris-virginica']="virginica"
iris$Species[iris$Species=='Iris-versicolor']="versicolor"
iris$Species=factor(iris$Species)
par(bg = 'white')
plot(iris$Petal.Length, iris$Petal.Width, pch=c(21,22,23)[unclass(iris$Species)], bg=c("red","green3","blue")[unclass(iris$Species)], main="Edgar Anderson's Iris Data")
grid()

#Using LDA Classification to classify the species
lda.fit=lda(Species ~ Petal.Length + Petal.Width, data=iris )

#Visualising the LDA Clusters using the partimat function
  par(bg = 'black')
tmp=partimat(Species ~ Petal.Length + Petal.Width, data=iris  ,method="lda") 

################################################################################

#Method 2- K-Means Clustering

x = iris[,2:5]
y = iris$Species
kc = kmeans(x,3)
par(bg = 'cyan')
plot(x[c("Petal.Width", "Petal.Length")], col=kc$cluster, pch=20)
points(kc$centers[,c("Petal.Width", "Petal.Length")], col=1:3, pch=12, cex=2)

