#Question 1
data(iris)

par(mfrow = c(1, 2))  # 1 row, 2 columns

boxplot(Sepal.Length ~ Species, data = iris, 
        xlab = "Species", ylab = "Sepal Length", 
        main = "Sepal Length by Species")

boxplot(Petal.Length ~ Species, data = iris, 
        xlab = "Species", ylab = "Petal Length", 
        main = "Petal Length by Species")

plot(Sepal.Length ~ Petal.Length, data = iris, 
     col = iris$Species, 
     xlab = "Petal Length", ylab = "Sepal Length",
     main = "Scatterplot of Sepal Length and Petal Length")
legend("topright", legend = unique(iris$Species), col = unique(iris$Species), pch = 1)
#Question 2
flip<-function(image)
{
 image.mat<-as.array(image[,,1,])
 flip.mat<-image.mat[nrow(image.mat):1,,]
 flipimage<-as.cimg(flip.mat)
 return(flipimage)
}
#question 5
calc<-function(i,n)
{
  if(i==x)
  {
    return(i/100)
  }
  else 
    return(i/100+(1-i/100)*calc(i+1,n))
}
prob=calc(1,12)
#we see that on (1+12)th day the probability goes over 50% to get half pill, hence we get 13 as the answer.