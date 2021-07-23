{
  library(dplyr)
  library(ggplot2)
  library(statsr)
  library(matlib)
  Sys.setlocale(category = "LC_ALL", locale = "english")
  library("writexl")
  library(deming)
  library(mcr) 
}

{
  X <- c(1:15)
  Y = 2 * X + 3
  #print(X)
}

{
  noiseX <- rnorm(length(X))
  noiseY <- rnorm(length(Y))
}

{
  x = X + noiseX
  y = Y + noiseY
}

#Illustration of sample
{
  plot(x, y, xlim = c(0, 17), ylim = c(0, 37), type = "p", pch = 16, col = "black")
  abline(c(3, 2), col = "green")
  #lines(X, Y, xlim = c(0, 17), ylim = c(0, 37), type = "o", col = "black")
}

#Deming Regression through these points

data(arsenate)

library(mcr)
dem.reg <- mcreg(x,y, method.reg = "Deming")
dem.reg@para[1:2]
{
  plot(x,y, main = "Blue - line gained by Deming regression, green - true relations", xlab = "x", ylab = "y", xlim = c(0, 17), ylim = c(0, 37), pch = 16)
  abline(dem.reg@para[1:2], col = "blue")
  abline(c(3, 2), col = "green")
}

#curve(2 * x + 3, X, xlim = c(0, 17), ylim = c(0, 37))
#curve(x, y, xlim = c(0, 17), ylim = c(0, 37), type = "p", pch = 16, col = "black")

{
  Point4x <- c(-5, -1, 0, 2, 4)
  noise = rnorm(length(Point4x), sd = 20)
  secondnoise = rnorm(length(Point4x), sd = 1)
  Point4y <- g(Point4x)
}

par(mfrow=c(1,2)) 

{
  
  g <- function(x) {
    return((x + 1) * (x^2 - 30) + 80)
  }

  {
    curve(g, xlim = c(-6, 6), ylim = c(-20, 140), lwd = 2, col = "blue", ylab = "True relation", main = "Sampled points with noise along OY")
    #curve((x + 1) * (x^2 - 30) + 100, xlim = c(-6, 6), ylim = c(-20, 140), add = TRUE, col = "blue", lwd = 1)
    #curve((x + 1) * (x^2 - 30) + 60, xlim = c(-6, 6), ylim = c(-20, 140), add = TRUE, col = "blue", lwd = 1)
  
      
    points(Point4x, Point4y + noise, pch = 16)
    
    for (i in 1:length(Point4x)) {
      arrows(Point4x[i], Point4y[i], Point4x[i], Point4y[i] + noise[i], angle = 20, length = 0.15, code = 2, lwd = 1)
    }
  }
  
  {
    curve(g, xlim = c(-6, 6), ylim = c(-20, 140), lwd = 2, col = "blue", ylab = "True relation", main = "Sampled points with noise along OY and OX")
    #curve((x + 1) * (x^2 - 30) + 100, xlim = c(-6, 6), ylim = c(-20, 140), add = TRUE, col = "blue", lwd = 1)
    #curve((x + 1) * (x^2 - 30) + 60, xlim = c(-6, 6), ylim = c(-20, 140), add = TRUE, col = "blue", lwd = 1)
    
    
    points(Point4x + secondnoise, Point4y + noise, pch = 16)
    
    for (i in 1:length(Point4x)) {
      arrows(Point4x[i], Point4y[i], Point4x[i] + secondnoise[i], Point4y[i] + noise[i], angle = 20, length = 0.15, code = 2, lwd = 1)
    }
  }
  
}

par(mfrow = c(1, 1))

#Calculate k and b for there points

print(x)

{
  A = sum(x^2) - sum(x) * mean(x)
  B = sum(x * (mean(y) - y))
  C = sum(x) * mean(x) - mean(x)^2
  D = -B - 2 * mean(x) * sum(y - mean(y))
  E = -sum((y - mean(y))^2)
 
  k <- 2.04896
  b <- mean(y) - k * mean(x)
  
  print(A)
}

{
  plot(x,y, main = "Red - line that minimizes functional", xlab = "x", ylab = "y", xlim = c(0, 17), ylim = c(0, 37), pch = 16)
  #abline(dem.reg@para[1:2], col = "blue")
  abline(c(b, k), col = "red")
  #abline(c(3, 2), col = "red")
  arrows(x[11], y[11], x[11], k * x[11] + b, length = 0.1)
  arrows(x[11], y[11], (y[11] - b) / k, y[11], length = 0.1)
}

F <- function(x, y, k, b) {
  return ((1 + 1 / k^2) * sum((y - k * x - b)^2))
}

{
  temp <- 0
  for (i in 1:length(x)) {
    temp = temp + (b + k * x[i] - y[i]) * x[i]
  }
  
  temp = (k^3+k) * temp - sum((b + k * x - y)^2)
  temp
}

F(x, y, 2.04, mean(y) - 2.04 * mean(x))
F(x, y, k, b)
F(x, y, 2, 3)

sum(y - b) / sum(x)

{
  temp <- 0
  for (i in 1:length(x)) {
    temp = temp + (b + k * x[i] - y[i]) * (k^3*x[i] + y[i] - b)
  }
  temp
}

c(2, 3) * c(5, 7)

{
  sum <- 0
  sum = sum + k^4 * sum(x^2)
  sum = sum + (k^3 - k) * (-sum(x * y) + b * sum(x))
  sum = sum - sum((y - b)^2)
  sum
}

sum((y-mean(y))^2)
sum(2 * (mean(x) - x) * (y - mean(y)))
sum((mean(x) - x)^2)

sum((mean(x) - x)^2)
sum((mean(x) - x) * (y - mean(y)))
-sum((y - mean(y))^2)


sum((b + k * x - y) * (k^3 * x + y - b))

sum(x^2) - sum(x) * mean(x)
sum((x - mean(x))^2)


SPDxy = sum(x * y) - mean(y) * sum(x)
SSDx = var(x) * (length(x) - 1)
SSDy = var(y) * (length(y) - 1)

beta = ((SSDy - SSDx) + sqrt((SSDy - SSDx)^2 + 4 * SPDxy^2)) / (2 * SPDxy)
alpha = mean(y) - mean(x) * beta

{
  plot(x,y, main = "Red - line that minimizes functional, Blue - Deming Regression line", xlab = "x", ylab = "y", xlim = c(0, 17), ylim = c(0, 37), pch = 16)
  #abline(dem.reg@para[1:2], col = "blue")
  abline(c(b, k), col = "red")
  abline(c(alpha, beta), col = "blue")
}