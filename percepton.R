#Script for signle perceptron with two inputs to clasificate two line-separeted sets

setClass("perceptron", slots = list(weights="matrix", shift="numeric", activationFunction="function"))

classificate <- function (inputs, perceptron) {
  a <- perceptron@weights %*% inputs + perceptron@shift
  y <- perceptron@activationFunction(a)
  
  return (y)
}

traineePerceptron <- function (xCords, yCords, teacherValues, activationFunction) {
  #amount of learning points
  n <- length(xCords)

  #Create matrix of sets to teach percepton from x and y cords.
  dendritesMatrix <- matrix(ncol = n, nrow = 0)
  dendritesMatrix <- rbind(dendritesMatrix, xCords, deparse.level = 0)
  dendritesMatrix <- rbind(dendritesMatrix, yCords, deparse.level = 0)
  
  #Randomly find entry values for input weights in <0,1> range.
  weights <- runif(2)
  #Randomly find entry value for shift value in <0,1> range.
  shift <- runif(1)
  
  #For each epoch 
  for (epoch in 1:n) {
    repeat {
      #Get x and y cords of <epoch> point and transpose to 2 rows and 1 column matrix
      dendrites <- t(t(dendritesMatrix[,epoch]))
      #Find value of vectored multiplication of weights and dendrites incremented by shift
      a <- weights %*% dendrites + shift
      #Find activation function value for result of operation above
      y <- activationFunction(a)
      #Check, if the result value of activation function is equal pattern value
      error <- teacherValues[epoch] - y
      if (error == 0) { #If error equals 0 go forward to the next epoch
        break
      } else { #If error different from 0, then teach perceptron by modyfication of weights and shift value
        shift <- shift + error
        weights <- weights + error %*% t(dendrites)
      }
    } 
  }
  
  perceptron <- new("perceptron", weights = weights, shift = shift, 
                   activationFunction = activationFunction)
  return (perceptron)
}

addPoint <- function (x, y, perceptron) {
  color <- "red"
  if (classificate(c(x, y), perceptron) == 1) {
    color <- "blue"
  }
  points(x, y, col = color)
}

activationFunction <- function (a) {
  if (a < 0) {
    return (0)
  } 
  return (1)
}

xTraineeCordsVector <- c(-8, 2, 4, -9, -6, 5, -2, 1, -6, 4)
yTraineeCordsVector <- c(20, 20, 25, -10, -5, 9, -10, -10, -25, -16)
teacherVector <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
perceptron <- traineePerceptron(xTraineeCordsVector, yTraineeCordsVector, teacherVector, activationFunction)

plot(-30:30, -30:30, type = "n")
points(xTraineeCordsVector[1:5], yTraineeCordsVector[1:5], col = "blue")
points(xTraineeCordsVector[6:10], yTraineeCordsVector[6:10], col = "red")
abline(h = 0, v = 0, col = "gray60")

addPoint(2, -20, perceptron)

