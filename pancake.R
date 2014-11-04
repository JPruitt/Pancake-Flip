# Author: Shaun Cunningham
# Date: 3 November 2014

#Execute trials on arrays from length 1 to largestArray.
largestArray <- 30

#Number of trials for each array size.
numObs <- 100

posLargest <- function(x, startAt=1)
{
  which(x==max(x[startAt:length(x)]), arr.ind=TRUE)[1]
}

#Sorts an array using the pancake algorithm, largest to smallest
#Return value is the number of flips needed to sort the array
pancakeSort <- function(x)
{
  flips <- 0
  
  for(bottomUnsorted in seq_along(x))
  {
    largest <- posLargest(x, bottomUnsorted)
    
    print(c("Unsorted position:", bottomUnsorted))
    print(c("Largest position:", largest))
    
    if(largest==bottomUnsorted)
    {
      print("No flip needed")
      print(x)
    }
    else if(largest == length(x))
    {
      print("One flip neeced")
      print(x)
      x[bottomUnsorted:length(x)] <- rev(x[bottomUnsorted:length(x)])
      flips <- flips+1
      print(c("Flipped ",bottomUnsorted," to ",length(x)))
      print(x)
    }
    else if(largest > bottomUnsorted)
    {
      print("Two flips needed")
      print(x)
      x[largest:length(x)] <- rev(x[largest:length(x)])
      flips <- flips+1
      print(c("Flipped ",largest," to ",length(x)))
      print(x)
      x[bottomUnsorted:length(x)] <- rev(x[bottomUnsorted:length(x)])
      flips <- flips+1
      print(c("Flipped ",bottomUnsorted," to ",length(x)))
      print(x)
    }    
  }
  
  flips
}

arraySize <- seq(1:largestArray)
observation <- seq(1:numObs)

results <- expand.grid(observation=observation,arraySize=arraySize)
#results$flips <- sapply(lapply(results$arraySize,runif), pancakeSort)
results$arrayToSort <- sapply(results$arraySize,function(x) rank(runif(x)))
results$flips <- sapply(results$arrayToSort, pancakeSort)

# Big-O bound of worst performance
bigO <- 2*(arraySize -1 )
par(pch=17)
plot(arraySize, bigO)
#legend(1,legend="Big-O Performance Bound",pch=17)

# Observed mean flips
obsMean <- aggregate(flips ~ arraySize, results, mean)
par(col="red")
par(pch=19)
points(obsMean$arraySize, obsMean$flips)
#legend(2,legend="Mean Flips Required", pch=19)
par(col="black")

# Expected number of flips, using my solution to the recurrence series
eFlips <- sapply(arraySize, function(x)  2*x + 1 - 3*sum(1/seq(x)))
par(col="blue")
par(pch=17)
points(arraySize, eFlips)
par(col="black")

# Actual observations of flips required
par(pch=1)
points(results$arraySize,results$flips)

# Just how well does expected number of flips predict the mean?
linModel <- lm(obsMean$flips ~ eFlips + 0)
summary(linModel)
# I got an R-squared of 0.9999 Nailed it!