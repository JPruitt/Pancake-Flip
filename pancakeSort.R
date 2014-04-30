k<-100
m<-30
n<-1
pancakeData <- data.frame(pancakes = integer(m), expectedFlips = numeric(m), maxFlips = integer(m), minFlips = integer(m))
## str(pancakeData)

while (n <= m) {
## Generate Small vector to sort.
x<-(1:n)
set.seed(654321)

xFlips<-(rep.int(0,k))

for (j in 1:k) {
xRandom<-sample(x)
## xRandom

## Get the highest number to the bottom
## Find the maximum value in the unsorted vector
## maxX<-max(xRandom)
## maxX

## Flip the largest pancake to the top
## Identify the index of the largest pancake
## xIndex<-which.max(xRandom)
## xIndex
## Subset the vector
## xTemp<-xRandom[1:xIndex]
## xTemp
## Reverse (flip) the sub-vector
## xRev<-rev(xTemp)
## xRev
## Add back to original vector
## xRandom[1:xIndex]<-xRev
## xRandom
## Flip largest pancake to the bottom
## xRandom<-rev(xRandom)
## xRandom

## Loop it from vector length to 2 (assumes 2n flips is max required)

## get length of original vector
xLength<-length(xRandom)
## xLength

## Assign vector to xSort
xSort<-xRandom

## Initialize counters
i<-xLength
flips<-1

## Initialize test variables
xTest<-sort(xRandom)
xCheck<-(xTest==xRandom)
xContinue<-sum(xCheck)

## for (i in xLength:2) {
while (xContinue < length(x)) {
  
  ## Identify the index of the largest pancake
  xIndex<-which.max(xSort)
  
  ## Subset the vector
  xTemp1<-xSort[1:xIndex]
  xTemp2<-xSort[xIndex+1:xLength]
  
  ## Reverse (flip) the sub-vector
  xRev<-rev(xTemp1)
  
  ## Add back to original vector
  xSort[1:xIndex]<-xRev
  
  ## Update original vector
  xRandom[1:xLength]<-xSort
  
  ## Print new vector
  ## print(xRandom)
  
  ## Add flip
  flips<-flips+1
  
  ## Check if continue
  xTest<-sort(xRandom)
  xCheck<-(xTest==xRandom)
  xContinue<-sum(xCheck)
  
  if (xContinue < length(x)) {
  
    ## Flip largest pancake to the bottom
    xSort<-rev(xSort)
  
    ## Udate original vector
    xRandom[1:xLength]<-xSort
  
    ## Print new vector
    ## print(xRandom)
  
    ## Assign smaller vector to xSort
    xSort<-xSort[1:i-1]
  
    ## Update vector length
    xLength<-length(xSort)
  
    ## update counters
    i<-i-1
    flips<-flips+1
  
    ## Check if continue
    xTest<-sort(xRandom)
    xCheck<-(xTest==xRandom)
    xContinue<-sum(xCheck)
  }
}

## output number of flips required
## print(flips)

xFlips[j]<-flips
}

## print result vector
xFlips

## Descriptive statistics
## expectedFlips<-mean(xFlips)
## expectedFlips
## maxFlips<-max(xFlips)
## maxFlips
## minFlips<-min(xFlips)
## minFlips
pancakeData$expectedFlips[n]<-mean(xFlips)
## pancakeData
pancakeData$maxFlips[n]<-max(xFlips)
## pancakeData
pancakeData$minFlips[n]<-min(xFlips)
## pancakeData
pancakeData$pancakes[n]<-n

n<-n+1
}

pancakeData

lm1<-lm(pancakeData$expectedFlips ~ pancakeData$pancakes)
plot(lm1)
