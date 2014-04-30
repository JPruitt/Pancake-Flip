## Generate Small vector to sort.
x<-(1:10)
set.seed(654321)
k<-1000
xFlips<-(rep.int(0,k))

for (j in 1:k) {
  xRandom<-sample(x)
  
  ## get length of original vector
  xLength<-length(xRandom)
  
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
  
  xFlips[j]<-flips
}

## print result vector
xFlips

## Descriptive statistics
expectedFlips<-mean(xFlips)
expectedFlips
maxFlips<-max(xFlips)
maxFlips
minFlips<-min(xFlips)
minFlips
