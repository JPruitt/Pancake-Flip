## x<-c(7,2,3,4,9,10,8,6,5,1)
## x<-c(2,3,4,9,10,8,7,6,5,1)
## x<-c(10,1,2,3,4,5,6,7,8,9)
x<-c(6,5,10,9,7,4,1,3,8,2)

Flip<-0
xSort<-x
sortedX<-xSort
Continue<-0
groupAvg<-0
groupMax<-0
 
while (Continue<length(sortedX)){

## Determine Stack Hieght
Length<-length(xSort)

## 1. Check to see if stack is in order
Test<-sort(sortedX)
Check<-(Test==sortedX)
Continue<-sum(Check)

## 2. Check for adjacent pancakes
if (Continue<length(sortedX)) {
  xGroup<-as.list(rep.int(0,Length-1))

  for (j in 1:(Length-1)) {
    if ((xSort[j]+1)==xSort[j+1]) {
      xGroup[[j]]<-c(xSort[j],xSort[j+1])
    } else if ((xSort[j]-1)==xSort[j+1]) {
      xGroup[[j]]<-c(xSort[j],xSort[j+1])
    }
  }
}

checkLength<-Length

while (checkLength>length(xGroup)) {

  xGroupCheck<-(rep(F,length(xGroup)))
  
  checkLength<-length(xGroup)

  ## Collapse xGroup
  for (i in 1:length(xGroup)) {
  xGroupCheck[i]<-sum(xGroup[[i]])>0
  }
  xGroup<-xGroup[xGroupCheck]

  ## 2.a. Can groups be aggregated?
  if (length(xGroup)>1) {
    for (j in 1:(length(xGroup)-1)) {
      if ((xGroup[[j]][length(xGroup[[j]])])==xGroup[[j+1]][1]) {
        xGroup[[j]]<-c(xGroup[[j]],xGroup[[j+1]][2:length(xGroup[[j+1]])])
        xGroup[[j+1]]<-c(0,0)
      } else if ((xGroup[[j+1]][length(xGroup[[j+1]])])==xGroup[[j]][1]) {
         xGroup[[j]]<-c(xGroup[[j+1]],xGroup[[j]][2:length(xGroup[[j+1]])])
         xGroup[[j]]<-c(0,0)
        } 
    }    
  }
  ## Collapse xGroup
  for (i in 1:length(xGroup)) {
    xGroupCheck[i]<-sum(xGroup[[i]])>0
  }
  xGroup<-xGroup[xGroupCheck]
}

## 3. Rule #1: Is the largest at the top?  
if (xSort[1]==max(xSort)) {
  ##Flip 
  xSort<-rev(xSort)
  
  ## and freeze the bottom group.
  inGroup<-0
  for (i in 1:length(xGroup)) {
    if (max(xSort) %in% xGroup[[i]]){
      inGroup<-1
    }
}
  
  ## Is max(x) in a group?
  if (Continue<length(sortedX)) {  
  if (inGroup>0) {
    xMatch<-(rep(0,length(xGroup)))
      for (j in 1:length(xGroup)){
        xMatch[j]<-match(max(xSort), xGroup[[j]])
      }
    ## Subtract length of bottom group from stack
    groupIndex<-which(xMatch>0)
    sortedX[1:Length]<-xSort
    Length<-Length-(length(xGroup[[groupIndex]]))
    } else {
      ## Subtract 1 from stack
      sortedX[1:Length]<-xSort
      Length<-Length-1
    }
  }
  xSort<-xSort[1:Length]
  ## 4. Rule #2: Insert spatula below the group with the highest sum and flip
} else {
  groupAvg<-as.numeric(c(lapply(xGroup, mean)))
  ## groupSums<-as.numeric(c(lapply(xGroup, sum)))
  maxGroup<-which.max(groupAvg)
  flipSpot<-tail(xGroup[[maxGroup]], n=1)
  xTemp<-xSort[1:which(xSort==flipSpot)]
  xTemp<-rev(xTemp)
  xSort[1:which(xSort==flipSpot)]<-xTemp
  sortedX[1:Length]<-xSort
  
  ## print(sortedX)
  
} 
Flip<-Flip+1 
}
## output sorted stack
 print(x) 
 print(sortedX)
 print(Flip) 
