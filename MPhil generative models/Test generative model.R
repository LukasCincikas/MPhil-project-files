#Test generative model with parameters ALPHA and GAMMA; CUMULATIVE utility
setwd("C:/Users/Lukas Cincikas/Documents/R/MPhil project files/MPhil generative models")

alpha <-  4 #runif(1,min=0,max=5)
gamma <-  4 #rgamma(1,1,scale=1)


Choice1Func <- function(ObjLeft=ObjLeft,alpha=alpha,gamma=gamma) #determines side/colour choice probability
{
  Pleft <- ObjLeft^alpha / (ObjLeft^alpha + (1-ObjLeft)^alpha)
  return(Pleft)
}

TotalIter <- 1
Stage <- 1
EU <- NA
Choice1 <- NA
Pbet <- NA
Choice2 <- NA
Product <- NA
CurrentPoints <- NA
LeftWon <- NA
LeftSideN <- NA
Subject_Wins <- NA
Block_track <- NA
#Main choice loop
for(block in 1:20)
{
  CurrentPoints[TotalIter] <- 100 #Resets points to 100 after each block
  BlockIter <- 1 #Resets the block iterations to 1
  while(CurrentPoints[TotalIter]>1 && BlockIter<10)
  {
    ObjLeft <- runif(1,min=1,max=10) #Determines number of squares on left in trial
    ObjLeft <- floor(ObjLeft) / 10
    LeftSideN[TotalIter] <- ObjLeft #keeps track of left n for output
    Pleft <- Choice1Func(ObjLeft,alpha,gamma) #Determines left colour choice probability
    Choice1[TotalIter] <- rbinom(1,size=1,prob=Pleft) #Colour choice output, 1=left, 0=right
    WinResult <- runif(1,min=0,max=1)
    if(WinResult>ObjLeft) #Generates the winning side
    {
      LeftWon[TotalIter] <- 0
    }else if (WinResult < ObjLeft)
    {
      LeftWon[TotalIter] <- 1
    }
    
    if(Choice1[TotalIter]==0) #If the choice was right, sets right evaluation probability for EU
    {
      ObjLeft <- 1 - ObjLeft
    }
    #setting EU values...
    EU[1] <- Choice1Func(ObjLeft,alpha,gamma) * (CurrentPoints[TotalIter] + CurrentPoints[TotalIter]*0.05) + (1 - Choice1Func(ObjLeft,alpha,gamma))*(CurrentPoints[TotalIter] - CurrentPoints[TotalIter]*0.05)
    EU[2] <- Choice1Func(ObjLeft,alpha,gamma) * (CurrentPoints[TotalIter] + CurrentPoints[TotalIter]*0.25) + (1 - Choice1Func(ObjLeft,alpha,gamma))*(CurrentPoints[TotalIter] - CurrentPoints[TotalIter]*0.25)
    EU[3] <- Choice1Func(ObjLeft,alpha,gamma) * (CurrentPoints[TotalIter] + CurrentPoints[TotalIter]*0.50) + (1 - Choice1Func(ObjLeft,alpha,gamma))*(CurrentPoints[TotalIter] - CurrentPoints[TotalIter]*0.50)
    EU[4] <- Choice1Func(ObjLeft,alpha,gamma) * (CurrentPoints[TotalIter] + CurrentPoints[TotalIter]*0.75) + (1 - Choice1Func(ObjLeft,alpha,gamma))*(CurrentPoints[TotalIter] - CurrentPoints[TotalIter]*0.75)
    EU[5] <- Choice1Func(ObjLeft,alpha,gamma) * (CurrentPoints[TotalIter] + CurrentPoints[TotalIter]*0.95) + (1 - Choice1Func(ObjLeft,alpha,gamma))*(CurrentPoints[TotalIter] - CurrentPoints[TotalIter]*0.95)
    
    Product[1] <- EU[1]*gamma
    Product[2] <- EU[2]*gamma
    Product[3] <- EU[3]*gamma
    Product[4] <- EU[4]*gamma
    Product[5] <- EU[5]*gamma
    
    for(k in 1:5)
    {
      while(Product[k]>700)
      {
        Product <- Product - 100
      }
    }
    
    SumFactor <- exp(Product[1]) + exp(Product[2]) + exp(Product[3]) + exp(Product[4]) + exp(Product[5]) #for less clunky Pbet eval
    for(i in 1:5)
    {
      Pbet[i] <- exp(Product[i]) / SumFactor #evalues prob of a choice of given bet size
    }
    
    Choice2[TotalIter] <- sample(c(0.05,0.25,0.5,0.75,0.95), size=1, replace=TRUE, prob=Pbet) #makes the bet choice
    if(LeftWon[TotalIter]==Choice1[TotalIter]) #updates the number of points after trial and keeps track of subject wins
    {
      CurrentPoints[TotalIter+1] <- CurrentPoints[TotalIter] + CurrentPoints[TotalIter]*Choice2[TotalIter]
      Subject_Wins[TotalIter] <- 1
    } else
    {
      CurrentPoints[TotalIter+1] <- CurrentPoints[TotalIter] - CurrentPoints[TotalIter]*Choice2[TotalIter]
      Subject_Wins[TotalIter] <- 0
    }
    CurrentPoints[TotalIter+1] <- round(CurrentPoints[TotalIter+1]) #Rounds the points. I THINK that's done in the actual task?
    TotalIter <- TotalIter + 1 #next TOTAL trial
    BlockIter <- BlockIter + 1 #next trial IN THIS BLOCK. Once this hits 10, the while loop has to reset.
  }
  Block_track[block] <- BlockIter-1
}

#Block_output <- NA
#Block_output[1:Block_track[1]] <- 1
#Block_output[(Block_track[1]+1):sum(Block_track[1:2])] <- 2
#Block_output[(sum(Block_track[1:2])+1):sum(Block_track[1:3])] <- 3   !!!!! these need to be put back in when making more realistic mock data
#Block_output[(sum(Block_track[1:3])+1):sum(Block_track[1:4])] <- 4

#Stage_output <- NA
#Stage_output[1:sum(Block_track[1:2])] <- 3
#Stage_output[(sum(Block_track[1:2])+1):sum(Block_track)] <- 5

converted_choice2 <- NA
for(l in 1:length(Choice2)) {
  if(Choice2[l] == 0.05) {
    converted_choice2[l] <- 1
  } else if(Choice2[l] == 0.25) {
    converted_choice2[l] <- 2
  } else if(Choice2[l] == 0.50) {
    converted_choice2[l] <- 3
  } else if(Choice2[l] == 0.75) {
    converted_choice2[l] <- 4
  } else {
    converted_choice2[l] <- 5
  }
}

mock_data <- data.frame(
#  Stage <- Stage_output, !!!!! also need to be put back in
#  Block <- Block_output,
  trial.initial.points <- CurrentPoints[1:(length(CurrentPoints)-1)],
  trial.final.points <- CurrentPoints[2:length(CurrentPoints)],
  n.left.colour.boxes <- LeftSideN*10,
  left.colour.chosen <- Choice1,
  percentage.staked <- Choice2*100,
  stake.index <- converted_choice2,
  left.won <- LeftWon,
  subject.won <- Subject_Wins
)

#names(mock_data)[1] <- "Stage" !!!!! also back in
#names(mock_data)[2] <- "Block"
names(mock_data)[1] <- "trial.initial.points"
names(mock_data)[2] <- "trial.final.points"
names(mock_data)[3] <- "n.left.colour.boxes"
names(mock_data)[4] <- "left.colour.chosen"
names(mock_data)[5] <- "percentage.staked"
names(mock_data)[6] <- "stake.index"
names(mock_data)[7] <- "left.won"
names(mock_data)[8] <- "subject.won"

write.csv(mock_data, file="mock_data5.csv", row.names = FALSE)