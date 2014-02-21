###############
#Problem Set 3#
#Taeyong Park # 
#Feb 20, 2014 #
###############

## Set my directory.
setwd("~/GitHub/ProblemSet4")
rm(list=ls())
options(digits=3)
options(stringsAsFactors=F) # To avoid changing strings to factors as default

# Load several libraries that would be used.
lapply(c("foreign", "plyr", "ggplot2",  "gridExtra", "lattice"), 
       library, character.only=TRUE)


########## I. Reading in data without a clean format ##########

# Create a function that parses and extracts data from a file to store them to specific directories.
parsingOut <- function(file="~/GitHub/ProblemSet4/NetLogo.csv"){
  
  ### 1. Top Directory ###
  #Use the scan function to name the top directory. 
  topDirName <- scan(file, skip=1, nlines=1, what=" ", sep=",", n=1)
  # Skip the first line of the file; Read one line; The mode of data is character; Separate by ","; Read only the first value of the line. 
  
  # In the same way, use the scan function to read the date and time that the simulation was run.
  dateTime <- scan(file, skip=2, nlines=1, what=" ", sep=",", n=1)
  
  # Since dateTime includes "/", ":" and "-", directory may not be created.
  # We want to use "_" or "." instead of them.
  dateTime <- gsub("/", "_", dateTime)
  dateTime <- gsub(":", ".", dateTime)
  dateTime <- gsub("-", ".", dateTime)
  dateTime <- gsub(" ", "_", dateTime) # Also, replace a blank with "_"
  # Combine the directory name and date and time
  topDir <- paste(topDirName, dateTime, sep="_")
  
  # Use the dir.create function to create the top directory.
  dir.create(path=topDir)
  # Set the directory as the working directory.
  setwd(dir=topDir) 
  
  ### 2. Sub-directory: Globals, Turtles, Plots ### 
  # Create three sub-directories.
  llply(.data=c("Globals", "Turtles", "Plots"), 
        .fun=function(x) dir.create(path=x))
  
  ## 1) Globals ##
  
  # Set a new working directory.
  setwd(dir="Globals")
  
  # Read the nineth and thenth lines.
  globals1<- scan(file=file, what="", nlines=1, sep=",", skip=8)
  globals2<- scan(file=file, what="", nlines=1, sep=",", skip=9)
  # Create an empty list that will contain globals1 and globals2.
  globals <- list()
  # First, store globals2 which includes numeric values to globals
  for (i in 1:length(globals1)){
    globals[[i]]<-globals2[i]
  }
  # Delete "[" and "]".
  for (i in 1:length(globals)){
    globals[[i]]<-gsub("\\[|\\]", "", globals[[i]])
  }
  # Name globals using globals1
  names(globals)<-globals1
  # Finally, use the dump function to export globals into an R file.
  dump("globals", file="Globals.R")
  
  
  ## 2) Turtles ##
  ## This sub-directory should contain csv files for districts, voters, activists, candidates, and parties.
  
  ## Set a new directory.
  setwd("C:/Users/Taeyong/Documents/GitHub/ProblemSet4/4JobTalk3.nlogo_10_05_2010_19.42.54.385_.0400/Turtles")
  
  ## Create a data frame
  # Read the column names.
  turtleNames <- scan(file=file, skip=12, n=38, what=" ", sep=",") # 38 items
  # Create a matrix to store the data for each of column to the corresponding column. 
  turtleData <- scan(file=file, skip=13, nlines=4786, what=" ", sep=",")
  turtleData <- matrix(turtleData, nrow=4786, byrow=TRUE) 
  dim(turtleData) # 4786 by 84 matrix
  # Drop the columns with empty cells.
  turtleData <- turtleData[,-c(39:84)]
  # Make a data frame.
  turtleData <- data.frame(turtleData)
  # Assign names
  colnames(turtleData) <- turtleNames
  
  ## Now, we want to drop the variables that are constant or missing for all agents of the relevant breed.
  # First, subset the data accroding to the categorization by the breed column.
  districtsData <- subset(turtleData, breed=="{breed districts}")
  votersData <- subset(turtleData, breed=="{breed voters}")
  activistData <- subset(turtleData, breed=="{breed activists}")
  candsData <- subset(turtleData, breed=="{breed cands}")
  partiesData <- subset(turtleData, breed=="{breed parties}")
  
  # Second, create a function to drop the variables that are constant or missing for all agents of the relevant breed.
  drop <-function(data){ # Input is any data frame.
    uniqueLength <-apply(data, MARGIN=2, FUN=function(x) length(unique(x))) # To detect those variables that contain nothing.
    missing <- which(uniqueLength==1) # Those variables whose uniqueLength is 1 are empty.
    data <- data[,-c(missing)] # Drop those variables.
    return(data) 
  } 
  districtsData <- drop(data=districtsData)
  votersData <- drop(data=votersData)
  activistsData <- drop(data=activistData)
  candsData <- drop(data=candsData)
  partiesData <- drop(data=partiesData)

  # Write out csv files.
  write.csv(districtsData, "Districts.csv")
  write.csv(votersData, "Voters.csv")
  write.csv(activistsData, "Activists.csv")
  write.csv(candsData, "Candidates.csv")
  write.csv(partiesData, "Parties.csv")
  
   
  ## 3) Plots ##
  # Set a new working directory.
  setwd("C:/Users/Taeyong/Documents/GitHub/ProblemSet4/4JobTalk3.nlogo_10_05_2010_19.42.54.385_.0400/Plots")

  ## Create four sub-directories under the Plots directory.
  llply(.data=c("PositionPlot", "WinnersPlot", 
                "PolarizationPlot", "IncumbentPercentagePlot"), 
        .fun=function(x) dir.create(path=x))
  
  ## 3-1) PositionPlot ##
  
  # Set the directory.
  setwd("PositionPlot")
  
  # Read in the column names.
  positionNames <- scan(file=file, skip=8545, nlines=1, what=" ", sep=",", n=4) # We have four unique names. 
  
  ## First, we want to create a matrix that represents dimention 1.
  D1 <- scan(file=file, skip=8546, nlines=169, what=" ", sep=",")
  D1 <- matrix(D1, nrow=169, byrow=TRUE)
  # There are no data after 24th columns; Drop them.
  D1 <- D1[,-c(25:84)]
  # We should find the average position of incumbent candidates (Red, Blue), activists, and voters.
  # We see that each of items "Red", "Blue", "RedActivists", "RedVoters", "BlueVoters", and "BlueActivists" has four columns.
  # Create matices for these items.
  RedCandidates <- D1[,c(1:4)]
  BlueCandidates<-D1[,c(5:8)]
  RedActivists <- D1[,c(9:12)]  
  RedVoters <- D1[,c(13:16)]
  BlueVoters <- D1[,c(17:20)]
  BlueActivists <- D1[,c(21:24)]
  # Use the rbind function to combine these matrices and create one stacked data. 
  D1 <- rbind(RedCandidates, BlueCandidates, RedActivists, RedVoters, BlueVoters, BlueActivists)
  # Make a data frame and assign column names.
  D1 <- data.frame(D1)
  colnames(D1) <- positionNames
  
  # Add a new variable to D1 to categorize the data into six different items. 
  D1$category <- c(rep("RedCandidates", 169), rep("BlueCandidates", 169), rep("RedActivists", 169),
                  rep("RedVoters", 169), rep("BlueVoters", 169), rep("BlueActivists", 169))
  # Write out a csv file
  write.csv(D1, "D1.csv")
  
  ## Second, we want to create a matrix that represents dimention 2.
  ## The same method can be used as D1.
  D2 <- scan(file=file, skip=8730, nlines=169, what=" ", sep=",") # Skip 8730 rows since D2 starts at 8731st row.
  D2 <- matrix(D2, nrow=169, byrow=TRUE)
  # There are no data after 24th columns; Drop them.
  D2 <- D2[,-c(25:84)]
  # We should find the average position of incumbent candidates (Red, Blue), activists, and voters.
  # We see that each of items "Red", "Blue", "RedActivists", "RedVoters", "BlueVoters", and "BlueActivists" has four columns.
  # Create matices for these items.
  RedCandidates <- D2[,c(1:4)]
  BlueCandidates<-D2[,c(5:8)]
  RedActivists <- D2[,c(9:12)]  
  RedVoters <- D2[,c(13:16)]
  BlueVoters <- D2[,c(17:20)]
  BlueActivists <- D2[,c(21:24)]
  # Use the rbind function to combine these matrices and create one stacked data. 
  D2 <- rbind(RedCandidates, BlueCandidates, RedActivists, RedVoters, BlueVoters, BlueActivists)
  # Make a data frame and assign column names.
  D2 <- data.frame(D2)
  colnames(D2) <- positionNames
  
  # Add a new variable to D1 to categorize the data into six different items. 
  D2$category <- c(rep("RedCandidates", 169), rep("BlueCandidates", 169), rep("RedActivists", 169),
                   rep("RedVoters", 169), rep("BlueVoters", 169), rep("BlueActivists", 169))
  # Write out a csv file
  write.csv(D2, "D2.csv")
  
  ##  Third, we want to create a matrix that represents dimention 2.
  ## The same method can be used as D1 and D2.
  D3 <- scan(file=file, skip=8914, nlines=169, what=" ", sep=",") # Skip 8914 rows since D2 starts at 8915th row.
  D3 <- matrix(D3, nrow=169, byrow=TRUE)
  # There are no data after 24th columns; Drop them.
  D3 <- D3[,-c(25:84)]
  # We should find the average position of incumbent candidates (Red, Blue), activists, and voters.
  # We see that each of items "Red", "Blue", "RedActivists", "RedVoters", "BlueVoters", and "BlueActivists" has four columns.
  # Create matices for these items.
  RedCandidates <- D3[,c(1:4)]
  BlueCandidates<-D3[,c(5:8)]
  RedActivists <- D3[,c(9:12)]  
  RedVoters <- D3[,c(13:16)]
  BlueVoters <- D3[,c(17:20)]
  BlueActivists <- D3[,c(21:24)]
  # Use the rbind function to combine these matrices and create one stacked data. 
  D3 <- rbind(RedCandidates, BlueCandidates, RedActivists, RedVoters, BlueVoters, BlueActivists)
  # Make a data frame and assign column names.
  D3 <- data.frame(D3)
  colnames(D3) <- positionNames
  
  # Add a new variable to D1 to categorize the data into six different items. 
  D3$category <- c(rep("RedCandidates", 169), rep("BlueCandidates", 169), rep("RedActivists", 169),
                   rep("RedVoters", 169), rep("BlueVoters", 169), rep("BlueActivists", 169))
  # Write out a csv file
  write.csv(D3, "D3.csv")
  
  ## Finally, we want to include a PDF file plotting some meaningful and creative graphs.
  ## We want to see how these quantities varied across the simulation.
  ## We might be interested in comparing variable "y" across candidates and voters or comparing variable "y" across candidates and activists.
  
  ## Let's do these excercises using each of D1, D2, and D3 data.
  # Import the three data sets.
  D1 <- read.csv("D1.csv")
  D2 <- read.csv("D2.csv")
  D3 <- read.csv("D3.csv")
  # The output should be saved as a pdf file named "Positoins.pdf".
  pdf("Positions.pdf", width=10, height=5, pointsize=10) # Set appropriate size of the output.
  
  # Start with the D1.
  # First, investigate the average positions of the candidates and voters. 
  voterCandidateD1 <- ggplot(data=subset(D1, category=="RedCandidates" | category== "RedVoters" | category == "BlueCandidates" | category=="BlueVoters"), 
                           aes(x=x, y=y, colour=category)) + geom_point() + xlab("Simulation") + ylab("Avg. Position") + scale_colour_manual(name="", values=c("RedCandidates"="red", "RedVoters"="orange", "BlueCandidates"="blue", "BlueVoters"="skyblue")) + ggtitle("Voters-Candidates on D1")
  # Second, investigate the average positions of the candidates and activists. 
  voterActivistD1 <- ggplot(data=subset(D1, category=="RedCandidates" | category== "RedActivists" | category == "BlueCandidates" | category=="BlueActivists"), 
                           aes(x=x, y=y, colour=category)) + geom_point() + xlab("Simulation") + ylab("Avg. Position") + scale_colour_manual(name="", values=c("RedCandidates"="red", "RedActivists"="violet", "BlueCandidates"="blue", "BlueActivists"="aquamarine")) + ggtitle("Activists-Candidates on D1")  
 
  # Do the same exercises using D2.
  voterCandidateD2 <- ggplot(data=subset(D2, category=="RedCandidates" | category== "RedVoters" | category == "BlueCandidates" | category=="BlueVoters"), 
                           aes(x=x, y=y, colour=category)) + geom_point() + xlab("Simulation") + ylab("Avg. Position") + scale_colour_manual(name="", values=c("RedCandidates"="red", "RedVoters"="orange", "BlueCandidates"="blue", "BlueVoters"="skyblue")) + ggtitle("Voters-Candidates on D2")
  voterActivistD2 <- ggplot(data=subset(D2, category=="RedCandidates" | category== "RedActivists" | category == "BlueCandidates" | category=="BlueActivists"), 
                          aes(x=x, y=y, colour=category)) + geom_point() + xlab("Simulation") + ylab("Avg. Position") + scale_colour_manual(name="", values=c("RedCandidates"="red", "RedActivists"="violet", "BlueCandidates"="blue", "BlueActivists"="aquamarine")) + ggtitle("Activists-Candidates on D2")  
  
  # Do the same exercises using D3.
  voterCandidateD3 <- ggplot(data=subset(D3, category=="RedCandidates" | category== "RedVoters" | category == "BlueCandidates" | category=="BlueVoters"), 
                             aes(x=x, y=y, colour=category)) + geom_point() + xlab("Simulation") + ylab("Avg. Position") + scale_colour_manual(name="", values=c("RedCandidates"="red", "RedVoters"="orange", "BlueCandidates"="blue", "BlueVoters"="skyblue")) + ggtitle("Voters-Candidates on D3")
  voterActivistD3 <- ggplot(data=subset(D3, category=="RedCandidates" | category== "RedActivists" | category == "BlueCandidates" | category=="BlueActivists"), 
                          aes(x=x, y=y, colour=category)) + geom_point() + xlab("Simulation") + ylab("Avg. Position") + scale_colour_manual(name="", values=c("RedCandidates"="red", "RedActivists"="violet", "BlueCandidates"="blue", "BlueActivists"="aquamarine")) + ggtitle("Activists-Candidates on D3")  
  # Use the grid.arrange function to make the outputs arranged.
  grid.arrange(voterCandidateD1, voterActivistD1,
               voterCandidateD2, voterActivistD2,
               voterCandidateD3, voterActivistD3, ncol=2)
  dev.off() # Close the pdf device. 
  
  
  ## 3-2) WinnersPlot ##
  ## Set a new directory
  setwd("C:/Users/Taeyong/Documents/GitHub/ProblemSet4/4JobTalk3.nlogo_10_05_2010_19.42.54.385_.0400/Plots")
  setwd("WinnersPlot")
  
  ## First, create a csv file.
  # Read the column names.
  winnersNames <- scan(file=file, skip=9139, nlines=1, what=" ", sep=",", n=4) # Again, we have four unique names.
  # Read 169 lines, since x= 0 ~ 168, and create a matrix.
  winners <- scan(file=file, skip=9140, nlines=169, what=" ", sep=",")
  winners <- matrix(winners, nrow=169, byrow=TRUE)
  # Just as we did for the Position data, remove all the blank columns.
  winners <- winners[,-c(13:84)] # We don't have any data after 12th columns.  
  
  # We have two categories of interest: Blue and Red. 
  BlueCandidate <- winners[,c(1:4)]
  RedCandidate <- winners[,c(9:12)]
  # Combine these two data sets to make a stacked data frame and assign the column names.
  winners <- rbind(BlueCandidate, RedCandidate)
  winners <- data.frame(winners)  
  colnames(winners) <- winnersNames
  # Just as we did for the Position data, add a category variable to the data set.
  winners$category <- c(rep("BlueCandidate", 169), rep("RedCandidate", 169))
  # Write out a csv file
  write.csv(winners, "Winner.csv")
  
  ## Second, we want to create a pdf file plotting meaningful graphs.
  ## We might be interested in comparing ¡°y¡± that shows the percentage across Blue and Red candidates.
  # Import the csv file.
  winners <- read.csv("Winner.csv")
  
  pdf("Winner.pdf", width=7, height=7, pointsize=10) 
  winnerPlot <- ggplot(winners, aes(x=x, y=y, colour=category)) + geom_point() +xlab("Time") + ylab("Percentage of winning") + scale_colour_manual(name="", values=c("BlueCandidate"="blue", "RedCandidate"="red"))
  grid.arrange(winnerPlot)
  dev.off()
  
  ## 3-3) Polarization
  ## Set a new directory.
  setwd("C:/Users/Taeyong/Documents/GitHub/ProblemSet4/4JobTalk3.nlogo_10_05_2010_19.42.54.385_.0400/Plots")
  setwd("PolarizationPlot")
  
  ## First, create a csv file.
  # Read the column names.
  polarizationNames <- scan(file=file, skip=9320, nlines=1, what=" ", sep=",", n=4) # Four unique names
  # Use the same method to parse out the data as above.
  polarization <- scan(file=file, skip=9321, nlines=169, what=" ", sep=",")
  polarization <- matrix(polarization, nrow=169, byrow=TRUE)
  polarization <- polarization[,-c(13:84)]
  candidates <- polarization[,c(1:4)]
  voters <- polarization[,c(5:8)]
  activists <- polarization[,c(9:12)]
  polarization <- rbind(candidates, voters, activists)
  polarization <- data.frame(polarization)
  colnames(polarization) <- polarizationNames
  polarization$category <- c(rep("candidates", 169), rep("voters", 169), rep("activists", 169))
  # Write out a csv file.
  write.csv(polarization, "Polarization.csv")
  
  ## Second, create a pdf file.
  # Import the data and plot a meaningful graph.
  polarization <- read.csv("Polarization.csv")
  pdf("PolarizationPlot.pdf", width=10)
  polarizationPlot <- ggplot(polarization, aes(x=x, y=y, colour=category)) + geom_point() + xlab("Time") + ylab("Distance between the mean position")
  grid.arrange(polarizationPlot, ncol=1)
  dev.off()
  
  ## 3-4) IncumbentPercentage
  ## Set a new directory.
  setwd("C:/Users/Taeyong/Documents/GitHub/ProblemSet4/4JobTalk3.nlogo_10_05_2010_19.42.54.385_.0400/Plots")
  setwd("IncumbentPercentagePlot")
   
  ## First, create a csv file.
  # Read the column names
  incumbentNames <- scan(file=file, skip=9499, nlines=1, what=" ", sep=",", n=4)
  # Use the same method to parse out the data as above.
  incumbent <- scan(file=file, skip=9500, nlines=169, what=" ", sep=",")
  incumbent <- matrix(incumbent, nrow=169, byrow=TRUE)
  incumbent <- incumbent[,c(1:4)] 
  colnames(incumbent) <- incumbentNames
  incumbent <- data.frame(incumbent)
  write.csv(incumbent, "IncumbentWins.csv")
  
  ## Second, create a pdf file
  incumbent <- read.csv("IncumbentWins.csv")
  pdf("IncumbentWins.pdf")
  incumbentPlot <- ggplot(incumbent, aes(x=x, y=y)) + geom_point() + xlab("Time") + ylab("Precentage of Winning")
  grid.arrange(incumbentPlot, ncol=1)
  dev.off()
} #End of parsingOut function



########## II. JMR Exercises ##########
##Set the new directory.
setwd("~/GitHub/ProblemSet4")

### 1. Ch.4 Problem 3 ###

# I created an R file that has the following code under the current directory.
# The name of the R file is square_cube.R
n <- 7

cat("number     square      cube\n\n")
for(i in 1:n){
  square <- i^2
  cube <- i^3
  cat(format(i, width=6),
      format(square, width=10),
      format(cube, width=9),
      "\n", sep="")
}

# Read the R file using source().
source("square_cube.r") # I find the results that I should get.


### 1. Ch.4 Problem 4 ###
# I created an R file that has the following code under the current directory.
# The name of the R file is mult_table.R
mtable <- matrix(NA, 9, 9)
for (i in 1:9){
  for (j in 1:9){
    mtable[i,j] <- i*j
  }
}
show(mtable)

# Read the file using the source function.
source("mult_table.r") # I find the results that I should get.


### 1. Ch.7 Problem 3 ###

# Set a seed for reproductions.
set.seed(0520)
# Use rnorm function to generate 100 random values following the normal distribution having mean 160 sd 20, and make a data frame. 
pop <- data.frame(m = rnorm(100, 160, 20), 
                  f = rnorm(100, 160, 20))

# Use the function appeared in the book. 
next.gen <- function(pop){ # Input is the pop data frame.
  pop$m <- sample(pop$m) # Randomly select 100 values.
  pop$m <- apply(pop, 1, mean) # Apply the mean to the row.
  pop$f <- pop$m 
  return(pop) # Output is the pop data frame. 
} 

# Now, we want to gerate nine generations. 
# Create an empty list.
popList <- list()
popList[[1]] <- pop # The first generation.
# Use the next.gen function to generate 2-8 generations.
for (i in 1:8){
  popList[[i+1]] <- next.gen(popList[[i]])
}

# Now, we want to use the lattice package to plot the distribution of men's heights.
# Create an empty list for men's height.
popMen <- list()
for (i in 1:9){
  popMen[[i]] <- c(popList[[i]][,1])  # First column of popList indicates men's heights.
}
# Unlist the list and make a data frame for the plot. 
popMen <- data.frame(unlist(popMen))
colnames(popMen) <- "height"
# Add a generation variable.
popMen$generation <- rep(1:9, rep(100,9))

# We loaded the lattice library. Use the histgram function.
histogram(~ height | generation, data=popMen, layout=c(3,3))  # 3 by 3 matrix-like layout


### 1. Ch.7 Problem 4 ###

## To reproduce Figure 6.1, first install spuRs package
install.packages("spuRs")
library(spuRs)
data(treeg)
par(mfrow=c(1,1), mar=c(4,4,1,0.5))
## We may use xyplot available from the lattice package . 
xyplot(height.ft ~ age, data=treeg,
       group=tree.ID, type="l",  # Use the group option to plot the lines in one graph.
       xlab="height(feet)", ylab="age(years)",
       main="Height against age for all 66 trees in the tree growth data")
