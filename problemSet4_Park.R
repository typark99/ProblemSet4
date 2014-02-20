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
  # Set a new working directory
  setwd("C:/Users/Taeyong/Documents/GitHub/ProblemSet4/4JobTalk3.nlogo_10_05_2010_19.42.54.385_.0400/Plots")

  ## Create four sub-directories under the Plots directory.
  llply(.data=c("PositionPlot", "WinnersPlot", 
                "PolarizationPlot", "IncumbentPercentagePlot"), 
        .fun=function(x) dir.create(path=x))
  

}




