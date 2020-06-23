rm(list=ls()) # clears everything in R memory
dev.off() # clears plots from R studio

workingDirectory <-"~/Dropbox/_NDpostdoc/DataInjestion/MarkB" # Set working directory
setwd(workingDirectory)  
dataFileName <-"MarkB.csv"  # name of CSV output from tableau
depositorShortName <- "Ageep"  # This is used to make study/sample name etc
YearToExport <-"" # Keep as "" if not doing years or it will break something else

library(dplyr)
library(maps)
library(ggplot2)

source("~/Dropbox/_NDpostdoc/DataInjestion/SamSAF_maker.R")  #This pulls in all my magic scipts etc that do / check etc everything

mydata <- read.csv(dataFileName, stringsAsFactors = FALSE)

checkFieldNames(mydata)  

mydata <- fixCommonFieldNameIssues(mydata)

checkFieldNames(mydata)  



# Rename Mis-named Fields

  # use the pattern: 
  #       mydata <- rename(mydata,"new variable name" = "existing variable name")


# Hard Code Some Values:

  # use the pattern: 
  #       mydata$variableName <- "value"  (no quotes if number)

  # WARNING: This is most likely place a very bad error that won't be caught will be introduced.
  #           Make sure you intend on using these values, as they overright anything in the inport file

  #mydata$GPS_qualifier <- "IA"
  #mydata$developmental_stage <- "adult"
  #mydata$sex <- "female"
  #mydata$attractant <- NA
  #mydata$trap_type <- "CDCLIGHT"


# Remove extraenous fields:

  # use the pattern: 
  #       mydata$variableNameToRemove <- NULL

  
#mydata$location_description <- trimws(mydata$location_description)  #help standarize location_descriptions by trimming white space

checkFieldNames(mydata)

checkDataForCommonErrors(mydata)

#mydata <- OneYearOnly(mydata, as.numeric(YearToExport))  # If wanted, only export one year, YearToExport above.

mydata <- samplesCollectionNzeros(mydata) # Adds sample and collection IDs, removes zeros, makes blank collections

mydata <- SAF_standarize(mydata)

# Plot average point on map to make sure its in right place.      
  p <- plotAvgGPSpoints(mydata)
  p

mydata.config <- configMaker(mydata,depositorShortName)
    
writeFiles(mydata, mydata.config,depositorShortName,YearToExport, writeConfig = TRUE) #Write files  | if writeConfig = FALSE, config file not written (e.g. you have been manually editing it)
print(PopBioWizzardHelper(depositorShortName,YearToExport))  #Prints out line of code to paste into console to run Dan's SAF wizzard 


file.copy("~/Dropbox/_NDpostdoc/DataInjestion/PopBioWizard.pl", workingDirectory) # Puts a copy of the SAF wizzard into the working directory
    
    
