library(dplyr)
library(maps)
library(ggplot2)
library(cowplot)

    swapLatLong <- function(dataIn){
      
      
      dataIn$Lat <- dataIn$GPS_longitude
      dataIn$Long <- dataIn$GPS_latitude
      
      dataIn$GPS_latitude <- dataIn$Lat 
      dataIn$GPS_longitude <- dataIn$Long
      
      dataIn$Lat <- NULL
      dataIn$Long <- NULL
      
      print("swapLatLong function has been run!!!!!!!!!!!")
      
      return(dataIn)
    }

    checkDataForCommonErrors <- function(dataIn, showMismatches){
      
      Thedata <- dataIn
      
    # Adjust date data type so can use data functions in R
      
      Thedata$collection_start_date <- as.Date(Thedata$collection_start_date, "%Y-%m-%d")
      Thedata$collection_end_date <- as.Date(Thedata$collection_end_date, "%Y-%m-%d")
      
    # Makes sure start date is before end date
      
      Thedata$dayspan <- Thedata$collection_end_date - Thedata$collection_start_date
      
      Thedata$dayspan <-as.numeric(Thedata$dayspan)
        
      errors <- subset(Thedata, dayspan < 0)
      
      if (nrow(errors) > 1) {
        print("Warning: there are end dates preceding start dates:")
        print(summary(as.factor(Thedata$days)))
        print("(Top row = day lengths, bottom row = number of rows)")
        print(errors)
      }
      
    # How many different day lengths are there?
      
      if (nrow(as.data.frame(unique(Thedata$days))) > 1) {
        print("Warning: multiple calculated day lengths:")
        print(summary(as.factor(Thedata$days)))
        print("(Top row = day lengths, bottom row = number of rows)")
      }
      
    # Check for mismatched data (e.g. things that should be 1:1 like lat to long)
        
      mismatchCheck("collection_end_date","collection_start_date",Thedata,showMismatches)
      mismatchCheck("collection_start_date","collection_end_date",Thedata,showMismatches)
      
      mismatchCheck("GPS_latitude","GPS_longitude",Thedata,showMismatches)
      mismatchCheck("GPS_longitude","GPS_latitude",Thedata,showMismatches)
      
      mismatchCheck("GPS_latitude","trap_duration",Thedata,showMismatches)
      mismatchCheck("GPS_longitude","trap_duration",Thedata,showMismatches)
      
      if("location_description" %in% colnames(Thedata)){
        mismatchCheck("GPS_latitude","location_description",Thedata,showMismatches)
        mismatchCheck("GPS_longitude","location_description",Thedata,showMismatches)
        mismatchCheck("location_description","GPS_latitude",Thedata,showMismatches)
        mismatchCheck("location_description","GPS_longitude",Thedata,showMismatches)
      }
      
      if("trap_ID" %in% colnames(Thedata)){
        mismatchCheck("GPS_latitude","trap_ID",Thedata,showMismatches)
        mismatchCheck("GPS_longitude","trap_ID",Thedata,showMismatches)
      }
      
      if("sample_comment" %in% colnames(Thedata)){
        mismatchCheck("GPS_latitude","sample_comment",Thedata,showMismatches)
        mismatchCheck("GPS_longitude","sample_comment",Thedata,showMismatches)
      }
      
      
      #Look for duplicate data, such as two traps at same GPS point
         
       ThedataIn.2 <- subset(Thedata, species != "BLANK")
      
       ThedataIn.2$smash <- paste(ThedataIn.2$collection_end_date,
                                 ThedataIn.2$GPS_latitude,
                                 ThedataIn.2$GPS_longitude,
                                 ThedataIn.2$species, 
                                 ThedataIn.2$trap_type,
                                 ThedataIn.2$attractant,
                                 ThedataIn.2$sex,
                                 sep='_') 
    
        counts <- 
          ThedataIn.2 %>% 
          group_by(smash) %>% 
          summarise(n = n())
        
        errorsOut <- subset(counts, n > 1)
        
        if(nrow(errorsOut) > 0){
          print("WARNING: There are rows that have identical collection end date X GPS X species X trap type X attractant X sex")
          print("This kind of isssue can exisit if two traps were co-located for example")
          print("This may also indicate that a row of a wide orginial dataset had the wrong location entered, thus causing a duplication. This would likely result in no errors being thrown above for multiple traps at  same lat or long")
          print(errorsOut)
          
            if(showMismatches){
              
            }
        }
      
  }
    
     mismatchCheck <- function(f1,f2,Thedata,showMismatches) {
       
      # This function takes 2 fields f1 and f2 that should be 1:1 relationship, eg. latitude to location_description and makes sure this is the case
      
      field1 <- f1
      field2 <- f2
      

      
      uniquePairsBetweenField1andField2 <- unique(Thedata[,c(field1,field2)])  # Finds all the combinations between Field1 and Field2
      
      # Aggregates by field1 (they should all be 1 if no problems). Note the undrescore after group_by is so that can pass variable names
      countSiteIDsPerField2 <- 
        uniquePairsBetweenField1andField2 %>% 
        group_by_(field1,field2) %>% 
        summarise(n = n())
      
      countSiteIDsPerField2 <- 
        uniquePairsBetweenField1andField2 %>% 
        group_by_(field1) %>% 
        summarise(n = n())
      
      problemField1s <- subset(countSiteIDsPerField2, n > 1 ) # leave only one with more then one lat per trapID
      
      
      field2.phrase <- paste("#",field2,"'s")
      problemField1s <- dplyr::rename(problemField1s, !!field2.phrase := "n")
      
      
      if(nrow(problemField1s) > 0){
        print("")
        print(paste("WARNING: There are  ", field1,"s sharing/have multiple  ", field2,". (note if trap_ids are NOT stable year to year, this could be ok. Run checkDataForCommonErrors() after subseting by year", sep=""))
        print(problemField1s)
        
        if(showMismatches){
          print("")
          print(merge(uniquePairsBetweenField1andField2,problemField1s)) # determine the duplicate pairs and print to screen
        }
      }
    }
    
    fixCommonFieldNameIssues <- function(dataIn) {
      #
      # Fix common field name issues 
      #
      
      
      #make all field names lower case, execept "GPS"
      
      colnames(dataIn) <- tolower(colnames(dataIn))
      
      if("gps_longitude" %in% colnames(dataIn)){
        dataIn <- dplyr::rename(dataIn,"GPS_longitude" = "gps_longitude")
      }
      
      if("gps_latitude" %in% colnames(dataIn)){
        dataIn <- dplyr::rename(dataIn,"GPS_latitude" = "gps_latitude")
      }
      
      if("gps_qualifier" %in% colnames(dataIn)){
        dataIn <- dplyr::rename(dataIn,"GPS_qualifier" = "gps_qualifier")
      }
      
      if("trap_id" %in% colnames(dataIn)){
        dataIn <- dplyr::rename(dataIn,"trap_ID" = "trap_id")
      }
      
      # check  'species' and 'species_name_out' are not both there
      
      if("species" %in% colnames(dataIn)){
        if("species_name_out" %in% colnames(dataIn)){
          print("Warning: 'species' and 'species_name_out' are both columns in the import file")
        }
      }
      
      #my dictionary of field names to look for their corrections 
      
      fixMe <-data.frame(stringsAsFactors = FALSE, rightWord = c("sample_count"), wrongWord = c("count"))
      fixMe <- rbind(list("GPS_latitude","lat"),fixMe)
      fixMe <- rbind(list("GPS_longitude", "long"),fixMe)
      fixMe <- rbind(list("collection_start_date", "collection_date_start"),fixMe)
      fixMe <- rbind(list("collection_end_date", "collection_date_end"),fixMe)
      fixMe <- rbind(list("location_description", "location"),fixMe)
      fixMe <- rbind(list("species_comment", "species_description"),fixMe)
      
      print("The following field names were modified (other fields may have had case corrected):")
      
      for(i in 1:nrow(fixMe)){
        if(as.character(fixMe[i,2]) %in% colnames(dataIn)){
          wrong <- fixMe[i,2]
          right <- fixMe[i,1]
          names(dataIn)[names(dataIn) == wrong] <- right
          print(paste("     ",wrong))
        }
      }
      
      return(dataIn)
    }  
    
    OneYearOnly <- function(dataIN, yearIN){
      
      dataIN$collection_start_date <- as.Date(dataIN$collection_start_date)
      dataIN$collection_end_date <-as.Date(dataIN$collection_end_date)
      
      dataIN$theYear <- as.numeric(format(dataIN$collection_start_date,'%Y'))
      
      dataIN <- subset(dataIN, theYear == yearIN)   #Subset by year, if neccessary  #Subset by year, if neccessary
      
      dataIN$theYear <-NULL
      
      print(paste("ONLY ONE YEAR EXPORTED:", yearIN))
      
      return <- dataIN
    }
  
    checkFieldNames <- function(dataIn) {
      #
      # Check and see what fields are missing from the input file and flag missing fields
      #
      
      expectedFields <-c("collection_start_date","collection_end_date", "GPS_latitude","GPS_longitude", "trap_type", "attractant","trap_number", "GPS_qualifier", "trap_duration", "species", 
                         "species_identification_method", "developmental_stage", "sex", "sample_count")
      
      optionalFields <-c("trap_ID","location_description","collection_comment","sample_comment","species_comment", "vbsp") # add the optional field list
      
      
      # Find missing fields
      
      print("")
      print("The following fields are missing from the input file:")
      
      for(i in 1:length(expectedFields)){
        if(expectedFields[i] %in% colnames(dataIn)) cat("") else print(paste("     ",expectedFields[i],sep=" "))
      }
      
      # Find optional fields
      
      print("")
      print("Optional Fields Detected:")
      
      for(i in 1:length(optionalFields)){
        if(optionalFields[i] %in% colnames(dataIn)) print(paste("     ",optionalFields[i],sep=" "))
      }
      
      # Find extraneous field names
      
      print("")
      print("Extraneous Fields Detected:")
      
      dataIn.colnames <-colnames(dataIn)
      
      for(i in 1:length(dataIn.colnames)){
        if(dataIn.colnames[i] %in% expectedFields || dataIn.colnames[i] %in% optionalFields){
        } else{
          print(paste("     ",dataIn.colnames[i],sep=" "))
        }
      }
      
      #special case of vbsp
      
      if("vbsp" %in% colnames(dataIn)){
      } else{
        print("")
        print("Note: vbsp not detected.")
      }
      
    }  
    
    samplesCollectionNzeros <- function(dataIn) {
      #
      # add sample IDs, collection IDs, and deal with zeros, and make blanks
      #
      
      # Deal with emtpy collections, delete blank (zero) rows
        dataIn$collection_ID <- paste(dataIn$collection_end_date,dataIn$GPS_latitude,dataIn$GPS_longitude,dataIn$trap_type,dataIn$attractant,sep='_') # Make a temp collection ID
        
        # Generate a list of all collections that had at least 1 mozzie collected
          dataIn.temp <- dataIn
          dataIn.temp <- subset(dataIn, dataIn.temp$sample_count > 0)
          dataIn.temp <- as.data.frame(unique(dataIn.temp$collection_ID))
          dataIn.temp$NotEmpty <- "notEmpty"
          dataIn.temp$`unique(dataIn.temp$collection_ID)` <-as.character(dataIn.temp$`unique(dataIn.temp$collection_ID)`)
          
          dataIn <- merge(dataIn, dataIn.temp, all.x = TRUE, by.x = "collection_ID", by.y = 'unique(dataIn.temp$collection_ID)') # Once this line is exectued, all blank collections will lack 'notEmpty'
          print(unique(dataIn$NotEmpty))
          
          dataIn$sample_count[is.na(dataIn$NotEmpty)] <- -1 # Assigns -1 to all sample_counts from empty collections
          dataIn$repeateInstance <- duplicated(dataIn$collection_ID) # Marks each first sample in a collection with FALSE    dataIn <- dataIn[!(dataIn$sample_count == -1 & dataIn$repeateInstance == TRUE),] # This line leaves only one species per collection, that was blank
          dataIn$species[dataIn$sample_count == -1] <- "BLANK" 
          dataIn$vbsp[dataIn$sample_count == -1] <- "BLANK"
      
      # Remove blanks and zeros, deal with BLANKS
      
        dataIn <- subset(dataIn, sample_count != 0)
        dataIn <- subset(dataIn, !is.na(sample_count))
        dataIn$sample_count[dataIn$sample_count==-1] <- 0 # Assigns 0 to all sample_counts from empty collections
      
      #Assign collection ID
      
        dataIn <- transform(dataIn, collection_ID = as.numeric(interaction(collection_start_date, GPS_latitude,GPS_longitude,trap_type, drop=TRUE)))
        dataIn$collection_ID <- sprintf("%05i",dataIn$collection_ID) 
        dataIn$collection_ID <- paste(depositorShortName,"_",YearToExport,"_collection_",dataIn$collection_ID,sep="")
      
      #Assign sample ID
        dataIn$sample_ID <- 1:nrow(dataIn) 
        dataIn$sample_ID <- sprintf("%05i",dataIn$sample_ID) #Adds leading zeros to make sure have 5 digits, e.g. 00001, 00002, etc
        dataIn$sample_ID <- paste(depositorShortName,"_",YearToExport,"_sample_",dataIn$sample_ID,sep="")
      
      #remove double __ if they exist
        dataIn$collection_ID <- gsub("__", "_", dataIn$collection_ID)
        dataIn$sample_ID <- gsub("__", "_", dataIn$sample_ID)
      
      # Check for mupltiple end dates for same collection 
        dataIn.1 <- unique(dataIn[,c('collection_end_date','collection_ID')])
        dataIn.2 <- aggregate(collection_end_date ~ collection_ID, data= dataIn.1, NROW)
        dataIn.2 <- subset(dataIn.2,collection_end_date > 1)
        
        if(NROW(dataIn.2) > 0){
          print("+++++ WARNING! +++++")
          print("There are mupltiple end dates for same collection ")
          print(dataIn.2)

        }
      
      return(dataIn)
    }
    
    SAF_standarize <- function(dataIn){
      
      #force some field values to upper or lower case
      
      dataIn$sex <- tolower(dataIn$sex)
      dataIn$developmental_stage <- tolower(dataIn$developmental_stage)
      dataIn$attractant <- tolower(dataIn$attractant)
      
      dataIn$trap_type <-toupper(dataIn$trap_type)
      
      #ensure multi-attractants have no commas
      
      dataIn$attractant <- gsub(",", ";", dataIn$attractant)
      
      #ensure multi-attractants have no leading white spaces
      
      dataIn$attractant <- gsub("; ", ";", dataIn$attractant)
      
      #dplyr::rename some protocols to standarize them
      
      dataIn$species_identification_method[dataIn$species_identification_method=="morphological"] <- "MORPHO"
      dataIn$species_identification_method[dataIn$species_identification_method=="morpho"] <- "MORPHO"

      dataIn$attractant[dataIn$attractant=="alfalfa"] <- "alfalfa infusion"
      
      dataIn$attractant[dataIn$attractant=="hay"] <- "hay or grass infusion"
      dataIn$attractant[dataIn$attractant=="grass"] <- "hay or grass infusion"
      
      #standarize developmental_stage
      
      dataIn$developmental_stage <- tolower(dataIn$developmental_stage)
      dataIn$developmental_stage[dataIn$developmental_stage=="adults"] <- "adult"
      dataIn$developmental_stage[dataIn$developmental_stage=="larvae"] <- "larva"
      dataIn$developmental_stage[dataIn$developmental_stage=="pupae"] <- "pupa"
      dataIn$developmental_stage[dataIn$developmental_stage=="eggs"] <- "egg"
      
      return(dataIn)
    }
    
    configMaker <- function(dataIn,depositorShortName){
      
      ### MAKE CONFIG FILE
      
      # Check and see if vbsp coloumn exists 
      
        if("vbsp" %in% colnames(dataIn)){
        }else{
          print("Note: No vbsp coloumn - this wizzard won't export species")
          dataIn$vbsp <- ""
        }
      
        if("" %in% dataIn$vbsp){
          print("WARNING: There are balnk values in the vbsp coloumn - they will be species missing from the config file")
        }
      
      # species List 
      
      mydata.config <- dataIn[,c("vbsp","species")]
      mydata.config <- unique(mydata.config)
      mydata.config <- mydata.config[order(mydata.config$species),]  #put species in alphabetical order
      
      mydata.config <- subset(mydata.config, species != "BLANK")
      mydata.config <- as.data.frame(unique(mydata.config))
      mydata.config$key <- ""
      mydata.config$value <- paste(mydata.config$species, " : " ,mydata.config$vbsp,sep="") 
      mydata.config <- mydata.config[,c("key", "value")]
      mydata.config$value <- paste("  ",mydata.config$value,sep="") #add leading 2 spaces to match YAML format
      
      mydata.config <- rbind(c("study_species :",NULL),mydata.config)  # add species section title
      
      # Developmental stages
      
      mydata.config <- rbind(mydata.config, c("",NULL)) 
      mydata.config <- rbind(mydata.config, c("study_developmental_stages :",NULL))  # add section title
      
      if("adult" %in% mydata$developmental_stage){
        mydata.config <- rbind(mydata.config,c("  adult : IDOMAL:0000655")) 
      }
      
      if("vector organism" %in% mydata$developmental_stage){
        mydata.config <- rbind(mydata.config,c("  vector organism : IDOMAL:0000644")) 
      }
      
      if("pupa" %in% mydata$developmental_stage){
        mydata.config <- rbind(mydata.config,c("  pupa : IDOMAL:0000654")) 
      }
      
      if("larva" %in% mydata$developmental_stage){
        mydata.config <- rbind(mydata.config,c("  larva : IDOMAL:0000653")) 
      }
      
      if("egg" %in% mydata$developmental_stage){
        mydata.config <- rbind(mydata.config,c("  egg : IDOMAL:XXXXXXXX")) 
      }
      
      # Sex 
      
      mydata.config <- rbind(mydata.config, c("",NULL)) 
      mydata.config <- rbind(mydata.config, c("study_sexes :",NULL))  # add section title
      
      if("female" %in% mydata$sex){
        mydata.config <- rbind(mydata.config,c("  female : PATO:0000383")) 
      }
      
      if("male" %in% mydata$sex){
        mydata.config <- rbind(mydata.config,c("  male : PATO:0000384")) 
      }
      
      if("mixed" %in% mydata$sex){
        mydata.config <- rbind(mydata.config,c("  mixed : PATO:0001338")) 
      }
      
      # Study protocols 
      
      mydata.config <- rbind(mydata.config, c("",NULL)) 
      mydata.config <- rbind(mydata.config, c("study_protocols :",NULL))  # add section title
      
      # Collection protocols
      
      # Trap Types  
      
      
      knownTraps <- c("EVS"
                      , "NJLT"
                      , "CDCLIGHT"
                      , "GRAVID"
                      , "BGSENT"
                      , "INDOOR LIGHT TRAP"
                      , "OUTDOOR LIGHT TRAP"
                      , "PSC"
                      , "PIT TRAP"
                      , "ABC TRAP"
                      ,"OUTDOOR HLC"
                      ,"INDOOR HLC"
                      , "DIP"
                      , "ANIMAL TRAP"
                      , "GENERIC TRAP")
      
      trapsInStudy <- unique(dataIn$trap_type) 
      
      for(i in 1:length(trapsInStudy)){
        if(trapsInStudy[i] %in% knownTraps){
        } else{
          print(paste("Collection Method Not Handled:",trapsInStudy[i], "- will need to be manually modified in config file. Handled trap types:"))
          print(knownTraps) # Prints a list of handled trap types
          
          mydata.config <- rbind(mydata.config, c(paste("  - study_protocol_name :",trapsInStudy[i])))
          mydata.config <- rbind(mydata.config, c("    study_protocol_type : XXX"))
          mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : XXX"))
          mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : XXX"))
          mydata.config <- rbind(mydata.config, c("    study_protocol_description : Mosquitoes were caught using XXX"))
        }
      }
      
      if("NJLT" %in% mydata$trap_type) {
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : NJLT"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : new jersey trap catch"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : IRO"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : 0000031"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : Mosquitoes were caught using a New Jersey light trap"))
      }
      
      if("CDCLIGHT" %in% mydata$trap_type) {     
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : CDCLIGHT"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : CDC light trap"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : VSMO"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : 0000727"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : Mosquitoes were caught using a CDC light trap"))
      }
      
      if("GRAVID" %in% mydata$trap_type) { 
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : GRAVID"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : gravid trap"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : VSMO"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : 0001508"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : Mosquitoes were caught using a gravid trap"))
      }
      
      if("BGSENT" %in% mydata$trap_type) { 
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : BGSENT"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : BG-Sentinel trap"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : VSMO"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : 0001906"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : Mosquitoes were caught using a Biogents BG-Sentinel trap")) 
      }
      
      if("INDOOR LIGHT TRAP" %in% mydata$trap_type) { 
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : INDOOR LIGHT TRAP"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : Indoor light trap catch"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : MIRO"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : 30000009"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : Mosquitoes were caught using an indoor light trap")) 
      }
      
      if("OUTDOOR LIGHT TRAP" %in% mydata$trap_type) { 
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : OUTDOOR LIGHT TRAP"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : Outdoor light trap catch"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : MIRO"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : 30000010"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : Mosquitoes were caught using an outdoor light trap")) 
      }
      
      if("PSC" %in% mydata$trap_type) { 
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : PSC"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : pyrethrum spray catch"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : MIRO"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : 30000023"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : Mosquitoes were collected following application of pyrethrum to the inside of a dwelling")) 
      }
      
      if("PIT TRAP" %in% mydata$trap_type) { 
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : PIT TRAP"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : artificial pit shelter"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : IRO"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : 0000005"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : Resting mosquitoes were collected from pit shelters")) 
      }
      
      if("ABC TRAP" %in% mydata$trap_type) { 
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : ABC TRAP"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : ABC trap catch"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : IRO"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : 0000027"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : Mosquitoes were collected using an American Biophysics Corporation (ABC) light trap")) 
      }
      
      if("OUTDOOR HLC" %in% mydata$trap_type) { 
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : OUTDOOR HLC"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : man biting catch - outdoors"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : MIRO"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : 30000018"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : Mosquitoes were collected using a human landing catch, outdoors")) 
      }
      
      if("EVS" %in% mydata$trap_type) { 
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : EVS"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : EVS trap catch"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : IRO"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : 0000029"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : Mosquitoes were collected using an EVS trap")) 
      }
        
      if("INDOOR HLC" %in% mydata$trap_type) { 
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : INDOOR HLC"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : man biting catch - indoors"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : MIRO"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : 30000019"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : Mosquitoes were collected using a human landing catch, indoors")) 
      }
      
      if("DIP" %in% mydata$trap_type) { 
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : DIP"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : collection of larvae from dippers"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : MIRO"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : 30000063"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : Larvae were collected from water using dippers")) 
      }
      
      if("GENERIC TRAP" %in% mydata$trap_type) { 
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : GENERIC TRAP"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : XXX"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : XXX"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : XXX"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : XXX")) 
      }
      
      # Identification Method
      
      if("MORPHO" %in% mydata$species_identification_method){
        
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : MORPHO")) 
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : morphological"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : MIRO"))  
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : 30000039"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : Collected mosquitoes were identified by morphological examination."))
      }
      
      if("PCR" %in% dataIn$species_identification_method){
        
        mydata.config <- rbind(mydata.config, c("  - study_protocol_name : PCR")) 
        mydata.config <- rbind(mydata.config, c("    study_protocol_type : PCR-based species identification"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_source_ref : MIRO"))  
        mydata.config <- rbind(mydata.config, c("    study_protocol_type_term_accession_number : 30000040"))
        mydata.config <- rbind(mydata.config, c("    study_protocol_description : Collected mosquitoes were identified by PCR examination."))
      }
      
      knownIDmethods <- c("MORPHO",
                          "PCR")
      
      IDmethodsInStudy <- unique(dataIn$species_identification_method)
      
      for(i in 1:length(IDmethodsInStudy)){
        if(IDmethodsInStudy [i] %in% knownIDmethods){
        } else{
          print(paste("Identification method not handled:",IDmethodsInStudy [i], "- will need to be manually added to config file"))
          
        }
      }
      
      
      # Other
      
      mydata.config <- rbind(mydata.config, c("",NULL)) 
      mydata.config <- rbind(mydata.config, c("study_terms :",NULL))  # add section title
      
      mydata.config <- rbind(mydata.config,c("  pool : EFO:0000663"))
      
      # attractants
      
      knownAttractants <- c("bg-lure"
                            , "light"
                            , "co2"
                            , "none"
                            , "human"
                            , "cow" 
                            , "hay or grass infusion"
                            , "alfalfa infusion"
                            , "chicken"
                            , "uv light"
                            , "yeast"
                            , "octenol"
                            , "organic infusion"
                            , "attractive substance")
      
      attractantsinStudy <- unique(dataIn$attractant) # a vector with all the unique attractant values

      
      #If there is multi-attractant fields (e.g. thing1;thing2;thing3) make seperate rows in config file. It can handle up to 3 attractants since it cycles through the list 2x
      
        attractantsinStudy.2 <-vector() # an empty vector 
  
        for(y in 1:2){  
          for(i in 1:length(attractantsinStudy)){
            if(grepl(";",attractantsinStudy[i])){
              attractantsinStudy.2 <-  c(attractantsinStudy.2, unlist(strsplit(attractantsinStudy[i], ";")))
            }
            else{
              attractantsinStudy.2 <-  append(attractantsinStudy.2, attractantsinStudy[i])
            }
          }
        }
        
        attractantsinStudy  <- unique(attractantsinStudy.2)
        
   #Config text for attractants
        
      for(i in 1:length(attractantsinStudy)){
        print(attractantsinStudy [i]) #TempForTestng
        if(attractantsinStudy [i] %in% knownAttractants){
        } else{
          print(paste("Attractant not handled:",attractantsinStudy [i], "- ontology number will need to be manually added to config file"))
          print("Handled attractants are:")
          print(knownAttractants) # Prints list of known attractants from list above
          
          mydata.config <- rbind(mydata.config, c(paste("  ",attractantsinStudy [i]," :",sep="")))  #adds line to config file (without id number)
        }
      }
      
      if("light" %in% attractantsinStudy){
        mydata.config <- rbind(mydata.config, c("  light : IRO:0000139")) 
      }
      
      if("co2" %in% attractantsinStudy){
        mydata.config <- rbind(mydata.config, c("  co2 : IRO:0000035")) 
      }
      
      if("none" %in% attractantsinStudy){
        mydata.config <- rbind(mydata.config, c("  none : IRO:0000153")) 
      }
      
      if("hay or grass infusion" %in% attractantsinStudy){
        mydata.config <- rbind(mydata.config, c("  hay or grass infusion : IRO:0000037")) 
      }

      if("human" %in% attractantsinStudy){
        mydata.config <- rbind(mydata.config, c("  human : VBsp:0001357")) 
      }
      
      if("cow" %in% attractantsinStudy){
        mydata.config <- rbind(mydata.config, c("  cow : IRO:XXX")) 
      }
      
      if("alfalfa infusion" %in% attractantsinStudy){
        mydata.config <- rbind(mydata.config, c("  alfalfa infusion: IRO:0001059")) 
      }
      
      if("bg-lure" %in% attractantsinStudy){
        mydata.config <- rbind(mydata.config, c("  bg-lure : IRO:0001060")) 
      }
        
      if("chicken" %in% attractantsinStudy){
          mydata.config <- rbind(mydata.config, c("  chicken : IRO:0000158")) 
      }
        
      if("uv light" %in% attractantsinStudy){
          mydata.config <- rbind(mydata.config, c("  uv light : IRO:0000193")) 
      }       
        
      if("yeast" %in% attractantsinStudy){
          mydata.config <- rbind(mydata.config, c("  yeast : IRO:0000159")) 
      }   
        
      if("octenol" %in% attractantsinStudy){
          mydata.config <- rbind(mydata.config, c("  octenol : IRO:0000036")) 
      } 
        
      if("organic infusion" %in% attractantsinStudy){
          mydata.config <- rbind(mydata.config, c("  organic infusion : IRO:0001058")) 
      } 
        
      if("attractive substance" %in% attractantsinStudy){
        mydata.config <- rbind(mydata.config, c("  attractive substance : IRO0000034")) 
      }   
      
      # Clean up config file
      
      mydata.config$key <- paste(mydata.config$key,"",mydata.config$value)
      mydata.config$key <- NULL
      
      # Fill config file with header stuff
      
      configTop <- data.frame("value" = c(
        
        "study_title : XXX",
        "study_submission_date : XXX",
        "study_description : Mosquito abundance in XXX",
        "",
        
        "study_tags :",
        "  - study_tag : abundance",
        "    study_tag_term_source_ref : VBcv",
        "    study_tag_term_accession_number : 0001085",
        "",
        
        "  - study_tag : XXX",
        "    study_tag_term_source_ref : VBcv",
        "    study_tag_term_accession_number : XXX",
        "",
        
        "study_publications :",
        "  - study_publication_doi : XXX",
        "    study_publication_author_list : XXX",
        "    study_publication_title : XXX",
        "    study_publication_status : published",
        "    study_publication_status_term_source_ref : EFO",
        "    study_publication_status_term_accession_number : 0001796",
        "study_publications :",
        "  - study_publication_author_list : XXX",
        "    study_publication_title : XXX",
        "    study_publication_status : website",
        "    study_publication_status_term_source_ref : VBcv",
        "    study_publication_status_term_accession_number : 0000667",
        "    comments : http://www. XXX",
        "",
        "study_contacts :",
        "  - study_person_last_name : XXX",
        "    study_person_first_name : XXX",
        "    study_person_email : XXX",
        "    study_person_affiliation : XXX",
        ""), stringsAsFactors = FALSE)
      
      mydata.config <-rbind(configTop,mydata.config)
      
      names(mydata.config)[names(mydata.config) == "value"] <- paste("study_identifier : 2020-abundance-",depositorShortName,"- XXX",sep="")  # this makes the very first row say the right thing
      
      return(mydata.config)
    }      
    
    plotAvgGPSpoints <- function(dataIn){        
      #DEPRECATED
      
      print("WARNING: This script deprecated, use plotAvgGPSpoints2(dataIn = mydata,googleMaps = FALSE) instead")
      
      # Plot average point on map to make sure its in right place.
      
      dataIn <- mydata
     
      # Calculate projcet mean lat & long
        meanLong <- mean(dataIn$GPS_longitude)
        meanLat <- mean(dataIn$GPS_latitude)
      
      # Plot histograms of lat/long
        
        lat <- ggplot(dataIn, aes(x=GPS_latitude)) + geom_histogram()
        long <-ggplot(dataIn, aes(x=GPS_longitude)) + geom_histogram()
        distribution <- ggplot(dataIn, aes(y=GPS_latitude, x=GPS_longitude))+ geom_point()
        
        p <- plot_grid(lat,long,distribution)

        
      
      # This code makes a R map
        
        #world<-map_data('world')
        #sf<-data.frame(long=meanLong,lat=meanLat)
        #p <- ggplot(legend=FALSE) +
        #  geom_polygon( data=world, aes(x=long, y=lat,group=group)) +
        #  xlab("") + ylab("")
        #p <- p + geom_point(data=sf,aes(long,lat),colour="green",size=4)
        #p
      
      # Open google maps to avg lat/long
      
        browseURL(paste("https://www.google.com/maps/dir//",meanLat,",",meanLong,"/@",meanLat,",",meanLong,",7z", sep=""))  # Opens google maps to the avg lat / long of the project
        print("Your browser should have opened to the avg Lat/Long of the project. This is determined by taking the mean of all latitudes and mean of all longitudes from the entire project")
        print(paste("https://www.google.com/maps/dir//",meanLat,",",meanLong,"/@",meanLat,",",meanLong,",7z", sep=""))  # Opens google maps to the avg lat / long of the project)
      return(p)
    }

    plotAvgGPSpoints2 <- function(dataIn,googleMaps){        
      # Plot average point on map to make sure its in right place.
      
      dataIn <- mydata # FOR TESTING ONLY
      
      wm<-map_data("world") %>% filter(region != "Antartica" ) %>% fortify()
      
      ## site coords
      
      site_points <- dataIn %>% distinct(GPS_latitude,GPS_longitude)
      
      # generate and plot map
      
      site_plot <- ggplot() + coord_fixed() +
        geom_map(data =wm, map = wm,
                 aes(group = group, map_id= region),
                 fill = "darkgrey") +
        geom_point(data = fortify(site_points), aes(GPS_longitude, GPS_latitude),
                   colour = "blue", size = 1) +
        theme_classic() +
        xlim(min(dataIn$GPS_longitude) - 15, max(dataIn$GPS_longitude) + 15) + 
        ylim(min(dataIn$GPS_latitude) - 25, max(dataIn$GPS_latitude) + 25)
             
      #plot(site_plot)
    
      ####
      
      # Calculate projcet mean lat & long
      #meanLong <- mean(dataIn$GPS_longitude)
      #meanLat <- mean(dataIn$GPS_latitude)
      
      # Plot histograms of lat/long
      
      lat <- ggplot(dataIn, aes(x=GPS_latitude)) + geom_histogram()
      long <-ggplot(dataIn, aes(x=GPS_longitude)) + geom_histogram()
      distribution <- ggplot(dataIn, aes(y=GPS_latitude, x=GPS_longitude))+ geom_point()
      
      p <- plot_grid(lat,long,distribution)
      q <- plot_grid(p,site_plot)
      
      # Open google maps to avg lat/long
     
       meanLong <- mean(dataIn$GPS_longitude)
       meanLat <- mean(dataIn$GPS_latitude)
      
       if(googleMaps == TRUE){
          browseURL(paste("https://www.google.com/maps/dir//",meanLat,",",meanLong,"/@",meanLat,",",meanLong,",7z", sep=""))  # Opens google maps to the avg lat / long of the project
          print("Your browser should have opened to the avg Lat/Long of the project. This is determined by taking the mean of all latitudes and mean of all longitudes from the entire project")
          print(paste("https://www.google.com/maps/dir//",meanLat,",",meanLong,"/@",meanLat,",",meanLong,",7z", sep=""))  # Opens google maps to the avg lat / long of the project)
         }
      
      return(q)
    }
    
  
    
    PopBioWizzardHelper <- function(depositorShortNameIN,YearToExportIN){
      
      print(depositorShortNameIN)
      
      lineTorun <- paste("perl PopBioWizard.pl --zeroes --output-directory ",depositorShortNameIN,"_isa-tab_",YearToExportIN," --file ",depositorShortNameIN,"_",YearToExportIN,"_SAF.txt --config ", depositorShortNameIN,"_config_",YearToExportIN,".txt --isatab",sep="")
      
      return(lineTorun)
      
      ## template text:  perl PopBioWizard.pl --zeroes --output-directory isa-tab --file Tchuinkam_SAF.txt --config Tchuinkam_config_.txt --isatab
      
      
    }
    
    writeFiles <- function(mydataIN, mydata.configIN,depositorShortNameIN,YearToExportIN, writeConfig){
      
      mydata.out <- mydataIN 
      mydata.out$vbsp <- NULL # remove the vbsp field - don't need it
      
      write.table(mydata.out, file=paste(depositorShortName,"_",YearToExport,"_SAF.txt",sep=""), quote=FALSE, sep='\t', row.names = FALSE)  
      
      if(writeConfig == TRUE){
      write.table(mydata.configIN, file=paste(depositorShortName,"_config_",YearToExport,".txt",sep=""), quote=FALSE, sep='\t', row.names = FALSE) 
      }
      
    }


    

    











removeDuplicateRows <- function(df, showPie = TRUE) { # Removes duplicate samples without discarding entire collections
  throwItOut <- subset(df %>% count(GPS_latitude, GPS_longitude, collection_start_date, species, sex), n == 1)
  throwItOut$n <- NULL
  percent <- paste(c(as.character(ceiling(100 * (nrow(df)-nrow(throwItOut)) / nrow(df))), "%"), collapse = "")
  if (percent == "0%") {
    cat("No duplicate rows were detected, so no data was removed!  (:\n")
  } else {
    cat(paste("You have successfully removed", percent, "of your data.\n"))
    if (showPie) {
      pie(
        c(nrow(throwItOut), nrow(df) - nrow(throwItOut)),
        c("Good Data", "Duplicates"),
        5000, 1, TRUE, 90,
        col = c("green", "red")
      )
    }
  }
  return(
    inner_join(throwItOut, df, by = c("GPS_latitude", "GPS_longitude", "collection_start_date", "species", "sex"))
  )
}

removeDuplicates <- function(df, showPie = TRUE, showTables = TRUE, outputCSVs = TRUE) { # Removes collections that have duplicate samples
  whenInDoubt <- subset(df %>% count(GPS_latitude, GPS_longitude, collection_start_date, species, sex), n != 1)
  whenInDoubt$temporaryCollectionID <- paste(whenInDoubt$GPS_latitude, whenInDoubt$GPS_longitude, whenInDoubt$collection_start_date)
  x <- df
  x$temporaryCollectionID <- paste(x$GPS_latitude, x$GPS_longitude, x$collection_start_date)
  throwItOut <- subset(x, !(temporaryCollectionID %in% whenInDoubt$temporaryCollectionID))
  percent <- paste(c(as.character(ceiling(100 * (nrow(df)-nrow(throwItOut)) / nrow(df))), "%"), collapse = "")
  if (percent == "0%") {
    cat("No collections contained duplicate rows, so no data was removed!  (:\n")
  } else {
    totalCollections = length(unique(x$temporaryCollectionID))
    removedCollections = length(unique(whenInDoubt$temporaryCollectionID))
    cat(paste("You have successfully removed", percent, "of your data.\n"))
    cat(paste(removedCollections, "out of", totalCollections, "collections were thrown out for containing duplicates.\n"))
    if (showPie) {
      pie(
        c(nrow(throwItOut), nrow(df) - nrow(throwItOut) - nrow(whenInDoubt) * mean(whenInDoubt$n), nrow(whenInDoubt) * mean(whenInDoubt$n)),
        c("Good Data", "Collections w/ Duplicates", "Duplicates"),
        5000, 1, TRUE, 90,
        col = c("green", "yellow", "red")
      )
    }
    if (showTables || outputCSVs) {
      naughtyData <<- subset(x, temporaryCollectionID %in% whenInDoubt$temporaryCollectionID)
      naughtyData$temporaryCollectionID <<- NULL
      x$a <- paste(x$GPS_latitude, x$GPS_longitude, x$collection_start_date, x$species, x$sex)
      whenInDoubt$a <- paste(whenInDoubt$GPS_latitude, whenInDoubt$GPS_longitude, whenInDoubt$collection_start_date, whenInDoubt$species, whenInDoubt$sex)
      dataWithDuplicatesFlagged <<- df
      dataWithDuplicatesFlagged$is.duplicate <<- as.character(x$a %in% whenInDoubt$a)
      if (showTables) {
        View(naughtyData)
        View(dataWithDuplicatesFlagged)
      }
      if (outputCSVs) {
        write.csv(naughtyData, "naughty_data.csv")
        write.csv(dataWithDuplicatesFlagged, "duplicates_flagged.csv")
        cat("\nThe following CSV files were successfully saved to the working directory:\n")
        cat(" - naughty_data.csv        (The collections with duplicates)\n")
        cat(" - duplicates_flagged.csv  (The original data, with an extra column to signify which rows are duplicates.)\n")
        cat("If you don't want to save CSV files with this function, set the function parameter \"outputCSVs\" to FALSE.\n")
      }
    }
  }
  throwItOut$temporaryCollectionID <- NULL
  return(throwItOut)
}
