#' buildBottleIdFile.R
#'
#' @description Reads QAT files in the user selected folder and creates a list of first sample IDs and number of bottles fired for each CTD cast.
#'
#' @author Jeff Jackson
#' Date: 17-FEB-2021
#' Last Updated: 30-OCT-2023
#'
#' @param headerlineflag boolean variable (1 for TRUE 0 for FALSE) to indicate if the QAT files contain a header line.
#' @param mission_year numeric variable to indicate the year the mission was conducted.
#' 
#' @returns bottle_id.txt file containing the QAT file name, starting sample id and number of bottles fired.
#'
#' @examples 
#' > buildBottleIdFile(1)
#'
#' Copyright 2021, DFO, Bedford Institute of Oceanography, Canada.
#' All Rights Reserved.

#' ------------------------------------------------------------------------
#'
#'   Report any bugs to DataServicesDonnees@dfo-mpo.gc.ca
#'
#'   Jeff Jackson (22-FEB-2021)
#'   - Updated metadata to conform to ROxygen2 guidelines.
#'   - Fixed the error in the read.csv calls to manage the header correctly.
#'   
#'   Jeff Jackson (29-MAR-2023)
#'   - Added the input parameter 'mission_year' so when the mission is before
#'   2014; column names can be assigned to the QAT columns read in since 
#'   column names were not used prior to 2014.
#' 
#'   Jeff Jackson (30-OCT-2023)
#'   - Modified the code to select the directory as the old code stopped working
#'   for newer versions of R.
#' 
#' ------------------------------------------------------------------------
  
library(rstudioapi)
library(utils)

buildBottleIdFile <- function(headerlineflag, mission_year) {

  # Get user to select the folder containing the QAT files to be processed
	file_dir <- selectDirectory(getwd(), caption = "QAT file folder")

  # Save the current working directory
  curdir <- getwd()
  
  # Change to the user selected folder
  setwd(file_dir)
  
  # Get a list of all QAT files in the folder
  qatFiles <- list.files(file_dir, pattern = "\\.QAT")
  
  # Get the number of QAT files to process
  nQATS <- length(qatFiles)

  # Loop through the QAT files retrieving the bottle id information
  for (i in 1:nQATS) {
    
    # Current QAT file being processed
    qat_file <- qatFiles[i]
    
    # If QAT files has a headerline or not read file accordingly
    if (headerlineflag) {
      dfQAT <- read.csv(qat_file, header = TRUE)
    } else {
      dfQAT <- read.csv(qat_file, header = FALSE)
  
      if (mission_year < 2014) {
        
        # Assign names to the qatDF fields since the files did not have column names.
        # Note that these column names may not be correct or even the right number;
        # since the files changed based on the sensors deployed on the CTD.
        names(dfQAT) <- c('cruise_number','event','eventLatitude','eventLongitude', 'trip_number','sample_id',
                          'date','time','Sigma0','Sbeox0','Potemp068C','Sal00','Scan','T090C','PrDM','C0S_m',
                          'rosette','Sigma1','Sbeox1','POTM_02','Sal11','TE90_02','C1S_m','FLOR_1')
        
      }
      
    }
    
    # Get the minimum sample id in the current QAT file
    min_id <- min(dfQAT$sample_id)
    
    # Get the number of sample ids in the current QAT file
    numbot <- length(dfQAT$sample_id)

    print(paste0("Processing File: ", qat_file, " with starting ID: ", as.numeric(min_id), " and nBottles: ", as.numeric(numbot)))

    # Get the QAT file name without the extension
    ss <- strsplit(qat_file, split = "[.]")
    ff <- ss[[1]][1]
    
    # Print current QAT info to the output file
    cat(paste0(tolower(ff), ", ", min_id, ", ", numbot, "\n"), file = "bottle_ids.txt", append = TRUE)
  }

  # Change working folder back to original
  setwd(curdir)
  
}
