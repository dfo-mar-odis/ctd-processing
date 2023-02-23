# Read in the ODF files that have incorrect times. Fix the times and output new 
# ODF files.

# Jeff Jackson
# Last Updated: 23-FEB-2023

library(dplyr)
library(lubridate)
library(tibble)

# Source the ODF Toolbox scripts
sourceDir('C:/DEV/GitHub/ODF-Toolbox/odfR/R')

curdir <- getwd()
setwd(paste0(curdir, '/Step_6_Visual_Inspection'))

# Get a list of the files to be updated
odfFiles <- list.files(pattern = '*.ODF')

# Specify the list of event patterns that should be used to identify the files
# to be modified.
pats <- c("_029", "_063", "_064")

for (i in 1:length(pats)) {

  ix <- grep(pats[i], odfFiles)
  
  eventFiles <- odfFiles[ix]

  for (j in 1:length(ix)) {
    
    odfFile <- eventFiles[j]
    a <- read_odf(odfFile)
    event <- as.numeric(a$EVENT_HEADER$EVENT_NUMBER)
    
    # Fix the times in most of the ODF files as they are incorrect
    sdt <- parse_date_time(a$EVENT_HEADER$START_DATE_TIME, 'd-b-Y H:M:OS')
    
    if (event == 30) {
      stime <- ymd_hms('2022-07-15 22:22:00')
    } else if (event == 65) {
      stime <- ymd_hms('2022-07-22 17:33:00')
    } else if (event == 66) {
      stime <- ymd_hms('2022-07-22 19:59:00')
    }
    
    # Get the SYTM channel
    otime <- as_tibble_col(a$DATA$SYTM_01, column_name = 'sytm')
    
    y <- length(otime$sytm)
    
    # Compute the differences in time between each value in the SYTM channel 
    # and the event's original start date-time
    odiff <- otime$sytm - sdt
    
    # Add the period differences to the correct starting date-time
    ntime <- stime + odiff
    
    # Replace the values in SYTM with the corrected values
    a$DATA$SYTM_01 <- ntime
    
    # Update Start_Date_Time and End_Date_Time fields with their correct values.
    # First change the digits output for fractional seconds to 2 to adhere to 
    # the ODF format specification; along with changing the abbreviated month to 
    # uppercase.
    op <- options(digits.secs=2)
    a$EVENT_HEADER$START_DATE_TIME <- toupper(strftime(stime, format = '%d-%b-%Y %H:%M:%OS', tz='UTC'))
    a$EVENT_HEADER$END_DATE_TIME <- toupper(strftime(tail(ntime, 1), format = '%d-%b-%Y %H:%M:%OS', tz='UTC'))
    
    # Write the updated ODF file to the UPDATED folder
    setwd(paste0(curdir, '/Step_7_Final_Metadata_Update'))
    write_odf(a, odfFile)
    setwd(paste0(curdir, '/Step_6_Visual_Inspection'))
    
  }
}

setwd(curdir)
