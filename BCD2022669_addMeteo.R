# Read in the Prince 5 ODF files and add a METEO_HEADER with the information
# gathered from the log sheets.

# Jeff Jackson
# Last Updated: 23-JAN-2023

library(dplyr)
library(lubridate)
library(tibble)

# Source the ODF Toolbox scripts
sourceDir("C:/DEV/GitHub/ODF-Toolbox/odfR/R")

setwd("C:/DEV/Data/2022/BCD2022669/CTD/DATASHOP_PROCESSING")
 
at <- rep(-99.0, 11)
ap <- rep(-99.0, 11)
ws <- c(5.14, 7.72, 3.09, 6.69, 0, 5.66, 2.57, 7.72, 9.26, -99.0, 13.12)
wd <- c(22.5, 315, 315, 135, -99.0, 180, 157.5, 180, 157.5, -99.0, 315)
ss <- c(1, 1, 2, 3, 0, 2, 2, 3, 2, -99.0, 4)
cc <- c(8, 7, 2, 8, 8, 1, 9, 8, 9, -99.0, 8)
it <- rep(0, 11)
mc <- c("SST was 7.4 degrees C", "SST was 3.2 degrees C", "SST was 3.0 degrees C", "", 
        "SST was 9.2 degrees C", "SST was 11.3 degrees C", "SST was 15.8 degrees C",
        "SST was 12.4 degrees C", "SST was 12.0 degrees C", "", "SST was 8.0 degrees C")

curdir <- getwd()

setwd(paste0(curdir, "/Step_3_Update_Metadata"))

files <- list.files(pattern = '*.ODF')

for (i in 1:length(files)) {
  
  odfFile <- files[i]
  a <- read_odf(odfFile)
  event <- as.numeric(a$EVENT_HEADER$EVENT_NUMBER)
  b <- odfAddMeteoHeader(a)
  b$METEO_HEADER$AIR_TEMPERATURE <- at[event]
  b$METEO_HEADER$ATMOSPHERIC_PRESSURE <- ap[event]
  b$METEO_HEADER$WIND_SPEED <- ws[event]
  b$METEO_HEADER$WIND_DIRECTION <- wd[event]
  b$METEO_HEADER$SEA_STATE <- ss[event]
  b$METEO_HEADER$CLOUD_COVER <- cc[event]
  b$METEO_HEADER$ICE_THICKNESS <- it[event]
  b$METEO_HEADER$METEO_COMMENTS <- mc[event]
  
  # Fix the times in most of the ODF files as they are incorrect
  if (event >= 2) {
    
    sdt <- parse_date_time(b$EVENT_HEADER$START_DATE_TIME, 'd-b-Y H:M:OS')
    
    if (event == 2) {
      stime <- ymd_hms("2022-02-15 13:31:00")
    } else if (event == 3) {
      stime <- ymd_hms("2022-03-15 12:22:00")
    } else if (event == 4) {
      stime <- ymd_hms("2022-05-26 12:25:00")
    } else if (event == 5) {
      stime <- ymd_hms("2022-06-14 12:24:00")
    } else if (event == 6) {
      stime <- ymd_hms("2022-07-13 12:40:00")
    } else if (event == 7) {
      stime <- ymd_hms("2022-08-26 12:28:00")
    } else if (event == 8) {
      stime <- ymd_hms("2022-09-26 12:27:00")
    } else if (event == 9) {
      stime <- ymd_hms("2022-10-13 12:11:00")
    } else if (event == 11) {
      stime <- ymd_hms("2022-12-19 13:10:00")
    }
    
    # Get the SYTM channel
    otime <- as_tibble_col(b$DATA$SYTM_01, column_name = "sytm")
  
    y <- length(otime$sytm)
    
    # Compute the differences in time between each value in the SYTM channel 
    # and the event's original start date-time
    odiff <- otime$sytm - sdt
    
    # Add the period differences to the correct starting date-time
    ntime <- stime + odiff
  
    # Replace the values in SYTM with the corrected values
    b$DATA$SYTM_01 <- ntime
   
    # Update Start_Date_Time and End_Date_Time fields with their correct values.
    # First change the digits output for fractional seconds to 2 to adhere to 
    # the ODF format specification; along with changing the abbreviated month to 
    # uppercase.
    op <- options(digits.secs=2)
    b$EVENT_HEADER$START_DATE_TIME <- toupper(strftime(stime, format = '%d-%b-%Y %H:%M:%OS', tz='UTC'))
    b$EVENT_HEADER$END_DATE_TIME <- toupper(strftime(tail(ntime, 1), format = '%d-%b-%Y %H:%M:%OS', tz='UTC'))
  }
  
  # Write the updated ODF file to the UPDATED folder
  setwd(paste0(curdir, "/Step_4_Add_Meteo_Header"))
  write_odf(b, odfFile)
  setwd(paste0(curdir, "/Step_3_Update_Metadata"))
  
}

setwd(curdir)
