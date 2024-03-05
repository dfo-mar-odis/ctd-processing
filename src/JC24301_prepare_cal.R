#' Prepare the HUD2021185 discrete data for calibrating the CTD data.

#' Bedford Institute of Oceanography, Fisheries and Oceans Canada.
#' You may distribute under the terms of either the GNU General Public
#' License or the Apache v2 License, as specified in the README file.

#' Script to prepare the discrete CTD data and bottle oxygen and conductivity data 
#' to be used in calibrating the CTD data.
#' 
#' Output:
#'   HUD2021185_Prepared_Data.RData: a copy of the workspace containing the 
#'   variables created and resulting calibration coefficients.
#'
#' IMPORTANT NOTES:
#' 
#'   Added .Rprofile to the project directory in order to set up some required settings to make 
#'   this code run correctly.  Especially running the command: options(stringsAsFactors = FALSE) 
#'   because this kept factor levels from being assigned to the data frame which was causing a lot
#'   of issues with the conductivity processing.
#'
#'   Report any bugs to DataServicesDonnees@@calDFo-mpo.gc.ca
#' 
#' ODSToolbox Version: 2.0
#'
#' Created: 21-DEC-2021
#' Last Updated: 12-JAN-2022
#' 
#' @export 
#' @examples
#' HUD2021185_prepare_cal()
#' 
#' @details 
#' Source:
#'   Ocean Data and Information Services,
#'   BecalDFord Institute of Oceanography, DFO, Canada.
#'   DataServicesDonnees@@calDFo-mpo.gc.ca
#' 
#' @author Ocean Data and Information Services
#'

# ------------------------------------------------------------------------
#   Updates:
# 
#     Jeff Jackson (12-JAN-2022)
#     - Ran the calibrations scripts using the corrected CTD data to test 
#       whether the calibrations worked properly.
# 
# ------------------------------------------------------------------------

####  The oxygen and conductivity outlier calculations were done based on the 1.5 IQR 

library(readxl)
library(dplyr)
library(gsw)
library(oce)

#
# set working directory, input & output files
#
wk_dir         <- 'C:/My_CTD/CTD_Processing/Step_1_Compute_Calibrations'

input_QAT_file <- 'JC24301_QAT.xlsx'            # concatenated QAT files
input_OXY_file <- 'JC24301_oxy_JB.xlsx'         # Winkler Dissolved Oxygen Data
input_SAL_file <- 'JC24301_salts_final.xlsx'    # Autosal Salinity Data

input_QAT_path <- file.path(wk_dir, input_QAT_file)
input_OXY_path <- file.path(wk_dir, input_OXY_file)
input_SAL_path <- file.path(wk_dir, input_SAL_file)

output_OXY_file <- 'JC24301_Oxygen_Rpt.csv'
output_SAL_file <- 'JC24301_Salinity_Rpt.csv'

output_OXY_path <- file.path(wk_dir, 'Input', output_OXY_file)
output_SAL_path <- file.path(wk_dir, 'Input', output_SAL_file)

setwd(wk_dir)
cat('Preparing the data has started ...')

# -----------------------------------------------------------------------------
# read in the discrete CTD data (concatenated QAT files)
#
corrected <- 0    # 0 or 1, Hu: what's this variable for?

qatDF <- read_excel(input_QAT_path, 1)

#
# extract Event & Sample_ID columns
#
Event     <- qatDF$event
Sample_ID <- qatDF$sample_id

#
# remove the extra double quotes around date-time strings
#
qatDF$date <- noquote(qatDF$date)
qatDF$time <- noquote(qatDF$time)

#
# rename some of the columns
#
colnames(qatDF)[10:11] <- c('Sigma0', 'Sbeox0')
colnames(qatDF)[17]    <- c('C0S_m')
colnames(qatDF)[19:20] <- c('Sigma1', 'Sbeox1')
colnames(qatDF)[24]    <- c('C1S_m')

#
# re-order the QAT data frame
#
qatDF <- qatDF %>%
    # order oxygen report by event and sample ID
    dplyr::arrange(Event,Sample_ID) %>%
    # add a new variable called ID that can be used to plot the data
    # and/or keep it sorted properly
    dplyr::mutate(ID=as.numeric(row.names(qatDF)))

#
# find missing Sample IDs
#
missing_ids <- setdiff(490270:491223, qatDF$sample_id)

# -----------------------------------------------------------------------------
# read in the discrete Winkler Dissolved Oxygen data
#
oxyDF <- read_excel(input_OXY_path, skip = 9, col_names = TRUE)

#
# rename certain oxyDF columns
#
colnames(oxyDF)[1:3] <- c('SampleID', 'BottleNumber', 'O2_Concentration')

#
# find which oxygen values are NaN and change them to NA
#
inan <- which(is.nan(oxyDF$O2_Concentration) == TRUE)
oxyDF$O2_Concentration[inan] <- NA

#
# get the unique Oxygen Sample IDs
#
oxyID <- as.numeric(substr(oxyDF$SampleID, 1, 6))

#
# identify the indices for the unique and duplicate Oxygen Sample IDs
#
UO <- !duplicated(oxyID)
DO <- duplicated(oxyID)

#
# get the Oxygen IDs that are unique and duplicate(s)
#
oxyUniqID <- oxyID[which(UO == TRUE)]
oxyDupID  <- oxyID[which(DO == TRUE)]

#
# get the unique Oxygen values
#
oxyUniqBottle <- oxyDF$O2_Concentration[UO]

#
# create an bottle oxygen duplicate array the same size as the unique oxygen
# array. Put duplicate values in corresponding index for identical Sample ID
#
oxyDupBottle <- array(data = NA, dim = length(oxyUniqID))

for (i in 1:length(oxyDupID)) {
    j <- which((oxyID == oxyDupID[i]) == TRUE)

    # get the values for each ID
    dupOxy <- oxyDF$O2_Concentration[j[2]]

    # find the index for the current ID in oxyDupID
    k <- which((oxyUniqID == oxyDupID[i]) == TRUE)

    # put the duplicate oxygen value into the oxyDupBottle array
    oxyDupBottle[k] <- dupOxy
}

#
# clear temporary variables
#
rm(i, j, k, UO, DO, oxyID, dupOxy)

# -----------------------------------------------------------------------------
# read the discreat Autosal Salinity data
#
condDF <- read_excel(input_SAL_path, skip = 1, col_names = TRUE,
                     guess_max = 2511)

#
# rename certain condDF columns
#
colnames(condDF) <- c('SampleID', 'Reading#', 'Value#', 'BottleLabel',
                      'DateTime', 'BathTemperature', 'UncorrectedRatio',
                      'UncorrectedRatioStandDev', 'Correction', 'AdjustedRatio',
                      'CalculatedSalinity', 'CalculatedSalinityStandDev',
                      'Comments')

#
# where the BottleLabel value is blank assign it as 'NA'
#
condDF$BottleLabel[condDF$BottleLabel == ''] <- NA

#
# get indices of records that contain a Sample ID or Standard Batch Number
#
x <- which(!is.na(condDF$BottleLabel) == TRUE)

#
# get indices that only refer to a numeric Sample ID
#
y <- as.numeric(condDF$BottleLabel[x])

#
# drop all records except those containing a numeric Sample ID
#
condDF <- condDF[x[!is.na(y)],]

#
# update the data frame by dropping all levels that no longer exist
#
condDF <- droplevels(condDF)
condID <- as.numeric(condDF$BottleLabel)

condBottle <- as.numeric(condDF$AdjustedRatio)
salBottle  <- as.numeric(condDF$CalculatedSalinity)

#
# Standard Batch P163
#
Standard_K15     <- 0.99985
KCl_Conductivity <- 42.914

Standard_Conductivity <- KCl_Conductivity * Standard_K15
condBottle            <- condBottle * Standard_Conductivity    # mS/cm

#
# identify the indices for the unique and duplicate conductivity Sample IDs
#
UC <- !duplicated(condID)
DC <- duplicated(condID)

#
# get the conductivity IDs that are unique and duplicate(s)
#
condUniqID <- condID[which(UC == TRUE)]
condDupID  <- condID[which(DC == TRUE)]

#
# get the unique conductivity and salinity values
#
condUniqBottle <- condBottle[UC]
salUniqBottle  <- salBottle[UC]


# ******************************************
cat('\n')
# cat(qatDF$sample_id)
lapply(qatDF, head)
cat(mode(oxyDF))
cat("\n")
print(oxyDF[13,3])
print(oxyDF)
print(missing_ids)
print('_AAAAAAAAAAA')
# print(oxyDF$SampleID)
# str(qatDF, max=1)
cat('\n')
# x <- 0
# stopifnot(x > 1)
# ******************************************

#
# create a bottle conductivity duplicate array which is the same size as the
# unique conductivity array. Put the duplicate values in the corresponding
# index for the identical Sample ID
#
condDupBottle <- array(data = NA, dim = length(condUniqID))
salDupBottle  <- array(data = NA, dim = length(condUniqID))

for (i in 1:length(condDupID)) {
    j <- which((condID == condDupID[i]) == TRUE)

    # get the values for each ID
    dupCond <- condBottle[j[2]]
    dupSal  <- salBottle[j[2]]

    # find the index for the current ID in condDupID
    k <- which((condUniqID == condDupID[i]) == TRUE)

    # put the duplicate values into the appropriate arrays
    condDupBottle[k] <- dupCond
    salDupBottle[k] <- dupSal
}

#
# clear temporary variables
#
# Hu: comment out following line because it's a duplication, next next line is same
#
# rm(Standard_K15, KCl_Conductivity, Standard_Salinity, Standard_Conductivity)

#
# clear temporary variables
#
# Hu: comment out following line because 'Standard_Salinity' not found,
#     I replace it with the same line without 'Standard_Salinity'
#
# Hu: 'Standard_Salinity' not found, I removed it and changed the line as below
#
# rm(Standard_K15, KCl_Conductivity, Standard_Salinity, Standard_Conductivity)
# 
rm(Standard_K15, KCl_Conductivity, Standard_Conductivity)
# rm(x, y, qatFile, oxyFile, salFile)
rm(x, y, input_QAT_path, input_OXY_path, input_SAL_path)

# -----------------------------------------------------------------------------
# now create vectors for the Bottle Oxygen and Conductivity data that are
# the same size as the CTD vectors
#
sampleID <- qatDF$sample_id
y        <- length(sampleID)

oxygenBottleID        <- array(data = NA, dim = y)
oxygenBottle          <- array(data = NA, dim = y)
oxygenBottleDuplicate <- array(data = NA, dim = y)

conductivityBottleID        <- array(data = NA, dim = y)
conductivityBottle          <- array(data = NA, dim = y)
conductivityBottleDuplicate <- array(data = NA, dim = y)

salinityBottle          <- array(data = NA, dim = y)
salinityBottleDuplicate <- array(data = NA, dim = y)

rm(y)

#
# find the indices for each Oxygen ID
#
for (i in 1:length(oxyUniqID)) {
    j <- which((sampleID == oxyUniqID[i]) == TRUE)

    # put the Oxygen information into the correct spots in the full vectors
    oxygenBottleID[j]        <- oxyUniqID[i]
    oxygenBottle[j]          <- oxyUniqBottle[i]
    oxygenBottleDuplicate[j] <- oxyDupBottle[i]
}

#
# clear temporary variables
#
rm(i, j, oxyUniqID, oxyUniqBottle, oxyDupID, oxyDupBottle)

#
# find the indices for each Conductivity ID
#
for (i in 1:length(condUniqID)) {
    j <- which((sampleID == condUniqID[i]) == TRUE)

    # put the Conductivity information into correct spots in full vectors
    conductivityBottleID[j]        <- condUniqID[i]
    conductivityBottle[j]          <- condUniqBottle[i]
    conductivityBottleDuplicate[j] <- condDupBottle[i]

    salinityBottle[j]          <- salUniqBottle[i]
    salinityBottleDuplicate[j] <- salDupBottle[i]
}

#
# clear temporary variables
#
rm(i, j, condID, condBottle, salBottle)

nrows          <- length(sampleID)
station_number <- Event

#
# create a data.frame to hold the oxygen data
#
oxyReportDF             <- data.frame(station_number)
oxyReportDF$Event       <- station_number
oxyReportDF$SAMPLE_ID   <- sampleID
oxyReportDF$Start_Depth <- qatDF$PrDM
oxyReportDF$Oxy_CTD_P   <- qatDF$Sbeox0
oxyReportDF$Oxy_CTD_S   <- qatDF$Sbeox1
oxyReportDF$Oxy_W_Rep1  <- oxygenBottle
oxyReportDF$Oxy_W_Rep2  <- oxygenBottleDuplicate

#
# output the Oxygen report
#
write.table(oxyReportDF, file = output_OXY_path, quote = FALSE, sep = ',',
            row.names = FALSE, na = '')

#
# create a data.frame to hold the salinity data
#
# # Hu: I use T090C and T190C, so I replace T068C, T168C
#
#       salReportDF$Temp_CTD_P <- qatDF$T068C # IPTS-68
#       salReportDF$Temp_CTD_S <- qatDF$T168C # IPTS-68
#
# salinityBottle <- format(round(salinityBottle, 3), nsmall=3)


salReportDF             <- data.frame(station_number)
salReportDF$Event       <- station_number
salReportDF$SAMPLE_ID   <- sampleID
salReportDF$Start_Depth <- qatDF$PrDM
salReportDF$Temp_CTD_P  <- qatDF$T090C    # IPTS-90
salReportDF$Temp_CTD_S  <- qatDF$T190C    # IPTS-90
salReportDF$Cond_CTD_P  <- qatDF$C0S_m    # S/m
salReportDF$Cond_CTD_S  <- qatDF$C1S_m    # S/m
salReportDF$Sal_CTD_P   <- qatDF$Sal00
salReportDF$Sal_CTD_S   <- qatDF$Sal11
salReportDF$Sal_Rep1    <- salinityBottle
salReportDF$Sal_Rep2    <- salinityBottleDuplicate
salReportDF$Cond_Rep1   <- conductivityBottle # PSU
salReportDF$Cond_Rep2   <- conductivityBottleDuplicate

#
# output the Oxygen report
#
write.table(salReportDF, file = output_SAL_path, quote = FALSE, sep = ',',
            row.names = FALSE, na = '')

# rm(conductivityBottle, conductivityBottleID)
rm(oxygenBottleID, oxygenBottle, oxygenBottleDuplicate)
rm(conductivityBottleID, conductivityBottle, salinityBottle)
rm(station_number, inan, nrows, sampleID)

#
# save the workspace variables to the file 'HUD2021185_Prepared_Data.RData'
#
save.image(file = 'JC24301_Prepared_Data.RData')

cat('The prepared data has been saved to the file JC24301_Prepared_Data.mat')
