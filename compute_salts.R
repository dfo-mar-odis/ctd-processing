#' @title compute_salts.R
#' @author Jeff Jackson
#' 
#' @param ids An array of sample id numbers
#' @param autosal An array of water sample conductivity ratios obtained from a 
#' 8400B Autosal Lab salinomter.
#' @param batch A string containing the IAPSO Standard Seawater batch identifier
#' @returns A data frame containing the ids and drift corrected salinities 
#' (a.k.a. salts).
#'   
#' @note Report any bugs to DataServicesDonnees@dfo-mpo.gc.ca
#' 
#' @examples
#' compute_salts(sampleIDs, CRATx2, 'P151')
#' 
#' @details 
#' @source Ocean Data and Information Services, Bedford Institute of 
#' Oceanography, DFO, Canada.
#' 
#' Created: 03-MAR-2023
#' Last Updated: 03-MAR-2023
#' 
#' You may distribute under the terms of either the GNU General Public
#' License or the Apache v2 License, as specified in the README file.

library(gsw)
library(oce)
library(tibble)

compute_salts <- function(ids, autosal, batch) {
  
  output_df <- tibble(ids, autosal)
  
  names(output_df)[2] <- "drift_corrected_salts"

  batchIds <- which(ids == batch)
  nBatchIds <- length(batchIds)
  
  # Loop through the sections of conductivity ratios to compute salts that are 
  # adjusted by the drifts.
  for (i in 1:(nBatchIds - 1)) {

    # Get the Standard Seawater values that form the endpoints for the current 
    # section.
    b1 <- batchIds[i]
    b2 <- batchIds[i+1]
    
    standard1 <- autosal[b1]
    standard2 <- autosal[b2]
    
    total_drift <- standard2 - standard1
    
    n <- b2 - b1
    s <- 1:n
    
    # Compute the salts
    salts <- gsw_SP_salinometer(autosal[(b1+1):b2]/2, 24)
    
    # Calculate the drift over the section
    drift <- (total_drift / n) * s
    
    # Compute the drift corrected salts
    corrected_salts <- salts + drift
  
    output_df$drift_corrected_salts[(b1+1):b2] <- corrected_salts
    
  }
  
  # Remove the Standard Seawater rows from the output data frame
  output_df <- output_df[-batchIds,]
  
  return (output_df)
}
