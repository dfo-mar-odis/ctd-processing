#
# check if a plot is going to a png file, if yes, create a png file
#
is_fig_a_png <- function(isPng, figNum, figMission, figTitle, corrected) {
  if( isPng ) {
    if (corrected) {
      pngFile <- paste('Plots/', 'Figure_', as.character(figNum), 
                       '_', figMission, figTitle, '_Corrected.png', sep = '')
      png(filename = pngFile, width = 2000, height = 1500, res = 300)
    } else {
      pngFile <- paste('Plots/', 'Figure_', as.character(figNum), 
                       '_', figMission, figTitle, '_Uncorrected.png', sep = '')
      png(filename = pngFile, width = 2000, height = 1500, res = 300)
    }
  }
}


#
# Load Required Packages
#
library(dplyr)
library(gsw)
library(ggplot2)

# -----------------------------------------------------------------------------
# Hu: it seems we do not need to load RData for this processing
#
# Load the work space variables from the file 'BCD2024666_Prepared_Data.RData'
#
# ### load('BCD2024666_Prepared_Data.RData')
# -----------------------------------------------------------------------------

mission  <- 'BCD2024666'
fig_out  <- 'T'    # T: plots as png files, F: plots to Plots pane in RStudio

col_blue <- '#0066CC'
col_red  <- '#CC3300'

# Indicate last figure number used for oxygen calibration figures
fig_num  <- 8 

# Boolean indicating if the QAT data being used is uncorrected or corrected.
# corrected <- 0 # Uncorrected
corrected <- 1 # Corrected

#
# Read in Data
#
wd <- getwd()

if (corrected) {
  input_file <- file.path(wd, 'Input', 'BCD2024666_Salinity_Rpt_Corrected.csv')
} else {
  input_file <- file.path(wd, 'Input', 'BCD2024666_Salinity_Rpt_Uncorrected.csv')
}

df_data_raw <- read.csv(input_file)

#
# Convert T68 to T90
# -----------------------------------------------------------------------------
# Hu: I used T90 in CTDDAP, so don't need to convert here
#
# ### df_data_raw$Temp_CTD_P_90 <- oce::T90fromT68(df_data_raw$Temp_CTD_P)
# ### df_data_raw$Temp_CTD_S_90 <- oce::T90fromT68(df_data_raw$Temp_CTD_S)
# -----------------------------------------------------------------------------
df_data_raw$Temp_CTD_P_90 <- df_data_raw$Temp_CTD_P
df_data_raw$Temp_CTD_S_90 <- df_data_raw$Temp_CTD_S

#
# Determine bottle conductivity based on CTD temperature (ITS-90) and pressure.
# Convert units from mS/cm to S/m by dividing by 10.
#
df_data_raw$botcond <- gsw_C_from_SP(df_data_raw$Sal_Rep1, 
                                     t = df_data_raw$Temp_CTD_P_90,
                                     p = df_data_raw$Start_Depth) / 10

#
# Step 1: Filter outliers between CTD sensor and Bottle Conductivity
#
prim_bot_cond_diff <- df_data_raw$Cond_CTD_P - df_data_raw$botcond

fig_num <- fig_num + 1
title <- paste0('_Boxplot_of_Bottle_Conductivity_Differences')
is_fig_a_png(fig_out, fig_num, mission, title, corrected)

prim_bot_cond_box <- boxplot(prim_bot_cond_diff,
                             main = paste0('Figure ', as.character(fig_num), ': ', mission,
                                          ' Boxplot of Bottle Conductivity Differences'),
                             ylab = paste0(' CTD Conductivity - Bottle Conductivity (S/m)'),
                             cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

if( fig_out ) { dev.off() }

prim_bot_cond_outliers <- prim_bot_cond_diff %in% prim_bot_cond_box$out
df_data_raw_PB <- df_data_raw %>% .[!prim_bot_cond_outliers,]

#
# Plot the outliers (in red)
#
fig_num <- fig_num + 1
title <- '_Outliers_Outside_1.5IQR'
is_fig_a_png(fig_out, fig_num, mission, title, corrected)

plot(prim_bot_cond_diff, col = col_blue, pch = 20,
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  ' Outliers Outside 1.5*IQR', sep = ''),
     xlab = 'Ordered by Event and Increasing Sample ID',
     ylab = paste0(' CTD Conductivity - Bottle Conductivity (S/m)'),
     ylim = c(-0.10,0.10),
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

outliers_index <- which(prim_bot_cond_outliers == TRUE)
new_prim_bot_cond_diff <- matrix(NA,length(prim_bot_cond_diff))
new_prim_bot_cond_diff[outliers_index] <- prim_bot_cond_box$out
points(new_prim_bot_cond_diff, col = col_red, pch = 20)
abline(prim_bot_cond_box$stats[1], 0, lty = 3)    # Lower Whisker
abline(prim_bot_cond_box$stats[3], 0)             # Median
abline(prim_bot_cond_box$stats[5], 0, lty = 3)    # Upper Whisker

if( fig_out ) { dev.off() }

#
# Compute Linear Calibration Coefficients for CTD Conductivity
#
pcond   <- df_data_raw_PB$Cond_CTD_P
bcond4p <- df_data_raw_PB$botcond

#
# CTD sensor conductivity slope and intercept calculations
#
fig_num <- fig_num + 1
title <- paste0('_Linear_Regression_Fit_for_CTD_Conductivity')
is_fig_a_png(fig_out, fig_num, mission, title, corrected)

plot(pcond , bcond4p, col = col_blue,
     main = paste0('Figure ', as.character(fig_num), ': ', mission,
                  ' Linear Regression Fit for CTD Conductivity'),
     xlab = 'CTD Conductivity (S/m)',
     ylab = 'Autosal Conductivity (S/m)',
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

fit3 <- lm(bcond4p ~ pcond)
abline(fit3, col = col_blue)

if( fig_out ) { dev.off() }

summary(fit3)
b1_PC <- summary(fit3)$coefficients[1,1]    # Intercept
b2_PC <- summary(fit3)$coefficients[2,1]    # Slope

#
# Correct the CTD Sensor conductivity data
#
df_data_raw_PB$Cond_CTD_P_corr <- b1_PC + (b2_PC * pcond)

#
# Plot the CTD sensor Conductivity Differences before and after the linear 
# correction.
#
pdiff <- pcond - bcond4p

fig_num <- fig_num + 1
title <- paste0('_', '_CTD-Autosal_Conductivity_Differences')
is_fig_a_png(fig_out, fig_num, mission, title, corrected)

plot(pdiff, col = col_blue, pch = 20,
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  paste0(' CTD - Autosal Conductivity Differences')),
     xlab = 'Ordered by Event and Increasing Sample ID',
     ylab = 'CTD - Autosal (S/m)',
     ylim = range(-0.008,0.008),
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

mean_pdiff <- mean(pdiff, na.rm = TRUE)
abline(mean_pdiff, 0, col = col_blue)
pdiff_cor <- df_data_raw_PB$Cond_CTD_P_corr - bcond4p
points(pdiff_cor, col = col_red, pch = 20)
mean_pdiff_cor <- mean(pdiff_cor, na.rm = TRUE)
abline(mean_pdiff_cor, 0, col = col_red)

if( fig_out ) { dev.off() }
