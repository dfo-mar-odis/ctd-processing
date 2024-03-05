#
# check if a plot is going to a png file, if yes, create a png file
#
is_fig_a_png <- function(isPng, figNum, figMission, figTitle) {
    if( isPng ) {
        pngFile <- paste('Plots/Figure_', as.character(figNum), '_', figMission,
                         figTitle, '.png', sep = '')
        png(filename = pngFile, width = 2000, height = 1500, res = 300)
    }
}


#
# Load Required Packages
#
library(dplyr)
library(gsw)
library(ggplot2)

mission  <- 'HUD2014004'
fig_out  <- 'T'    # T: plots as png files, F: plots to Plots pane in RStudio

col_blue <- '#0066CC'
col_red  <- '#CC3300'
fig_num  <- 11     # that's because Calibration Oxygen produces 11 figures

#
# Read in Data
#
wd <- getwd()
print(wd)
input_file <- file.path(wd, 'Input', 'HUD2014004_Salinity_Rpt.csv')
df_data_raw <- read.csv(input_file)

#
# Convert T68 to T90
#
df_data_raw$Temp_CTD_P_90 <- oce::T90fromT68(df_data_raw$Temp_CTD_P)
df_data_raw$Temp_CTD_S_90 <- oce::T90fromT68(df_data_raw$Temp_CTD_S)

#
# Determine bottle conductivity based on CTD temperature (ITS-90) and pressure.
# Convert units from mS/cm to S/m by dividing by 10.
#
df_data_raw$botcond <- gsw_C_from_SP(df_data_raw$Sal_Rep1,
                                     t = (df_data_raw$Temp_CTD_P_90
                                          + df_data_raw$Temp_CTD_S_90) / 2,
                                     p = df_data_raw$Start_Depth) / 10

#
# Step 1: Filter outliers between CTD primary and secondary conductivity
#
ctd_cond_diff <- df_data_raw$Cond_CTD_P - df_data_raw$Cond_CTD_S

fig_num <- fig_num + 1
title <- '_Boxplot_of_CTD_Conductivity_Differences'
is_fig_a_png(fig_out, fig_num, mission, title)

ctd_cond_box <- boxplot(ctd_cond_diff,
                        main = paste('Figure ', as.character(fig_num), ': ', mission,
                                     ' Boxplot of CTD Conductivity Differences', sep = ''),
                        ylab = 'Primary CTD Conductivity - Secondary CTD Conductivity (S/m)',
                        cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

if( fig_out ) { dev.off() }

ctd_cond_outliers <- ctd_cond_diff %in% ctd_cond_box$out
df_data_filtered <- df_data_raw %>% .[!ctd_cond_outliers,]

#
# Plot the outliers (in red)
#
fig_num <- fig_num + 1
title <- '_Outliers_Outside_1.5IQR'
is_fig_a_png(fig_out, fig_num, mission, title)

plot(ctd_cond_diff, col = col_blue, pch = 20,
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  ' Outliers Outside 1.5*IQR', sep = ''),
     xlab = 'Ordered by Event and Increasing Sample ID',
     ylab = 'Primary CTD Conductivity - Secondary CTD Conductivity (S/m)',
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

outliers_index <- which(ctd_cond_outliers == TRUE)
new_ctd_cond_diff <- matrix(NA, length(ctd_cond_diff))
new_ctd_cond_diff[outliers_index] <- ctd_cond_box$out
points(new_ctd_cond_diff, col = col_red, pch = 20)
abline(ctd_cond_box$stats[1], 0, lty = 3)    # Lower Whisker
abline(ctd_cond_box$stats[3], 0)             # Median
abline(ctd_cond_box$stats[5], 0, lty = 3)    # Upper Whisker

if( fig_out ) { dev.off() }

#
# Step 2: Filter outliers between CTD primary and Bottle Conductivity
#
prim_bot_cond_diff <- df_data_filtered$Cond_CTD_P - df_data_filtered$botcond

fig_num <- fig_num + 1
title <- '_Boxplot_of_Primary-Bottle_Conductivity_Differences'
is_fig_a_png(fig_out, fig_num, mission, title)

prim_bot_cond_box <- boxplot(prim_bot_cond_diff,
                             main = paste('Figure ', as.character(fig_num), ': ', mission,
                                          ' Boxplot of Primary-Bottle Conductivity Differences', sep = ''),
                             ylab = 'Primary CTD Conductivity - Bottle Conductivity (S/m)',
                             cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

if( fig_out ) { dev.off() }

prim_bot_cond_outliers <- prim_bot_cond_diff %in% prim_bot_cond_box$out
df_data_filtered_PB <- df_data_filtered %>% .[!prim_bot_cond_outliers,]

#
# Plot the outliers (in red)
#
fig_num <- fig_num + 1
title <- '_Outliers_Outside_1.5IQR'
is_fig_a_png(fig_out, fig_num, mission, title)

plot(prim_bot_cond_diff, col = col_blue, pch = 20,
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  ' Outliers Outside 1.5*IQR', sep = ''),
     xlab = 'Ordered by Event and Increasing Sample ID',
     ylab = 'Primary CTD Conductivity - Bottle Conductivity (S/m)',
     ylim = c(-0.010,0.010),
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
# Step 3: Filter outliers between CTD secondary and bottle Conductivity
#
sec_bot_cond_diff <- df_data_filtered$Cond_CTD_S - df_data_filtered$botcond

fig_num <- fig_num + 1
title <- '_Boxplot_of_Secondary-Bottle_Conductivity_Differences'
is_fig_a_png(fig_out, fig_num, mission, title)

sec_bot_cond_box <- boxplot(sec_bot_cond_diff,
                            main = paste('Figure ', as.character(fig_num), ': ', mission, 
                                         ' Boxplot of Secondary - Bottle Conductivity Differences', sep = ''),
                            ylab = 'Secondary CTD Conductivity - Bottle Conductivity (S/m)',
                            cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

if( fig_out ) { dev.off() }

sec_bot_cond_outliers <- sec_bot_cond_diff %in% sec_bot_cond_box$out
df_data_filtered_SB <- df_data_filtered %>% .[!sec_bot_cond_outliers,]

#
# Plot the outliers (in red)
#
fig_num <- fig_num + 1
title <- '_Outliers_Outside_1.5IQR'
is_fig_a_png(fig_out, fig_num, mission, title)

plot(sec_bot_cond_diff, col = col_blue, pch = 20,
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  ' Outliers Outside 1.5*IQR', sep = ''),
     xlab = 'Ordered by Event and Increasing Sample ID',
     ylab = 'Secondary CTD Conductivity - Bottle Conductivity (S/m)',
     ylim = c(-0.25,0.10),
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

isb = which(sec_bot_cond_outliers == TRUE)
new_sec_bot_cond_diff = matrix(NA, length(sec_bot_cond_diff))
new_sec_bot_cond_diff[isb] = sec_bot_cond_box$out
points(new_sec_bot_cond_diff, col = col_red, pch = 20)
abline(sec_bot_cond_box$stats[1], 0, lty = 3)    # Lower Whisker
abline(sec_bot_cond_box$stats[3], 0)             # Median
abline(sec_bot_cond_box$stats[5], 0, lty = 3)    # Upper Whisker

if( fig_out ) { dev.off() }

#
# Compute Linear Calibration Coefficients for Primary Conductivity (3562)
#
pcond   <- df_data_filtered_PB$Cond_CTD_P
bcond4p <- df_data_filtered_PB$botcond

scond   <- df_data_filtered_SB$Cond_CTD_S
bcond4s <- df_data_filtered_SB$botcond

#
# CTD Primary conductivity slope and intercept calculations
#
fig_num <- fig_num + 1
title <- '_Linear_Regression_Fit_for_Primary_CTD_Conductivity'
is_fig_a_png(fig_out, fig_num, mission, title)

plot(pcond , bcond4p, col = col_blue,
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  ' Linear Regression Fit for Primary CTD Conductivity', sep = ''),
     xlab = 'Primary CTD Conductivity (S/m)',
     ylab = 'Autosal Conductivity (S/m)',
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

fit3 <- lm(bcond4p ~ pcond)
abline(fit3, col = col_blue)

if( fig_out ) { dev.off() }

summary(fit3)
b1_PC <- summary(fit3)$coefficients[1,1]    # Intercept
b2_PC <- summary(fit3)$coefficients[2,1]    # Slope

#
# Correct the CTD Primary conductivity data
#
df_data_filtered_PB$Cond_CTD_P_corr <- b1_PC + (b2_PC * pcond)

#
# Plot the Conductivity Differences before and after the linear correction
# for the Primary sensor
#
pdiff <- pcond - bcond4p

fig_num <- fig_num + 1
title <- '_Primary_CTD-Autosal_Conductivity_Differences'
is_fig_a_png(fig_out, fig_num, mission, title)

plot(pdiff, col = col_blue, pch = 20,
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  ' Primary CTD - Autosal Conductivity Differences', sep = ''),
     xlab = 'Ordered by Event and Increasing Sample ID',
     ylab = 'Primary CTD - Autosal (S/m)',
     ylim = range(-0.003,0.003),
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

mean_pdiff <- mean(pdiff, na.rm = TRUE)
abline(mean_pdiff, 0, col = col_blue)
pdiff_cor <- df_data_filtered_PB$Cond_CTD_P_corr - bcond4p
points(pdiff_cor, col = col_red, pch = 20, ylim = range(-0.003,0.003) )
mean_pdiff_cor <- mean(pdiff_cor, na.rm = TRUE)
abline(mean_pdiff_cor, 0, col = col_red)

if( fig_out ) { dev.off() }

#
# Compute Linear Calibration Coefficients for Secondary Conductivity (3561)
#

#
# There was a large change in the behavior of the secondary conductivity sensor.
# Before event 112 the secondary conductivity sensor varied significantly from 
# the primary conductivity sensor. Therefore two linear calibrations were created
# for the secondary conductivity sensor for the two differing data segments.
#

x <- df_data_filtered_SB$Event < 112

df_data_filtered_SB1 <- df_data_filtered_SB[x,]
df_data_filtered_SB2 <- df_data_filtered_SB[!x,]

### Segment 1 ###
scond1   <- df_data_filtered_SB1$Cond_CTD_S
bcond4s1 <- df_data_filtered_SB1$botcond

#
# CTD Secondary conductivity slope and intercept calculations
#
fig_num <- fig_num + 1
title <- '_Linear_Regression_Fit_for_Secondary_CTD_Conductivity_Segment_1'
is_fig_a_png(fig_out, fig_num, mission, title)

plot(scond1, bcond4s1, col = col_blue,
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  ' Linear Regression Fit for Secondary CTD Conductivity (Segment 1)', sep = ''),
     xlab = 'Secondary CTD Conductivity (S/m)',
     ylab = 'Autosal Conductivity (S/m)',
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

fit4 <- lm(bcond4s1 ~ scond1)
abline(fit4, col = col_blue)

if( fig_out ) { dev.off() }

summary(fit4)
b1_SC1 <- summary(fit4)$coefficients[1,1]    # Intercept
b2_SC1 <- summary(fit4)$coefficients[2,1]    # Slope

#
# Correct the CTD conductivity data
#
df_data_filtered_SB1$Cond_CTD_S_corr <- b1_SC1 + (b2_SC1 * scond1)
df_data_filtered_SB1$Cond_CTD_S_corr <- b1_SC1 + (b2_SC1 * scond1)

#
# Plot the Conductivity Differences before and after the linear correction
# for the Secondary sensor
#
sdiff1 <- scond1 - bcond4s1

fig_num <- fig_num + 1
title <- '_Secondary_Conductivity_Differences'
is_fig_a_png(fig_out, fig_num, mission, title)

plot(sdiff1, col = col_blue, pch = 20,
     main = paste0('Figure ', as.character(fig_num), ': ', mission, ' Secondary CTD - Autosal Conductivity Diffs (Segment 1)'),
     ylab = 'Secondary CTD - Autosal (S/m)',
     ylim = range(-0.02,0.01),
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

mean_sdiff1 <- mean(sdiff1, na.rm = TRUE)
abline(mean_sdiff1, 0, col = col_blue)
sdiff1_cor <- df_data_filtered_SB1$Cond_CTD_S_corr - bcond4s1
points(sdiff1_cor, col = col_red, pch = 20, ylim = range(-0.02, 0.01) )
mean_sdiff1_cor <- mean(sdiff1_cor, na.rm = TRUE)
abline(mean_sdiff1_cor, 0, col = col_red)

if( fig_out ) { dev.off() }


### Segment 2 ###

scond2   <- df_data_filtered_SB2$Cond_CTD_S
bcond4s2 <- df_data_filtered_SB2$botcond

#
# CTD Secondary conductivity slope and intercept calculations
#
fig_num <- fig_num + 1
title <- '_Linear_Regression_Fit_for_Secondary_CTD_Conductivity_Segment_1'
is_fig_a_png(fig_out, fig_num, mission, title)

plot(scond2, bcond4s2, col = col_blue,
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  ' Linear Regression Fit for Secondary CTD Conductivity (Segment 2)', sep = ''),
     xlab = 'Secondary CTD Conductivity (S/m)',
     ylab = 'Autosal Conductivity (S/m)',
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

fit5 <- lm(bcond4s2 ~ scond2)
abline(fit5, col = col_blue)

if( fig_out ) { dev.off() }

summary(fit5)
b1_SC2 <- summary(fit5)$coefficients[1,1]    # Intercept
b2_SC2 <- summary(fit5)$coefficients[2,1]    # Slope

#
# Correct the CTD conductivity data
#
df_data_filtered_SB2$Cond_CTD_S_corr <- b1_SC2 + (b2_SC2 * scond2)

#
# Plot the Conductivity Differences before and after the linear correction
# for the Secondary sensor
#
sdiff2 <- scond2 - bcond4s2

fig_num <- fig_num + 1
title <- '_Secondary_Conductivity_Differences_Segment_2'
is_fig_a_png(fig_out, fig_num, mission, title)

plot(sdiff2, col = col_blue, pch = 20,
     main = paste0('Figure ', as.character(fig_num), ': ', mission, ' Secondary CTD - Autosal Conductivity Diffs (Segment 2)'),
     ylab = 'Secondary CTD - Autosal (S/m)',
     ylim = range(-0.02,0.01),
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

mean_sdiff2 <- mean(sdiff2, na.rm = TRUE)
abline(mean_sdiff2, 0, col = col_blue)
sdiff2_cor <- df_data_filtered_SB2$Cond_CTD_S_corr - bcond4s2
points(sdiff2_cor, col = col_red, pch = 20, ylim = range(-0.02, 0.01) )
mean_sdiff2_cor <- mean(sdiff2_cor, na.rm = TRUE)
abline(mean_sdiff2_cor, 0, col = col_red)

if( fig_out ) { dev.off() }


#
# Plot the Sensor Conductivity Differences before and after the linear
# correction for the sensors
#
# -----------------------------------------------------------------------------
# Hu: Above we calculate slopes and intercepts for Primary and Secondary
#     sensors independently, now we need to plot the differences between
#     Primary and Secondary, we can use above results, but the lengths
#     of Primary and Secondary results are different, we can use the overlap
#     part according the index, but it wouldn't make too much difference
#     if we filter Primary first, then filter Secondary, although they are
#     no longer independently, but the plots wouldn't look too much different.
#

# 
# Combine the outlier arrays into a data frame then create a third Boolean 
# column where its values are set to TRUE if either outlier value for that row 
# was TRUE.
#
df <- tibble(prim_bot_cond_outliers, sec_bot_cond_outliers)
df <- df %>% mutate(cond_outliers = if_any(.cols = contains('bot'), I))

df_data_filtered_PSB <- df_data_filtered %>% .[!df$cond_outliers,]

pcond   <- df_data_filtered_PSB$Cond_CTD_P
bcond4p <- df_data_filtered_PSB$botcond

scond   <- df_data_filtered_PSB$Cond_CTD_S
bcond4s <- df_data_filtered_PSB$botcond

Cond_CTD_P_corr <- b1_PC + (b2_PC*pcond)

y <- df_data_filtered_PSB$Event < 112

Cond_CTD_S_corr1 <- b1_SC1 + (b2_SC1 * scond[y])
Cond_CTD_S_corr2 <- b1_SC2 + (b2_SC2 * scond[!y])

df_data_filtered_PSB$Cond_CTD_P_corr <- Cond_CTD_P_corr
df_data_filtered_PSB$Cond_CTD_S_corr <- c(Cond_CTD_S_corr1, Cond_CTD_S_corr2)

#
# -----------------------------------------------------------------------------

cdiff1 <- pcond - scond
cdiff1_corr <- df_data_filtered_PSB$Cond_CTD_P_corr - df_data_filtered_PSB$Cond_CTD_S_corr

fig_num <- fig_num + 1
title <- '_Conductivity_Sensor_Differences'
is_fig_a_png(fig_out, fig_num, mission, title)

plot(df_data_filtered_PSB$Start_Depth, cdiff1, col = col_blue, pch = 20,
     ylim = range(-0.03,0.03),
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  ' Conductivity Sensor Differences', sep = ''),
     ylab = 'Primary (#3561) - Secondary (#3562) (S/m)',
     xlab = 'Pressure (dbar)',
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

mean_cdiff1 <- mean(cdiff1, na.rm = TRUE)
abline(mean_cdiff1, 0, col = col_blue)
points(df_data_filtered_PSB$Start_Depth, cdiff1_corr, col = col_red, pch = 20, ylim = range(-0.03,0.03))
mean_cdiff1_corr <- mean(cdiff1_corr, na.rm = TRUE)
abline(mean_cdiff1_corr, 0, col = col_red)

if( fig_out ) { dev.off() }

#
# Plot the Sensor Conductivity Differences before and after the linear
# correction for the sensors.
#
fig_num <- fig_num + 1
title <- '_Conductivity_Sensor_Differences'
is_fig_a_png(fig_out, fig_num, mission, title)

plot(cdiff1, pch = 20, col = col_blue,
     ylim = range(-0.03,0.03),
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  ' Conductivity Sensor Differences', sep = ''),
     ylab = 'Primary (#3561) - Secondary (#3562) (S/m)',
     xlab = 'Ordered by Event and Increasing Sample ID',
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

mean_cdiff1 <- mean(cdiff1, na.rm = TRUE)
abline(mean_cdiff1, 0, col = col_blue)
points(cdiff1_corr, col = col_red, pch = 20, ylim = range(-0.03,0.03))
mean_cdiff1_corr <- mean(cdiff1_corr, na.rm = TRUE)
abline(mean_cdiff1_corr, 0, col = col_red)

if( fig_out ) { dev.off() }

#
# Calculate the Salinity (PSU) of the CTD sensors using the CTD conductivity,
# temperature (ITS-90) and pressure
#
ctdsal1 <- (oce::swSCTp(df_data_filtered_PSB$Cond_CTD_P_corr,
                        temperature = df_data_filtered_PSB$Temp_CTD_P_90,
                        pressure = df_data_filtered_PSB$Start_Depth,
                        conductivityUnit = "S/m", eos = "gsw"))

ctdsal2 <- (oce::swSCTp(df_data_filtered_PSB$Cond_CTD_S_corr,
                        temperature = df_data_filtered_PSB$Temp_CTD_S_90,
                        pressure = df_data_filtered_PSB$Start_Depth,
                        conductivityUnit = "S/m", eos = "gsw"))

#
# Plot the Sensor Salinity Differences before and after the linear correction
# for the sensors
#
sdiff1 <- df_data_filtered_PSB$Sal_CTD_P - df_data_filtered_PSB$Sal_CTD_S
sdiff1_corr <- ctdsal1 - ctdsal2

fig_num <- fig_num + 1
title <- '_Salinity_Sensor_Differences'
is_fig_a_png(fig_out, fig_num, mission, title)

plot(df_data_filtered_PSB$Start_Depth, sdiff1, pch = 20, col = col_blue,
     main = paste('Figure ', as.character(fig_num), ': ', mission, 
                  ' Salinity Sensor Differences', sep = ''),
     ylab = 'Primary (#3561) - Secondary (#3562) (PSU)',
     xlab = 'Pressure (dbar)',
     ylim = range(-0.15,0.25),
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

mean_sdiff1 <- mean(sdiff1, na.rm = TRUE)
abline(mean_sdiff1, 0, col = col_blue)
points(df_data_filtered_PSB$Start_Depth, sdiff1_corr, col = col_red, pch = 20,
       ylim = range(-0.15, 0.25))
mean_sdiff1_corr <- mean(sdiff1_corr, na.rm = TRUE)
abline(mean_sdiff1_corr, 0, col = col_red)

if( fig_out ) { dev.off() }

#
# Plot the Sensor Salinity Differences before and after the linear correction
# for the sensors
#
fig_num <- fig_num + 1
title <- '_Salinity_Sensor_Differences'
is_fig_a_png(fig_out, fig_num, mission, title)

plot(sdiff1, pch = 20, col = col_blue,
     main = paste('Figure ', as.character(fig_num), ': ', mission, 
                  ' Salinity Sensor Differences', sep = ''),
     ylab = 'Primary (#3561) - Secondary (#3562) (PSU)',
     xlab = 'Ordered by Event and Increasing Sample ID',
     ylim = range(-0.15,0.25),
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)

mean_sdiff1 <- mean(sdiff1, na.rm = TRUE)
abline(mean_sdiff1, 0, col = col_blue)
points(sdiff1_corr, col = col_red, pch = 20, ylim = range(-0.15,0.25))
mean_sdiff1_corr <- mean(sdiff1_corr, na.rm = TRUE)
abline(mean_sdiff1_corr, 0, col = col_red)

if( fig_out ) { dev.off() }


# #
# # Plot the Sensor Salinity Differences before and after the linear correction
# # for the sensors using events before 112
# #
# 
# dfx <- df_data_filtered_PSB[x,]
# sdiffx <- dfx$Sal_CTD_P - dfx$Sal_CTD_S
# sdiffx_corr <- ctdsal1[x] - ctdsal2[x]
# 
# fig_num <- fig_num + 1
# title <- '_Salinity_Sensor_Differences'
# is_fig_a_png(fig_out, fig_num, mission, title)
# 
# plot(sdiffx, pch = 20, col = col_blue,
#      main = paste('Figure ', as.character(fig_num), ': ', mission, 
#                   ' Salinity Sensor Differences', sep = ''),
#      ylab = 'Primary (#3561) - Secondary (#3562) (PSU)',
#      xlab = 'Ordered by Event and Increasing Sample ID',
#      ylim = range(-0.15,0.25),
#      cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)
# 
# mean_sdiffx <- mean(sdiffx, na.rm = TRUE)
# abline(mean_sdiffx, 0, col = col_blue)
# points(sdiffx_corr, col = col_red, pch = 20, ylim = range(-0.15,0.25))
# mean_sdiffx_corr <- mean(sdiffx_corr, na.rm = TRUE)
# abline(mean_sdiffx_corr, 0, col = col_red)
# 
# if( fig_out ) { dev.off() }
# 
# #
# # Plot the Sensor Salinity Differences before and after the linear correction
# # for the sensors using events from 112 on
# #
# 
# y <- which(df_data_filtered_PSB$Event >= 112)
# dfy <- df_data_filtered_PSB[y,]
# sdiffy <- dfy$Sal_CTD_P - dfy$Sal_CTD_S
# sdiffy_corr <- ctdsal1[y] - ctdsal2[y]
# 
# fig_num <- fig_num + 1
# title <- '_Salinity_Sensor_Differences'
# is_fig_a_png(fig_out, fig_num, mission, title)
# 
# plot(sdiffy, pch = 20, col = col_blue,
#      main = paste('Figure ', as.character(fig_num), ': ', mission, 
#                   ' Salinity Sensor Differences', sep = ''),
#      ylab = 'Primary (#3561) - Secondary (#3562) (PSU)',
#      xlab = 'Ordered by Event and Increasing Sample ID',
#      ylim = range(-0.15,0.25),
#      cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.6, las = 1)
# 
# mean_sdiffy <- mean(sdiffy, na.rm = TRUE)
# abline(mean_sdiffy, 0, col = col_blue)
# points(sdiffy_corr, col = col_red, pch = 20, ylim = range(-0.15,0.25))
# mean_sdiffy_corr <- mean(sdiffy_corr, na.rm = TRUE)
# abline(mean_sdiffy_corr, 0, col = col_red)
# 
# if( fig_out ) { dev.off() }
