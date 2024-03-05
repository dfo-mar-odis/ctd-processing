#
# check if a plot is going to a png file, if yes, create a png file
#
is_fig_a_png <- function(isPng, figNum, figMission, figTitle, corrected) {
  if( isPng ) {
    if (corrected) {
      pngFile <- paste('Plots/Figure_', as.character(figNum), '_', figMission,
                       figTitle, '_Corrected.png', sep = '')
      png(filename = pngFile, width = 2000, height = 1500, res = 300)
    } else {
      pngFile <- paste('Plots/Figure_', as.character(figNum), '_', figMission,
                       figTitle, '_Uncorrected.png', sep = '')
      png(filename = pngFile, width = 2000, height = 1500, res = 300)
    }
  }
}


#
# load required packages
#
library(dplyr)
library(tidyr)
library(ggplot2)
library(xlsx)

mission  <- 'HUD2014030'
fig_out  <- 'T'    # T: plots as png files, F: plots to Plots pane in RStudio

col_blue <- '#0066CC'
col_red  <- '#CC3300'
fig_num  <- 0

corrected <- 0

#
# read in data
#
wd <- getwd()
print(wd)
input_file <- file.path(wd, 'Input', 'HUD2014030_Oxygen_Rpt.csv')
df_data_raw <- read.csv(input_file)

df_data_raw <- df_data_raw %>%
    dplyr::mutate(., winklervg = rowMeans(cbind(.$Oxy_W_Rep1, .$Oxy_W_Rep2), na.rm = TRUE)) %>%
    dplyr::arrange(Event, SAMPLE_ID)

#
# filter outliers between Winkler replicates
#
Oxy_W_diff <- df_data_raw$Oxy_W_Rep1 - df_data_raw$Oxy_W_Rep2

fig_num <- fig_num + 1
title <- '_Boxplot_of_Winkler_Replicate_Differences'
is_fig_a_png(fig_out, fig_num, mission, title, corrected)

Oxy_W_box <- boxplot(Oxy_W_diff, col = col_blue,
                     main = paste('Figure ', as.character(fig_num), ': ', mission,
                            ' Boxplot of Winkler Replicate Differences', sep = ''),
                     ylab = 'Oxygen Difference (ml/l)')

if( fig_out ) { dev.off() }

Oxy_W_outliers <- Oxy_W_diff %in% Oxy_W_box$out

#
# plot the outliers in red
#
fig_num <- fig_num + 1
title <- '_Outliers_Outside_1.5IQR'
is_fig_a_png(fig_out, fig_num, mission, title, corrected)

plot(Oxy_W_diff, col = col_blue, pch = 20,
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  ' Outliers Outside 1.5*IQR', sep = ''),
     xlab = "Ordered by Event and Increasing Sample ID",
     ylab = "Winkler Rep1 - Rep 2 (ml/l)" )

outliers_index = which(Oxy_W_outliers == TRUE)
new_Oxy_W_diff <- matrix(NA, length(Oxy_W_diff))
new_Oxy_W_diff[outliers_index] = Oxy_W_box$out
points(new_Oxy_W_diff, col = col_red, pch = 20)

if( fig_out ) { dev.off() }

#
# subset original data removing the Winker difference outliers
#
df_data_filtered_W <- df_data_raw %>% .[!Oxy_W_outliers ,]

#
# Original SOC values for the CTD oxygen sensors used during the mission 
# primary sensor (0133) and the secondary sensor (1588).
#
soc1 <- 0.3903
soc2 <- 0.5347

#
# Compare the primary sensor (0133) with the secondary sensor (1588)
#
# filter outliers between CTD sensors
#
Oxy_CTD_diff <- df_data_filtered_W$Oxy_CTD_P - df_data_filtered_W$Oxy_CTD_S

fig_num <- fig_num + 1
title <- '_Boxplot_of_CTD_Oxygen_Sensor_Differences'
is_fig_a_png(fig_out, fig_num, mission, title, corrected)

Oxy_CTD_box <- boxplot(Oxy_CTD_diff,
                       main = paste('Figure ', as.character(fig_num), ': ', mission,
                       ' Boxplot of CTD Oxygen Sensor Differences', '\n',
                       'Primary (0133) - Secondary (1588)', sep = ''),
                       ylab = "Oxygen Difference (ml/l)")

if( fig_out ) { dev.off() }

Oxy_CTD_outliers <- Oxy_CTD_diff %in% Oxy_CTD_box$out

#
# plot the outliers between CTD sensors in red
#
fig_num <- fig_num + 1
title <- '_Outliers_Outside_1.5IQR'
is_fig_a_png(fig_out, fig_num, mission, title, corrected)

plot(Oxy_CTD_diff, col = col_blue, pch = 20,
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  ' Outliers Outside 1.5*IQR', sep = ''),
     xlab = "Ordered by Event and Increasing Sample ID",
     ylab = "CTD Oxygen Primary - Secondary (ml/l)")
outliers_index = which(Oxy_CTD_outliers == TRUE)
new_Oxy_CTD_diff <- matrix(NA, length(Oxy_CTD_diff))
new_Oxy_CTD_diff[outliers_index] <- Oxy_CTD_box$out
points(new_Oxy_CTD_diff, col = col_red, pch = 20)

if( fig_out ) { dev.off() }

#
# subset the data removing the CTD difference outliers
#
df_data_filtered_WC <- df_data_filtered_W %>% .[!Oxy_CTD_outliers ,]

#
# filter outliers between the CTD primary and average of Winkler replicates
#
primary_winkler_diff <- df_data_filtered_WC$Oxy_CTD_P - df_data_filtered_WC$winklervg
primary_winkler_diff <- primary_winkler_diff - mean(primary_winkler_diff, na.rm = TRUE)

fig_num <- fig_num + 1
title <- '_Boxplot_of_CTD_Primary-Winkler_Oxygen_Differences'
is_fig_a_png(fig_out, fig_num, mission, title, corrected)

primary_winkler_box <- boxplot(primary_winkler_diff,
                               main = paste('Figure ', as.character(fig_num), ': ', mission, 
                               ' Boxplot of \n CTD Primary - Winkler Oxygen Differences', sep = ''),
                               ylab = "Oxygen Difference (ml/l)")

if( fig_out ) { dev.off() }

primary_winkler_outliers <- primary_winkler_diff %in% primary_winkler_box$out

#
# plot the outliers in red
#
fig_num <- fig_num + 1
title <- '_Identified_Outliers_Outside_of_1.5IQR'
is_fig_a_png(fig_out, fig_num, mission, title, corrected)

plot(primary_winkler_diff, col = col_blue, pch = 20,
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  ' Identified Outliers Outside of 1.5*IQR', sep = ''),
     xlab = "Ordered by Event and Increasing Sample ID",
     ylab = "CTD Primary - Winkler (ml/l)")

new_primary_winkler_diff <- matrix(NA, length(primary_winkler_diff))
outliers_index <- which(primary_winkler_outliers == TRUE)
new_primary_winkler_diff[outliers_index] <- primary_winkler_box$out
points(new_primary_winkler_diff, col = col_red, pch = 20)

if( fig_out ) { dev.off() }

df_data_filtered_WCO <- df_data_filtered_WC %>% .[!primary_winkler_outliers ,]

#
# filter outliers between CTD secondary and Winkler replicates (average)
#
secondary_winkler_diff <- df_data_filtered_WC$Oxy_CTD_S - df_data_filtered_WC$winklervg
secondary_winkler_diff <- secondary_winkler_diff - mean(secondary_winkler_diff, na.rm = TRUE)

fig_num <- fig_num + 1
title <- '_Boxplot_of_CTD_Secondary-Winkler_Oxygen_Differences'
is_fig_a_png(fig_out, fig_num, mission, title, corrected)

secondary_winkler_box <- boxplot(secondary_winkler_diff,
                                 main = paste('Figure ', as.character(fig_num), ': ', mission,
                                              ' Boxplot of \n CTD Secondary - Winkler Oxygen Differences', sep = ''),
                                 ylab = "Oxygen Difference (ml/l)")

if( fig_out ) { dev.off() }

secondary_winkler_outliers <- secondary_winkler_diff %in% secondary_winkler_box$out

#
# drop the rows that contained outliers
#
df_data_filtered_WCS <- df_data_filtered_WC %>% .[!secondary_winkler_outliers ,]

#
# calculate new SOC for Primary CTD Oxygen
#
soc1_ratio <- mean(df_data_filtered_WCO$winklervg / df_data_filtered_WCO$Oxy_CTD_P, na.rm = TRUE)
soc1_new <- soc1 * soc1_ratio

#
# Combine the outlier arrays into a data frame. Create a third Boolean column 
# where its values are set to TRUE if either outlier Boolean value for the first 
# two columns was TRUE.
#
dfOutliers <- tibble(primary_winkler_outliers, secondary_winkler_outliers)
dfOutliers <- dfOutliers %>% mutate(oxygen_outliers = if_any(.cols = contains('winkler'), I))
df_data_filtered_WCOS <- df_data_filtered_WC %>% .[!dfOutliers$oxygen_outliers,]

#
# apply correction to primary CTD oxygen
#
df_data_filtered__corr <- df_data_filtered_WCOS %>% 
                              dplyr::mutate(., Oxy_CTD_P_corr = soc1_ratio * Oxy_CTD_P)

#
# calculate new SOC for secondary CTD Oxygen
#
soc2_ratio <- mean(df_data_filtered_WCS$winklervg / df_data_filtered_WCS$Oxy_CTD_S, na.rm = TRUE)
soc2_new <- soc2*soc2_ratio

#
# apply correction to secondary CTD oxygen
#
df_data_filtered__corr <- df_data_filtered__corr %>%
                              dplyr::mutate(., Oxy_CTD_S_corr = soc2_ratio * Oxy_CTD_S)

#
# Compute differences between CTD oxygen sensors before and after correction
#
diff_Oxy_CTD = abs(df_data_filtered__corr$Oxy_CTD_P - df_data_filtered__corr$Oxy_CTD_S)
diff_Oxy_CTD_corr = abs(df_data_filtered__corr$Oxy_CTD_P_corr - df_data_filtered__corr$Oxy_CTD_S_corr)
print(paste0('Mean of CTD sensor differences before correction: ', mean(diff_Oxy_CTD)))
print(paste0('Mean of CTD sensor differences after correction: ', mean(diff_Oxy_CTD_corr)))

#
# plot the outliers in red ####
#
fig_num <- fig_num + 1
title <- '_Outliers_Outside_1.5IQR'
is_fig_a_png(fig_out, fig_num, mission, title, corrected)

plot(secondary_winkler_diff, col = col_blue, pch = 20,
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  ' Outliers Outside 1.5*IQR', sep = ''),
     xlab = "Ordered by Event and Increasing Sample ID",
     ylab = "CTD Secondary - Winkler (ml/l)")

new_secondary_winkler_diff <- matrix(NA, length(secondary_winkler_diff))
outliers_index <- which(secondary_winkler_outliers == TRUE)
new_secondary_winkler_diff[outliers_index] <- secondary_winkler_box$out
points(new_secondary_winkler_diff, col = col_red, pch = 20)

if( fig_out ) { dev.off() }

#
# Plot Primary CTD Oxygen (0133) data
#
# rearrange the data for plotting
#
df_tmp1 <- df_data_filtered__corr %>%
    dplyr::select(., Oxy_CTD_P, Oxy_CTD_P_corr, winklervg) %>%
    tidyr::gather(., key, ctd, Oxy_CTD_P, Oxy_CTD_P_corr) %>%
    dplyr::mutate(., key=ifelse(key == "Oxy_CTD_P", "Uncorrected", "Corrected"))

fig_num <- fig_num + 1
title <- '_Primary_Oxygen_LR'
is_fig_a_png(fig_out, fig_num, mission, title, corrected)

#
# initialize plot
#
p1 <- ggplot() +
      coord_cartesian() +
      scale_x_continuous(limits = c(2,8), name = "Winkler (ml/l)") +
      scale_y_continuous(limits = c(2,8), name = "CTD (ml/l)") +
      scale_shape_manual(values = c(20,20)) +
      scale_color_manual(values = c(col_red, col_blue))

#
# plot 1:1 line
#
p1 <- p1 + geom_line(mapping = aes(x = x, y = y),
                     data = data.frame(x = 2:8, y = 2:8),
                     stat = 'identity',
                     position = position_identity())

#
# plot data
#
p1 <- p1 + geom_point(data = df_tmp1,
                      mapping = aes(x = winklervg, y = ctd, shape = key, color = key),
                      stat = 'identity',
                      position = position_identity(),
                      size = 2)

#
# customize plot components
#
p1 <- p1 + theme_bw() + ggtitle(paste('Figure ', as.character(fig_num), ': ', mission,
                                      ' CTD Primary Oxygen Correction', sep = '')) +
           theme(axis.text = element_text(colour = 'black',
                                          angle = 0,
                                          hjust = 0.5,
                                          vjust = 0.5,
                                          size = 12),
                 axis.title = element_text(size = 14, face = 'bold'),
                 plot.title = element_text(colour = 'black', face = 'bold',
                                           hjust = 0.5, vjust = 1, size = 15),
                 legend.text = element_text(size = 12),
                 legend.position = c(.15, .85),
                 legend.direction = 'vertical',
                 legend.key.size = unit(0.6, 'cm'),
                 legend.key.width = unit(0.5, 'cm'),
                 legend.title = element_blank(),
                 panel.border = element_rect(colour = 'black',
                                             fill = NA,
                                             linewidth = 0.65),
                 plot.margin = unit(c(1.0,1.0,1.0,1.0), 'cm'))

suppressWarnings(print(p1))

if( fig_out ) { dev.off() }

#
# Plot Secondary CTD Oxygen (1588) data
#
# rearrange the data for plotting
#
df_tmp2 <- df_data_filtered__corr %>%
    dplyr::select(., Oxy_CTD_S, Oxy_CTD_S_corr, winklervg) %>%
    tidyr::gather(., key, ctd, Oxy_CTD_S, Oxy_CTD_S_corr) %>%
    dplyr::mutate(., key=ifelse(key == "Oxy_CTD_S", "Uncorrected", "Corrected"))

fig_num <- fig_num + 1
title <- '_Secondary_Oxygen_LR'
is_fig_a_png(fig_out, fig_num, mission, title, corrected)

#
# initialize plot
#
p2 <- ggplot() +
      coord_cartesian() +
      scale_x_continuous(limits = c(2,8), name = 'Winkler (ml/l)') +
      scale_y_continuous(limits = c(2,8), name = 'CTD (ml/l)') +
      scale_shape_manual(values = c(20,20)) +
      scale_color_manual(values = c(col_red, col_blue))

#
# plot 1:1 line
#
p2 <- p2 + 
      geom_line(
          mapping = aes(x = x, y = y),
          data = data.frame(x = 2:8, y = 2:8),
          stat = 'identity',
          position = position_identity()
      )  

#
# plot data
#
p2 <- p2 + geom_point(data = df_tmp2,
                      mapping = aes(x = winklervg,
                                    y = ctd,
                                    shape = key,
                                    color = key),
                      stat = 'identity',
                      position = position_identity(),
                      size = 2)

#
# customize the plot components
#
p2 <- p2 + theme_bw() + ggtitle(paste('Figure ', as.character(fig_num), ': ', mission,
                                      ' CTD Secondary Oxygen Correction', sep = '')) +
           theme(axis.text = element_text(colour = 'black',
                                          angle = 0,
                                          hjust = 0.5,
                                          vjust = 0.5,
                                          size = 12),
                 axis.title = element_text(size = 14, face = 'bold'),
                 plot.title = element_text(colour = 'black', face = 'bold',
                                           hjust = 0.5, vjust = 1, size = 15),
                 legend.text = element_text(size = 12),
                 legend.position = c(.15, .85),
                 legend.direction = 'vertical',
                 legend.key.size = unit(0.6, 'cm'),
                 legend.key.width = unit(0.5,'cm'),
                 legend.title = element_blank(),
                 panel.border = element_rect(colour = 'black',
                                             fill = NA,
                                             linewidth = .65),
                 plot.margin = unit(c(1.0,1.0,1.0,1.0), 'cm'))

suppressWarnings(print(p2))

if( fig_out ) { dev.off() }

#
# Plot the CTD Oxygen Differences between the two sensors before and after Correction
#
fig_num <- fig_num + 1
title <- '_Oxygen_Differences'
is_fig_a_png(fig_out, fig_num, mission, title, corrected)

#
# plot the CTD oxygen differences between the two sensors before and after Correction
#
odiff <- df_data_filtered__corr$Oxy_CTD_P - df_data_filtered__corr$Oxy_CTD_S

#
# Plot 11
#
plot(odiff, col = col_blue, pch = 20,
     main = paste('Figure ', as.character(fig_num), ': ', mission,
                  ' Calibrated CTD Oxygen Differences', sep = ''),
     xlab = 'Ordered by Event and Increasing Sample ID',
     ylab = 'Primary - Secondary (ml/l)',
     ylim = range(-0.5, 0.5),
     cex.main = 1.25,
     cex.lab = 1.0, cex.axis = 1.0)

mean_odiff <- mean(odiff, na.rm = TRUE)
abline(mean_odiff, 0, col = col_blue)
odiff_corr <- df_data_filtered__corr$Oxy_CTD_P_corr - df_data_filtered__corr$Oxy_CTD_S_corr
points(odiff_corr, col = col_red, pch = 20, ylim = range(-0.5, 0.5))
mean_odiff_corr <- mean(odiff_corr, na.rm = TRUE)
abline(mean_odiff_corr, 0, col = col_red)
mean_odiff
mean_odiff_corr

if( fig_out ) { dev.off() }

#
# Output the final corrected data into Excel spreadsheets for the two sensor groups
#
write.xlsx(df_data_filtered__corr, "oxyCorDf.xlsx", col.names = TRUE, row.names = FALSE)
