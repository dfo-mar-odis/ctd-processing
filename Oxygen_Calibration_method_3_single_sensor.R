#
# check if a plot is going to a png file, if yes, create a png file
#
is_fig_a_png <- function(isPng, figNum, figMission, figTitle, corrected, group) {
  if( isPng ) {
    if (corrected) {
      if (!is.null(group)) {
        pngFile <- paste('Plots/Figure_', as.character(figNum), 
                         '_', figMission, figTitle, '_Group', group, 
                         '_Corrected.png', sep = '')
      } else {
        pngFile <- paste('Plots/Figure_', as.character(figNum), 
                         '_', figMission, figTitle, '_Corrected.png', sep = '')
      }
      png(filename = pngFile, width = 2000, height = 1500, res = 300)
    } else {
      if (!is.null(group)) {
        pngFile <- paste('Plots/Figure_', as.character(figNum), 
                         '_', figMission, figTitle, '_Group', group, 
                         '_Uncorrected.png', sep = '')
      } else {
        pngFile <- paste('Plots/Figure_', as.character(figNum), 
                         '_', figMission, figTitle, '_Uncorrected.png', sep = '')
      }
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

mission  <- 'BCD2024666'
fig_out  <- 'T'    # T: plots as png files, F: plots to Plots pane in RStudio

col_blue <- '#0066CC'
col_red  <- '#CC3300'
fig_num  <- 0

# Boolean indicating if the QAT data being used is uncorrected or corrected.
# corrected <- 0
corrected <- 1

# Check if the Plots folder exists; if it does not then create it.
if (dir.exists("Plots") == FALSE) {
  dir.create("Plots")
}

#
# read in data
#
wd <- getwd()
print(wd)

if (corrected) {
  input_file <- file.path(wd, 'Input', 'BCD2024666_Oxygen_Rpt_Corrected.csv')
} else {
  input_file <- file.path(wd, 'Input', 'BCD2024666_Oxygen_Rpt_Uncorrected.csv')
}

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
is_fig_a_png(fig_out, fig_num, mission, title, corrected, NULL)

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
is_fig_a_png(fig_out, fig_num, mission, title, corrected, NULL)

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

# Create empty variables that will hold be used to concatenate the group 
# results together.
df_data_filtered_corr <- c()
socs_old <- c()
socs_new <- c()
soc_ratios <- c()

for (group in 1:2) {

  #
  # Original SOC value for the CTD oxygen sensor
  #
  if (group == 1) {
    # Event 1 (sensor 3068)
    soc <- 0.48466
    df_data_filtered_WG <- df_data_filtered_W %>%
      filter(Event == 1)
  } else {
    # Events 2 to 7 (sensor 2524)
    soc <- 0.58842
    df_data_filtered_WG <- df_data_filtered_W %>%
      filter(Event > 1)
  }
  
  #
  # filter outliers between the CTD sensor and average of Winkler replicates
  #
  winkler_ratio <- df_data_filtered_WG$winklervg / df_data_filtered_WG$Oxy_CTD_P
  winkler_diff <- winkler_ratio - mean(winkler_ratio, na.rm = TRUE)
  
  fig_num <- fig_num + 1
  title <- paste0('_Boxplot_of_CTD_Winkler_Oxygen_Differences_Group', group)
  is_fig_a_png(fig_out, fig_num, mission, title, corrected, group)
  
  winkler_box <- boxplot(winkler_diff,
                         main = paste('Figure ', as.character(fig_num), ': ', mission, 
                         ' Boxplot of \n CTD - Winkler Oxygen Differences (Group', 
                         group, ')', sep = ''),
                         ylab = "Oxygen Difference (ml/l)")
  
  if( fig_out ) { dev.off() }
  
  winkler_outliers <- winkler_diff %in% winkler_box$out
  
  #
  # plot the outliers in red
  #
  fig_num <- fig_num + 1
  title <- paste0('_Identified_Outliers_Outside_of_1.5IQR_Group', group)
  is_fig_a_png(fig_out, fig_num, mission, title, corrected, group)
  
  plot(winkler_diff, col = col_blue, pch = 20,
       main = paste('Figure ', as.character(fig_num), ': ', mission,
                    ' Identified Outliers \n Outside of 1.5*IQR (Group', 
                    group, ')', sep = ''),
       xlab = "Ordered by Event and Increasing Sample ID",
       ylab = paste0("CTD - Winkler (ml/l)")
      )
  
  new_winkler_diff <- matrix(NA, length(winkler_diff))
  outliers_index <- which(winkler_outliers == TRUE)
  new_winkler_diff[outliers_index] <- winkler_box$out
  points(new_winkler_diff, col = col_red, pch = 20)
  
  if( fig_out ) { dev.off() }
  
  df_data_filtered_WGO <- df_data_filtered_WG %>% .[!winkler_outliers ,]
  
  #
  # calculate the new SOC values for CTD Oxygen sensors
  #
  soc_ratio <- mean(df_data_filtered_WGO$winklervg / df_data_filtered_WGO$Oxy_CTD_P, na.rm = TRUE)
  soc_new <- soc * soc_ratio

  #
  # apply correction to CTD oxygen sensor
  #
  df_data_filtered__corr <- df_data_filtered_WGO %>% 
      dplyr::mutate(., Oxy_CTD_P_corr = soc_ratio * Oxy_CTD_P)

  #
  # Plot CTD Oxygen data
  #
  # rearrange the data for plotting
  #
  df_tmp1 <- df_data_filtered__corr %>%
    dplyr::select(., Oxy_CTD_P, Oxy_CTD_P_corr, winklervg) %>%
    tidyr::gather(., key, ctd, Oxy_CTD_P, Oxy_CTD_P_corr) %>%
    dplyr::mutate(., key=ifelse(key == "Oxy_CTD_P", "Uncorrected", "Corrected"))
  
  fig_num <- fig_num + 1
  title <- paste0('_Oxygen_LR_Group', group)
  is_fig_a_png(fig_out, fig_num, mission, title, corrected, group)
  
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
                                        ' CTD Oxygen Correction \n (Group', 
                                        group, ')', sep = '')) +
    theme(axis.text = element_text(colour = 'black',
                                   angle = 0,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   size = 12),
          axis.title = element_text(size = 14, face = 'bold'),
          plot.title = element_text(colour = 'black', face = 'bold',
                                    hjust = 0.5, vjust = 1, size = 15),
          legend.text = element_text(size = 12),
          legend.position.inside = c(.15, .85),
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

  df_data_filtered_corr <- rbind(df_data_filtered_corr, df_data_filtered__corr)
  socs_old <- c(socs_old, soc)
  socs_new <- c(socs_new, soc_new)
  soc_ratios <- c(soc_ratios, soc_ratio)
}

#
# Output the final corrected data into Excel spreadsheets for the two sensor groups
#
write.xlsx(df_data_filtered_corr, "oxyCorDf.xlsx", col.names = TRUE, row.names = FALSE)
