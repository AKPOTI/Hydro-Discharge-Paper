# Load required libraries
library(ggplot2)
library(dplyr)
library(hydroGOF)

# Set the directory where CSV files are located
input_dir <- "C:/00MyWorks/WAPlus/Paper2_VegHydro_Validation/data/VegHydro_Outputs/"

# Set the directory where the output plots will be saved
output_dir <- "C:/00MyWorks/WAPlus/Paper2_VegHydro_Validation/Results/VegET_outputs/"

# Get a list of all .csv files in the directory
csv_files <- list.files(path = input_dir, pattern = "zonal_.*\\.csv$")

# Loop through all .csv files
for (csv_file in csv_files) {
  # Extract the basin name from the .csv file name
  basin_name <- gsub("zonal_", "", gsub(".csv", "", csv_file))
  
  # Read the data from the .csv file
  data <- read.csv(paste0(input_dir, csv_file))
  
  # Compute correlation coefficients (R-squared), KGE, NSE, and RMSE
  ssebop_r2 <- rPearson(data$et_veget, data$et_ssebop)^2
  modis_r2 <- rPearson(data$et_veget, data$et_modis)^2
  
  # Compute KGE, NSE, and RMSE
  ssebop_KGE <- KGE(data$et_veget, data$et_ssebop)
  ssebop_NSE <- NSE(data$et_veget, data$et_ssebop)
  ssebop_RMSE <- rmse(data$et_veget, data$et_ssebop)
  
  modis_KGE <- KGE(data$et_veget, data$et_modis)
  modis_NSE <- NSE(data$et_veget, data$et_modis)
  modis_RMSE <- rmse(data$et_veget, data$et_modis)
  
  # Create scatter plot
  scatter_plot <- ggplot(data, aes(y = et_veget)) +
    geom_point(aes(x = et_ssebop, color = "SSEBop")) +
    geom_point(aes(x = et_modis, color = "MODIS16")) +
    geom_smooth(aes(x = et_ssebop), method = "lm", se = FALSE, linetype = "solid", color = "blue") +
    geom_smooth(aes(x = et_modis), method = "lm", se = FALSE, linetype = "solid", color = "red") +
    labs(y = "ETa - VegHydro [mm] ", x = "ETa - MODIS16, SSEBop [mm]") +
    theme_bw() +
    scale_color_manual(values = c("MODIS16" = "red", "SSEBop" = "blue")) +
    ggtitle(paste0(basin_name)) +
    
    annotate("text", x = 0, y = 152, hjust = 0, vjust = 1, 
             label = paste("MODIS16 vs VegHydro \nR² =", round(modis_r2, 2), 
                           "\nKGE =", round(modis_KGE, 2), 
                           "\nNSE =", round(modis_NSE, 2), 
                           # ...[previous code here]...
                           
                           "\nRMSE =", round(modis_RMSE, 2)), 
             size = 3, col = "red") +
    annotate("text", x = 25, y = 152, hjust = 0, vjust = 1, 
             label = paste("SSEBop vs VegHydro \nR² =", round(ssebop_r2, 2), 
                           "\nKGE =", round(ssebop_KGE, 2), 
                           "\nNSE =", round(ssebop_NSE, 2), 
                           "\nRMSE =", round(ssebop_RMSE, 2)), 
             size = 3, col = "blue") +
    
    
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 10, face = "bold"),
          legend.background = element_rect(fill = "grey80", size = 0.2, linetype = "blank", colour = "black"),
          legend.position=c(0,0.97),
          legend.margin=margin(t=-15),
          legend.justification = "left",
          legend.direction = "horizontal",
          legend.spacing.x = unit(4, 'mm'),
          axis.text.x = element_text(angle = 0, hjust = 1),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  
  # Save the scatter plot as a high-resolution PDF
  ggsave(filename = paste0(output_dir, basin_name, "_scatterplot_et.pdf"), 
         plot = scatter_plot, 
         device = "pdf", 
         width = 8, 
         height = 6, 
         units = "in", 
         dpi = 300)
  
  # Print the scatter plot
  print(scatter_plot)
}

