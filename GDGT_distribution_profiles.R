# Clear the console and environment
cat("\014") # Clears console
rm(list=ls())
graphics.off()

# Load relevant packages
library("readxl")
library("tidyverse")
library("gt")
library("dplyr")

# Load data
dat <- read_excel("El_Tatio_GDGTs_2025.xlsx")

# Inspect the data structure
str(dat)  # Check the structure and see if it has the expected columns
print(head(dat))  # View the first few rows of the dataset
colnames(dat)  # Check the column names to ensure all necessary data is present

# Function to create barplot
create_barplot <- function(data, title_suffix) {
  start_color <- "cyan3"
  end_color <- "deeppink2"
  GDGT_palette <- colorRampPalette(c(start_color, end_color))(9)
  
  # Arrange data by pH
  data <- arrange(data, pH)
  
  # Inspect the data before transformation
  print(colnames(data))  # Ensure the correct columns exist
  print(head(data))      # View the first few rows to check values
  
  # Extract pH values for later use (based on unique sites)
  pH_values <- unique(data$pH)
  
  # Reshape data into a matrix including Site and all iGDGT columns
  matrix <- data %>%
    select(Site, iGDGT.0, iGDGT.1, iGDGT.2, iGDGT.3, iGDGT.4, iGDGT.5, iGDGT.6, iGDGT.7, iGDGT.8) %>%
    group_by(Site) %>%  # Group by Site to aggregate duplicates if needed
    summarise(across(starts_with("iGDGT"), mean, na.rm = TRUE)) %>%  # Take the mean of duplicates
    ungroup() %>%
    column_to_rownames(var = "Site") %>%
    t()  # Transpose to get iGDGTs as rows and sites as columns
  # Normalize the values
  matrix <- apply(matrix, 2, function(x) x / sum(x, na.rm = TRUE))
  
  # Inspect the matrix dimensions
  print(dim(matrix))  # Check the dimensions of the reshaped matrix
  print(rownames(matrix))  # Verify row names (iGDGT types)
  print(head(matrix))  # Inspect the first few rows of the matrix
  
  # Save the barplot PNG
  png(paste0("iGDGT_Profiles_by_pH_", title_suffix, ".png"), width = 4.5, height = 3, units = 'in', res = 300)
  par(mfrow = c(1, 1), mar = c(3, 4, 2, 8), mgp = c(3, 0.4, 0))
  
  # Create the stacked barplot only if there are unique columns
  if (ncol(matrix) > 0) {
    barplot(matrix,
            col = GDGT_palette,
            border = "white",
            space = 0.08,
            las = 1,
            yaxt = "n",
            xaxt = "n",
            xlab = "")
    
    # Correctly adjust the X-axis labels
    num_sites <- ncol(matrix)
    axis(1, at = seq(0.5, num_sites + 0.5, length.out = num_sites),
         las = 1,
         labels = pH_values,  # Correct naming for each bar
         tck = -0.035,
         cex.axis = 1)
    axis(2, at = seq(0, 1, 0.5),
         las = 1,
         labels = TRUE,
         tck = -0.035,
         cex.axis = 1,
         line = 0)
    
    # Titles and legends
    title(main = paste(title_suffix, "iGDGT Profiles"), line = 0.5, cex.main = 1.2, outer = FALSE, adj = 0.5)
    title(xlab = "pH", line = 1.4, cex.lab = 1.2, outer = FALSE)
    title(ylab = "Rel. Abundance", line = 1.8, cex.lab = 1.2, outer = FALSE)
    
    # Create the legend
    legend("topright",
           xpd = TRUE,
           cex = 1.1,
           inset = c(-0.55, -0.05),
           bty = "n",
           legend = c("iGDGT-0", "iGDGT-1", "iGDGT-2", "iGDGT-3", "iGDGT-4",
                      "iGDGT-5", "iGDGT-6", "iGDGT-7", "iGDGT-8"),
           col = rev(GDGT_palette),
           pt.cex = 1.5,
           pch = c(15, 15, 15, 15))
  } else {
    warning("No data available for plotting.")
  }
  
  dev.off()  # Close the graphics device
}  

# Create separate datasets for CL and IPL
CL_data <- subset(dat, Type == "CL")
IPL_data <- subset(dat, Type == "IPL")

# Create barplots for both datasets with corresponding titles
create_barplot(CL_data, "Core")
create_barplot(IPL_data, "IPL")
