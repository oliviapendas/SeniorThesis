# Function to create barplot
create_barplot <- function(data, title_suffix) {
  # Create color palette for iGDGTs, with 9 breaks
  start_color <- "cyan3"
  end_color <- "deeppink2"
  GDGT_palette <- colorRampPalette(c(start_color, end_color))(9)
  
  # Reshape data into a matrix with Site Names as column names and iGDGT abundances as row names
  data <- arrange(data, pH)  # Sort by pH
  matrix <- data %>%
    select(Site, iGDGT.0, iGDGT.1, iGDGT.2, iGDGT.3, iGDGT.4, iGDGT.5, iGDGT.6, iGDGT.7, iGDGT.8) %>%
    ungroup() %>%
    column_to_rownames(var = "Site") %>%
    t()
  
  # Save the barplot PNG
  png(paste0("iGDGT_Profiles_by_pH_", title_suffix, ".png"), width = 4.5, height = 3, units = 'in', res = 300)
  par(mfrow = c(1, 1), mar = c(3, 4, 2, 8), mgp = c(3, 0.4, 0))
  
  # Create the stacked barplot
  barplot(matrix,
          col = GDGT_palette,
          border = "white",
          space = 0.08,
          las = 1,
          yaxt = "n",
          xaxt = "n",
          xlab = "")
  
  # Add in X-axis labels based on pH integer bins
  axis(1, at = c(0.6, 2.7, 3.8, 6, 7),
       las = 1,
       labels = c("3", "4", "5", "6", "7"),
       tck = -0.035,
       cex.axis = 1)
  
  axis(2, at = seq(0, 1, .5),
       las = 1,
       labels = TRUE,
       tck = -0.035,
       cex.axis = 1,
       line = 0)
  
  # Add minor Y ticks
  axis(2, at = seq(0, 1, .25),
       las = 1,
       labels = FALSE,
       tck = -0.02,
       cex.axis = 0.75,
       line = 0)
  
  # Add titles
  title(main = paste(title_suffix, "iGDGT Profiles"), line = 0.5, cex.main = 1.2, outer = FALSE, adj = 0.5)
  title(xlab = "pH", line = 1.4, cex.lab = 1.2, outer = FALSE)
  title(ylab = "Rel. Abundance", line = 1.8, cex.lab = 1.2, outer = FALSE)
  
  # Add legend for iGDGTs
  legend("topright", 
         xpd = TRUE,  # plots in outer margins
         cex = 1.1,
         inset = c(-0.55, -0.05),
         bty = "n",
         legend = c("iGDGT-0", "iGDGT-1", "iGDGT-2", "iGDGT-3", "iGDGT-4",
                    "iGDGT-5", "iGDGT-6", "iGDGT-7", "iGDGT-8"),
         col = rev(GDGT_palette),
         pt.cex = 1.5,
         pch = c(15, 15, 15, 15))
  
  dev.off()
}

# Create separate datasets for CL and IPL
CL_data <- subset(dat, Type == "CL")
IPL_data <- subset(dat, Type == "IPL")

# Create barplots for both datasets with corresponding titles
create_barplot(CL_data, "Core")
create_barplot(IPL_data, "IPL")

#### Add corresponding iGDGT vs. pH regressions for each iGDGT moiety ####
create_regression_plots <- function(data, title_suffix) {
  max_abundance <- max(sapply(data[, paste0("iGDGT.", 0:8)], max))
  png(paste0("iGDGT_vs_pH_regressions_by_moiety_", title_suffix, ".png"),
      width = 10, height = 4, units = 'in', res = 300)  # Adjust width and height as needed
  
  par(mfrow = c(2, 5), mar = c(3.5, 4, 2, 1), mgp = c(2.5, 0.5, 0))
  
  # Loop through each iGDGT lipid column and create a scatterplot
  for (i in 0:8) {
    col_name <- paste0("iGDGT.", i)
    plot(data$pH, data[[col_name]],
         main = paste("iGDGT", i),
         xlab = "",
         ylab = "Relative Abundance",
         las = 1,
         pch = 16,
         col = "cyan3",
         cex = 2,
         ylim = c(0, max_abundance))
    
    # Add an X-axis title
    title(xlab = "pH", line = 1.5)
    
    # Fit linear regression model
    lm_model <- lm(data[[col_name]] ~ data$pH)
    
    # Get R-squared value and p-value
    r_squared <- summary(lm_model)$r.squared
    p_value <- summary(lm_model)$coefficients[2, 4]
    
    # Add regression line with adjusted properties based on p-value
    abline(lm_model, col = ifelse(p_value < 0.05, "black", "grey70"), lwd = 2, lty = ifelse(p_value < 0.05, 1, 2))
    
    # Add R-squared and p-value to plot
    legend("topleft",
           legend = c(paste("R² =", round(r_squared, 2)),
                      paste("p =", signif(p_value, digits = 3))),
           col = "black",
           bty = "n",
           cex = 1.2)
  }
  
  dev.off()  # Save the plot
}

# Create regression plots for both CL and IPL datasets with corresponding titles
create_regression_plots(CL_data, "CL")
create_regression_plots(IPL_data, "IPL")