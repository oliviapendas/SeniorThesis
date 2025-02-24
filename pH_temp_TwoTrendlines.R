#### Ring Index vs. Temp with points colored by pH ####

# Clean the dataset
dat_clean <- na.omit(dat[, c("Temp", "Ring_index", "Type", "pH")])
cat("Number of rows in cleaned data:", nrow(dat_clean), "\n")

# Define a function to map pH values to colors in the range 1 to 10
color_map_pH <- colorRampPalette(c("darkorchid4", "thistle2"))(10 - 1 + 1)

# Add a new column "col_pH" with the corresponding color hex codes
dat <- transform(dat,
                 col_pH = color_map_pH[cut(dat$pH, breaks = 9, labels = FALSE) + 1])

# Make dataframe for scalebar
pH <- c(seq(1, 10, 1))
col_pH <- c(rep(NA, length(pH)))
col_pH_dat <- as.data.frame(cbind(pH, col_pH))
col_pH_dat <- transform(col_pH_dat,
                        col_pH = color_map_pH[cut(pH, breaks = 9, labels = FALSE) + 1])

# Linear regression
TempvsRI <- lm(dat$Ring_index ~ dat$Temp)
summary(TempvsRI)

# Extract the coefficients and R² value
coefficients <- coef(TempvsRI)
slope <- round(coefficients[2], 2)
intercept <- round(coefficients[1], 2)
r_squared <- round(summary(TempvsRI)$r.squared, 2)

# Save the plot as a PNG file
png("RingIndex_vs_temp.png", width = 4, height = 3, units = 'in', res = 300)
par(mfrow = c(1, 1), mar = c(3, 3, 2, 6), mgp = c(3, 0.4, 0))

# Create the plot
plot(dat$Ring_index ~ dat$Temp,
     las = 1,
     xlim = c(50, 90),
     ylim = c(0, 5),
     pch = NA,  # Do not plot default shape
     cex = 2,
     col = dat$col_pH,  # Color code points by pH
     lwd = 1.5,
     xlab = "",
     ylab = "",
     xaxt = "n",
     yaxt = "n")

# Separate the data into CL and IPL
CL_data <- subset(dat, Type == "CL")
IPL_data <- subset(dat, Type == "IPL")

# Plot CL data points (squares)
points(CL_data$Temp, CL_data$Ring_index,  
       pch = 15,   # Square shape for CL
       cex = 1,   
       col = CL_data$col_pH)  # Use color mapping

# Plot IPL data points (circles)
points(IPL_data$Temp, IPL_data$Ring_index,  
       pch = 1,    # Circle shape for IPL
       cex = 1,   
       col = IPL_data$col_pH)  # Use color mapping

# Add regression line
abline(TempvsRI, lwd = 3, lty = 2, col = "grey70")

# Add regression parameters
equation_text <- paste("RI =", slope, "* T +", intercept)
r_squared_text <- paste("R² =", r_squared)
text(50, 0.6, pos = 4, labels = equation_text, col = "grey70", cex = 0.8)
text(50, 0.2, pos = 4, labels = r_squared_text, col = "grey70", cex = 0.8)

#### add 95% CI to linear regression ####
# 1. Create a continuous sequence of Temperature values for the predictions.
new_data <- data.frame(Temp = seq(min(dat$Temp, na.rm = TRUE), 
                                  max(dat$Temp, na.rm = TRUE), 
                                  length.out = 16))

# 2. Predict confidence intervals using the TempvsRI model.
ci_new <- predict(TempvsRI, newdata = new_data, interval = "confidence", level = 0.95)

# 3. Add CI polygon to the plot.
polygon(c(new_data$Temp, rev(new_data$Temp)),
        c(ci_new[, "lwr"], rev(ci_new[, "upr"])),
        col = rgb(col2rgb("grey")[1] / 255,
                  col2rgb("grey")[2] / 255,
                  col2rgb("grey")[3] / 255,
                  alpha = 0.3),
        border = NA)  # No border on the polygon

# Add axes
axis(1, at = seq(50, 90, 10), las = 1, tck = -0.035, cex.axis = 1, line = 0)
axis(2, at = seq(0, 5, 1), las = 1, tck = -0.035, cex.axis = 1, line = 0)

# Titles
title(main = "Ring Index vs. Temp", line = 0.5, cex.main = 1.2)
title(xlab = "T °C", line = 1.4, cex.lab = 1.3)
title(ylab = "Ring Index", line = 1.2, cex.lab = 1.3)

# Legend for pH
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
legend("topright", inset=c(-0.3,-0.2), title = "pH", cex = 0.5, bty = "n",
       legend = seq(1, 10, by = 1),
       pch = 15,  # Square markers for pH
       col = color_map_pH,  # Color mapping for pH
       pt.cex = 2)

# Add Legend for CL and IPL shapes
legend("bottomright", inset = c(-0.65, -0.2), title = "Type",
       legend = c("CL (Squares)", "IPL (Circles)"),
       pch = c(15, 1),  # Square for CL, Circle for IPL
       col = c("black", "black"),  # Placeholder colors
       pt.cex = 1, 
       bty = "n",
       cex = 0.8,          # Smaller text size (adjust as needed)
       text.width = strwidth("CL (Squares)"), # To ensure both entries fit
       y.intersp = 0.7)    # Closer spacing between legend entries)

dev.off()  # Save the plot