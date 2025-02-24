#### Ring Index vs. pH with points colored by temp ####
# Assume the 'dat' data frame already exists and contains the required columns.

# Define a function to map temp values to colors in the 50 to 100 °C range
color_map <- colorRampPalette(c("blue", "red"))(100 - 50 + 1)

# Add a new column "col_Temp" with the corresponding color hex codes
dat <- transform(dat,
                 col_Temp = color_map[cut(Temp, breaks = 50, labels = FALSE) + 1])

# Separating data into CL and IPL
CL_data <- subset(dat, Type == "CL")
IPL_data <- subset(dat, Type == "IPL")

# Linear regression for cl and IPL
pHvsCL <- lm(Ring_index ~ pH, data = CL_data)  # Regression for CL
pHvsIPL <- lm(Ring_index ~ pH, data = IPL_data)  # Regression for IPL

# Create the plot
png("RingIndex_vs_pH.png", width = 4, height = 3, units = 'in', res = 300)
par(mfrow = c(1, 1), mar = c(3, 3, 2, 6), mgp = c(3, 0.4, 0))  # Increased right margin

# Create the initial plot
plot(dat$pH, dat$Ring_index,
     las = 1,
     xlim = c(1, 10),
     ylim = c(0, 5),
     pch = NA,  # Do not plot default shape
     cex = 2,
     col = dat$col_Temp,  # Color code points by Temperature
     lwd = 1.5,
     xlab = "",
     ylab = "",
     xaxt = "n",
     yaxt = "n")

# Add CL points (squares)
points(CL_data$pH, CL_data$Ring_index,  
       pch = 15,   # Square shape for CL
       cex = 1,   
       col = CL_data$col_Temp)  # Use color mapping

# Add IPL points (circles)
points(IPL_data$pH, IPL_data$Ring_index,  
       pch = 1,    # Circle shape for IPL
       cex = 1,   
       col = IPL_data$col_Temp)  # Use color mapping

# Add the trend line for CL (dotted line)
abline(pHvsCL, col = "black", lwd = 2, lty = 3)  # Dotted line for CL

# Add the trend line for IPL (dashed line)
abline(pHvsIPL, col = "black", lwd = 2, lty = 2)  # Dashed line for IPL

# Extract coefficients and R² values for CL
coefficients_CL <- coef(pHvsCL)
slope_CL <- round(coefficients_CL[2], 1)
intercept_CL <- round(coefficients_CL[1], 1)
r_squared_CL <- round(summary(pHvsCL)$r.squared, 2)

# Extract coefficients and R² values for IPL
coefficients_IPL <- coef(pHvsIPL)
slope_IPL <- round(coefficients_IPL[2], 1)
intercept_IPL <- round(coefficients_IPL[1], 1)
r_squared_IPL <- round(summary(pHvsIPL)$r.squared, 2)

# Add regression parameters for CL
equation_text_CL <- paste("CL: RI =", slope_CL, "* pH +", intercept_CL)
r_squared_text_CL <- paste("R² =", r_squared_CL)


# Add regression parameters for IPL
equation_text_IPL <- paste("IPL: RI =", slope_IPL, "* pH +", intercept_IPL)
r_squared_text_IPL <- paste("R² =", r_squared_IPL)

# Create a combined legend for equations and lines
legend("bottomleft", inset = c(0, 0),
       legend = c(equation_text_CL, r_squared_text_CL,
                  equation_text_IPL, r_squared_text_IPL),
       col = "black",  # Set text color to black
       lwd = 2, 
       lty = c(3, NA, 2, NA),  # Use correct lty values for dotted and dashed
       bty = "n", cex = 0.5)  

# Add axes
axis(1, at = seq(1, 10, 1), las = 1, tck = -0.035, cex.axis = 1, line = 0)
axis(2, at = seq(0, 5, 1), las = 1, tck = -0.035, cex.axis = 1, line = 0)

# Titles
title(main = "Ring Index vs pH", line = 0.5, cex.main = 1.2)
title(xlab = "pH", line = 1.4, cex.lab = 1.3)
title(ylab = "Ring Index", line = 1.2, cex.lab = 1.3)

# Legend for Temperature
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
legend("right", inset = c(-0.4, 2), title = "T °C",
       legend = seq(50, 100, by = 10),
       pch = 15,  # Square markers for temperature
       col = color_map[seq(1, 100, 10)],  # Color mapping for temperatures
       pt.cex = 3,
       bty = "n",
       y.intersp = 0.8,    # Closer spacing between legend items
       title.adj = 0.8)   # Center the title relative to legend items

# Add Legend for CL and IPL shapes
legend("bottomright", inset = c(-0.65, -0.3), title = "Type",
       legend = c("CL (Squares)", "IPL (Circles)"),
       pch = c(15, 1),  # Square for CL, Circle for IPL
       col = c("black", "black"),  # Colors matching the trend lines
       pt.cex = 1,
       bty = "n",
       cex = 0.8,          # Smaller text size (adjust as needed)
       text.width = strwidth("CL (Squares)"), # To ensure both entries fit
       y.intersp = 0.7)    # Closer spacing between legend entries

dev.off()  # Save the plot  
