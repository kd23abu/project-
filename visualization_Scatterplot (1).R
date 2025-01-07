library(tidyverse)  # Importing packages

# Read the dataset
ds <- read.csv("C:\\Users\\adars\\OneDrive\\Documents\\BUDGET\\mumbai\\mumbai.csv")
ds <- ds[!(is.na(ds$Rating) | ds$Rating == "" | is.na(ds$Price) | ds$Price == ""), ]

# Convert Rating to numeric if it's a factor or character
ds$Rating <- as.numeric(ds$Rating)
ds$Price <- as.numeric(gsub("[^0-9\\.]", "", ds$Price))
ds <- ds[!is.na(ds$Price), ]

# Log transformation of Price
ds$LogPrice <- log(ds$Price)

# Group Ratings into bins
ds$RatingGroup <- cut(ds$Rating, 
                      breaks = c(2.5, 3.0, 3.5, 4.0, 4.5, 5.0),
                      labels = c("2.5-3.0", "3.0-3.5", "3.5-4.0", "4.0-4.5", "4.5-5.0"),
                      right = FALSE)

# Remove extreme outliers using quantiles
quantiles <- quantile(ds$LogPrice, probs = c(0.01, 0.99), na.rm = TRUE)
ds <- ds[ds$LogPrice >= quantiles[1] & ds$LogPrice <= quantiles[2], ]

# Save the plot as a PDF file
pdf(file = "scatter_visualization.pdf")

# Create a scatter plot
ggplot(ds, aes(x = RatingGroup, y = LogPrice, color = RatingGroup)) + 
  ggtitle("Scatter Plot of Price Across Rating Groups (Log Scale)") + 
  xlab("Rating Group") + 
  ylab("Log of Price") +
  geom_jitter(width = 0.2, alpha = 0.7) +  # Add jitter for better visualization
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  
  scale_y_continuous(trans = 'log10') +  # Maintain the log scale for prices
  guides(color = guide_legend(title = "Rating Group"))

dev.off()

