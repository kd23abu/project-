# Import essential libraries
library(tidyverse)
library(tidyr)

# Read the dataset
mumbai<-read.csv("C:\\Users\\adars\\OneDrive\\Documents\\BUDGET\\mumbai\\mumbai.csv", header = TRUE, na.strings = c(""))

# Transform the data
mum <-spread(mumbai, Rating, Price)

# Prepare data for chi-square test
mum_chi_tst <- sapply(mum, FUN = table)
mum_chi_tst <- as.numeric(unlist(mum_chi_tst))
print(chisq.test(mum_chi_tst, simulate.p.value = TRUE, B = 10000))

# Visualization
source("~/BUDGET/mumbai/visualization.R")
png(file = "visualization1.png")
ggplot(ds, aes(x = RatingGroup, y = LogPrice, fill = RatingGroup)) + 
  ggtitle("Distribution of Price Across Rating Groups (Log Scale)") + 
  xlab("Rating Group") + 
  ylab("Log of Price") +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  
  scale_y_continuous(trans = 'log10')
dev.off()

