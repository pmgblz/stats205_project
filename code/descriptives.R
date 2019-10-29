
# Packages 
library(dplyr)       # Data manipulation (0.8.0.1)
library(fBasics)     # Summary statistics (3042.89)
library(corrplot)    # Correlations (0.84)
library(psych)       # Correlation p-values (1.8.12)
library(grf)         # Generalized random forests (0.10.2)
library(rpart)       # Classification and regression trees, or CART (4.1-13)
library(rpart.plot)  # Plotting trees (3.0.6)
library(treeClust)   # Predicting leaf position for causal trees (1.1-7)
library(car)         # linear hypothesis testing for causal tree (3.0-2)
library(devtools)    # Install packages from github (2.0.1)
library(readr)       # Reading csv files (1.3.1)
library(tidyr)       # Database operations (0.8.3)
library(tibble)      # Modern alternative to data frames (2.1.1)
library(knitr)       # RMarkdown (1.21)
library(kableExtra)  # Prettier RMarkdown (1.0.1)
library(ggplot2)     # general plotting tool (3.1.0)
library(haven)       # read stata files (2.0.0)
library(aod)         # hypothesis testing (1.3.1)
library(evtree)      # evolutionary learning of globally optimal trees (1.0-7)
library(haven)
library(data.table)
library(caret)
library(magick)
library(xtable)
library(stargazer)

# Run path
if (dir.exists("/Users/paula/stats205_project")) {
  setwd("/Users/paula/stats205_project")
} else {
  setwd("~/jcasti15/public/UberSafety")
}

# Load data 
day <- read_csv("data/day.csv")
hour <- read_csv("data/hour.csv")


# Clean data 
df <- hour
setDT(df)
df[, season := as.factor(ifelse(season == 1, "Spring", 
                                  ifelse(season == 2, "Summer", 
                                         ifelse(season == 3, "Fall", 
                                                ifelse(season == 4, "Winter", NA)))))]

df[, weathersit := as.factor(ifelse(weathersit == 1, "Clear", 
                                ifelse(weathersit == 2, "Misty", 
                                       ifelse(weathersit == 3, "Rain", 
                                              ifelse(weathersit == 4, "Thunderstorm", NA)))))]

df <- df[, -c("instant")]


# dummify the data
dmy <- dummyVars(" ~ .", data = df)
df <- data.frame(predict(dmy, newdata = df))
df

# Drop rows containing missing values (doesn't actually do anything)
df <- na.omit(df)

# further cleaning 
setDT(df)
df[, yr := ifelse(df$yr == 0, 2011, 2012)]

### Descriptives ### 
# Make a data.frame containing summary statistics of interest
summ_stats <- fBasics::basicStats(df[, -c("dteday")])
summ_stats <- as.data.frame(t(summ_stats))

# Rename some of the columns for convenience
summ_stats <- summ_stats[c("Mean", "Stdev", "Minimum", "1. Quartile", "Median",  "3. Quartile", "Maximum")]
colnames(summ_stats)[colnames(summ_stats) %in% c('1. Quartile', '3. Quartile')] <- c('Lower quartile', 'Upper quartile')

row.names(summ_stats) <- c("Fall", "Spring", "Summer", "Winter", 
                           "Year", "Month", "Hour",
                           "Holiday", "Day of Week", "Workday", "Clear Weather", "Misty Weather", 
                           "Rainy Weather", "Thunderstorm", "Temperature",
                           "Felt Temperature", "Humidity", "Windspeed", "Casual Users", 
                           "Registered Users", "All Users")



# save
stargazer(summ_stats,
          type = "latex", 
          summary=FALSE, rownames=TRUE,
          digits = 2) -> sumstats 

tabular_positions <- grep("tabular", sumstats)
sumstats <- sumstats[tabular_positions[1]:tabular_positions[2]]
write(sumstats,  file="output/tables/summary_stats.tex")

# Note: if the plot looks too cramped, try increasing fig.width and fig.height in the line above
df_plot <- df
colnames(df_plot) <- c("Date", "Fall", "Spring", "Summer", "Winter", 
                  "Year", "Month", "Hour",
                  "Holiday", "Day of Week", "Workday", "Clear Weather", "Misty Weather", 
                  "Rainy Weather", "Thunderstorm", "Temperature",
                  "Felt Temperature", "Humidity", "Windspeed", "Casual Users", 
                  "Registered Users", "All Users")

# correlation plot 
pairwise_pvalues <- psych::corr.test(df_plot, df_plot)$p
png(filename="output/plots/corrplot.png")
pairwise_pvalues <- psych::corr.test(df_plot, df_plot)$p
corrplot(cor(df_plot),
         type="upper",
         tl.col="black",
         order="hclust",
         tl.cex=0.6,
         addgrid.col = "black",
         p.mat=pairwise_pvalues,
         sig.level=0.05,
         number.font=10,
         insig="blank") 
dev.off()

# plot daily trends
day %>% 
  ggplot(aes(dteday, cnt)) + geom_point(size = 0.5) + 
  xlab("Day") + ylab("Total Number of Users")

ggsave("output/plots/users_by_day.png")






