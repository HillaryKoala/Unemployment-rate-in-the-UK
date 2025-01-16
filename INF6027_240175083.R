# install.packages("tidyverse")
# install.packages("MASS")
# install.packages("readxl")
# install.packages("openxlsx")
# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("scales")
# install.packages("patchwork")
# install.packages("ggcorrplot")
# install.packages("gridExtra")

library(tidyverse)   # A collection of R packages (e.g., ggplot2, dplyr, tidyr) for data manipulation, visualization, and analysis.
library(MASS)        # Contains functions and datasets for statistical methods, including support for linear and generalized linear models.
library(readxl)      # Allows reading data from Excel files (.xls and .xlsx) into R.
library(openxlsx)    # Enables reading, writing, and editing Excel files without requiring external dependencies like Java.
library(ggplot2)     # A core part of tidyverse; used for creating static, dynamic, and interactive visualizations based on the grammar of graphics.
library(tidyr)       # Helps tidy messy data by reshaping and reorganizing it into a clean and consistent format.
library(scales)      # Provides tools for scaling axes in visualizations, such as custom tick marks, formatting, and transformations.
library(patchwork)   # Simplifies combining multiple ggplot2 plots into a single cohesive layout.
library(ggcorrplot)  # Used for visualizing correlation matrices with customizable and aesthetically pleasing correlation plots.
library(gridExtra)   # Facilitates arranging multiple plots or tables into a grid layout.

# Load UNEMPLOYMENT Data
# Download from https://www.ons.gov.uk/ (https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/UNEMPLOYMENT/datasets/Unemploymentbypreviousindustrialsectorunem03)
# Rename the file as 'UNEM.xls'
file_path <- "/Users/hillaryzhang/Desktop/UNEM.xls" # Change the route
RAWUNRATE <- read_excel(file_path, sheet = "People_rates")  # Read the sheet we need
colnames(RAWUNRATE)[1] <- "Quarter" # Rename the column name
colnames(RAWUNRATE)[2] <- "UNRATE" # Rename the column name

RAWFUNRATE <- read_excel(file_path, sheet = "Women_rates")  # Read the sheet we need
colnames(RAWFUNRATE)[1] <- "Quarter" # Rename the column name
colnames(RAWFUNRATE)[2] <- "FUNRATE" # Rename the column name

RAWMUNRATE <- read_excel(file_path, sheet = "Men_rates")  # Read the sheet we need
colnames(RAWMUNRATE)[1] <- "Quarter" # Rename the column name
colnames(RAWMUNRATE)[2] <- "MUNRATE" # Rename the column name

# Data from January to March 2019 onward has been reweighed, causing a step change discontinuity.
# Therefore, we only use the data before reweighed.

UNRATE <- RAWUNRATE[9:180, c(1, 2)] # Get Quarter Energy Consumption from 1995 to 2019
str(UNRATE$Quarter) # Check the type of data -> chr
str(UNRATE$UNRATE) # Check the type of data -> chr
UNRATE$Quarter <- gsub("\\[r\\]", "", UNRATE$Quarter) # Delete note
UNRATE$Quarter <- gsub("20194", "2019", UNRATE$Quarter) # Delete note
UNRATE$Quarter  <- trimws(UNRATE$Quarter )  # Delete space
UNRATE$UNRATE <- as.numeric(UNRATE$UNRATE) # Change the data type from chr to num

FUNRATE <- RAWFUNRATE[9:180, c(1, 2)] # Get Quarter Energy Consumption from 1995 to 2019
str(FUNRATE$Quarter) # Check the type of data -> chr
str(FUNRATE$FUNRATE) # Check the type of data -> chr
FUNRATE$Quarter <- gsub("\\[r\\]", "", FUNRATE$Quarter) # Delete note
FUNRATE$Quarter <- gsub("20192", "2019", FUNRATE$Quarter) # Delete note
FUNRATE$Quarter  <- trimws(FUNRATE$Quarter )  # Delete space
FUNRATE$FUNRATE <- as.numeric(FUNRATE$FUNRATE) # Change the data type from chr to num

MUNRATE <- RAWMUNRATE[9:180, c(1, 2)] # Get Quarter Energy Consumption from 1995 to 2019
str(MUNRATE$Quarter) # Check the type of data -> chr
str(MUNRATE$MUNRATE) # Check the type of data -> chr
MUNRATE$Quarter <- gsub("\\[r\\]", "", MUNRATE$Quarter) # Delete note
MUNRATE$Quarter <- gsub("20192", "2019", MUNRATE$Quarter) # Delete note
MUNRATE$Quarter  <- trimws(MUNRATE$Quarter )  # Delete space
MUNRATE$MUNRATE <- as.numeric(MUNRATE$MUNRATE) # Change the data type from chr to num

COMUNRATE1 <- merge(UNRATE, FUNRATE, by = "Quarter",) # Combine three datasets
COMUNRATE <- merge(COMUNRATE1, MUNRATE, by = "Quarter",)

COMUNRATE$GENDERGAP <- (COMUNRATE$FUNRATE - COMUNRATE$MUNRATE) # Calculate the unemployment gender gap

# Complete primary dataset

# Load GDP Data
# Download from https://www.ons.gov.uk/ (https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/uksecondestimateofgdpdatatables)
# Rename the file as 'GDP.xlsx'
file_path <- "/Users/hillaryzhang/Desktop/GDP.xlsx" # Change the route
RAWGDP <- read.xlsx(file_path, sheet = "A2 AGGREGATES") # Read the sheet we need
GDP <- RAWGDP[87:365, c(1, 4)] # Get Annual Energy Consumption from 1970 to 2023
colnames(GDP)[1] <- "Quarter" # Rename the column name
colnames(GDP)[2] <- "GDP" # Rename the column name
str(GDP$Quarter) # Check the type of data -> chr
str(GDP$GDP) # Check the type of data -> chr
GDP$GDP <- as.numeric(GDP$GDP) # Change the data type from chr to num
GDP$LOGGDP <- log(GDP$GDP) # Calculate the log value
GDP$GROWTHRATE <- c(NA, (GDP$GDP[-1] - GDP$GDP[-nrow(GDP)]) / GDP$GDP[-nrow(GDP)] * 100) # Calculate GDP growth rate


# Load Energy Data
# Download from https://www.data.gov.uk/ (https://www.data.gov.uk/dataset/26afb14b-be9a-4722-916e-10655d0edc38/energy_consumption_in_the_uk)
# Rename the file as 'ENERGY.xlsx'
file_path <- "/Users/hillaryzhang/Desktop/ENERGY.xlsx" # Change the route
RAWENERGY <- read_excel(file_path, sheet = "Table C1")  # Read the sheet we need
ENERGY <- RAWENERGY[6:59, c(1, 57)] # Get Annual Energy Consumption from 1970 to 2023
colnames(ENERGY)[1] <- "Year" # Rename the column name
colnames(ENERGY)[2] <- "ENERGY" # Rename the column name
str(ENERGY$Year) # Check the type of data -> chr
str(ENERGY$ENERGY) # Check the type of data -> chr
ENERGY$Year <- as.numeric(ENERGY$Year) # Change the data type from chr to num
ENERGY$ENERGY <- as.numeric(ENERGY$ENERGY) # Change the data type from chr to num

# Combine all dataset
# Combine unemployment dataset and GDP dataset
COMUNRATE$Quarter <- gsub("Jan-Mar (\\d{4})", "\\1Q1", COMUNRATE$Quarter) # unify the format
COMUNRATE$Quarter <- gsub("Apr-Jun (\\d{4})", "\\1Q2", COMUNRATE$Quarter) # unify the format
COMUNRATE$Quarter <- gsub("Jul-Sep (\\d{4})", "\\1Q3", COMUNRATE$Quarter) # unify the format
COMUNRATE$Quarter <- gsub("Oct-Dec (\\d{4})", "\\1Q4", COMUNRATE$Quarter) # unify the format

COMUNRATE <- COMUNRATE[order(COMUNRATE$Quarter), ] # Sort by order
COMUNRATE <- COMUNRATE[1:(nrow(COMUNRATE)-53), ] # Delete redundant data

GDP$Quarter <- gsub("(\\d{4}) Q(\\d)", "\\1Q\\2", GDP$Quarter) # unify the format


COMBINEDATA1 <- merge(COMUNRATE, GDP, by = "Quarter")

# Combine unemployment dataset and Energy dataset

COMBINEDATA1$Year <- substr(COMBINEDATA1$Quarter, 1, 4) # unify the format
str(COMBINEDATA1$Year) # Check the type of data -> chr
COMBINEDATA1$Year <- as.numeric(COMBINEDATA1$Year) # Change the data type from chr to num to unify the format
COMBINEDATA <- merge(COMBINEDATA1, ENERGY, by = "Year")
COMBINEDATA <- COMBINEDATA[order(COMBINEDATA$Quarter), ] # Sort by order

# Add Brexit as dummy variable (0:pre-Brexit, 1:post-Brexit)
# Since the referendum is held in June, we set a lag and consider 2017 as the dividing point.
COMBINEDATA$Brexit <- ifelse(COMBINEDATA$Year < 2017, 0, 1)

# The dataset is setup.
# Summary 1
summary(COMBINEDATA)

# checking any NaN in the dataset
which(is.na(COMBINEDATA)) 

# Plot the figures

# Figure 1: Line graph - Unemployment rate
COMBINEDATAF1 <- COMBINEDATA
# Change the format of the data
COMBINEDATAF1$Quarter <- as.Date(paste0(substr(COMBINEDATAF1$Quarter, 1, 4), "-", 
                          (as.integer(substr(COMBINEDATAF1$Quarter, 6, 6)) - 1) * 3 + 1, "-01"), "%Y-%m-%d")

plot1 <- ggplot(COMBINEDATAF1, aes(x = Quarter, y = UNRATE)) +
  geom_line(color = "blue", size = 1) +            # Line
  scale_x_date(
    breaks = seq(as.Date("1995-01-01"), as.Date("2024-01-01"), by = "3 years"), # 3 years per tag
    labels = date_format("%Y")                   # Set it as year
  ) +
  labs(x = "Year", y = "UNEMPLOYMENT RATE", title = "UNEMPLOYMENT RATE BY QUARTER") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),                   
    panel.grid.minor = element_blank(),                 
    panel.border = element_blank(),              
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
print(plot1)


# Figure 2:
COMBINEDATAF2 <- COMBINEDATA
# Change the format of the data
COMBINEDATAF2$Quarter <- factor(COMBINEDATAF2$Quarter, levels = COMBINEDATAF2$Quarter)


# Graph above: Female/Male UNEMPLOYMENT Rate
plot21 <- ggplot(COMBINEDATAF2, aes(x = Quarter)) +
  geom_bar(aes(y = MUNRATE, fill = "Male"), stat = "identity", position = "dodge", width = 0.6) +
  geom_bar(aes(y = FUNRATE, fill = "Female"), stat = "identity", position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "red")) + 
  labs(x = NULL, y = "UNRATE", fill = "Gender", title = "UNEMPLOYMENT RATE BY GENDER") +
  # set the year gap in x-axis as 3
  scale_x_discrete(breaks = unique(COMBINEDATAF2$Quarter)[seq(1, length(unique(COMBINEDATAF2$Quarter)), 12)], 
                   labels = unique(COMBINEDATAF2$Quarter)[seq(1, length(unique(COMBINEDATAF2$Quarter)), 12)]) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_line(color = "black")
  )

# Graph below: Gender Gap
plot22 <- ggplot(COMBINEDATAF2, aes(x = Quarter, y = GENDERGAP)) +
  geom_bar(stat = "identity", fill = "purple", width = 0.6) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) + 
  # set the year gap in x-axis as 3
  scale_x_discrete(breaks = unique(COMBINEDATAF2$Quarter)[seq(1, length(unique(COMBINEDATAF2$Quarter)), 12)], 
                   labels = unique(COMBINEDATAF2$Quarter)[seq(1, length(unique(COMBINEDATAF2$Quarter)), 12)]) +
  labs(x = "Year", y = "GENDER GAP") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black")
  )

plot2 <- plot21 / plot22

print(plot2)

# Figure 3: Line graph of GDP and Dot plot of Log(GDP)
COMBINEDATAF3 <- COMBINEDATA

COMBINEDATAF3$Quarter <- factor(COMBINEDATAF3$Quarter, levels = COMBINEDATAF3$Quarter)

sfactor <- max(COMBINEDATAF3$GDP)/max(COMBINEDATAF3$LOGGDP)
plot3 <- ggplot(COMBINEDATAF3, aes(x = Quarter)) +
  # GDP: Bar chart
  geom_bar(aes(y = GDP), stat = "identity", fill = "blue", width = 0.6) +
  # LOG(GDP)：Line chart
  geom_point(aes(y = 0.75*sfactor*LOGGDP), color = "red", size = 2) +
  scale_x_discrete(breaks = unique(COMBINEDATAF2$Quarter)[seq(1, length(unique(COMBINEDATAF2$Quarter)), 12)], 
                   labels = unique(COMBINEDATAF2$Quarter)[seq(1, length(unique(COMBINEDATAF2$Quarter)), 12)]) +
  scale_y_continuous(name = "GDP", sec.axis = sec_axis(trans= ~. /sfactor /0.75, name = "LOG(GDP)")) +
  # set the year gap in x-axis as 3
  scale_x_discrete(breaks = unique(COMBINEDATAF3$Quarter)[seq(1, length(unique(COMBINEDATAF3$Quarter)), 12)], 
                   labels = unique(COMBINEDATAF3$Quarter)[seq(1, length(unique(COMBINEDATAF3$Quarter)), 12)]) +
  labs(title = "GDP and Log(GDP)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.major = element_blank(),                 
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y.left = element_line(color = "black"),
    axis.line.y.right = element_line(color = "black"),
    legend.position = "none"                           
  )

print(plot3)


# Figure 4: : Line graph of Energy Consumption
COMBINEDATAF4 <- COMBINEDATA

COMBINEDATAF4 <- COMBINEDATAF4 %>% distinct(Year, .keep_all = TRUE)

# 
plot4 <- ggplot(COMBINEDATAF4, aes(x = Year, y = ENERGY)) +
  geom_line(color = "blue", size = 1) +            # Line
  labs(x = "Year", y = "ENERGY CONSUMPTION", title = "ENERGY CONSUMPTION BY YEAR") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),                   
    panel.grid.minor = element_blank(),                 
    panel.border = element_blank(),              
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
print(plot4)


# Figure 5: Dot plot of UNRATE and the other three variables
COMBINEDATAF5 <- COMBINEDATA

# Figure 5
plot51 <- ggplot(COMBINEDATAF5, aes(x = UNRATE, y = GENDERGAP)) +
  geom_point(color = "blue") +
  labs(title = "UNRATE vs GENDER GAP", x = "UNRATE", y = "GENDER GAP") +
  theme_minimal()
cor.test( COMBINEDATAF5$UNRATE, COMBINEDATAF5$GENDERGAP)

plot52 <- ggplot(COMBINEDATAF5, aes(x = UNRATE, y = LOGGDP)) +
  geom_point(color = "green") +
  labs(title = "UNRATE vs LOG(GDP)", x = "UNRATE", y = "LOG(GDP)") +
  theme_minimal()
cor.test( COMBINEDATAF5$UNRATE, COMBINEDATAF5$LOGGDP)


plot53 <- ggplot(COMBINEDATAF5, aes(x = UNRATE, y = ENERGY)) +
  geom_point(color = "red") +
  labs(title = "UNRATE vs ENERGY CONSUMPTION", x = "UNRATE", y = "ENERGY CONSUMPTION") +
  theme_minimal()
cor.test( COMBINEDATAF5$UNRATE, COMBINEDATAF5$ENERGY)

plot5 <- grid.arrange(plot51, plot52, plot53, ncol = 1)
print(plot5)


# Figure 6: Correlation among independent variables
COMBINEDATAF6 <- COMBINEDATA
colnames(COMBINEDATAF6)[6] <- "Gender Gap" # Rename the column name
colnames(COMBINEDATAF6)[8] <- "Log(GDP)" # Rename the column name
colnames(COMBINEDATAF6)[10] <- "Energy Consumption" # Rename the column name

cor_matrix <- cor(COMBINEDATAF6[c("Gender Gap", "Log(GDP)", "Energy Consumption")])


# plot the correlation graph
plot6 <- ggcorrplot(cor_matrix, method = "circle", 
                    type = "lower", 
                    lab = TRUE,
                    colors = c("red", "white", "blue"), 
                    title = "Correlation Matrix among Gender Gap, Log(GDP), Energy Consumption")

print(plot6)
# Log(GDP) shows a strong positive correlation with the Gender Gap, with a coefficient of 0.76.
# On the other hand, Log(GDP) has a strong negative correlation with Energy Consumption, at -0.82.


# Splitting the data
# When adding the dummy variable from 2017, using a 80%:20% train:test split results in insufficient data.
# Therefore, we adapt a 90/10 split.
COMBINEDATA_train <- COMBINEDATA[1:104,]
COMBINEDATA_test <- COMBINEDATA[105:116,]

# Summary of train dataset
summary(COMBINEDATA_train)

# Run some single linear regression
models1 <- lm(UNRATE ~ GENDERGAP, data = COMBINEDATA_train)
models2 <- lm(UNRATE ~ LOGGDP, data = COMBINEDATA_train)
models3 <- lm(UNRATE ~ ENERGY, data = COMBINEDATA_train)
models4 <- lm(UNRATE ~ GROWTHRATE, data = COMBINEDATA_train)
# Summary 2
summary(models1) # T value of coefficient is -10.95.
# Summary 3
summary(models2) # T value of coefficient is -4.393.
# Summary 4
summary(models3) # T value of coefficient is 0.481.
# Summary 5
summary(models4) # T value of coefficient is 0.754.

# Run some multivariate linear regression
modelm1 <- lm(UNRATE ~ GENDERGAP + LOGGDP + ENERGY + Brexit, data = COMBINEDATA_train)
modelm2 <- lm(UNRATE ~ GENDERGAP + LOGGDP + Brexit, data = COMBINEDATA_train)
# Summary 6
summary(modelm1) # Summary of the results
# Summary 7
summary(modelm2) # Delete Energy Consumption from independent variables
coef(modelm1)

# Figure 7: UNRATE = β0+ β1*GENDERGAP + β2*LOGGDP + β2*Brexit + ϵ
plot(modelm2, which=1)

# Predict
COMBINEDATA_multi_test <- COMBINEDATA_test 
mean <- mean(COMBINEDATA_multi_test$UNRATE)
COMBINEDATA_multi_test$squares  <- COMBINEDATA_multi_test$UNRATE - mean
COMBINEDATA_multi_test$predicted <- predict(modelm2, newdata=COMBINEDATA_multi_test)
COMBINEDATA_multi_test$residuals <- COMBINEDATA_multi_test$predicted - COMBINEDATA_multi_test$UNRATE
COMBINEDATA_multi_test

# Calculate R squared
rmse <- sqrt(mean((COMBINEDATA_multi_test$UNRATE - COMBINEDATA_multi_test$predicted)^2))
rmse
# Summary 8
summary(COMBINEDATA_test)
sst <- sum(COMBINEDATA_multi_test$squares^2)
sst
sse <- sum(COMBINEDATA_multi_test$residuals^2)
sse
rsquared <- sse/sst # R squared = 8.631288, which is not in the range of [0,1].
# It can be explained by the small data size.
rsquared