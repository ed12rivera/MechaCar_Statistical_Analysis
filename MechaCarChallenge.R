# Import dependencies
library(dplyr)
library(tidyverse)

# Linear regression to predict mpg
# Read the csv file
car_data <- read.csv("Data/MechaCar_mpg.csv")

# Generate multiple linear regression model
lm(mpg~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=car_data)

summary(lm(mpg~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=car_data))

# Summary statistics on suspension coils
suspension_data <- read.csv("Data/Suspension_Coil.csv")

# Create summary DataFrames
total_summary <- suspension_data %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))
lot_summary <- suspension_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI) , Variance = var(PSI) , SD = sd(PSI),.groups = 'keep' )

# T-tests on suspension coils
t.test(suspension_data$PSI, mu=1500)

t.test(subset(suspension_data,Manufacturing_Lot=="Lot1")$PSI, mu = 1500)
t.test(subset(suspension_data,Manufacturing_Lot=="Lot2")$PSI, mu = 1500)
t.test(subset(suspension_data,Manufacturing_Lot=="Lot3")$PSI, mu = 1500)
