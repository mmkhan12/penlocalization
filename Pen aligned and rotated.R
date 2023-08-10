
#Set working directory
setwd("C:/Users/Hp 14/Documents/expand_unity_exp_framework_data-main/data/processedtool/omnibus")
getwd()
dir()

#Load packages
library(data.table)
library(tidyverse)
library(collections)
library(ggbeeswarm)
library(ggplot2)
library(reshape2)
library(readxl)
library(ggpubr)
library(dplyr) 
library(magrittr)

#Load omnibus trial_results file
data_path <- "omnibus_raw.csv"

all_data <- fread(data_path)

#Filter for only aligned and rotated reaches and exclude last 4 blocks(hand trials)
reaches <- all_data %>%
  filter(type == 'aligned'|type =='rotated')%>%
  filter(block_num <= 59)

ggplot(reaches, aes(x=trial_num, y=all_diff))+
  geom_point()+
  ylim(-70,70)

#Find mean
reaches_mean <- reaches %>%
  group_by(trial_num,ppid, type)%>%
  summarise(mean = mean(all_diff))


#######Pen path
#Calculate pen endpoint angle
data_pen <- "omnibus_pen.csv"

pen_data <- fread(data_pen)

pen_data <- read.csv("pen_pos.csv")
reaches <- pen_data %>%
  filter(type == 'aligned'|type =='rotated')%>%
  filter(block_num <= 67)

reaches <- reaches %>%
  group_by(ppid,trial_num) %>%
  slice_tail(n=1) %>%
  mutate(endpoint_angle=atan2(pen_pos_z, pen_pos_x) * 180/pi)
reaches$final_diff <- reaches$endpoint_angle - reaches$target_angle 

#Calculate final out angle
dx <- reaches$pen_pos_x - reaches$home_x
dz <- reaches$pen_pos_z - reaches$home_z
# Calculate the angle in radians using atan2 function
angle_final_radians <- atan2(dz, dx)
# Convert the angle from radians to degrees
reaches$angle_final_degrees <- angle_final_radians * 180 / pi



# Function to calculate the distance between two points
distance <- function(x1, z1, x2, z2) {
  return(sqrt((x2 - x1)^2 + (z2 - z1)^2))
}

# Group the data by trial number
grouped_reaches <- pen_data %>%
  group_by(trial_num) %>%
  mutate(dist_to_path = distance(home_x, home_z, pen_pos_x, pen_pos_z))

# Filter for rows where the distance to path is approximately 3cm
filtered_reaches <- grouped_reaches %>%
  filter(dist_to_path >= 0.027 & dist_to_path <= 0.0305)

# Get the first row for each trial that meets the condition
result_data <- filtered_reaches %>%
  group_by(trial_num) %>%
  slice_min(order_by = abs(dist_to_path - 0.03), n = 1)

dx <- result_data$pen_pos_x - result_data$home_x
dz <- result_data$pen_pos_z - result_data$home_z
# Calculate the angle in radians using atan2 function
angle_final_radians <- atan2(dz, dx)
# Convert the angle from radians to degrees
result_data$angle_3cm_degrees <- angle_final_radians * 180 / pi

#FIND ENDPOINT ANGLE

dx <- pen$pen_pos_x - pen$home_x
dz <- pen$pen_pos_z - pen$home_z
# Calculate the angle in radians using atan2 function
angle_final_radians <- atan2(dz, dx)
# Convert the angle from radians to degrees
pen$angle_final_degrees <- angle_final_radians * 180 / pi
pen <- pen %>%
  group_by(ppid,trial_num) %>%
  slice_tail(n=1) %>%
  mutate(endpoint_angle=atan2(pen_pos_z, pen_pos_x) * 180/pi)


result_data <- result_data %>%
  left_join(select(pen, angle_final_degrees))



#Save data in excel format
install.packages("writexl")
library(writexl)
write_xlsx(reaches,"C:\\Users\\Hp 14\\Documents\\file name.xlsx")

