#Set working directory
setwd("C:/Users/Hp 14/Documents/expand_unity_exp_framework_data-main/data/processedtool/per_ppt/18749")
getwd()
dir()

#Load packages
library(dplyr) 
library(magrittr)
library(ggplot2)

#Load participant's hand and pen path file
hand_data <- read.csv("hand_pos.csv")
pen_data <- read.csv("pen_pos.csv")

#Filter data frame for only nocursor trials and exclude practice block
nocursor_hand <- hand_data %>%
  filter(type=="nocursor", block_num >4)
nocursor_pen <- pen_data %>%
  filter(type=="nocursor", block_num >4)

#Add new column (trial type) to add the four sub conditions
NoCursor_hand <- nocursor_hand %>%
  filter(block_num != 71)%>%
  mutate(trial_type = case_when(
    block_num %in% c(18,33) ~ "aligned hand",
    block_num %in% c(21,36) ~ "aligned pen",
    block_num %in% c(48,63) ~ "rotated hand",
    block_num %in% c(51,66) ~ "rotated pen",
    TRUE ~ "other"
  ))

NoCursor_pen <- nocursor_pen %>%
  filter(block_num != 71)%>%
  mutate(trial_type = case_when(
    block_num %in% c(18,33) ~ "aligned hand",
    block_num %in% c(21,36) ~ "aligned pen",
    block_num %in% c(48,63) ~ "rotated hand",
    block_num %in% c(51,66) ~ "rotated pen",
    TRUE ~ "other"
  ))

#Filter for only aligned no cursor reaches
aligned_nocursor_hand <- subset(NoCursor_hand, trial_type %in% c("aligned pen"))
aligned_nocursor_pen <- subset(NoCursor_pen, trial_type %in% c("aligned pen"))

#Create the plot for all aligned no cursor reaches
ggplot() +
  geom_point(data = aligned_nocursor_hand, aes(x = hand_pos_x, y = hand_pos_z, color = "Hand"), size = 3, alpha = 0.3) +
  geom_point(data = aligned_nocursor_pen, aes(x = pen_pos_x, y = pen_pos_z, color = "Pen"), size = 3, alpha = 0.3) +
  geom_point(data = aligned_nocursor_pen, aes(x = home_x, y = home_z, color = "Home"), size = 4, shape = 19) +
  labs(title = "Aligned Pen Cursor",
       x = "X Coordinate",
       y = "Z Coordinate") +
  scale_color_manual(values = c("Hand" = "blue", "Pen" = "red", "Home" = "purple", "Target Angle" = "green")) +
  theme_minimal()

