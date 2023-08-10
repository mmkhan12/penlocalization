#Load participant's pen path file
hand_data <- read.csv("pen_pos.csv")
#Filter data frame for only nocursor trials and exclude practice block
nocursor <- hand_data %>%
  filter(type=="nocursor", block_num >4)
#Add new column (trial type) to add the four sub conditions
NoCursor <- nocursor %>%
  filter(block_num != 71)%>%
  mutate(trial_type = case_when(
    block_num %in% c(18,33) ~ "aligned hand",
    block_num %in% c(21,36) ~ "aligned pen",
    block_num %in% c(48,63) ~ "rotated hand",
    block_num %in% c(51,66) ~ "rotated pen",
    TRUE ~ "other"
  ))

#Filter for only aligned no cursor reaches
aligned_nocursor <- subset(NoCursor, trial_type %in% c("aligned hand", "aligned pen"))

#Create the plot for all aligned no cursor reaches
ggplot(aligned_nocursor) +
  geom_point(aes(x = hand_pos_x, y = hand_pos_z, color = "Hand"), size = 3, alpha = 0.3) +
  geom_point(aes(x = pen_pos_x, y = pen_pos_z, color = "Pen"), size = 3, alpha = 0.3) +
  geom_point(aes(x = home_x, y = home_z, color = "Home"), size = 4, shape = 19) +
  labs(title = "Aligned Cursor",
       x = "X Coordinate",
       y = "Z Coordinate") +
  scale_color_manual(values = c("Hand" = "blue", "Pen" = "red", "Home" = "purple", "Target Angle" = "green")) +
  theme_minimal()

