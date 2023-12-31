#Set working directory
setwd("C:/Users/Hp 14/Documents/expand_unity_exp_framework_data-main/data/processedpen/omnibus")
getwd()
dir()

#Load omnibus trial_results file
data_hand <- "omnibus_raw.csv"
hand_data <- fread(data_hand)

#ppid from int to chr
hand_data$ppid <- as.character(hand_data$ppid)

#Part 1
#Filter last 3 blocks of aligned hand reaches
aligned_hand <- hand_data %>%
  filter(type=='aligned', block_num > 19)
aligned_diff <- aligned_hand$hand_3cm_out_angle - aligned_hand$target_angle
aligned_hand$aligned_diff <- aligned_hand$hand_3cm_out_angle - aligned_hand$target_angle
#select_ppid <- c("16926", "38624", "62761", "65728", "67376", "71112","70933", "85043", "90766", "91332", "94302", "79915", "16043", "39328", "12689",
              #   "56319","56620","57790","58030","59350","59470","60743","68230","71950","72040","77620")
aligned_hand <- aligned_hand %>%
  #filter(ppid %in% select_ppid)%>%
  group_by(trial_num)%>%
  summarise(aligned_mean=mean(aligned_diff),
            aligned_median=median(aligned_diff),
            sd_err = sd(aligned_diff, na.rm = TRUE),
            n=n(),.groups = 'drop') %>%
  mutate(lower_ci = aligned_median - qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)),
         upper_ci = aligned_median + qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)))

  
  aligned_hand$sequential_trials <- 1:(nrow(aligned_hand))
  
  
  # Plot the data continuously without gaps using the new sequential trial numbers
  ggplot(aligned_hand, aes(x = sequential_trials, y = aligned_mean)) +
    geom_point() 
    
   # Plot the data with x-axis labels every 10 units apart
aligned_hreach <- aligned_hand %>%
  ggplot(aes(x = sequential_trials, y = aligned_median)) +
  theme_classic()+
  theme(legend.position = "none")+
  labs(
    x = "Trial Number",
    y = "Median angular deviation (°)",
    #title="Aligned Hand Reaches"
  )

aligned_hreach <- aligned_hreach+
  geom_hline(
    yintercept = c(0, 30), linewidth = 0.4,
    colour = "#999999", linetype = "dashed"
  ) 

# set font size to 12
aligned_hreach <- aligned_hreach +
  theme(text = element_text(size = 11))

# add confidence intervals and data points
aligned_hreach <- aligned_hreach + geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
                     fill = "#CC0066", alpha=0.5, color=NA)+ 
  geom_line(color = "#CC0066")+ geom_point(color = "#CC0066", size=1)+
  scale_y_continuous(breaks = seq(-15, 65, by = 15), limits = c(-15, 65))+
  theme(panel.background = element_rect(fill = "#F0F0F0"))+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.x = element_blank())





  

####Part 1 Rotated session
#ppid from int to chr
hand_data$ppid <- as.character(hand_data$ppid)

rotated_hand <- hand_data %>%
  filter(type=='rotated', block_num < 30)%>%
  mutate(rotated_diff = ifelse(type == "rotated",
                               hand_3cm_out_angle - (target_angle - 30)))
select_ppid <- c("16926", "38624", "62761", "65728", "67376", "71112","70933", "85043", "90766", "91332", "94302", "79915", "16043", "39328", "12689",
                 "56319","56620","57790","58030","59350","59470","60743","68230","71950","72040","77620")

rotated_hand <- rotated_hand %>%
  filter(ppid %in% select_ppid)%>%
  group_by(trial_num)%>%
  summarise(rotated_mean=mean(rotated_diff),
            rotated_median=median(rotated_diff),
            sd_err = sd(rotated_diff, na.rm = TRUE),
            n=n(),.groups = 'drop') %>%
            mutate(lower_ci = rotated_median - qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)),
                   upper_ci = rotated_median + qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)))


# Re-number the trial numbers starting from 1
rotated_hand$sequential_trials <- 1:(nrow(rotated_hand))

# Plot the data with re-numbered trials starting from 1 and custom breaks/labels
rotated_hreach <- ggplot(rotated_hand, aes(x = sequential_trials, y = rotated_median)) +
  geom_point()+ 
  theme_classic()+
  theme(legend.position = "none")+
  labs(
    x = "Trial Number",
    y = "Median angular deviation (°)"
    #title="Rotated Hand Reaches"
  )

rotated_hreach <- rotated_hreach+
  geom_hline(
    yintercept = c(0, 30), linewidth = 0.4,
    colour = "#999999", linetype = "dashed"
  ) 

# set font size to 11
rotated_hreach <- rotated_hreach +
  theme(text = element_text(size = 11))
# add confidence intervals and data points
rotated_hreach <- rotated_hreach + geom_ribbon(aes(ymin = lower_ci,
                                                   ymax = upper_ci),
                                               fill = "#CC0066", alpha=0.5, color=NA)+ 
  geom_line(color = "#CC0066")+ geom_point(color = "#CC0066", size=1)+
  scale_y_continuous(breaks = seq(-15, 65, by = 15), limits = c(-15, 65))+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.x = element_blank())


  
