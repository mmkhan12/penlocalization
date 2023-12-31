#Load omnibus trial_results file
data_hand <- "omnibus_raw.csv"
localization <- fread(data_hand)%>%
  filter(type=="localization")%>%
  filter(!(trial_num >= 19 & trial_num <= 26))
#select_ppid <- c("16926", "38624", "62761", "65728", "67376", "71112", "85043", "90766", "91332", "94302", "79915", "16043", "39328", "12689",
             #    "56319","56620","57790","58030","59350","59470","60743","68230","71950","72040","77620")

Localization <- localization %>%
 # filter(ppid %in% select_ppid)%>%
  mutate(localization_type = case_when(
    trial_num < 191 ~ "aligned hand",
    trial_num > 194 ~ "rotated hand"))
local_arc_acqr <- Localization$localizing_angle - Localization$arc_aquired_angle
Localization$local_diff <- local_arc_acqr



#Median
localization_summary <- Localization %>%
  group_by(ppid,localization_type) %>%
  summarise(local_median = median(local_diff),
            local_mean = mean(local_diff),
            local_sd = sd(local_diff))

#Identify and remove outliers
filtered_data <- Localization %>%
  left_join(localization_summary, by = c("ppid", "localization_type")) %>%
  mutate(
    is_outlier = local_diff > local_mean + 3 * local_sd | local_diff < local_mean - 3 * local_sd
  ) %>%
  filter(!is_outlier) %>%
  select(-is_outlier)

# Identify outliers from the original Localization dataframe
outliers <- Localization %>%
  left_join(localization_summary, by = c("ppid", "localization_type")) %>%
  filter(local_diff > local_mean + 3 * local_sd | local_diff < local_mean - 3 * local_sd) %>%
  select(-local_median, -local_mean, -local_sd)


#Comparison stats
#paired t-test
aligned_group <- filtered_data %>%
  filter(localization_type == "aligned hand") %>%
  pull(local_median)

rotated_group <- filtered_data %>%
  filter(localization_type == "rotated hand") %>%
  pull(local_median)

diffs <- rotated_group - aligned_group
t.test(diffs)

filtered_data %>%
  group_by(localization_type) %>%
  summarise(num_points = n())

#plot
hand_localization <- filtered_data %>% 
  ggplot(aes(x=localization_type, y=local_median, fill=localization_type))+
  geom_boxplot(width=0.7, alpha=0.3)+
  geom_point(aes(fill=localization_type),size=2,shape=21, alpha=0.5)+
  geom_line(aes(group=ppid), color='gray', alpha=0.4)+
  theme_classic()+
  scale_color_manual(values = c("aligned hand"="#00CCCC", "rotated hand"="#00CCCC"))+
  scale_fill_manual(values = c("aligned hand"="#00CCCC", "rotated hand"="#00CCCC"))+
  theme(
    legend.position="none",
    axis.text = element_text(size = 12), 
    axis.title.y = element_text(size = 12)
  ) +
  xlab("")+
  ylab("End-effector localization (°)")+
  scale_y_continuous(breaks = seq(-15, 50, by = 15), limits = c(-15, 30))+
  scale_x_discrete(labels=c('Aligned', 'Rotated'))+
  geom_hline(
    yintercept = c(0, 15), linewidth = 0.4,
    colour = "#999999", linetype = "dotted"
  ) +
  geom_hline(
    yintercept = c(0, 0), linewidth = 0.4,
    colour = "#999999", linetype = "dotted"
  ) +
  geom_hline(
    yintercept = c(0, 10), linewidth = 0.4,
    colour = "#999999", linetype = "dotted")+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.x = element_blank())
print(hand_localization)









# Assuming your data frame is called Localization
ppid_70933_rotated_hand <- subset(Localization, ppid == 70933 & localization_type == "rotated hand")

# Print the local_diff values for ppid 70933 and rotated hand localization type
print(ppid_70933_rotated_hand$local_diff)

# Count trials with local_diff greater than 20 for ppid 70933 and rotated hand localization type
num_trials_greater_than_20 <- sum(ppid_70933_rotated_hand$local_diff > 20)

# Print the result
print(num_trials_greater_than_20)

# Count trials with local_diff greater than 20 for ppid 70933 and rotated hand localization type
num_trials_less_than_20 <- sum(ppid_70933_rotated_hand$local_diff < 20)

# Print the result
print(num_trials_less_than_20)


