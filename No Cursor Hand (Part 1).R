#Load omnibus trial_results file
data_hand <- "omnibus_raw.csv"
nocursor <- fread(data_hand)%>%
  filter(type=="nocursor", block_num >7)
#select_ppid <- c("16926", "38624", "62761", "65728", "67376", "71112", "85043", "90766", "91332", "94302", "79915", "16043", "39328", "12689",
               #  "56319","56620","57790","58030","59350","59470","60743","68230","71950","72040","77620")
NoCursor <- nocursor %>%
  #filter(ppid %in% select_ppid)%>%
  mutate(nocursor_type = case_when(
    trial_num <= 191 ~ "aligned hand",
    trial_num >= 343 ~ "rotated hand"))
nocursor_diff <- (NoCursor$hand_final_angle - NoCursor$target_angle)*(-1)
NoCursor$nocursor_diff <- nocursor_diff

#Mean
nocursor_summary <- NoCursor %>%
  group_by(ppid,nocursor_type) %>%
  summarise(nocursor_median = median(nocursor_diff),
            nocursor_mean = mean(nocursor_diff),
            nocursor_sd = sd(nocursor_diff)) 
#Identify and remove outliers
filtered_nhand <- NoCursor %>%
  left_join(nocursor_summary, by = c("ppid", "nocursor_type")) %>%
  mutate(
    is_outlier = nocursor_diff > nocursor_mean + 3 * nocursor_sd | nocursor_diff < nocursor_mean - 3 * nocursor_sd
  ) %>%
  filter(!is_outlier) %>%
  select(-is_outlier)


# Identify outliers from the original Localization dataframe
outliers_nhand <- NoCursor %>%
  left_join(nocursor_summary, by = c("ppid", "nocursor_type")) %>%
  filter(nocursor_diff > nocursor_mean + 3 * nocursor_sd | nocursor_diff < nocursor_mean - 3 * nocursor_sd) %>%
  select(-nocursor_median, -nocursor_mean, -nocursor_sd)

#Comparison stats
#paired t-test
naligned_group <- nocursor_median %>%
  filter(nocursor_type == "aligned hand") %>%
  pull(nocursor_median)

nrotated_group <- nocursor_median %>%
  filter(nocursor_type == "rotated hand") %>%
  pull(nocursor_median)

ndiffs <- nrotated_group - naligned_group
t.test(ndiffs)

#plot
hand_nocursor <- filtered_nhand %>% 
  ggplot(aes(x=nocursor_type, y=nocursor_median, fill=nocursor_type, alpha=0.6))+
  geom_boxplot(width=0.7)+
  geom_point(aes(fill=nocursor_type),size=2,shape=21, alpha=0.5)+
  geom_line(aes(group=ppid), color='gray', alpha=0.4)+
  theme_classic()+
  scale_color_manual(values = c("aligned hand"="#CC99FF", "rotated hand"="#CC99FF"))+
  scale_fill_manual(values = c("aligned hand"="#CC99FF", "rotated hand"="#CC99FF"))+
  theme(
    legend.position="none",
    axis.text = element_text(size = 12), 
    axis.title.y = element_text(size = 12)
  ) +
  xlab("")+
  ylab("Median no cursor deviation (°)")+
  #scale_x_discrete(labels=c('Aligned', 'Rotated'))+
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
    colour = "#999999", linetype = "dotted"
  ) +
  scale_y_continuous(breaks = seq(-15, 30, by = 15), limits = c(-15, 30))+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.x = element_blank())
print(hand_nocursor)
