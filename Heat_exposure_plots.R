library(survival)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(survminer)
library(Gmisc, quietly = TRUE)
library(glue)
library(htmlTable)
library(grid)
library(magrittr)
library(forestploter)

#flowchart
org_cohort <- boxGrob(glue("All adult kidney transplants from 2010",
                           "n = {pop}",
                           pop = txtInt(303072),
                           .sep = "\n"))
eligible <- boxGrob(glue("First-time, kidney-only KTRs",
                         "n = {pop}",
                         pop = txtInt(252049),
                         .sep = "\n"))
follow_up_cohort <- boxGrob(glue("Survival, graft survival, and no rejection episodes during the first year post-transplant",
                                 "n = {incl}",
                                 incl = txtInt(171007),
                                 .sep = "\n"))
creat_cohort <- boxGrob(glue("KTRs with at least two creatinine measurements from one-year post-transplant",
                             "n = {incl}",
                             incl = txtInt(125128),
                             .sep = "\n"))
consistent_cohort <- boxGrob(glue("KTRs with consistent one-year follow-up periods",
                                  "n = {incl}",
                                  incl = txtInt(63351),
                                  .sep = "\n"))

grid.newpage()
vert <- spreadVertical(org_cohort,
                       eligible = eligible,
                       follow_up_cohort = follow_up_cohort,
                       creat_cohort=creat_cohort,
                       consistent_cohort)
for (i in 1:(length(vert) - 1)) {
  connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>%
    print
}
vert

#creatinine doubling curve
creat_censoring<- data_log_log %>% group_by(PT_CODE) %>% summarize(CREATININE_STATUS=max(creatinine_status), max_sub_period=max(sub_period), FU_TIME_MAX=max(time_diff_from_first_years)) 
data_wide_consistent <- merge(data_wide_consistent, creat_censoring, by="PT_CODE")

km_fit <- survfit(Surv(as.numeric(FOLLOW_UP_UNTIL_SCR_10YEARS), 
                       as.numeric(CR_10YEARS)) ~ HI_accum_quartile, 
                  data = data_wide_consistent)
ggsurvplot(km_fit, 
           data = data_wide_consistent, 
           pval = TRUE,
           risk.table = TRUE,
           legend.title = "", 
           legend.labs = c("Q1", "Q2", "Q3", "Q4"),
           xlab = "Time from Transplantation, Years", 
           ylab = "Doubling of Serum Creatinine-Free Survival Probability", 
           break.time.by=1,
           censor=T,
           censor.size=1,
           cumevents = T,
           tables.height = 0.2,
           xlim=c(2,10),
)

ggsurvplot(km_fit, 
           data = data_wide_consistent, 
           pval = TRUE,
           risk.table = F,
           break.time.by=1,
           censor=T,
           censor.size=1,
           xlim=c(2,10),
           ylim=c(0.6,1),
           xlab = "", 
           ylab = "", 
           legend.labs = c("Q1", "Q2", "Q3", "Q4"),
)


#eGFR trajectory by quartiles
ggplot(data_consistentFU_final, aes(x = round(time_diff_from_first_years), 
                                    y = percent_change_GFR_CKD_EPI, 
                                    color = as.factor(HI_accum_quartile), 
                                    group = HI_accum_quartile)) +
  stat_summary(fun = mean, geom = "line") +
  labs(
    title = "eGFR Trajectory According to Time-Accumulated Heat Exposure",
    x = "Time from First Observation, Years",
    y = "eGFR, % Change from Baseline",
    color = "Heat Exposure Quartile"
  ) +
  xlim(c(0,6)) + theme_clean(base_size=16)



#Forest plot
Forest_plot_data <- read.csv("Forest_plot_data.csv")
Forest_plot_data <- Forest_plot_data
Forest_plot_data$Subgroup <- ifelse(is.na(Forest_plot_data$est), 
                                      Forest_plot_data$Subgroup,
                                      paste0("   ", Forest_plot_data$Subgroup))

df <- Forest_plot_data[, c("Subgroup", "Number.of.participants", "est", "lo", "hi", "P.for.interaction")]

names(df) <- c("Subgroup", "Number_of_Participants", "eGFR_slope", "Lower", "Upper", "P_for_interaction")
df$eGFR_CI <- paste0(round(df$eGFR_slope, 2), " (", round(df$Lower, 2), "-", round(df$Upper, 2), ")")
plot_df <- data.frame(Subgroup = df$Subgroup)
plot_df$`Number of Participants`<- df$Number_of_Participants
plot_df$'' <- paste(rep(" ", 20), collapse = " ")
plot_df$`P for interaction`<- df$P_for_interaction
plot_df[is.na(plot_df)] <- " "
# Create the forest plot
p <- forest(data = plot_df, est = df$eGFR_slope, lower = df$Lower, upper = df$Upper,
            ci_column = 3, ref_line = 0, xlab = c("eGFR change for each year with average heat index >30°C, %"), xlim = c(-4, 4), title = "Subgroup Analysis" , footnote = "44 KTRs had missing data on diabetes mellitus, 56 on KDPI, and 2 on delayed graft function")
print(p)


#log-log plot

df <- data.frame(
  temperature = c(27, 28, 29, 30, 31),
  HR = c(1.156186, 1.220084, 1.330873, 1.44742528, 1.63065),
  lower_CI = c(0.9072109, 0.9312917, 0.9720922, 0.993693516, 1.016993),
  upper_CI = c(1.473489, 1.598431, 1.822073, 2.10833613, 2.614602),
  p_value = c(0.2408, 0.1489, 0.0745, 0.0540, 0.0424)
)

ggplot(df, aes(x = temperature, y = HR)) +
  geom_point(size = 3, color = "navy") +
  geom_smooth(aes(ymin = lower_CI, ymax = upper_CI), 
              stat = "identity", fill = "gray29", alpha = 0.25, color = "navy", linetype = "solid") +
  theme_clean() +
  ylim(0.7,2.62)+
  labs(
    x = "Heat Index (°C)",
    y = "Hazard Ratio (95% CI)",
    title = "Doubling of serum creatinine",
    subtitle="Complementary multivariable log-log discrete time survival model", 
  ) +theme(axis.text.x=element_text(size = 14), axis.text.y=element_text(size = 14), axis.title.x=element_text(size = 14), axis.title.y=element_text(size = 14))

#histograms
distribution_plot <- data_consistentFU_final%>% filter(sub_period>1) %>% dplyr::select(HEAT_27, HEAT_28, HEAT_29, HEAT_30, HEAT_31)

par(mfrow = c(2, 3), cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2) 

hist(distribution_plot$HEAT_27, 
     main = "Heat Index > 27°C", 
     xlab = "Proportion of Days with Heat Index above the Threshold", 
     col = "lightblue", 
     border = "black", 
     breaks = 20, 
     probability = TRUE,
     xlim = c(0, 0.6))

hist(distribution_plot$HEAT_28, 
     main = "Heat Index > 28°C", 
     xlab = "Proportion of Days with Heat Index above the Threshold", 
     col = "lightgreen", 
     border = "black", 
     breaks = 20, 
     probability = TRUE, 
     xlim = c(0, 0.6))

hist(distribution_plot$HEAT_29, 
     main = "Heat Index > 29°C", 
     xlab = "Proportion of Days with Heat Index above the Threshold", 
     col = "lightcoral", 
     border = "black", 
     breaks = 20, 
     probability = TRUE,
     xlim = c(0, 0.6))

hist(distribution_plot$HEAT_30, 
     main = "Heat Index > 30°C", 
     xlab = "Proportion of Days with Heat Index above the Threshold", 
     col = "lightyellow", 
     border = "black", 
     breaks = 20, 
     probability = TRUE, 
     xlim = c(0, 0.6))

hist(distribution_plot$HEAT_31, 
     main = "Heat Index > 31°C", 
     xlab = "Proportion of Days with Heat Index above the Threshold", 
     col = "lightpink", 
     border = "black", 
     breaks = 20, 
     probability = TRUE,
     xlim = c(0, 0.6))


#maps
zcta_shapes <- zctas(cb = FALSE)
(tigris_use_cache = TRUE)
zcta_shapes<- as.data.frame(zcta_shapes)
zcta_shapes<- zcta_shapes %>% select(GEOID20, INTPTLON20, INTPTLAT20)

#add missing coordinates manually (not shared)

zcta_shapes <- zcta_shapes %>%
  mutate(zipcode = as.character(GEOID20)) %>%
  left_join(heat_indices_accumulated, by = "zipcode")
us<-map_data('state')
hawaii_map<-map_data('world', region="USA:Hawaii")

us<- us %>% filter(long<125)
KTC<- zcta_shapes %>% filter (!is.na(HEAT_30_daily)) 
KTC$INTPTLON20 <- as.numeric(KTC$INTPTLON20)
KTC$INTPTLAT20 <- as.numeric(KTC$INTPTLAT20)

KTC$HI_proportion<- (KTC$HEAT_30_daily/5809)

hawaii<-  KTC %>% filter (INTPTLON20 <= -145)
KTC<- KTC %>% filter (INTPTLON20 >-125)


samples <- data_wide_consistent %>%
  group_by(TX_CTR_CODE) %>%
  mutate(sample_size = n()) %>% summarize(sample_size=max(sample_size), GEOID20=max(zipcode))

KTC <- merge(KTC, samples, by="GEOID20")
hawaii<-  merge(hawaii, samples, by="GEOID20")

US_map <- ggplot(KTC, aes(INTPTLON20, INTPTLAT20)) +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), color = 'gray', fill = NA, alpha = .35) +
  geom_point(aes(color = HI_proportion, size = sample_size), alpha = .5) +  # Adjust alpha for visibility
  scale_color_gradientn(colors = c("blue", "yellow", "orange", "red")) +  # Custom color scale from blue to red
  scale_size_continuous(range = c(2, 10)) + 
  theme_minimal() +
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank(),  # Remove grid lines
    panel.border = element_blank(), # Remove panel border
    plot.background = element_rect(fill = "white"),  # Set white background
    panel.background = element_rect(fill = "white")  # Set white background for the plot area
  ) + labs(
    color = "Proportion of Days with Heat Index >30°C",   # Change color legend title
    size = "Kidney Transplant Recipients"     # Change size legend title (if needed)
  )

hawaii_map<-ggplot(hawaii, aes(INTPTLON20, INTPTLAT20)) +
  geom_polygon(data = hawaii_map, aes(x = long, y = lat, group = group), color = 'gray', fill = NA, alpha = .35) +
  geom_point(aes(color = HI_proportion, size = sample_size), alpha = .5) +  # Adjust alpha for visibility
  scale_color_gradientn(colors = c("blue", "yellow", "orange", "red")) +  # Custom color scale from blue to red
  scale_size_continuous(range = c(2, 10)) + 
  theme_minimal() +
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank(),  # Remove grid lines
    panel.border = element_blank(), # Remove panel border
    plot.background = element_rect(fill = "white"),  # Set white background
    panel.background = element_rect(fill = "white")  # Set white background for the plot area
  ) + guides(
    color = "none",  # Remove the color legend
    size = "none"    # Remove the size legend
  )

pr_map<-map_data('world', region="Puerto Rico")
pr_sf <- tigris::states(cb = TRUE, resolution = "20m") %>%
  dplyr::filter(NAME == "Puerto Rico")
pr_sf <- pr_sf %>%
  mutate(zipcode = "00919") %>%
  left_join(heat_indices_accumulated, by = "zipcode")
pr_sf<- pr_sf %>% mutate(INTPTLON20="-66.0520", INTPTLAT20="18.4094")
pr_sf$INTPTLON20 <- as.numeric(pr_sf$INTPTLON20)
pr_sf$INTPTLAT20 <- as.numeric(pr_sf$INTPTLAT20)
pr_sf$HI_proportion<- (pr_sf$HEAT_30_daily/5809)
pr_sf$sample_size<- 657
pr_map<- ggplot(pr_sf, aes(INTPTLON20, INTPTLAT20)) +
  geom_polygon(data = pr_map, aes(x = long, y = lat, group = group), color = 'gray', fill = NA, alpha = .35) +
  geom_point(aes(color = HI_proportion, size = sample_size), alpha = .5) +  # Adjust alpha for visibility
  scale_color_gradientn(colors = c("blue", "yellow", "orange", "red")) +  # Custom color scale from blue to red
  scale_size_continuous(range = c(2, 10)) + xlim(c(-67.3,-65.2)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank(),  # Remove grid lines
    panel.border = element_blank(), # Remove panel border
    plot.background = element_rect(fill = "white"),  # Set white background
    panel.background = element_rect(fill = "white")  # Set white background for the plot area
  ) + guides(
    color = "none",  # Remove the color legend
    size = "none"    # Remove the size legend
  )

grid.arrange(hawaii_map, pr_map, ncol = 2)
