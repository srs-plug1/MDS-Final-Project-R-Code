# Plotting daily rainfall totals:
rainDL_ave <- average_columns(rainDL, c(2:5), "rainDL")
daily_rain <- plot_ly(data = rainDL_ave,
                      y = ~rainDL,
                      type = "box")
daily_rain

# Prepare data:
rainHR1 <- aggregate_data(rain5, 5, 60, 2)
rainHR2 <- aggregate_data(rain5, 5, 60, 3)
rainHR3 <- aggregate_data(rain5, 5, 60, 4)
rainHR4 <- aggregate_data(rain5, 5, 60, 5)
rainHR <- combine_data(list(rainHR1, rainHR2, rainHR3, rainHR4), c("l3", "l4", "l7", "l8"))
rainHR_ave <- average_columns(rainHR, c(2, 3, 4, 5), "rainHR")

rain60_ave <- average_columns(rain60, c(2:5), "rain60")


# Functions:

sensitivity_A <- test_mit(rain60_ave, c(6, 12, 18, 24, 30, 36, 42, 48), 60)

box_df <- sensitivity_A[[1]]
box_df <- box_df[2:nrow(box_df),]
totals_df <- sensitivity_A[[2]]
totals_df <- totals_df[2:nrow(totals_df),]

# Plotting:

ggplot(box_df,
       aes(x = as.factor(mit),
           y = depth,
           fill = as.factor(mit))) +
  geom_boxplot() +
  ggtitle("Rainfall Event Depths for Different MIT Values") +
  xlab("MIT Value, hours") +
  ylab("Event Depth, mm")

ggplot(box_df,
       aes(x = as.factor(mit),
           y = duration,
           fill = as.factor(mit))) +
  geom_boxplot() +
  ggtitle("Rainfall Event Durations for Different MIT Values") +
  xlab("MIT Value, hours") +
  ylab("Event Duration, hours")

ggplot(box_df,
       aes(x = as.factor(mit),
           y = ave_intensity,
           fill = as.factor(mit))) +
  geom_boxplot() +
  ggtitle("Rainfall Event Average Intensities for Different MIT Values") +
  xlab("MIT Value, hours") +
  ylab("Event Average Intensity, mm/h")

ggplot(box_df,
       aes(x = as.factor(mit),
           y = adwp,
           fill = as.factor(mit))) +
  geom_boxplot() +
  ggtitle("ADWP Lengths for Different MIT Values") +
  xlab("MIT Value, hours") +
  ylab("ADWP, hours")

ggplot(box_df,
       aes(x = as.factor(mit),
           y = return,
           fill = as.factor(mit))) +
  geom_point() +
  ggtitle("Rainfall Event Return Periods for Different MIT Values") +
  xlab("MIT Value, hours") +
  ylab("Event Return Periods, years")

# Totals

ggplot(totals_df,
       aes(x = as.factor(mit),
           y = events)) +
  geom_col() +
  ggtitle("Number of Rainfall Events Identified for Different MIT Values") +
  xlab("MIT Value, hours") +
  ylab("Number of Events")


ggplot(totals_df,
       aes(x = mit,
           y = extremes)) +
  geom_col() +
  ggtitle("Number of Extreme Rainfall Events Identified for Different MIT Values") +
  xlab("MIT Value, hours") +
  ylab("Number of Extreme Events") +
  scale_x_continuous(breaks = c(6, 12, 18, 24, 30, 36, 42, 48))


# Summarising:
summary_list <- summarise_test_mit(box_df, totals_df)

summary <- summary_list[[1]]
summary_geom <- summary_list[[2]]

transp_summary <- as.data.frame(t(summary[, -1]))
colnames(transp_summary) <- summary[, 1]
transp_summary <- rownames_to_column(transp_summary, var = "event_char")
transp_summary <- melt(transp_summary, id.vars = "event_char", 
                       variable.name = "mit",
                       value.name = "value")

transp_summary_geom <- as.data.frame(t(summary_geom[, -1]))
colnames(transp_summary_geom) <- summary_geom[, 1]
transp_summary_geom <- rownames_to_column(transp_summary_geom, var = "event_char")
transp_summary_geom <- melt(transp_summary_geom, id.vars = "event_char", 
                       variable.name = "mit",
                       value.name = "value")


ggplot(data = transp_summary,
       aes(x = event_char,
           y = value)) +
  geom_boxplot()

ggplot(data = transp_summary_geom,
       aes(x = event_char,
           y = value)) +
  geom_boxplot()

# Just 6-hour MIT, without MIT column or return column:
summary_mit_6 <- box_df[box_df$mit == 6, c(2,3,5,6)]
summary_mit_6 <- summary_mit_6 %>% select(depth, duration, ave_intensity, adwp)

# Transpose to facilitate plotting boxplot:
transp_summary_mit_6 <- as.data.frame(t(summary_mit_6))
transp_summary_mit_6 <- rownames_to_column(transp_summary_mit_6, var = "event_char")
transp_summary_mit_6 <- melt(transp_summary_mit_6, id.vars = "event_char",
                            value.name = "value")
transp_summary_mit_6 <- transp_summary_mit_6[, -2]

transp_summary_mit_6$event_char <- factor(transp_summary_mit_6$event_char, levels = unique(transp_summary_mit_6$event_char))

ggplot(data = transp_summary_mit_6,
       aes(x = event_char,
           y = value)) + 
  geom_boxplot() +
  ggtitle("Distributions of Rain Event Properties \n(For Comparison Against Dunkerley (2008)), MIT = 6h") +
  xlab("") +
  ylab("Value in h, mm, mm/h")

plot_ly(data = summary_mit_6) %>%
  add_boxplot(y = ~depth,
              name = "Event Depth",
              yaxis = "y1") %>%
  add_boxplot(y = ~duration,
              name = "Event Duration",
              yaxis = "y1") %>%
  add_boxplot(y = ~ave_intensity,
              name = "Event Mean Intensity",
              yaxis = "y1") %>%
  add_boxplot(y = ~adwp,
              name = "Event ADWP",
              yaxis = "y2") %>%
  layout(
    #title = "Distributions of Rain Event Properties \n(For Comparison Against Dunkerley (2008)), MIT = 6h",
         yaxis = list(title = "Depth, Duration, Intensity (mm, h, mm/h respectively)",
                      range = c(0, 50)),
         yaxis2 = list(title = "ADWP (h)",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 500)),
         showlegend = F,
         margin = list(r = 80))


# Taking log:
transp_summary_mit_6_log <- transp_summary_mit_6
transp_summary_mit_6_log$value <- log(transp_summary_mit_6$value)

summary_mit_6_log <- summary_mit_6
summary_mit_6_log$depth <- log(summary_mit_6_log$depth)
summary_mit_6_log$duration <- log(summary_mit_6_log$duration)
summary_mit_6_log$ave_intensity <- log(summary_mit_6_log$ave_intensity)
summary_mit_6_log$adwp <- log(summary_mit_6_log$adwp)


ggplot(data = transp_summary_mit_6_log,
       aes(x = event_char,
           y = value)) + 
  geom_boxplot() +
  ggtitle("Distributions of Rain Event Properties \n(For Comparison Against Dunkerley (2008)), MIT = 6h") +
  xlab("") +
  ylab("Log of Value in h, mm, mm/h")


plot_ly(data = summary_mit_6_log) %>%
  add_boxplot(y = ~depth,
              name = "Mean Depth") %>%
  add_boxplot(y = ~duration,
              name = "Mean Duration") %>%
  add_boxplot(y = ~ave_intensity,
              name = "Mean Intensity") %>%
  add_boxplot(y = ~adwp,
              name = "Mean ADWP") %>%
  layout(
    #title = "Distributions of Log of Rain Event Properties \n(For Comparison Against Dunkerley (2008)), MIT = 6h",
         yaxis = list(title = "Log of Value of Depth, Duration, Intensity, ADWP \n(mm, h, mm/h, h respectively)",
                      range = c(-4, 8)),
         showlegend = F,
         margin = list(r = 80))


# PART A TESTING MINIMUM DEPTH THRESHOLD:

#test_threshold_function <- rainfall_events_thresh(rain60_ave, 24, 0.6)
#test_threshold_function_events <- classify_events_thresh(test_threshold_function, 60)

sens_analysis_threashold <- test_rain_threashold(rain60_ave, c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4), 6, 60)


box_df_thresh <- sens_analysis_threashold[[1]]
box_df_thresh <- box_df_thresh[2:nrow(box_df_thresh),]
totals_df_thresh <- sens_analysis_threashold[[2]]
totals_df_thresh <- totals_df_thresh[2:nrow(totals_df_thresh),]

event_df_list <- rain_threshold(rain60_ave, c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4), 24, 60, ddf_table)

# Obtain event numbers for events obtained just using MIT = 24 criterion:
actual_event_num <- rain60_events_only[, c(1,5)]
colnames(actual_event_num)[2] <- "actual_event"

# Isolate events with return periods, combine with actual event number:
event_df_0.2 <- event_df_list[[1]]
event_df_0.2 <- combine_data(list(event_df_0.2, actual_event_num))
event_df_0.2 <- event_df_0.2[!is.na(event_df_0.2$return_period_interval),]

event_df_0.4 <- event_df_list[[2]]
event_df_0.4 <- combine_data(list(event_df_0.4, actual_event_num))
event_df_0.4 <- event_df_0.4[!is.na(event_df_0.4$return_period_interval),]

event_df_0.6 <- event_df_list[[3]]
event_df_0.6 <- combine_data(list(event_df_0.6, actual_event_num))
event_df_0.6 <- event_df_0.6[!is.na(event_df_0.6$return_period_interval),]

event_df_0.8 <- event_df_list[[4]]
event_df_0.8 <- combine_data(list(event_df_0.8, actual_event_num))
event_df_0.8 <- event_df_0.8[!is.na(event_df_0.8$return_period_interval),]

event_df_1 <- event_df_list[[5]]
event_df_1 <- combine_data(list(event_df_1, actual_event_num))
event_df_1 <- event_df_1[!is.na(event_df_1$return_period_interval),]

event_df_1.2 <- event_df_list[[6]]
event_df_1.2 <- combine_data(list(event_df_1.2, actual_event_num))
event_df_1.2 <- event_df_1.2[!is.na(event_df_1.2$return_period_interval),]

event_df_1.4 <- event_df_list[[7]]
event_df_1.4 <- combine_data(list(event_df_1.4, actual_event_num))
event_df_1.4 <- event_df_1.4[!is.na(event_df_1.4$return_period_interval),]

event_df_1.6 <- event_df_list[[8]]
event_df_1.6 <- combine_data(list(event_df_1.6, actual_event_num))
event_df_1.6 <- event_df_1.6[!is.na(event_df_1.6$return_period_interval),]

event_df_1.8 <- event_df_list[[9]]
event_df_1.8 <- combine_data(list(event_df_1.8, actual_event_num))
event_df_1.8 <- event_df_1.8[!is.na(event_df_1.8$return_period_interval),]

event_df_2 <- event_df_list[[10]]
event_df_2 <- combine_data(list(event_df_2, actual_event_num))
event_df_2 <- event_df_2[!is.na(event_df_2$return_period_interval),]

event_df_2.2 <- event_df_list[[11]]
event_df_2.2 <- combine_data(list(event_df_2.2, actual_event_num))
event_df_2.2 <- event_df_2.2[!is.na(event_df_2.2$return_period_interval),]

event_df_2.4 <- event_df_list[[12]]
event_df_2.4 <- combine_data(list(event_df_2.4, actual_event_num))
event_df_2.4 <- event_df_2.4[!is.na(event_df_2.4$return_period_interval),]


# Return periods for different MIT values:

rain60_mit6 <- rainfall_events(rain60_ave, 6)
rain60_mit6 <- classify_events(rain60_mit6, 60)
rain60_mit6_rp <- return_period(rain60_mit6, ddf_table)
rain60_mit6_rp <- combine_data(list(rain60_mit6_rp, actual_event_num))

rain60_mit48 <- rainfall_events(rain60_ave, 48)
rain60_mit48 <- classify_events(rain60_mit48, 60)
rain60_mit48_rp <- return_period(rain60_mit48, ddf_table)

rain60_mit12 <- rainfall_events(rain60_ave, 12)
rain60_mit12 <- classify_events(rain60_mit12, 60)
rain60_mit12_rp <- return_period(rain60_mit12, ddf_table)


# Plotting:

ggplot(box_df_thresh,
       aes(x = as.factor(min_rain_threshold),
           y = depth,
           fill = as.factor(min_rain_threshold))) +
  geom_boxplot() +
  ggtitle("Rainfall Event Depths for Different Minimum Rain Thresholds") +
  xlab("Minimum Rain Threshold, mm") +
  ylab("Event Depth, mm")

ggplot(box_df_thresh,
       aes(x = as.factor(min_rain_threshold),
           y = duration,
           fill = as.factor(min_rain_threshold))) +
  geom_boxplot() +
  ggtitle("Rainfall Event Durations for Different Minimum Rain Thresholds") +
  xlab("Minimum Rain Threshold, mm") +
  ylab("Event Duration, hours")

ggplot(box_df_thresh,
       aes(x = as.factor(min_rain_threshold),
           y = ave_intensity,
           fill = as.factor(min_rain_threshold))) +
  geom_boxplot() +
  ggtitle("Rainfall Event Average Intensities for Different Minimum Rain Thresholds") +
  xlab("Minimum Rain Threshold, mm") +
  ylab("Event Average Intensity, mm/h")

ggplot(box_df_thresh,
       aes(x = as.factor(min_rain_threshold),
           y = adwp,
           fill = as.factor(min_rain_threshold))) +
  geom_boxplot() +
  ggtitle("ADWP Lengths for Different Minimum Rain Thresholds") +
  xlab("Minimum Rain Threshold, mm") +
  ylab("ADWP, hours")

ggplot(box_df_thresh,
       aes(x = as.factor(min_rain_threshold),
           y = return,
           fill = as.factor(min_rain_threshold))) +
  geom_point() +
  ggtitle("Rainfall Event Return Periods for Different Minimum Rain Thresholds") +
  xlab("Minimum Rain Threshold, mm") +
  ylab("Event Return Periods, years")

ggplot(box_df_thresh,
       aes(x = as.factor(min_rain_threshold),
           y = pre_event_rain,
           fill = as.factor(min_rain_threshold))) +
  geom_boxplot() +
  ggtitle("Total Pre-event Rain for Different Minimum Rain Thresholds") +
  xlab("Minimum Rain Threshold, mm") +
  ylab("Pre-Event Rain, mm")

pre_event_group_mean <- aggregate(x = box_df_thresh$pre_event_rain , by = list(box_df_thresh$min_rain_threshold), FUN = mean)

ggplot(pre_event_group_mean,
       aes(x = Group.1,
           y = x)) +
  geom_point() +
  ggtitle("Mean Pre-Event Rain for Different Minimum Rain Thresholds") +
  xlab("Minimum Rain Threshold, mm") +
  ylab("Mean Pre-Event Rain, mm")

# Totals

ggplot(totals_df_thresh,
       aes(x = as.factor(min_rain_threshold),
           y = events)) +
  geom_col() +
  ggtitle("Number of Rainfall Events Identified for Different Minimum Rain Thresholds") +
  xlab("Minimum Rain Threshold, mm") +
  ylab("Number of Events")


ggplot(totals_df_thresh,
       aes(x = min_rain_threshold,
           y = extremes)) +
  geom_col() +
  ggtitle("Number of Extreme Rainfall Events Identified for Different Minimum Rain Thresholds") +
  xlab("Minimum Rain Threshold, mm") +
  ylab("Number of Extreme Events") +
  scale_x_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3))




