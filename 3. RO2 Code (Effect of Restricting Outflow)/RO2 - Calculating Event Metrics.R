## Preparing the input data: ##

l3_outHR_smooth <- outHR_smooth[, c(1:3)]
l3_moist_both_HR_int <- combine_data(list(l3_all_moist_A_int, l3_all_moist_B_int))
l3_vwc_both_HR_int <- combine_data(list(l3_vwc_A_int, l3_vwc_B_int))

l3_vwc5_both_HR_int <- l3_vwc_both_HR_int[, c(1, 2, 11)]
l3_vwc10_both_HR_int <- l3_vwc_both_HR_int[, c(1, 3, 12)]
l3_vwc20_both_HR_int <- l3_vwc_both_HR_int[, c(1, 4, 13)]
l3_vwc30_both_HR_int <- l3_vwc_both_HR_int[, c(1, 5, 14)]
l3_vwc40_both_HR_int <- l3_vwc_both_HR_int[, c(1, 6, 15)]
l3_vwc50_both_HR_int <- l3_vwc_both_HR_int[, c(1, 7, 16)]
l3_vwc60_both_HR_int <- l3_vwc_both_HR_int[, c(1, 8, 17)]
l3_vwc75_both_HR_int <- l3_vwc_both_HR_int[, c(1, 9, 18)]
l3_vwc100_both_HR_int <- l3_vwc_both_HR_int[, c(1, 10, 19)]

l3_moist5_both_HR_int <- l3_moist_both_HR_int[, c(1, 2, 11)]
l3_moist10_both_HR_int <- l3_moist_both_HR_int[, c(1, 3, 12)]
l3_moist20_both_HR_int <- l3_moist_both_HR_int[, c(1, 4, 13)]
l3_moist30_both_HR_int <- l3_moist_both_HR_int[, c(1, 5, 14)]
l3_moist40_both_HR_int <- l3_moist_both_HR_int[, c(1, 6, 15)]
l3_moist50_both_HR_int <- l3_moist_both_HR_int[, c(1, 7, 16)]
l3_moist60_both_HR_int <- l3_moist_both_HR_int[, c(1, 8, 17)]
l3_moist75_both_HR_int <- l3_moist_both_HR_int[, c(1, 9, 18)]
l3_moist100_both_HR_int <- l3_moist_both_HR_int[, c(1, 10, 19)]

# Calculating the soil moisture in litres for "full capacity".
# Full capacity VWC = 39%
full_capacity_moisture_L_all_depth <- 1*2*1*1000*0.39
full_capacity_moisture_L_5 <- 1*2*0.075*1000*0.39
full_capacity_moisture_L_10 <- 1*2*0.075*1000*0.39
full_capacity_moisture_L_20 <- 1*2*0.1*1000*0.39
full_capacity_moisture_L_30 <- 1*2*0.1*1000*0.39
full_capacity_moisture_L_40 <- 1*2*0.1*1000*0.39
full_capacity_moisture_L_50 <- 1*2*0.1*1000*0.39
full_capacity_moisture_L_60 <- 1*2*0.125*1000*0.39
full_capacity_moisture_L_75 <- 1*2*0.2*1000*0.39
full_capacity_moisture_L_100 <- 1*2*0.125*1000*0.39


## Average initial moisture content: ##
# Test versions only exclude based on outflow after 24 hours after the end of
# the event being greater than event depth.
out_met_test5 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist5_both_HR_int, l3_vwc5_both_HR_int, c(1:162), c(2,3), 0, 0, full_capacity_moisture_L_5, 10.8, c(5, 35, 89, 104), Inf)
out_met_test10 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist10_both_HR_int, l3_vwc10_both_HR_int, c(1:162), c(2,3), 0, 0, full_capacity_moisture_L_10, 10.8, c(5, 35, 89, 104), Inf)
out_met_test20 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist20_both_HR_int, l3_vwc20_both_HR_int, c(1:162), c(2,3), 0, 0, full_capacity_moisture_L_20, 10.8, c(5, 35, 89, 104), Inf)
out_met_test30 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist30_both_HR_int, l3_vwc30_both_HR_int, c(1:162), c(2,3), 0, 0, full_capacity_moisture_L_30, 10.8, c(5, 35, 89, 104), Inf)
out_met_test40 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist40_both_HR_int, l3_vwc40_both_HR_int, c(1:162), c(2,3), 0, 0, full_capacity_moisture_L_40, 10.8, c(5, 35, 89, 104), Inf)
out_met_test50 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist50_both_HR_int, l3_vwc50_both_HR_int, c(1:162), c(2,3), 0, 0, full_capacity_moisture_L_50, 10.8, c(5, 35, 89, 104), Inf)
out_met_test60 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist60_both_HR_int, l3_vwc60_both_HR_int, c(1:162), c(2,3), 0, 0, full_capacity_moisture_L_60, 10.8, c(5, 35, 89, 104), Inf)
out_met_test75 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist75_both_HR_int, l3_vwc75_both_HR_int, c(1:162), c(2,3), 0, 0, full_capacity_moisture_L_75, 10.8, c(5, 35, 89, 104), Inf)
out_met_test100 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist100_both_HR_int, l3_vwc100_both_HR_int, c(1:162), c(2,3), 0, 0, full_capacity_moisture_L_100, 10.8, c(5, 35, 89, 104), Inf)

mean(out_met_test30$initial_soil_moisture_A, na.rm = T)/full_capacity_moisture_L_30
mean(out_met_test30$initial_soil_moisture_B, na.rm = T)/full_capacity_moisture_L_30
full_capacity_moisture_L_30 - mean(out_met_test30$initial_soil_moisture_A, na.rm = T)

sum(mean(out_met_test5$initial_soil_moisture_A, na.rm = T),
    mean(out_met_test10$initial_soil_moisture_A, na.rm = T),
    mean(out_met_test20$initial_soil_moisture_A, na.rm = T),
    mean(out_met_test30$initial_soil_moisture_A, na.rm = T),
    mean(out_met_test40$initial_soil_moisture_A, na.rm = T),
    mean(out_met_test50$initial_soil_moisture_A, na.rm = T),
    mean(out_met_test60$initial_soil_moisture_A, na.rm = T),
    mean(out_met_test75$initial_soil_moisture_A, na.rm = T),
    mean(out_met_test100$initial_soil_moisture_A, na.rm = T)) / full_capacity_moisture_L_all_depth

full_capacity_moisture_L_all_depth - sum(mean(out_met_test5$initial_soil_moisture_A, na.rm = T),
                                         mean(out_met_test10$initial_soil_moisture_A, na.rm = T),
                                         mean(out_met_test20$initial_soil_moisture_A, na.rm = T),
                                         mean(out_met_test30$initial_soil_moisture_A, na.rm = T),
                                         mean(out_met_test40$initial_soil_moisture_A, na.rm = T),
                                         mean(out_met_test50$initial_soil_moisture_A, na.rm = T),
                                         mean(out_met_test60$initial_soil_moisture_A, na.rm = T),
                                         mean(out_met_test75$initial_soil_moisture_A, na.rm = T),
                                         mean(out_met_test100$initial_soil_moisture_A, na.rm = T))

# Test exclusion criterion: total_drainage_volume_A / total_drainage_volume_B:
# 40 (excludes 89 events, compared to 82 without this criterion):
out_met_test5_40 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist5_both_HR_int, l3_vwc5_both_HR_int, c(1:162), c(2,3), 0, 0, full_capacity_moisture_L_5, 10.8, c(5, 35, 89, 104), 40)
mean(out_met_test5_40$tot_drain_B_over_tot_drain_A, na.rm = T) # 7.159544

# 30 (excludes 90 events, compared to 82 without this criterion):
out_met_test5_30 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist5_both_HR_int, l3_vwc5_both_HR_int, c(1:162), c(2,3), 0, 0, full_capacity_moisture_L_5, 10.8, c(5, 35, 89, 104), 30)
mean(out_met_test5_30$tot_drain_B_over_tot_drain_A, na.rm = T) # 6.754353

# 20 (excludes 99 events, compared to 82 without this criterion):
out_met_test5_20 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist5_both_HR_int, l3_vwc5_both_HR_int, c(1:162), c(2,3), 0, 0, full_capacity_moisture_L_5, 10.8, c(5, 35, 89, 104), 20)
mean(out_met_test5_20$tot_drain_B_over_tot_drain_A, na.rm = T) # 4.430077

# 10:
out_met_test5_10 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist5_both_HR_int, l3_vwc5_both_HR_int, c(1:162), c(2,3), 0, 0, full_capacity_moisture_L_5, 10.8, c(5, 35, 89, 104), 10)
mean(out_met_test5_10$tot_drain_B_over_tot_drain_A, na.rm = T) # 3.3719

plot_mean <- data.frame(th = c(10, 20, 30, 40),
                        mean = c(3.3719, 4.430077, 6.754353, 7.159544))

plot(plot_mean)

## Calculating outflow metrics: ##
# Excluding events where outflow after 24 hours since the end of the event in 
# either or both outflows is less than 1% of the event depth for small events(
# less than 15 L total rainfall) and less than 5% for other events.
# Greenfield Runoff rate of 10.8 L/hour (0.18 L/min)
# Full capacity calculated based on 39% VWC being full capacity.

# New criteria:
# 1. Events where out by 24h after event end > in
# 2. Events where restricted > unrestricted (by event end)
# 3. Events where ratio of outflow B / outflow A greater than 10


# Soil moisture depth 5
out_met5 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist5_both_HR_int, l3_vwc5_both_HR_int, c(1:162), c(2,3), 0.05, 0.01, full_capacity_moisture_L_5, 10.8, c(5, 35, 89, 104), 10)

# Soil moisture depth 10
out_met10 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist10_both_HR_int, l3_vwc10_both_HR_int, c(1:162), c(2,3), 0.05, 0.01, full_capacity_moisture_L_10, 10.8, c(5, 35, 89, 104), 10)

# Soil moisture depth 20
out_met20 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist20_both_HR_int, l3_vwc20_both_HR_int, c(1:162), c(2,3), 0.05, 0.01, full_capacity_moisture_L_20, 10.8, c(5, 35, 89, 104), 10)

# Soil moisture depth 30
out_met30 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist30_both_HR_int, l3_vwc30_both_HR_int, c(1:162), c(2,3), 0.05, 0.01, full_capacity_moisture_L_30, 10.8, c(5, 35, 89, 104), 10)

# Soil moisture depth 40
out_met40 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist40_both_HR_int, l3_vwc40_both_HR_int, c(1:162), c(2,3), 0.05, 0.01, full_capacity_moisture_L_40, 10.8, c(5, 35, 89, 104), 10)

# Soil moisture depth 50
out_met50 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist50_both_HR_int, l3_vwc50_both_HR_int, c(1:162), c(2,3), 0.05, 0.01, full_capacity_moisture_L_50, 10.8, c(5, 35, 89, 104), 10)

# Soil moisture depth 60
out_met60 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist60_both_HR_int, l3_vwc60_both_HR_int, c(1:162), c(2,3), 0.05, 0.01, full_capacity_moisture_L_60, 10.8, c(5, 35, 89, 104), 10)

# Soil moisture depth 75
out_met75 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist75_both_HR_int, l3_vwc75_both_HR_int, c(1:162), c(2,3), 0.05, 0.01, full_capacity_moisture_L_75, 10.8, c(5, 35, 89, 104), 10)

# Soil moisture depth 100
out_met100 <- outflow_metrics_multiple(rain60_class, l3_outHR_smooth, l3_moist100_both_HR_int, l3_vwc100_both_HR_int, c(1:162), c(2,3), 0.05, 0.01, full_capacity_moisture_L_100, 10.8, c(5, 35, 89, 104), 10)


mean(out_met10$percent_change_moist_start_end_A[is.finite(out_met10$percent_change_moist_start_end_A)], na.rm = T)
mean(out_met10$percent_change_moist_start_end_B[is.finite(out_met10$percent_change_moist_start_end_B)], na.rm = T)


mean(out_met40$percent_change_moist_start_end_A[is.finite(out_met40$percent_change_moist_start_end_A)], na.rm = T)
mean(out_met40$percent_change_moist_start_24_A[is.finite(out_met40$percent_change_moist_start_24_A)], na.rm = T)
mean(out_met40$percent_change_moist_start_end_B[is.finite(out_met40$percent_change_moist_start_end_B)], na.rm = T)
mean(out_met40$percent_change_moist_start_24_B[is.finite(out_met40$percent_change_moist_start_24_B)], na.rm = T)

mean(out_met100$percent_change_moist_start_end_A[is.finite(out_met100$percent_change_moist_start_end_A)], na.rm = T)
mean(out_met100$percent_change_moist_start_24_A[is.finite(out_met100$percent_change_moist_start_24_A)], na.rm = T)
mean(out_met100$percent_change_moist_start_end_B[is.finite(out_met100$percent_change_moist_start_end_B)], na.rm = T)
mean(out_met100$percent_change_moist_start_24_B[is.finite(out_met100$percent_change_moist_start_24_B)], na.rm = T)


## PLOT ANY EVENT: ##
test_plot <- plot_event_outflows(rain60_events, l3_outHR_smooth, 89, 24, "L3 (Bare Earth)")
test_plot

par(mfrow = c(1,1))
hist(out_met30$total_rain_L, breaks = 12)
