# Two periods available:
# 2022-05-27 23:00:00 and 2022-06-10 22:00:00
# And: 2022-06-11 05:00:00 and 2022-06-18 14:00:00

# Plotting Rainfall:
rain60_events_L_l347_2022_may <- rain60_events_L %>%
  filter(
    between(time,
            ymd_hms("2022-05-27 23:00:00", tz = "UTC"),
            ymd_hms("2022-06-10 22:00:00", tz = "UTC"))
  )

plot_ly() %>%
  add_lines(data = rain60_events_L_l347_2022_may,
            x = ~time,
            y = ~rain60)
# Includes several short rainfall events, 


rain60_events_L_l347_2022_jun <- rain60_events_L %>%
  filter(
    between(time,
            ymd_hms("2022-06-11 05:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

plot_ly() %>%
  add_lines(data = rain60_events_L_l347_2022_jun,
            x = ~time,
            y = ~rain60)
# Is a dry period of 7 days


rain60_events_L_full_period_2022_may_jun <- rain60_events_L %>%
  filter(
    between(time,
            ymd_hms("2022-05-27 23:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )
  
plot_ly() %>%
    add_bars(data = rain60_events_L_full_period_2022_may_jun,
              x = ~time,
              y = ~rain60) %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Rainfall, L/hour"))

# Interpolate missing data in period 2022-05-27 23:00:00 to 2022-06-18 14:00:00
# Prepare outflow data:
# Restricted:
l3_outA_smooth_L <- l3_outHR_smooth_L[, c(1, 2)]
l4_outB_smooth_L <- l4_outHR_smooth_L[, c(1, 3)]
l7_outA_smooth_L <- l7_outHR_smooth_L[, c(1, 2)]

l3_outA_smooth_L_for_mb_l347 <- l3_outA_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2022-05-27 23:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l4_outB_smooth_L_for_mb_l347 <- l4_outB_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2022-05-27 23:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l7_outA_smooth_L_for_mb_l347 <- l7_outA_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2022-05-27 23:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l347_out_restr_L_for_mb_l347 <- combine_data(list(l3_outA_smooth_L_for_mb_l347,
                                                  l4_outB_smooth_L_for_mb_l347,
                                                  l7_outA_smooth_L_for_mb_l347))
vis_miss(complete_times(l347_out_restr_L_for_mb_l347))

# Unrestricted:
l3_outB_smooth_L <- l3_outHR_smooth_L[, c(1, 3)]
l4_outA_smooth_L <- l4_outHR_smooth_L[, c(1, 2)]
l7_outB_smooth_L <- l7_outHR_smooth_L[, c(1, 3)]

l3_outB_smooth_L_for_mb_l347 <- l3_outB_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2022-05-27 23:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l4_outA_smooth_L_for_mb_l347 <- l4_outA_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2022-05-27 23:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l7_outB_smooth_L_for_mb_l347 <- l7_outB_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2022-05-27 23:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l347_out_unres_L_for_mb_l347 <- combine_data(list(l3_outB_smooth_L_for_mb_l347,
                                                  l4_outA_smooth_L_for_mb_l347,
                                                  l7_outB_smooth_L_for_mb_l347))
vis_miss(complete_times(l347_out_unres_L_for_mb_l347))


# Prepare Soil Moisture Data:
# Restricted:
l3_smA_int_for_mb_l347 <- l3_all_moist_A_int %>%
  filter(
    between(time,
            ymd_hms("2022-05-27 23:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l4_smB_int_for_mb_l347 <- l4_all_moist_B_int %>%
  filter(
    between(time,
            ymd_hms("2022-05-27 23:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l7_smA_int_for_mb_l347 <- l7_all_moist_A_int %>%
  filter(
    between(time,
            ymd_hms("2022-05-27 23:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l347_sm_restr_L_for_mb_l347 <- combine_data(list(l3_smA_int_for_mb_l347,
                                                 l4_smB_int_for_mb_l347,
                                                 l7_smA_int_for_mb_l347))
vis_miss(complete_times(l347_sm_restr_L_for_mb_l347)) +
  theme(plot.margin = margin(t = 40, r = 50)) +
  ggtitle("Missing Soil Moisture Data from 27th May to 18th June 2022 (Restricted, L3, L4, L7 only)") +
  theme(axis.text.x = element_text(vjust = -0.25))

# Plot soil moisture data for L4:
plot_ly(data = l4_smB_int_for_mb_l347,
        x = ~time) %>%
  add_lines(y = ~l4_moist100_B,
            name = "100 cm Deep Sensor",
            line = list(dash = "solid")) %>%
  add_lines(y = ~l4_moist75_B,
            name = "75 cm Deep Sensor",
            line = list(dash = "solid")) %>%
  add_lines(y = ~l4_moist40_B,
            name = "40 cm Deep Sensor") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Moisture Content, L"))
# Interpolation reasonable for depths 75 cm and 40 cm.
# Error in 100 cm data (all zero in a period including rainfall events,
# and when the gravel layer above (75 cm) has higher moisture).
# Not suitable to use for mass balance.

# Interpolation of 40 cm data, to allow for use in examining soil moisture:
l4_smB_int_for_mb_l347$l4_moist40_B <- na.approx(l4_smB_int_for_mb_l347$l4_moist40_B)
l4_smB_int_for_mb_l347$l4_moist75_B <- na.approx(l4_smB_int_for_mb_l347$l4_moist75_B, rule = 2)
l4_smB_int_for_mb_l347$l4_moist100_B <- na.approx(l4_smB_int_for_mb_l347$l4_moist100_B)



# Unrestricted:
l3_smB_int_for_mb_l347 <- l3_all_moist_B_int %>%
  filter(
    between(time,
            ymd_hms("2022-05-27 23:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l4_smA_int_for_mb_l347 <- l4_all_moist_A_int %>%
  filter(
    between(time,
            ymd_hms("2022-05-27 23:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l7_smB_int_for_mb_l347 <- l7_all_moist_B_int %>%
  filter(
    between(time,
            ymd_hms("2022-05-27 23:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l347_sm_unres_L_for_mb_l347 <- combine_data(list(l3_smB_int_for_mb_l347,
                                                 l4_smA_int_for_mb_l347,
                                                 l7_smB_int_for_mb_l347))

plot_ly(data = l3_smB_int_for_mb_l347,
        x = ~time) %>%
  add_lines(y = ~l3_moist100_B) %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Moisture Content, L"))

vis_miss(complete_times(l347_sm_unres_L_for_mb_l347))

# Interpolate missing data (lysimeter 3 moisture 100 cm depth)
l3_smB_int_for_mb_l347$l3_moist100_B <- na.approx(l3_smB_int_for_mb_l347$l3_moist100_B)

which(is.na(l3_smB_int_for_mb_l347$l3_moist100_B))


## Mass balance (full period):
# Unrestricted:
l3B_for_mb_l347 <- combine_data(list(rain60_events_L_full_period_2022_may_jun,
                                l3_outB_smooth_L_for_mb_l347,
                                l3_smB_int_for_mb_l347))

l4A_for_mb_l347 <- combine_data(list(rain60_events_L_full_period_2022_may_jun,
                                l4_outA_smooth_L_for_mb_l347,
                                l4_smA_int_for_mb_l347))

l7B_for_mb_l347 <- combine_data(list(rain60_events_L_full_period_2022_may_jun,
                                l7_outB_smooth_L_for_mb_l347,
                                l7_smB_int_for_mb_l347))

l3B_mb_l347 <- mass_balance_any_period_cal(l3B_for_mb_l347, c(7:15), 6, 2.617265)
l4A_mb_l347 <- mass_balance_any_period_cal(l4A_for_mb_l347, c(7:15), 6, 2.617265)
l7B_mb_l347 <- mass_balance_any_period_cal(l7B_for_mb_l347, c(7:15), 6, 2.617265)

# Plots:

plot_mass_balance(l3B_mb_l347) %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Volume of Water, L"))
plot_mass_balance(l4A_mb_l347) %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Volume of Water, L"))
plot_mass_balance(l7B_mb_l347) %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Volume of Water, L"))


# Restricted:
l3A_for_mb_l347 <- combine_data(list(rain60_events_L_full_period_2022_may_jun,
                                     l3_outA_smooth_L_for_mb_l347,
                                     l3_smA_int_for_mb_l347))

l4B_for_mb_l347 <- combine_data(list(rain60_events_L_full_period_2022_may_jun,
                                     l4_outB_smooth_L_for_mb_l347,
                                     l4_smB_int_for_mb_l347))

l7A_for_mb_l347 <- combine_data(list(rain60_events_L_full_period_2022_may_jun,
                                     l7_outA_smooth_L_for_mb_l347,
                                     l7_smA_int_for_mb_l347))

l3A_mb_l347 <- mass_balance_any_period_cal(l3A_for_mb_l347, c(7:15), 6, 2.617265)
l4B_mb_l347 <- mass_balance_any_period_cal(l4B_for_mb_l347, c(7:15), 6, 2.617265)
l7A_mb_l347 <- mass_balance_any_period_cal(l7A_for_mb_l347, c(7:15), 6, 2.617265)

# Plots:

plot_mass_balance(l3A_mb_l347) %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Volume of Water, L"))
plot_mass_balance(l4B_mb_l347) %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Volume of Water, L"))
plot_mass_balance(l7A_mb_l347) %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Volume of Water, L"))



## Focus on Dry period after all rainfall events:
## (restricted, l3 and l7 only, as confirmed reliable data for this period):
# Period: 2022-06-08 11:00:00 to 2022-06-18 14:00:00

l3A_for_mb_l347_dry <- l3A_for_mb_l347 %>%
  filter(
    between(time,
            ymd_hms("2022-06-08 11:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l4B_for_mb_l347_dry <- l4B_for_mb_l347 %>%
  filter(
    between(time,
            ymd_hms("2022-06-08 11:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l7A_for_mb_l347_dry <- l7A_for_mb_l347 %>%
  filter(
    between(time,
            ymd_hms("2022-06-08 11:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l3A_mb_l347_dry <- mass_balance_any_period_cal(l3A_for_mb_l347_dry, c(7:15), 6, 2.617265)
l4B_mb_l347_dry <- mass_balance_any_period_cal(l4B_for_mb_l347_dry, c(7:15), 6, 2.617265)
l7A_mb_l347_dry <- mass_balance_any_period_cal(l7A_for_mb_l347_dry, c(7:15), 6, 2.617265)


l3_et15_l347_dry <- l3_et15 %>%
  filter(
    between(time,
            ymd_hms("2022-06-08 11:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l3_et15_l347_dry_L <- convert_ml_l(convert_mm_ml(l3_et15_l347_dry, 2), 2)

l4_et15_l347_dry <- l4_et15 %>%
  filter(
    between(time,
            ymd_hms("2022-06-08 11:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l4_et15_l347_dry_L <- convert_ml_l(convert_mm_ml(l4_et15_l347_dry, 2), 2)

l7.8_et15_l347_dry <- l7.8_et15 %>%
  filter(
    between(time,
            ymd_hms("2022-06-08 11:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

l7.8_et15_l347_dry_L <- convert_ml_l(convert_mm_ml(l7.8_et15_l347_dry, 2), 2)



plot_mass_balance(l3A_mb_l347_dry) %>%
  add_lines(data = l3_et15_l347_dry_L,
            y = ~cumsum(l3_et15),
            name = "PET Estimate (Bare Earth), Penman-Montieth") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Volume of Water, L"))

plot_mass_balance(l4B_mb_l347_dry) %>%
  add_lines(data = l4_et15_l347_dry_L,
            y = ~cumsum(l4_et15),
            name = "PET Estimate (Bare Earth), Penman-Montieth") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Volume of Water, L"))

plot_mass_balance(l7A_mb_l347_dry) %>%
  add_lines(data = l7.8_et15_l347_dry_L,
            y = ~cumsum(l7.8_et15),
            name = "PET Estimate (Reference Tall Crop), Penman-Montieth") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Volume of Water, L"))



# Lysimeter 3 (bare earth), average ET in L/day:
l347_dry_interval <- interval(l3A_mb_l347_dry[1, "time"], l3A_mb_l347_dry[nrow(l3A_mb_l347_dry), "time"])
l347_dry_interval_len_days <- (l347_dry_interval %/% hours(1)) / 24

l3_daily_et_l347 <- max(l3A_mb_l347_dry$cum_et) / l347_dry_interval_len_days
l3_daily_et_l347

# Lysimeter 4 (Iris sibirica), average ET in L/day:

l4_daily_et_l347 <- max(l4B_mb_l347_dry$cum_et) / l347_dry_interval_len_days
l4_daily_et_l347

# Lysimeter 7 (Iris sibirica), average ET in L/day:

l7_daily_et_l347 <- max(l7A_mb_l347_dry$cum_et) / l347_dry_interval_len_days
l7_daily_et_l347


# % increase in daily ET caused by adding grass:
(l4_daily_et_l347 - l3_daily_et_l347) / l3_daily_et_l347


# % increase in cumulative ET after 10.125 days:
(max(l4B_mb_l347_dry$cum_et) - max(l3A_mb_l347_dry$cum_et)) / max(l3A_mb_l347_dry$cum_et)



# % increase in daily ET caused by adding Iris sibirica:
(l7_daily_et_l347 - l3_daily_et_l347) / l3_daily_et_l347


# % increase in cumulative ET after 10.125 days:
(max(l7A_mb_l347_dry$cum_et) - max(l3A_mb_l347_dry$cum_et)) / max(l3A_mb_l347_dry$cum_et)




# Smooth lines:
l3A_l347_dry_spline <- smooth.spline(x = l3A_mb_l347_dry$time, y = l3A_mb_l347_dry$cum_et)
l3A_l347_dry_spline_time <- as.numeric(l3A_mb_l347_dry$time)
l3A_l347_dry_spline_pred <- predict(l3A_l347_dry_spline, l3A_l347_dry_spline_time)$y

l3A_et_spline_res <- data.frame(time = l3A_mb_l347_dry$time,
                                smooth_cum_et = l3A_l347_dry_spline_pred)

plot_ly() %>%
  add_lines(data = l3A_mb_l347_dry,
            x = ~time,
            y = ~cum_et) %>%
  add_lines(data = l3A_et_spline_res,
            x = ~time,
            y = ~smooth_cum_et)


l4B_l347_dry_spline <- smooth.spline(x = l4B_mb_l347_dry$time, y = l4B_mb_l347_dry$cum_et)
l4B_l347_dry_spline_time <- as.numeric(l4B_mb_l347_dry$time)
l4B_l347_dry_spline_pred <- predict(l4B_l347_dry_spline, l4B_l347_dry_spline_time)$y

l4B_et_spline_res <- data.frame(time = l4B_mb_l347_dry$time,
                                smooth_cum_et = l4B_l347_dry_spline_pred)

plot_ly() %>%
  add_lines(data = l4B_mb_l347_dry,
            x = ~time,
            y = ~cum_et) %>%
  add_lines(data = l4B_et_spline_res,
            x = ~time,
            y = ~smooth_cum_et)


l7A_l347_dry_spline <- smooth.spline(x = l7A_mb_l347_dry$time, y = l7A_mb_l347_dry$cum_et)
l7A_l347_dry_spline_time <- as.numeric(l7A_mb_l347_dry$time)
l7A_l347_dry_spline_pred <- predict(l7A_l347_dry_spline, l7A_l347_dry_spline_time)$y

l7A_et_spline_res <- data.frame(time = l7A_mb_l347_dry$time,
                                smooth_cum_et = l7A_l347_dry_spline_pred)

plot_ly() %>%
  add_lines(data = l7A_mb_l347_dry,
            x = ~time,
            y = ~cum_et) %>%
  add_lines(data = l7A_et_spline_res,
            x = ~time,
            y = ~smooth_cum_et)

# Plot all ET with spline:

plot_ly() %>%
  add_lines(data = l3A_mb_l347_dry,
            x = ~time,
            y = ~cum_et,
            name = "L3 Restricted, Mass-Balance ET Estimate") %>%
  add_lines(data = l3A_et_spline_res,
            x = ~time,
            y = ~smooth_cum_et,
            name = "L3 Restricted, Mass-Balance ET Estimate") %>%
  add_lines(data = l4B_mb_l347_dry,
            x = ~time,
            y = ~cum_et,
            name = "L4 Restricted, Mass-Balance ET Estimate") %>%
  add_lines(data = l4B_et_spline_res,
            x = ~time,
            y = ~smooth_cum_et,
            name = "L4 Restricted, Mass-Balance ET Estimate") %>%
  add_lines(data = l7A_mb_l347_dry,
            x = ~time,
            y = ~cum_et,
            name = "L7 Restricted, Mass-Balance ET Estimate") %>%
  add_lines(data = l7A_et_spline_res,
            x = ~time,
            y = ~smooth_cum_et,
            name = "L7 Restricted, Mass-Balance ET Estimate") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Cumulative ET, L"))


# Plot just splines:
mb_et_l347_dry <- plot_ly() %>%
  add_lines(data = l3A_et_spline_res,
            x = ~time,
            y = ~smooth_cum_et,
            name = "L3 Restricted, \n Mass-Balance ET Estimate") %>%
  add_lines(data = l4B_et_spline_res,
            x = ~time,
            y = ~smooth_cum_et,
            name = "L4 Restricted, \n Mass-Balance ET Estimate",
            line = list(dash = "dot")) %>%
  add_lines(data = l7A_et_spline_res,
            x = ~time,
            y = ~smooth_cum_et,
            name = "L7 Restricted, \n Mass-Balance ET Estimate",
            line = list(dash = "dashdot")) %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Cumulative ET, L"))

# Plots of each lysimeter ET and PET:
l3A_et_pet <- plot_ly() %>%
  add_lines(data = l3A_et_spline_res,
            x = ~time,
            y = ~smooth_cum_et,
            name = "Mass-Balance ET Estimate",
            showlegend = T,
            legendgroup = "mass-balance",
            line = list(color = "steelblue")) %>%
  add_lines(data = l3_et15_l347_dry_L,
            x = ~time,
            y = ~cumsum(l3_et15),
            name = "PET Estimate for Lysimeter Vegetation Type, \nPenman-Montieth",
            line = list(dash = "dash", color = "orange"),
            showlegend = T,
            legendgroup = "pet") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Lysimeter 3 \n Volume of Water, L"))

l4B_et_pet <- plot_ly() %>%
  add_lines(data = l4B_et_spline_res,
            x = ~time,
            y = ~smooth_cum_et,
            name = "L4 Restricted, Mass-Balance ET Estimate",
            legendgroup = "mass-balance",
            showlegend = F,
            line = list(color = "steelblue")) %>%
  add_lines(data = l4_et15_l347_dry_L,
            x = ~time,
            y = ~cumsum(l4_et15),
            name = "PET Estimate (Reference Grass), Penman-Montieth",
            line = list(dash = "dash", color = "orange"),
            legendgroup = "pet",
            showlegend = F) %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Lysimeter 4 \n Volume of Water, L"))

l7A_et_pet <- plot_ly() %>%
  add_lines(data = l7A_et_spline_res,
            x = ~time,
            y = ~smooth_cum_et,
            name = "L7 Restricted, Mass-Balance ET Estimate",
            legendgroup = "mass-balance",
            showlegend = F,
            line = list(color = "steelblue")) %>%
  add_lines(data = l7.8_et15_l347_dry_L,
            x = ~time,
            y = ~cumsum(l7.8_et15),
            name = "PET Estimate (Reference Tall Crop), Penman-Montieth",
            line = list(dash = "dash", color = "orange"),
            legendgroup = "pet",
            showlegend = F) %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Lysimeter 7 \n Volume of Water, L"))

l3A_et_pet
l4B_et_pet
l7A_et_pet

subplot(l3A_et_pet, l4B_et_pet, l7A_et_pet, nrows = 3,
        shareY = T, shareX = T) %>%
  layout(legend = list(orientation = "h",
                       x = 0.5,
                       y = -0.15))

# Plot of temperature and solar radiation for same period:
WS_solarHR_l347_dry <- WS_solarHR %>%
  filter(
    between(time,
            ymd_hms("2022-06-08 11:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

WS_tempHR_l347_dry <- WS_tempHR %>%
  filter(
    between(time,
            ymd_hms("2022-06-08 11:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )

solar_temp_l347_dry <- plot_ly(x = ~time) %>%
  add_lines(data = WS_solarHR_l347_dry,
            y = ~WS_solarHR,
            name = "Solar Radiation",
            yaxis = "y") %>%
  add_lines(data = WS_tempHR_l347_dry,
            y = ~WS_tempHR,
            name = "Temperature",
            yaxis = "y2",
            line = list(dash = "dot")) %>%
  layout(yaxis = list(title = "Solar Radiation, W/m²",
                      showgrid = F),
         yaxis2 = list(title = "Temperature, °C", overlaying = "y", side = "right",
                       showgrid = F),
         xaxis = list(title = "Date/Time, hourly intervals"))

solar_temp_l347_dry

# Combine temperature/solar radiation with mass-balance ET:

subplot(solar_temp_l347_dry, mb_et_l347_dry, nrows = 2, 
        shareX = T, shareY = T) %>%
  layout(legend = list(x = 1.18, y = 0.5, xanchor = "left", yanchor = "middle",
                       orientation = "v",
                       x = 0.5,
                       y = -0.5))


# Comparing:
rmse(l3A_mb_l347_dry$cum_et, l4B_mb_l347_dry$cum_et)
rmse(l3A_mb_l347_dry$cum_et, l7A_mb_l347_dry$cum_et)
rmse(l4B_mb_l347_dry$cum_et, l7A_mb_l347_dry$cum_et)

# NSE(sim, obs)
NSE(l3A_mb_l347_dry$cum_et, l4B_mb_l347_dry$cum_et)
NSE(l3A_mb_l347_dry$cum_et, l7A_mb_l347_dry$cum_et)
NSE(l4B_mb_l347_dry$cum_et, l7A_mb_l347_dry$cum_et)

NSE(l4B_mb_l347_dry$cum_et, l3A_mb_l347_dry$cum_et)
NSE(l7A_mb_l347_dry$cum_et, l3A_mb_l347_dry$cum_et)
NSE(l7A_mb_l347_dry$cum_et, l4B_mb_l347_dry$cum_et)

#nse(obs, sim)
nse(l3A_mb_l347_dry$cum_et, l4B_mb_l347_dry$cum_et)
nse(l3A_mb_l347_dry$cum_et, l7A_mb_l347_dry$cum_et)
nse(l4B_mb_l347_dry$cum_et, l7A_mb_l347_dry$cum_et)

r_squared(l3A_mb_l347_dry$cum_et, l4B_mb_l347_dry$cum_et)
r_squared(l3A_mb_l347_dry$cum_et, l7A_mb_l347_dry$cum_et)
r_squared(l4B_mb_l347_dry$cum_et, l7A_mb_l347_dry$cum_et)

# hydroGOF::R2 function gives the same output as hydroGOF::NSE
R2(l4B_mb_l347_dry$cum_et, l3A_mb_l347_dry$cum_et)
R2(l7A_mb_l347_dry$cum_et, l3A_mb_l347_dry$cum_et)
R2(l7A_mb_l347_dry$cum_et, l4B_mb_l347_dry$cum_et)


KGE(l3A_mb_l347_dry$cum_et, l4B_mb_l347_dry$cum_et)
KGE(l3A_mb_l347_dry$cum_et, l7A_mb_l347_dry$cum_et)
KGE(l4B_mb_l347_dry$cum_et, l7A_mb_l347_dry$cum_et)

which(l3A_mb_l347_dry$cum_et < 0)
which(l4B_mb_l347_dry$cum_et < 0)
which(l7A_mb_l347_dry$cum_et < 0)

# Remove first two negative ET values for KGE:
KGE(l3A_mb_l347_dry$cum_et[-c(1, 2)], l4B_mb_l347_dry$cum_et[-c(1, 2)])
KGE(l3A_mb_l347_dry$cum_et[-c(1, 2)], l7A_mb_l347_dry$cum_et[-c(1, 2)])
KGE(l4B_mb_l347_dry$cum_et[-c(1, 2)], l7A_mb_l347_dry$cum_et[-c(1, 2)])


## Plotting ratio of cumulative PET reached at each time:
l3_etHR_l347_dry_L <- aggregate_data(l3_et15_l347_dry_L, 15, 60, 2)
colnames(l3_etHR_l347_dry_L)[2] <- "l3_etHR"

plot_ly(x = ~time) %>%
  add_lines(data = l3_et15_l347_dry_L,
            y = ~cumsum(l3_et15)) %>%
  add_lines(data = l3_etHR_l347_dry_L,
            y = ~cumsum(l3_etHR))

l4_etHR_l347_dry_L <- aggregate_data(l4_et15_l347_dry_L, 15, 60, 2)
colnames(l4_etHR_l347_dry_L)[2] <- "l4_etHR"

plot_ly(x = ~time) %>%
  add_lines(data = l4_et15_l347_dry_L,
            y = ~cumsum(l4_et15)) %>%
  add_lines(data = l4_etHR_l347_dry_L,
            y = ~cumsum(l4_etHR))

l7.8_etHR_l347_dry_L <- aggregate_data(l7.8_et15_l347_dry_L, 15, 60, 2)
colnames(l7.8_etHR_l347_dry_L)[2] <- "l7.8_etHR"

plot_ly(x = ~time) %>%
  add_lines(data = l7.8_et15_l347_dry_L,
            y = ~cumsum(l7.8_et15)) %>%
  add_lines(data = l7.8_etHR_l347_dry_L,
            y = ~cumsum(l7.8_etHR))


l3A_mb_l347_dry_et_ratios <- cbind(l3A_mb_l347_dry, cumsum(l3_etHR_l347_dry_L$l3_etHR))
l4B_mb_l347_dry_et_ratios <- cbind(l4B_mb_l347_dry, cumsum(l4_etHR_l347_dry_L$l4_etHR))
l7A_mb_l347_dry_et_ratios <- cbind(l7A_mb_l347_dry, cumsum(l7.8_etHR_l347_dry_L$l7.8_etHR))

l3A_mb_l347_dry_et_ratios$AET_over_PET <- l3A_mb_l347_dry_et_ratios$cum_et / l3A_mb_l347_dry_et_ratios$`cumsum(l3_etHR_l347_dry_L$l3_etHR)`
l4B_mb_l347_dry_et_ratios$AET_over_PET <- l4B_mb_l347_dry_et_ratios$cum_et / l4B_mb_l347_dry_et_ratios$`cumsum(l4_etHR_l347_dry_L$l4_etHR)`
l7A_mb_l347_dry_et_ratios$AET_over_PET <- l7A_mb_l347_dry_et_ratios$cum_et / l7A_mb_l347_dry_et_ratios$`cumsum(l7.8_etHR_l347_dry_L$l7.8_etHR)`


plot_ly(x = ~time,
        y = ~AET_over_PET) %>%
  add_lines(data = l3A_mb_l347_dry_et_ratios,
            name = "Lysimeter 3 (Restricted)") %>%
  add_lines(data = l4B_mb_l347_dry_et_ratios,
            name = "Lysimeter 4 (Restricted)") %>%
  add_lines(data = l7A_mb_l347_dry_et_ratios,
            name = "Lysimeter 7 (Restricted)")


# Average AET/PET for each between 13th June and 18th June (stable part of graph):
l3A_mb_l347_dry_et_ratios_subset <- l3A_mb_l347_dry_et_ratios %>%
  filter(
    between(time,
            ymd_hms("2022-06-13 00:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 00:00:00", tz = "UTC"))
  )

l4B_mb_l347_dry_et_ratios_subset <- l4B_mb_l347_dry_et_ratios %>%
  filter(
    between(time,
            ymd_hms("2022-06-13 00:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 00:00:00", tz = "UTC"))
  )

l7A_mb_l347_dry_et_ratios_subset <- l7A_mb_l347_dry_et_ratios %>%
  filter(
    between(time,
            ymd_hms("2022-06-13 00:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 00:00:00", tz = "UTC"))
  )

mean(l3A_mb_l347_dry_et_ratios_subset$AET_over_PET)
sd(l3A_mb_l347_dry_et_ratios_subset$AET_over_PET)
mean(l4B_mb_l347_dry_et_ratios_subset$AET_over_PET)
sd(l4B_mb_l347_dry_et_ratios_subset$AET_over_PET)
mean(l7A_mb_l347_dry_et_ratios_subset$AET_over_PET)
sd(l7A_mb_l347_dry_et_ratios_subset$AET_over_PET)


## Examining ET rate in dry period for L3 and L7 (restricted) as a function of moisture in
## the soil (only):

# Calculate ET rate (hourly estimation):
# Add change in soil moisture since last timestep:
l3A_mb_l347_dry_et_rate <- l3A_mb_l347_dry
l3A_mb_l347_dry_et_rate$hourly_change_sm <- NA
for (i in 1:nrow(l3A_mb_l347_dry_et_rate)){
  if (i == 1){
    l3A_mb_l347_dry_et_rate[i, "hourly_change_sm"] <- 0
  }
  else{
    l3A_mb_l347_dry_et_rate[i, "hourly_change_sm"] <- (l3A_mb_l347_dry_et_rate[i, "total_moisture"] - l3A_mb_l347_dry_et_rate[(i - 1), "total_moisture"]) / 2.617265
  }
}

l3A_mb_l347_dry_et_rate$hourly_et_est <- NA
for (i in 1:nrow(l3A_mb_l347_dry_et_rate)){
  l3A_mb_l347_dry_et_rate[i, "hourly_et_est"] <- l3A_mb_l347_dry_et_rate[i, 2] - l3A_mb_l347_dry_et_rate[i, 6] - l3A_mb_l347_dry_et_rate[i, "hourly_change_sm"]
}

l3A_mb_l347_dry_et_rate$cum_hourly_et_est <- cumsum(l3A_mb_l347_dry_et_rate$hourly_et_est)


plot_ly(data = l3A_mb_l347_dry_et_rate, x = ~time) %>%
  add_lines(y = ~cum_et) %>%
  add_lines(y = ~cum_hourly_et_est)

l4B_mb_l347_dry_et_rate <- l4B_mb_l347_dry
l4B_mb_l347_dry_et_rate$hourly_change_sm <- NA
for (i in 1:nrow(l4B_mb_l347_dry_et_rate)){
  if (i == 1){
    l4B_mb_l347_dry_et_rate[i, "hourly_change_sm"] <- 0
  }
  else{
    l4B_mb_l347_dry_et_rate[i, "hourly_change_sm"] <- (l4B_mb_l347_dry_et_rate[i, "total_moisture"] - l4B_mb_l347_dry_et_rate[(i - 1), "total_moisture"]) / 2.617265
  }
}

l4B_mb_l347_dry_et_rate$hourly_et_est <- NA
for (i in 1:nrow(l4B_mb_l347_dry_et_rate)){
  l4B_mb_l347_dry_et_rate[i, "hourly_et_est"] <- l4B_mb_l347_dry_et_rate[i, 2] - l4B_mb_l347_dry_et_rate[i, 6] - l4B_mb_l347_dry_et_rate[i, "hourly_change_sm"]
}

l4B_mb_l347_dry_et_rate$cum_hourly_et_est <- cumsum(l4B_mb_l347_dry_et_rate$hourly_et_est)


plot_ly(data = l4B_mb_l347_dry_et_rate, x = ~time) %>%
  add_lines(y = ~cum_et) %>%
  add_lines(y = ~cum_hourly_et_est)


l7A_mb_l347_dry_et_rate <- l7A_mb_l347_dry
l7A_mb_l347_dry_et_rate$hourly_change_sm <- NA
for (i in 1:nrow(l7A_mb_l347_dry_et_rate)){
  if (i == 1){
    l7A_mb_l347_dry_et_rate[i, "hourly_change_sm"] <- 0
  }
  else{
    l7A_mb_l347_dry_et_rate[i, "hourly_change_sm"] <- (l7A_mb_l347_dry_et_rate[i, "total_moisture"] - l7A_mb_l347_dry_et_rate[(i - 1), "total_moisture"]) / 2.617265
  }
}

l7A_mb_l347_dry_et_rate$hourly_et_est <- NA
for (i in 1:nrow(l7A_mb_l347_dry_et_rate)){
  l7A_mb_l347_dry_et_rate[i, "hourly_et_est"] <- l7A_mb_l347_dry_et_rate[i, 2] - l7A_mb_l347_dry_et_rate[i, 6] - l7A_mb_l347_dry_et_rate[i, "hourly_change_sm"]
}

l7A_mb_l347_dry_et_rate$cum_hourly_et_est <- cumsum(l7A_mb_l347_dry_et_rate$hourly_et_est)


plot_ly(data = l7A_mb_l347_dry_et_rate, x = ~time) %>%
  add_lines(y = ~cum_et) %>%
  add_lines(y = ~cum_hourly_et_est)




# Plot hourly ET rates against total moisture (soil only):

plot_ly(data = l3A_mb_l347_dry_et_rate,
        x = ~total_moisture_soil_only) %>%
  add_lines(y = ~hourly_et_est)

plot_ly(data = l4B_mb_l347_dry_et_rate,
        x = ~total_moisture_soil_only) %>%
  add_lines(y = ~hourly_et_est)

plot_ly(data = l7A_mb_l347_dry_et_rate,
        x = ~total_moisture_soil_only) %>%
  add_lines(y = ~hourly_et_est)



# Calculate the strength of the relationship between


plot_ly(x = ~total_moisture_soil_only,
        y = ) %>%
  add_lines(data = l3A_mb_l347_dry)




