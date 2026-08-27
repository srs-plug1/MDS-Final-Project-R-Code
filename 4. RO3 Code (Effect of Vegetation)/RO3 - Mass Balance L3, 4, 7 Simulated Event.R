# Examining availability of data for simulated event (11th May 2022)
# Restricted outflow lysimeters: 
# L3A (14:00 – 15:00), L4B (13:45 – 14:45), L7A (16:40 – 17:40)

# Rainfall data in the following 10 days:

rain60_ave_L_sim_plus_10 <- rain60_ave_L %>%
  filter(between(time,
                 ymd_hms("2022-05-11 00:00:00", tz = "UTC"),
                 ymd_hms("2022-05-21 18:00:00", tz = "UTC")))

plot_ly() %>%
  add_bars(data = rain60_ave_L_sim_plus_10,
           x = ~time,
           y = ~rain60)

# There is a 4-day dry period following the May simulated events
# Natural rainfall events begin again on 2022-05-15 22:00:00
# L3A dry period begins 2022-05-11 15:00:00
# End (+ 4 days): 2022-05-15 15:00:00
l3A_sim_out_L <- l3_outA_smooth_L %>%
  filter(between(time,
                 ymd_hms("2022-05-11 15:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 15:00:00", tz = "UTC")))
vis_miss(l3A_sim_out_L)
l3A_sim_sm_L <- l3_all_moist_A_int %>%
  filter(between(time,
                 ymd_hms("2022-05-11 15:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 15:00:00", tz = "UTC")))
vis_miss(l3A_sim_sm_L) # Extrapolation required
plot_ly(data = l3A_sim_sm_L) %>%
  add_lines(x = ~time,
            y = ~l3_moist75_A)
l3A_sim_sm_L$l3_moist75_A <- na.approx(l3A_sim_sm_L$l3_moist75_A, rule = 2)
# Select rain:
l3A_sim_rain60_ave_L <- rain60_ave_L%>%
  filter(between(time,
                 ymd_hms("2022-05-11 15:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 15:00:00", tz = "UTC")))

l3A_for_mb_sim <- combine_data(list(l3A_sim_rain60_ave_L,
                                l3A_sim_out_L,
                                l3A_sim_sm_L))
l3A_mb_sim <- mass_balance_any_period_cal(l3A_for_mb_sim, c(4:12), 3, 2.617265)

plot_ly(data = l3A_mb_sim,
        x = ~time) %>%
  add_lines(y = ~cum_rain,
            name = "Cumulative Rainfall") %>%
  add_lines(y = ~cum_out,
            name = "Cumulative Outflow") %>%
  add_lines(y = ~rel_sm,
            name = "Change in Soil Moisture Compared to Period Start") %>%
  add_lines(y = ~rel_sm_scaled,
            name = "Change in Soil Moisture Compared to Period Start (calibrated)")  %>%
  add_lines(y = ~cum_et,
            name = "Mass Balance ET Estimate \n (Calibrated Soil Moisture Used)") %>%
  layout(xaxis = list(title ="Date/Time, hourly intervals"),
         yaxis = list(title = "Volume of Water, L"))

# Plotting from stable outflow/soil moisture change (Begins 2022-05-13 00:00:00):
l3A_sim_out_L_st <- l3_outA_smooth_L %>%
  filter(between(time,
                 ymd_hms("2022-05-13 00:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 15:00:00", tz = "UTC")))
l3A_sim_sm_L_st <- l3_all_moist_A_int %>%
  filter(between(time,
                 ymd_hms("2022-05-13 00:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 15:00:00", tz = "UTC")))
# Select rain:
l3A_sim_rain60_ave_L_st <- rain60_ave_L %>%
  filter(between(time,
                 ymd_hms("2022-05-13 00:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 15:00:00", tz = "UTC")))

l3A_for_mb_sim_st <- combine_data(list(l3A_sim_rain60_ave_L_st,
                                    l3A_sim_out_L_st,
                                    l3A_sim_sm_L_st))
l3A_mb_sim_st <- mass_balance_any_period_cal(l3A_for_mb_sim_st, c(4:12), 3, 2.617265)

l3_etHR_L_sim_st <- l3_etHR_L %>%
  filter(between(time,
                 ymd_hms("2022-05-13 00:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 15:00:00", tz = "UTC")))



plot_ly(data = l3A_mb_sim_st,
        x = ~time) %>%
  add_lines(y = ~cum_rain,
            name = "Cumulative Rainfall") %>%
  add_lines(y = ~cum_out,
            name = "Cumulative Outflow") %>%
  add_lines(y = ~rel_sm,
            name = "Change in Soil Moisture Compared to Period Start") %>%
  add_lines(y = ~rel_sm_scaled,
            name = "Change in Soil Moisture Compared to Period Start (calibrated)")  %>%
  add_lines(y = ~cum_et,
            name = "Mass Balance ET Estimate") %>%
  add_lines(data = l3_etHR_L_sim_st,
            y = ~cumsum(l3_etHR))

# Plotting just AET and PET:
l3_sim_aet_pet <- plot_ly(x = ~time) %>%
  add_lines(data = l3A_mb_sim_st,
            y = ~cum_et,
            name = "Mass Balance ET Estimate",
            showlegend = T,
            legendgroup = "mass-balance",
            line = list(color = "steelblue")) %>%
  add_lines(data = l3_etHR_L_sim_st,
            y = ~cumsum(l3_etHR),
            line = list(dash = "dot", color = "orange"),
            name = "PET Estimate (Bare Earth), Penman-Montieth",
            showlegend = T,
            legendgroup = "pet") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Lysimeter 3 \n Volume of Water, L"))


# L4B dry period begins 2022-05-11 14:45:00, to the nearest hour: 15:00:00
# End (+4 days): 2022-05-15 15:00:00
l4B_sim_out_L <- l4_outB_smooth_L %>%
  filter(between(time,
                 ymd_hms("2022-05-11 15:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 15:00:00", tz = "UTC")))
vis_miss(l4B_sim_out_L)
l4B_sim_sm_L <- l4_all_moist_B_int %>%
  filter(between(time,
                 ymd_hms("2022-05-11 15:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 15:00:00", tz = "UTC")))
vis_miss(l4B_sim_sm_L) # Insufficient soil moisture data available



# L7A dry period begins 2022-05-11 17:40:00, to the nearest hour: 18:00:00
# End (+4 days): 2022-05-15 18:00:00
l7A_sim_out_L <- l7_outA_smooth_L %>%
  filter(between(time,
                 ymd_hms("2022-05-11 18:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 18:00:00", tz = "UTC")))
vis_miss(l7A_sim_out_L)
l7A_sim_sm_L <- l7_all_moist_A_int %>%
  filter(between(time,
                 ymd_hms("2022-05-11 18:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 18:00:00", tz = "UTC")))
vis_miss(l7A_sim_sm_L) # Interpolation required
# Using earlier data:
l7A_sim_sm_L_for_int <- l7_all_moist_A_int %>%
  filter(between(time,
                 ymd_hms("2022-05-11 10:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 18:00:00", tz = "UTC")))

plot_ly(data = l7A_sim_sm_L_for_int) %>%
  add_lines(x = ~time,
            y = ~l7_moist75_A)
l7A_sim_sm_L_for_int$l7_moist75_A <- na.approx(l7A_sim_sm_L_for_int$l7_moist75_A)

# Re-select correct period:
l7A_sim_sm_L <- l7A_sim_sm_L_for_int %>%
  filter(between(time,
                 ymd_hms("2022-05-11 18:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 18:00:00", tz = "UTC")))

# Select rain:
l7A_sim_rain60_ave_L <- rain60_ave_L%>%
  filter(between(time,
                 ymd_hms("2022-05-11 18:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 18:00:00", tz = "UTC")))

l7A_for_mb_sim <- combine_data(list(l7A_sim_rain60_ave_L,
                                    l7A_sim_out_L,
                                    l7A_sim_sm_L))
l7A_mb_sim <- mass_balance_any_period_cal(l7A_for_mb_sim, c(4:12), 3, 2.617265)

plot_ly(data = l7A_mb_sim,
        x = ~time) %>%
  add_lines(y = ~cum_rain,
            name = "Cumulative Rainfall") %>%
  add_lines(y = ~cum_out,
            name = "Cumulative Outflow") %>%
  add_lines(y = ~rel_sm,
            name = "Change in Soil Moisture Compared to Period Start") %>%
  add_lines(y = ~rel_sm_scaled,
            name = "Change in Soil Moisture Compared to Period Start (calibrated)")  %>%
  add_lines(y = ~cum_et,
            name = "Mass Balance ET Estimate") %>%
  layout(xaxis = list(title ="Date/Time, hourly intervals"),
         yaxis = list(title = "Volume of Water, L"))



# Plotting from stable outflow/soil moisture change (Begins 2022-05-13 14:00:00):
l7A_sim_out_L_st <- l7_outA_smooth_L %>%
  filter(between(time,
                 ymd_hms("2022-05-13 14:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 18:00:00", tz = "UTC")))
l7A_sim_sm_L_st <- l7_all_moist_A_int %>%
  filter(between(time,
                 ymd_hms("2022-05-13 14:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 18:00:00", tz = "UTC")))
# Select rain:
l7A_sim_rain60_ave_L_st <- rain60_ave_L %>%
  filter(between(time,
                 ymd_hms("2022-05-13 14:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 18:00:00", tz = "UTC")))

l7A_for_mb_sim_st <- combine_data(list(l7A_sim_rain60_ave_L_st,
                                       l7A_sim_out_L_st,
                                       l7A_sim_sm_L_st))
l7A_mb_sim_st <- mass_balance_any_period_cal(l7A_for_mb_sim_st, c(4:12), 3, 2.617265)

l7.8_etHR_L_sim_st <- l7.8_etHR_L %>%
  filter(between(time,
                 ymd_hms("2022-05-13 14:00:00", tz = "UTC"),
                 ymd_hms("2022-05-15 18:00:00", tz = "UTC")))



plot_ly(data = l7A_mb_sim_st,
        x = ~time) %>%
  add_lines(y = ~cum_rain,
            name = "Cumulative Rainfall") %>%
  add_lines(y = ~cum_out,
            name = "Cumulative Outflow") %>%
  add_lines(y = ~rel_sm,
            name = "Change in Soil Moisture Compared to Period Start") %>%
  add_lines(y = ~rel_sm_scaled,
            name = "Change in Soil Moisture Compared to Period Start (calibrated)")  %>%
  add_lines(y = ~cum_et,
            name = "Mass Balance ET Estimate") %>%
  add_lines(data = l7.8_etHR_L_sim_st,
            y = ~cumsum(l7.8_etHR),
            name = "PET, Penman-Monteith Estimate (Reference Tall Crop)")


# Plotting just AET and PET:
l7_sim_aet_pet <- plot_ly(x = ~time) %>%
  add_lines(data = l7A_mb_sim_st,
            y = ~cum_et,
            name = "Mass Balance ET Estimate",
            showlegend = F,
            legendgroup = "mass-balance",
            line = list(color = "steelblue")) %>%
  add_lines(data = l7.8_etHR_L_sim_st,
            y = ~cumsum(l7.8_etHR),
            line = list(dash = "dot", color = "orange"),
            name = "PET Estimate (Bare Earth), Penman-Montieth",
            showlegend = F,
            legendgroup = "pet") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Lysimeter 7 \n Volume of Water, L"))

subplot(l3_sim_aet_pet, l7_sim_aet_pet, nrows = 2,
        shareY = T, shareX = T) %>%
  layout(legend = list(orientation = "h",
                       x = 0.4,
                       y = -0.15))
