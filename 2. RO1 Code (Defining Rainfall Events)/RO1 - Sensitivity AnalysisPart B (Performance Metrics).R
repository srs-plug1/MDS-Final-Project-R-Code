out_restr_HR_smooth <- combine_data(list(l3_outHR_smooth_A,
                                         l4_outHR_smooth_B,
                                         l7_outHR_smooth_A,
                                         l8_outHR_smooth_A))
out_unrestr_HR_smooth <- combine_data(list(l3_outHR_smooth_B,
                                           l4_outHR_smooth_A,
                                           l7_outHR_smooth_B,
                                           l8_outHR_smooth_B))

total_restr_moist_HR_all <- combine_data(list(l3_total_moist_A_int, l4_total_moist_B_int,
                                              l7_total_moist_A_int, l8_total_moist_A_int))
total_unrestr_moist_HR_all <- combine_data(list(l3_total_moist_B_int, l4_total_moist_A_int,
                                                l7_total_moist_B_int, l8_total_moist_B_int))

sens_perf_met <- sensitivity_perf_metrics(rain60_ave, total_unrestr_moist_HR_all, out_unrestr_HR_smooth, c(6, 12, 18, 24, 30, 36, 42, 48), 60)


plot_ly() %>%
  add_boxplot(data = sens_perf_met,
              x = ~mit,
              y = ~l3_initial_moisture) %>%
  layout(xaxis = list(title = "MIT Value"),
         yaxis = list(title = "Initial Soil Moisture Content, L"))

plot_ly() %>%
  add_boxplot(data = sens_perf_met,
              x = ~mit,
              y = ~l4_initial_moisture) %>%
  layout(title = "Lysimeter 4 (Unrestricted), Initial Moisture Content, L")

plot_ly() %>%
  add_boxplot(data = sens_perf_met,
              x = ~mit,
              y = ~l7_initial_moisture) %>%
  layout(title = "Lysimeter 7 (Unrestricted), Initial Moisture Content, L")

plot_ly() %>%
  add_boxplot(data = sens_perf_met,
              x = ~mit,
              y = ~l8_initial_moisture) %>%
  layout(title = "Lysimeter 8 (Unrestricted), Initial Moisture Content, L")



plot_ly() %>%
  add_boxplot(data = sens_perf_met,
              x = ~mit,
              y = ~l3_peak_att) %>%
  layout(yaxis = list(title = "Peak Flow Attenuation",
                      range = c(0, 1)),
         xaxis = list(title = "MIT Value"))

plot_ly() %>%
  add_boxplot(data = sens_perf_met,
              x = ~mit,
              y = ~l4_peak_att) %>%
  layout(yaxis = list(title = "Peak Flow Attenuation",
                      range = c(0, 1)),
         xaxis = list(title = "MIT Value"))

plot_ly() %>%
  add_boxplot(data = sens_perf_met,
              x = ~mit,
              y = ~l7_peak_att) %>%
  layout(yaxis = list(title = "Peak Flow Attenuation",
                      range = c(0, 1)),
         xaxis = list(title = "MIT Value"))

plot_ly() %>%
  add_boxplot(data = sens_perf_met,
              x = ~mit,
              y = ~l8_peak_att) %>%
  layout(yaxis = list(title = "Peak Flow Attenuation",
                      range = c(0, 1)),
         xaxis = list(title = "MIT Value"))

# Restricted:
sens_perf_met_r <- sensitivity_perf_metrics(rain60_ave, total_restr_moist_HR_all, out_restr_HR_smooth, c(6, 12, 18, 24, 30, 36, 42, 48), 60)





plot_ly() %>%
  add_boxplot(data = sens_perf_met_r,
              x = ~mit,
              y = ~l3_initial_moisture) %>%
  layout(title = "Lysimeter 3 (Restricted), Initial Moisture Content, L")

plot_ly() %>%
  add_boxplot(data = sens_perf_met_r,
              x = ~mit,
              y = ~l4_initial_moisture) %>%
  layout(title = "Lysimeter 4 (Restricted), Initial Moisture Content, L")

plot_ly() %>%
  add_boxplot(data = sens_perf_met_r,
              x = ~mit,
              y = ~l7_initial_moisture) %>%
  layout(title = "Lysimeter 7 (Restricted), Initial Moisture Content, L")

plot_ly() %>%
  add_boxplot(data = sens_perf_met_r,
              x = ~mit,
              y = ~l8_initial_moisture) %>%
  layout(title = "Lysimeter 8 (Restricted), Initial Moisture Content, L")




plot_ly() %>%
  add_boxplot(data = sens_perf_met_r,
              x = ~mit,
              y = ~l3_peak_att) %>%
  layout(yaxis = list(title = "Peak Flow Attenuation",
                      range = c(0, 1)),
         xaxis = list(title = "MIT Value"))

plot_ly() %>%
  add_boxplot(data = sens_perf_met_r,
              x = ~mit,
              y = ~l4_peak_att) %>%
  layout(yaxis = list(title = "Peak Flow Attenuation",
                      range = c(0, 1)),
         xaxis = list(title = "MIT Value"))

plot_ly() %>%
  add_boxplot(data = sens_perf_met_r,
              x = ~mit,
              y = ~l7_peak_att) %>%
  layout(yaxis = list(title = "Peak Flow Attenuation",
                      range = c(0, 1)),
         xaxis = list(title = "MIT Value"))

plot_ly() %>%
  add_boxplot(data = sens_perf_met_r,
              x = ~mit,
              y = ~l8_peak_att) %>%
  layout(yaxis = list(title = "Peak Flow Attenuation",
                      range = c(0, 1)),
         xaxis = list(title = "MIT Value"))
