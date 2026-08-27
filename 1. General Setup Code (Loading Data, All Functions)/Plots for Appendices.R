## Plotting missing data: ##
# Rain and Outflow

vis_miss(rain5) +
  theme(plot.margin = margin(t = 40)) +
  ggtitle("Missing Data in 5-minute Rainfall Data (Bioretention Cell Lysimeters)") +
  theme(axis.text.x = element_text(vjust = -0.25))

vis_miss(rain60) +
  theme(plot.margin = margin(t = 40)) +
  ggtitle("Missing Data in 60-minute Rainfall Data (Bioretention Cell Lysimeters)") +
  theme(axis.text.x = element_text(vjust = -0.25))

vis_miss(outHR) +
  theme(plot.margin = margin(t = 40)) +
  ggtitle("Missing Data in Outflow Data") +
  theme(axis.text.x = element_text(vjust = -0.25))



# Soil Moisture/VWC Before and After Interpolation

vis_miss(l3_all_moist_A_HR) +
  theme(plot.margin = margin(t = 40, r = 40)) +
  ggtitle("Missing Data in VWC Data (Lysimeter 3, Outflow A (Restricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))
vis_miss(l3_all_moist_A_HR_int) +
  theme(plot.margin = margin(t = 40, r= 40)) +
  ggtitle("Missing Data in Interpolated VWC Data (Lysimeter 3, Outflow A (Restricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))

vis_miss(l3_all_moist_B_HR) +
  theme(plot.margin = margin(t = 40, r = 40)) +
  ggtitle("Missing Data in VWC Data (Lysimeter 3, Outflow B (Unrestricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))
vis_miss(l3_all_moist_B_HR_int) +
  theme(plot.margin = margin(t = 40, r= 40)) +
  ggtitle("Missing Data in Interpolated VWC Data (Lysimeter 3, Outflow B (Unrestricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))

vis_miss(l4_all_moist_A_HR) +
  theme(plot.margin = margin(t = 40, r = 40)) +
  ggtitle("Missing Data in VWC Data (Lysimeter 4, Outflow A (Unrestricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))
vis_miss(l4_all_moist_A_HR_int) +
  theme(plot.margin = margin(t = 40, r= 40)) +
  ggtitle("Missing Data in Interpolated VWC Data (Lysimeter 4, Outflow A (Unrestricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))

vis_miss(l4_all_moist_B_HR) +
  theme(plot.margin = margin(t = 40, r = 40)) +
  ggtitle("Missing Data in VWC Data (Lysimeter 4, Outflow B (Restricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))
vis_miss(l4_all_moist_B_HR_int) +
  theme(plot.margin = margin(t = 40, r= 40)) +
  ggtitle("Missing Data in Interpolated VWC Data (Lysimeter 4, Outflow B (Restricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))


vis_miss(l7_all_moist_A_HR) +
  theme(plot.margin = margin(t = 40, r = 40)) +
  ggtitle("Missing Data in VWC Data (Lysimeter 7, Outflow A (Restricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))
vis_miss(l7_all_moist_A_HR_int) +
  theme(plot.margin = margin(t = 40, r= 40)) +
  ggtitle("Missing Data in Interpolated VWC Data (Lysimeter 7, Outflow A (Restricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))

vis_miss(l8_all_moist_B_HR) +
  theme(plot.margin = margin(t = 40, r = 40)) +
  ggtitle("Missing Data in VWC Data (Lysimeter 8, Outflow B (Unrestricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))
vis_miss(l8_all_moist_B_HR_int) +
  theme(plot.margin = margin(t = 40, r= 40)) +
  ggtitle("Missing Data in Interpolated VWC Data (Lysimeter 8, Outflow B (Unrestricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))

vis_miss(l8_all_moist_A_HR) +
  theme(plot.margin = margin(t = 40, r = 40)) +
  ggtitle("Missing Data in VWC Data (Lysimeter 8, Outflow A (Restricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))
vis_miss(l8_all_moist_A_HR_int) +
  theme(plot.margin = margin(t = 40, r= 40)) +
  ggtitle("Missing Data in Interpolated VWC Data (Lysimeter 8, Outflow A (Restricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))

vis_miss(l7_all_moist_B_HR) +
  theme(plot.margin = margin(t = 40, r = 40)) +
  ggtitle("Missing Data in VWC Data (Lysimeter 7, Outflow B (Unrestricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))
vis_miss(l7_all_moist_B_HR_int) +
  theme(plot.margin = margin(t = 40, r= 40)) +
  ggtitle("Missing Data in Interpolated VWC Data (Lysimeter 7, Outflow B (Unrestricted))") +
  theme(axis.text.x = element_text(vjust = -0.25))


## Plotting Extreme Events: ##
# Event 5:
# Select Data (2020-10-02 19:00:00 to 2020-10-06 03:00:00):
ev5_rain <- rain60_ave[rain60_ave$time >= "2020-10-01 19:00:00"  & rain60_ave$time <= "2020-10-07 03:00:00", ]
ev5_moistL3A <- l3_all_moist_A_int[l3_all_moist_A_int$time >= "2020-10-01 19:00:00"  & l3_all_moist_A_int$time <= "2020-10-07 03:00:00", ]
ev5_moistL3B <- l3_all_moist_B_int[l3_all_moist_B_int$time >= "2020-10-01 19:00:00"  & l3_all_moist_B_int$time <= "2020-10-07 03:00:00", ]
ev5_out <- outHR_smooth[outHR_smooth$time >= "2020-10-01 19:00:00"  & outHR_smooth$time <= "2020-10-07 03:00:00", ]

plot_ly() %>%
  add_trace(data = ev5_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev5_moistL3B,
            x = ~time,
            y = ~l3_moist5_B,
            yaxis = "y2",
            name = "Soil Moisture, 5 cm Depth",
            line = list(color = "red"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3B,
            x = ~time,
            y = ~l3_moist10_B,
            yaxis = "y2",
            name = "Soil Moisture, 10cm Depth",
            line = list(color = "darkorange"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3B,
            x = ~time,
            y = ~l3_moist20_B,
            yaxis = "y2",
            name = "Soil Moisture, 20cm Depth",
            line = list(color = "gold"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3B,
            x = ~time,
            y = ~l3_moist30_B,
            yaxis = "y2",
            name = "Soil Moisture, 30cm Depth",
            line = list(color = "green"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3B,
            x = ~time,
            y = ~l3_moist40_B,
            yaxis = "y2",
            name = "Soil Moisture, 40cm Depth",
            line = list(color = "blue"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3B,
            x = ~time,
            y = ~l3_moist50_B,
            yaxis = "y2",
            name = "Soil Moisture, 50cm Depth",
            line = list(color = "lightsteelblue"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3B,
            x = ~time,
            y = ~l3_moist60_B,
            yaxis = "y2",
            name = "Soil Moisture, 60cm Depth",
            line = list(color = "purple"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3B,
            x = ~time,
            y = ~l3_moist75_B,
            yaxis = "y2",
            name = "Soil Moisture, 75cm Depth",
            line = list(color = "black"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3B,
            x = ~time,
            y = ~l3_moist100_B,
            yaxis = "y2",
            name = "Soil Moisture, 100cm Depth",
            line = list(color = "grey"),
            opacity = 1) %>%
  layout(title = "Event 5 (MIT = 24h) Soil Moisture (Lysimeter 3 Unrestricted)",
         yaxis = list(title = "Rainfall, mm/hour",
                      range = c(20, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 200)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))


plot_ly() %>%
  add_trace(data = ev5_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev5_moistL3A,
            x = ~time,
            y = ~l3_moist5_A,
            yaxis = "y2",
            name = "Soil Moisture, 5 cm Depth",
            line = list(color = "red"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3A,
            x = ~time,
            y = ~l3_moist10_A,
            yaxis = "y2",
            name = "Soil Moisture, 10cm Depth",
            line = list(color = "darkorange"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3A,
            x = ~time,
            y = ~l3_moist20_A,
            yaxis = "y2",
            name = "Soil Moisture, 20cm Depth",
            line = list(color = "gold"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3A,
            x = ~time,
            y = ~l3_moist30_A,
            yaxis = "y2",
            name = "Soil Moisture, 30cm Depth",
            line = list(color = "green"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3A,
            x = ~time,
            y = ~l3_moist40_A,
            yaxis = "y2",
            name = "Soil Moisture, 40cm Depth",
            line = list(color = "blue"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3A,
            x = ~time,
            y = ~l3_moist50_A,
            yaxis = "y2",
            name = "Soil Moisture, 50cm Depth",
            line = list(color = "lightsteelblue"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3A,
            x = ~time,
            y = ~l3_moist60_A,
            yaxis = "y2",
            name = "Soil Moisture, 60cm Depth",
            line = list(color = "purple"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3A,
            x = ~time,
            y = ~l3_moist75_A,
            yaxis = "y2",
            name = "Soil Moisture, 75cm Depth",
            line = list(color = "black"),
            opacity = 1) %>%
  add_lines(data = ev5_moistL3A,
            x = ~time,
            y = ~l3_moist100_A,
            yaxis = "y2",
            name = "Soil Moisture, 100cm Depth",
            line = list(color = "grey"),
            opacity = 1) %>%
  layout(title = "Event 5 (MIT = 24h) Soil Moisture (Lysimeter 3 Restricted)",
         yaxis = list(title = "Rainfall, mm/hour",
                      range = c(20, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 200)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))



plot_ly() %>%
  add_trace(data = ev5_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev5_out,
            x = ~time,
            y = ~l3A,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev5_out,
            x = ~time,
            y = ~l3B,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(9, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 9000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))


plot_ly() %>%
  add_trace(data = ev5_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev5_out,
            x = ~time,
            y = ~l4B,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev5_out,
            x = ~time,
            y = ~l4A,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(9, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 9000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))


plot_ly() %>%
  add_trace(data = ev5_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev5_out,
            x = ~time,
            y = ~l7A,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev5_out,
            x = ~time,
            y = ~l7B,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(9, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 9000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))

plot_ly() %>%
  add_trace(data = ev5_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev5_out,
            x = ~time,
            y = ~l8A,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev5_out,
            x = ~time,
            y = ~l8B,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(9, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 9000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))


# Event 35:
# Select Data (2021-01-18 15:00:00 to 2021-01-21 10:00:00):
ev35_rain <- rain60_ave[rain60_ave$time >= "2021-01-18 15:00:00"  & rain60_ave$time <= "2021-01-21 10:00:00", ]
ev35_moistL3A <- l3_all_moist_A_int[l3_all_moist_A_int$time >= "2021-01-18 15:00:00"  & l3_all_moist_A_int$time <= "2021-01-21 10:00:00", ]
ev35_moistL3B <- l3_all_moist_B_int[l3_all_moist_B_int$time >= "2021-01-18 15:00:00"  & l3_all_moist_B_int$time <= "2021-01-21 10:00:00", ]
ev35_out <- outHR_smooth[outHR_smooth$time >= "2021-01-18 15:00:00"  & outHR_smooth$time <= "2021-01-21 10:00:00", ]

plot_ly() %>%
  add_trace(data = ev35_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev35_out,
            x = ~time,
            y = ~l3A,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev35_out,
            x = ~time,
            y = ~l3B,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(9, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 9000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))


plot_ly() %>%
  add_trace(data = ev35_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev35_out,
            x = ~time,
            y = ~l4B,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev35_out,
            x = ~time,
            y = ~l4A,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(9, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 9000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))


plot_ly() %>%
  add_trace(data = ev35_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev35_out,
            x = ~time,
            y = ~l7A,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev35_out,
            x = ~time,
            y = ~l7B,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(9, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 9000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))

plot_ly() %>%
  add_trace(data = ev35_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev35_out,
            x = ~time,
            y = ~l8A,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev35_out,
            x = ~time,
            y = ~l8B,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(9, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 9000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))



# Event 89:
# Select Data (2021-09-28 14:00:00 to 2021-10-07 00:00:00):
ev89_rain <- rain60_ave[rain60_ave$time >= "2021-09-28 14:00:00"  & rain60_ave$time <= "2021-10-07 00:00:00", ]
ev89_moistL3A <- l3_all_moist_A_int[l3_all_moist_A_int$time >= "2021-09-28 14:00:00"  & l3_all_moist_A_int$time <= "2021-10-07 00:00:00", ]
ev89_moistL3B <- l3_all_moist_B_int[l3_all_moist_B_int$time >= "2021-09-28 14:00:00"  & l3_all_moist_B_int$time <= "2021-10-07 00:00:00", ]
ev89_out <- outHR_smooth[outHR_smooth$time >= "2021-09-28 14:00:00"  & outHR_smooth$time <= "2021-10-07 00:00:00", ]

plot_ly() %>%
  add_trace(data = ev89_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev89_out,
            x = ~time,
            y = ~l3A,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev89_out,
            x = ~time,
            y = ~l3B,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(60, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 60000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))


plot_ly() %>%
  add_trace(data = ev89_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev89_out,
            x = ~time,
            y = ~l4B,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev89_out,
            x = ~time,
            y = ~l4A,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(60, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 60000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))


plot_ly() %>%
  add_trace(data = ev89_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev89_out,
            x = ~time,
            y = ~l7A,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev89_out,
            x = ~time,
            y = ~l7B,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(60, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 60000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))

plot_ly() %>%
  add_trace(data = ev89_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev89_out,
            x = ~time,
            y = ~l8A,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev89_out,
            x = ~time,
            y = ~l8B,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(60, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 60000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))


# Event 104:
# Select Data (2021-11-26 06:00:00 to 2021-12-03 03:00:00):
ev104_rain <- rain60_ave[rain60_ave$time >= "2021-11-26 06:00:00"  & rain60_ave$time <= "2021-12-03 03:00:00", ]
ev104_moistL3A <- l3_all_moist_A_int[l3_all_moist_A_int$time >= "2021-11-26 06:00:00"  & l3_all_moist_A_int$time <= "2021-12-03 03:00:00", ]
ev104_moistL3B <- l3_all_moist_B_int[l3_all_moist_B_int$time >= "2021-11-26 06:00:00"  & l3_all_moist_B_int$time <= "2021-12-03 03:00:00", ]
ev104_out <- outHR_smooth[outHR_smooth$time >= "2021-11-26 06:00:00"  & outHR_smooth$time <= "2021-12-03 03:00:00", ]

plot_ly() %>%
  add_trace(data = ev104_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev104_out,
            x = ~time,
            y = ~l3A,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev104_out,
            x = ~time,
            y = ~l3B,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(15, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 15000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))


plot_ly() %>%
  add_trace(data = ev104_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev104_out,
            x = ~time,
            y = ~l4B,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev104_out,
            x = ~time,
            y = ~l4A,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(15, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 15000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))


plot_ly() %>%
  add_trace(data = ev104_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev104_out,
            x = ~time,
            y = ~l7A,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev104_out,
            x = ~time,
            y = ~l7B,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(15, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 15000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))

plot_ly() %>%
  add_trace(data = ev104_rain,
            x = ~time,
            y = ~rain60,
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = ev104_out,
            x = ~time,
            y = ~l8A,
            yaxis = "y2",
            name = "Restricted Outflow",
            line = list(color = "red",
                        dash = "dash"),
            opacity = 1) %>%
  add_lines(data = ev104_out,
            x = ~time,
            y = ~l8B,
            yaxis = "y2",
            name = "Unrestricted Outflow",
            line = list(color = "green"),
            opacity = 1) %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(15, 0)),
         yaxis2 = list(title = "Soil Moisture, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 15000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"),
         margin = list(r = 150),
         legend = list(x = 1.05, y = 0.5))





## FULL MONITORING PERIOD: ##
# Rainfall with extreme events highlighted
rain60_events_ex <- rain60_events
for (i in 1:nrow(rain60_events_ex)){
  if (rain60_events_ex[i, "event"] %in% c(5, 35, 89, 104) & !is.na(rain60_events_ex[i, "event"])){
    rain60_events_ex[i, "extr"] <- "Extreme"
  }
  else{
    rain60_events_ex[i, "extr"] <- "Not Extreme"
  }
}

colourmap <- setNames(object = c("steelblue", "red"),
                      nm = c("Not Extreme", "Extreme"))

plot_ly(data = rain60_events_ex) %>%
  add_bars(x = ~time,
           y = ~rain60,
           color = ~extr,
           colors = colourmap) %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Rainfall, mm"),
         legend = list(orientation = "v",
                       x = 0.6,
                       y = 0.6))



# Exclude simulated events:
l3_outHR_smooth_no_sim <- l3_outHR_smooth %>%
  mutate(l3A = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                              ymd_hms("2022-05-14 00:00:00")), NA, l3A)) %>%
  mutate(l3B = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                              ymd_hms("2022-05-14 00:00:00")), NA, l3B)) %>%
  mutate(l3A = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                              ymd_hms("2022-08-27 01:00:00")), NA, l3A)) %>%
  mutate(l3B = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                              ymd_hms("2022-08-27 01:00:00")), NA, l3B))

plot_ly(x = ~time) %>%
  add_bars(data = rain60_events,
           y = ~rain60,
           name = "Rainfall",
           yaxis = "y1") %>%
  add_lines(data = l3_outHR_smooth_no_sim,
            y = ~l3A,
            name = "Restricted",
            yaxis = "y2",
            line = list(dash = "dot",
                        color = "red")) %>%
  add_lines(data = l3_outHR_smooth_no_sim,
            y = ~l3B,
            name = "Unrestricted",
            yaxis = "y2") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Rainfall, mm",
                      range = c(50, 0)),
         yaxis2 = list(title = "Outflow, ml",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 50000)))

sub_rain <- plot_ly(x = ~time) %>%
  add_bars(data = rain60_events,
           y = ~rain60,
           name = "Rainfall") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Rainfall, mm",
                      range = c(0, 25),
                      autorange = F))

sub_l3A_out <- plot_ly(x = ~time) %>%
  add_lines(data = l3_outHR_smooth_no_sim,
            y = ~l3A,
            name = "Restricted",
            line = list(dash = "dot",
                        color = "red"))%>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Outflow, ml",
                      range = c(0, 30000),
                      autorange = F))

sub_l3B_out <- plot_ly(x = ~time) %>%
  add_lines(data = l3_outHR_smooth_no_sim,
            y = ~l3B,
            name = "Unrestricted",
            line = list(color = "green"))%>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Outflow, ml",
                      range = c(0, 30000),
                      autorange = F))

subplot(sub_rain, sub_l3A_out, sub_l3B_out, nrows = 3,
        shareX = T, shareY = T) 

# Exclude simulated events:
l3_all_moist_B_int_no_sim <- l3_all_moist_B_int %>%
  mutate(l3_moist20_B = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                              ymd_hms("2022-05-14 00:00:00")), NA, l3_moist20_B)) %>%
  mutate(l3_moist50_B = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                              ymd_hms("2022-05-14 00:00:00")), NA, l3_moist50_B)) %>%
  mutate(l3_moist20_B = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                              ymd_hms("2022-08-27 01:00:00")), NA, l3_moist20_B)) %>%
  mutate(l3_moist50_B = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                              ymd_hms("2022-08-27 01:00:00")), NA, l3_moist50_B))
l4_all_moist_A_int_no_sim <- l4_all_moist_A_int %>%
  mutate(l4_moist20_A = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                                       ymd_hms("2022-05-14 00:00:00")), NA, l4_moist20_A)) %>%
  mutate(l4_moist50_A = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                                       ymd_hms("2022-05-14 00:00:00")), NA, l4_moist50_A)) %>%
  mutate(l4_moist20_A = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                                       ymd_hms("2022-08-27 01:00:00")), NA, l4_moist20_A)) %>%
  mutate(l4_moist50_A = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                                       ymd_hms("2022-08-27 01:00:00")), NA, l4_moist50_A))
l7_all_moist_B_int_no_sim <- l7_all_moist_B_int %>%
  mutate(l7_moist20_B = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                                       ymd_hms("2022-05-14 00:00:00")), NA, l7_moist20_B)) %>%
  mutate(l7_moist50_B = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                                       ymd_hms("2022-05-14 00:00:00")), NA, l7_moist50_B)) %>%
  mutate(l7_moist20_B = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                                       ymd_hms("2022-08-27 01:00:00")), NA, l7_moist20_B)) %>%
  mutate(l7_moist50_B = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                                       ymd_hms("2022-08-27 01:00:00")), NA, l7_moist50_B))
l8_all_moist_B_int_no_sim <- l8_all_moist_B_int %>%
  mutate(l8_moist20_B = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                                       ymd_hms("2022-05-14 00:00:00")), NA, l8_moist20_B)) %>%
  mutate(l8_moist50_B = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                                       ymd_hms("2022-05-14 00:00:00")), NA, l8_moist50_B)) %>%
  mutate(l8_moist20_B = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                                       ymd_hms("2022-08-27 01:00:00")), NA, l8_moist20_B)) %>%
  mutate(l8_moist50_B = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                                       ymd_hms("2022-08-27 01:00:00")), NA, l8_moist50_B))



plot_ly(x = ~time) %>%
  add_bars(data = rain60_events,
           y = ~rain60,
           name = "Rainfall",
           yaxis = "y1") %>%
  add_lines(data = l3_all_moist_B_int_no_sim,
            y = ~l3_moist20_B,
            name = "L3B",
            yaxis = "y2") %>%
  add_lines(data = l4_all_moist_A_int_no_sim,
            y = ~l4_moist20_A,
            name = "L4A",
            yaxis = "y2") %>%
  add_lines(data = l7_all_moist_B_int_no_sim,
            y = ~l7_moist20_B,
            name = "L7B",
            yaxis = "y2") %>%
  add_lines(data = l8_all_moist_B_int_no_sim,
            y = ~l8_moist20_B,
            name = "L8B",
            yaxis = "y2") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Rainfall, mm",
                      range = c(30, 0)),
         yaxis2 = list(title = "Soil Moisture, L",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 120)),
         legend = list(x = 0.25,
                       y = -0.15,
                       orientation = "h"),
         margin = list(r = 100))

plot_ly(x = ~time) %>%
  add_bars(data = rain60_events,
           y = ~rain60,
           name = "Rainfall",
           yaxis = "y1") %>%
  add_lines(data = l3_all_moist_B_int_no_sim,
            y = ~l3_moist50_B,
            name = "L3B",
            yaxis = "y2") %>%
  add_lines(data = l4_all_moist_A_int_no_sim,
            y = ~l4_moist50_A,
            name = "L4A",
            yaxis = "y2") %>%
  add_lines(data = l7_all_moist_B_int_no_sim,
            y = ~l7_moist50_B,
            name = "L7B",
            yaxis = "y2") %>%
  add_lines(data = l8_all_moist_B_int_no_sim,
            y = ~l8_moist50_B,
            name = "L8B",
            yaxis = "y2") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Rainfall, mm",
                      range = c(30, 0)),
         yaxis2 = list(title = "Soil Moisture, L",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 120)),
         legend = list(x = 0.25,
                       y = -0.15,
                       orientation = "h"),
         margin = list(r = 100))

# Rainfall and outflows (Lysimeter 3):
# Remove simulated events:
# Exclude 5th May 12:00:00 to 14th May 00:00:00 and from 24th August 11:00:00
# to 2022-08-27 01:00:00
l3_outHR_smooth_no_sim <- l3_outHR_smooth %>%
  mutate(l3A = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                              ymd_hms("2022-05-14 00:00:00")), NA, l3A)) %>%
  mutate(l3B = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                              ymd_hms("2022-05-14 00:00:00")), NA, l3B)) %>%
  mutate(l3A = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                              ymd_hms("2022-08-27 01:00:00")), NA, l3A)) %>%
  mutate(l3B = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                              ymd_hms("2022-08-27 01:00:00")), NA, l3B)) 


plot_ly() %>%
  add_trace(data = rain60_events,
            x = ~time,
            y = ~rain60_events[, 2],
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = l3_outHR_smooth_no_sim,
            x = ~time,
            y = ~l3A,
            name = "Restricted Outflow (L3A)",
            yaxis = "y2",
            line = list(color = "red", dash = "dot"),
            opacity = 1) %>%
  add_lines(data = l3_outHR_smooth_no_sim,
            x = ~time,
            y = ~l3B,
            name = "Unrestricted Outflow (L3B)",
            yaxis = "y2",
            line = list(color = "green"),
            opacity = 1,
            yaxis = "y1") %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(60, 0)),
         yaxis2 = list(title = "Outflow, ml/hour",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 60000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"))



# Rainfall and outflows (Lysimeter 4):

l4_outHR_smooth_no_sim <- l4_outHR_smooth %>%
  mutate(l4A = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                              ymd_hms("2022-05-14 00:00:00")), NA, l4A)) %>%
  mutate(l4B = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                              ymd_hms("2022-05-14 00:00:00")), NA, l4B)) %>%
  mutate(l4A = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                              ymd_hms("2022-08-27 01:00:00")), NA, l4A)) %>%
  mutate(l4B = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                              ymd_hms("2022-08-27 01:00:00")), NA, l4B)) 

plot_ly() %>%
  add_trace(data = rain60_events,
            x = ~time,
            y = ~rain60_events[, 2],
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = l4_outHR_smooth_no_sim,
            x = ~time,
            y = ~l4B,
            name = "Restricted Outflow (L4B)",
            yaxis = "y2",
            line = list(color = "red", dash = "dot"),
            opacity = 1) %>%
  add_lines(data = l4_outHR_smooth_no_sim,
            x = ~time,
            y = ~l4A,
            name = "Unrestricted Outflow (L4A)",
            yaxis = "y2",
            line = list(color = "green"),
            opacity = 1,
            yaxis = "y1") %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(60, 0)),
         yaxis2 = list(title = "Outflow, ml/hour",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 60000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"))



# Rainfall and outflows (Lysimeter 7):
l7_outHR_smooth_no_sim <- l7_outHR_smooth %>%
  mutate(l7A = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                              ymd_hms("2022-05-14 00:00:00")), NA, l7A)) %>%
  mutate(l7B = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                              ymd_hms("2022-05-14 00:00:00")), NA, l7B)) %>%
  mutate(l7A = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                              ymd_hms("2022-08-27 01:00:00")), NA, l7A)) %>%
  mutate(l7B = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                              ymd_hms("2022-08-27 01:00:00")), NA, l7B)) 


plot_ly() %>%
  add_trace(data = rain60_events,
            x = ~time,
            y = ~rain60_events[, 2],
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = l7_outHR_smooth_no_sim,
            x = ~time,
            y = ~l7A,
            name = "Restricted Outflow (L7A)",
            yaxis = "y2",
            line = list(color = "red", dash = "dot"),
            opacity = 1) %>%
  add_lines(data = l7_outHR_smooth_no_sim,
            x = ~time,
            y = ~l7B,
            name = "Unrestricted Outflow (L7B)",
            yaxis = "y2",
            line = list(color = "green"),
            opacity = 1,
            yaxis = "y1") %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(60, 0)),
         yaxis2 = list(title = "Outflow, ml/hour",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 60000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"))


# Rainfall and outflows (Lysimeter 8):
l8_outHR_smooth_no_sim <- l8_outHR_smooth %>%
  mutate(l8A = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                              ymd_hms("2022-05-14 00:00:00")), NA, l8A)) %>%
  mutate(l8B = ifelse(between(time, ymd_hms("2022-05-05 12:00:00"),
                              ymd_hms("2022-05-14 00:00:00")), NA, l8B)) %>%
  mutate(l8A = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                              ymd_hms("2022-08-27 01:00:00")), NA, l8A)) %>%
  mutate(l8B = ifelse(between(time, ymd_hms("2022-08-24 11:00:00"),
                              ymd_hms("2022-08-27 01:00:00")), NA, l8B)) 


plot_ly() %>%
  add_trace(data = rain60_events,
            x = ~time,
            y = ~rain60_events[, 2],
            type = "bar",
            name = "Rainfall",
            yaxis = "y1") %>%
  add_lines(data = l8_outHR_smooth_no_sim,
            x = ~time,
            y = ~l8A,
            name = "Restricted Outflow (L8A)",
            yaxis = "y2",
            line = list(color = "red", dash = "dot"),
            opacity = 1) %>%
  add_lines(data = l8_outHR_smooth_no_sim,
            x = ~time,
            y = ~l8B,
            name = "Unrestricted Outflow (L8B)",
            yaxis = "y2",
            line = list(color = "green"),
            opacity = 1,
            yaxis = "y1") %>%
  layout(yaxis = list(title = "Rainfall, mm/hour",
                      range = c(60, 0)),
         yaxis2 = list(title = "Outflow, ml/hour",
                       overlaying = "y",
                       side = "right",
                       range = c(0, 30000)),
         xaxis = list(title = "Date/Time (Hourly Intervals)"))
