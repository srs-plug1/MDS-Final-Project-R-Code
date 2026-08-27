## All Figures for RO2: ##

# Examining Impact of Small Events:
ratios <- plot_ly() %>%
  add_markers(data = out_met_test5,
              x = ~total_rain_L,
              y = ~tot_drain_B_over_tot_drain_A,
              name = "Ratio for one event",
              marker = list(symbol = "cross")) %>%
  layout(
    xaxis = list(title = "Event Depth, L", range = c(0, 50)),
    yaxis = list(title = "Total Drainage Outflow B / \n Total Drainage Outflow A", range = c(0, 70))
  )

in_out <- plot_ly() %>%
  add_markers(data = out_met_test5,
              x = ~total_rain_L,
              y = ~drainage_over_rain_A,
              name = "Restricted outflow (A)",
              marker = list(colour = "orange", symbol = "circle")) %>%
  add_markers(data = out_met_test5,
              x = ~total_rain_L,
              y = ~drainage_over_rain_B,
              name = "Unrestricted outflow (B)",
              marker = list(colour = "forestgreen", symbol = "square")) %>%
  add_segments(
    data = out_met_test5,
    x = ~total_rain_L,
    xend = ~total_rain_L,
    y = ~drainage_over_rain_A,
    yend = ~drainage_over_rain_B,
    name = "Points belonging to \n same event",
    line = list(color = "grey", opacity = 0.25)
  ) %>%
  layout(
    xaxis = list(title = "Event Depth, L"),
    yaxis = list(title = "Total Drainage Outflow / \n Total Event Rainfall", range = c(0, 1)))

subplot(ratios, in_out,
        nrows = 1, shareX = F, shareY = F, titleX = T, titleY = T)


# Individual plots version:

in_out %>%
  layout(legend = list(orientation = "v",
                       x = 0.7,
                       y = 0.15))
ratios


# Event 89
event89_outflow_plot <- plot_event_outflows(rain60_events, l3_outHR_smooth, 89,
                                            24, "L3 (Bare Earth)")
event89_outflow_plot <- event89_outflow_plot %>%
  layout(yaxis = list(title = "Rainfall, mm/h",
                      autorange = F,
                      range = c(80,0)),
         yaxis2 = list(title = "Outflow, ml/h",
                       autorange = F, 
                       range = c(0, 40000)),
         title = "",
         shapes = list(list(
           type = "line",
           x0 = 0,
           x1 = 1,
           xref = "paper",
           y0 = 10800,
           y1 = 10800,
           yref = "y2",
           line = list(color = "grey", dash = "dash"),
           name = "Greenfiled Runoff Rate \n (10,800 ml/h)"
         )),
         legend = list(x = 1.05),
         annotations = list(text = "Greenfiled Runoff Rate \n (10,800 ml/h)",
                            x = ymd_hms("2021-10-02 00:00:00"),
                            y = 10800,
                            xref = "x",
                            yref = "y2"))

event89_outflow_plot



# Radar plot
# Visualising medians (larger events):
out_met30_large <- out_met30[out_met30$total_rain_L > 17, ]
out_met10_large <- out_met10[out_met10$total_rain_L > 17, ]
out_met40_large <- out_met40[out_met40$total_rain_L > 17, ]
out_met75_large <- out_met75[out_met75$total_rain_L > 17, ]


radar_df_large <- data.frame(pfa = c(median(out_met30_large$peak_flow_attenuation_A, na.rm = T),
                               median(out_met30_large$peak_flow_attenuation_B, na.rm = T)),
                       ttp = c(median(out_met30_large$time_to_peak_A, na.rm = T),
                               median(out_met30_large$time_to_peak_B, na.rm = T)),
                       vr = c(median(out_met30_large$volume_reduction_A, na.rm = T),
                              median(out_met30_large$volume_reduction_B, na.rm = T)),
                       drain_vol = c(median(out_met30_large$total_drainage_volume_A, na.rm = T),
                                     median(out_met30_large$total_drainage_volume_B, na.rm = T)),
                       drain_vol_24 = c(median(out_met30_large$total_drainage_volume_A_24, na.rm = T),
                                        median(out_met30_large$total_drainage_volume_B_24, na.rm = T)),
                       percent_gfrr = c(median(out_met30_large$gf_percent_reached_at_max_A, na.rm = T),
                                        median(out_met30_large$gf_percent_reached_at_max_B, na.rm = T)))
rownames(radar_df_large) <- c("Restricted (A)", "Unrestricted (B)")

max_min_large <- data.frame(pfa = c(max(out_met30_large$peak_flow_attenuation_A,
                                  out_met30_large$peak_flow_attenuation_B,
                                  na.rm = T),
                              min(out_met30_large$peak_flow_attenuation_A,
                                  out_met30_large$peak_flow_attenuation_B,
                                  na.rm = T)),
                      ttp = c(max(out_met30_large$time_to_peak_A,
                                  out_met30_large$time_to_peak_B,
                                  na.rm = T),
                              min(out_met30_large$time_to_peak_A,
                                  out_met30_large$time_to_peak_B,
                                  na.rm = T)),
                      vr = c(max(out_met30_large$volume_reduction_A,
                                 out_met30_large$volume_reduction_B,
                                 na.rm = T),
                             min(out_met30_large$volume_reduction_A,
                                 out_met30_large$volume_reduction_B,
                                 na.rm = T)),
                      drain_vol = c(max(out_met30_large$total_drainage_volume_A,
                                        out_met30_large$total_drainage_volume_B,
                                        na.rm = T),
                                    min(out_met30_large$total_drainage_volume_A,
                                        out_met30_large$total_drainage_volume_B,
                                        na.rm = T)),
                      drain_vol_24 = c(max(out_met30_large$total_drainage_volume_A_24,
                                           out_met30_large$total_drainage_volume_B_24,
                                           na.rm = T),
                                       min(out_met30_large$total_drainage_volume_A_24,
                                           out_met30_large$total_drainage_volume_B_24,
                                           na.rm = T)),
                      percent_gfrr = c(max(out_met30_large$gf_percent_reached_at_max_A,
                                           out_met30_large$gf_percent_reached_at_max_B,
                                           na.rm = T),
                                       min(out_met30_large$gf_percent_reached_at_max_A,
                                           out_met30_large$gf_percent_reached_at_max_B,
                                           na.rm = T)))

radar_df_large <- rbind(max_min_large, radar_df_large)

line_col <- c("black", "red")[1:2]
names <- c("Restricted, A", "Unrestricted, B")

radarchart(radar_df_large, axistype=1, vlabels = c("PFA", "TTP", "VR", "Drainage Vol. (End)",
                                 "Drainage Vol. (24 h)", "Percent GFRR"),
           pcol = line_col)
legend(x=0.6, y=1.2, legend = names, col = line_col, pt.bg = line_col, pch = 21)


# Visualising medians (all events examined):

radar_df <- data.frame(pfa = c(median(out_met30$peak_flow_attenuation_A, na.rm = T),
                               median(out_met30$peak_flow_attenuation_B, na.rm = T)),
                       ttp = c(median(out_met30$time_to_peak_A, na.rm = T),
                               median(out_met30$time_to_peak_B, na.rm = T)),
                       vr = c(median(out_met30$volume_reduction_A, na.rm = T),
                              median(out_met30$volume_reduction_B, na.rm = T)),
                       drain_vol = c(median(out_met30$total_drainage_volume_A, na.rm = T),
                                     median(out_met30$total_drainage_volume_B, na.rm = T)),
                       drain_vol_24 = c(median(out_met30$total_drainage_volume_A_24, na.rm = T),
                                        median(out_met30$total_drainage_volume_B_24, na.rm = T)),
                       percent_gfrr = c(median(out_met30$gf_percent_reached_at_max_A, na.rm = T),
                                        median(out_met30$gf_percent_reached_at_max_B, na.rm = T)))
rownames(radar_df) <- c("Restricted (A)", "Unrestricted (B)")

max_min <- data.frame(pfa = c(max(out_met30$peak_flow_attenuation_A,
                                  out_met30$peak_flow_attenuation_B,
                                  na.rm = T),
                              min(out_met30$peak_flow_attenuation_A,
                                  out_met30$peak_flow_attenuation_B,
                                  na.rm = T)),
                      ttp = c(max(out_met30$time_to_peak_A,
                                  out_met30$time_to_peak_B,
                                  na.rm = T),
                              min(out_met30$time_to_peak_A,
                                  out_met30$time_to_peak_B,
                                  na.rm = T)),
                      vr = c(max(out_met30$volume_reduction_A,
                                 out_met30$volume_reduction_B,
                                 na.rm = T),
                             min(out_met30$volume_reduction_A,
                                 out_met30$volume_reduction_B,
                                 na.rm = T)),
                      drain_vol = c(max(out_met30$total_drainage_volume_A,
                                        out_met30$total_drainage_volume_B,
                                        na.rm = T),
                                    min(out_met30$total_drainage_volume_A,
                                        out_met30$total_drainage_volume_B,
                                        na.rm = T)),
                      drain_vol_24 = c(max(out_met30$total_drainage_volume_A_24,
                                           out_met30$total_drainage_volume_B_24,
                                           na.rm = T),
                                       min(out_met30$total_drainage_volume_A_24,
                                           out_met30$total_drainage_volume_B_24,
                                           na.rm = T)),
                      percent_gfrr = c(max(out_met30$gf_percent_reached_at_max_A,
                                           out_met30$gf_percent_reached_at_max_B,
                                           na.rm = T),
                                       min(out_met30$gf_percent_reached_at_max_A,
                                           out_met30$gf_percent_reached_at_max_B,
                                           na.rm = T)))

radar_df <- rbind(max_min, radar_df)



radarchart(radar_df, axistype=1, vlabels = c("PFA", "TTP", "VR", "Drainage Vol. (End)",
                                                   "Drainage Vol. (24 h)", "Percent GFRR"),
           pcol = line_col)
legend(x=0.6, y=1.2, legend = names, col = line_col, pt.bg = line_col, pch = 21)
