## Calibration of soil moisture:

rain60_events_L <- rainfall_events(rain60_ave_L, 24)

### With L3B (unrestricted) ###

## Event 89:
# Select Event 89 data from litres dataframe:
ev89_L <- rain60_events_L[rain60_events_L$event == 89 & !is.na(rain60_events_L$event),]

# Event period (2021-09-28 14:00:00 to 2021-10-07 00:00:00)
ev89_L <- ev89_L %>%
  filter(
    between(time,
            ymd_hms("2021-09-28 14:00:00", tz = "UTC"),
            ymd_hms("2021-10-07 00:00:00", tz = "UTC"))
  )

plot(ev89_L$time, ev89_L$rain60)

ev89_smB_L <- l3_all_moist_B_int %>%
  filter(
    between(time,
            ymd_hms("2021-09-28 14:00:00", tz = "UTC"),
            ymd_hms("2021-10-07 00:00:00", tz = "UTC"))
  )

l3_outB_smooth <- l3_outHR_smooth[, c(1, 3)]
l3_outB_smooth_L <- convert_ml_l(l3_outB_smooth, 2)

ev89_outB_L <- l3_outB_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2021-09-28 14:00:00", tz = "UTC"),
            ymd_hms("2021-10-07 00:00:00", tz = "UTC"))
  )

ev89_calibration <- combine_data(list(ev89_L, ev89_outB_L, ev89_smB_L))

ev89_mb_calibration <- mass_balance_any_period(ev89_calibration, c(7:15), 6)

for (i in 1:nrow(ev89_mb_calibration)){
  ev89_mb_calibration[i, "in_minus_out"] <- ev89_mb_calibration[i, "cum_rain"] - ev89_mb_calibration[i, "cum_out"]
}


plot_ly(data = ev89_mb_calibration) %>%
  add_lines(x = ~time,
            y = ~cum_rain,
            lines = list(color = "steelblue"),
            name = "Cumulative Rainfall") %>%
  add_lines(x = ~time,
            y = ~cum_out,
            lines = list(color = "forestgreen"),
            name = "Cumulative Outflow") %>%
  add_lines(x = ~time,
            y = ~rel_sm,
            lines = list(color = "brown"),
            name = "Cumulative Change in Soil Moisture \n (Change in Soil Moisture Compared to Begining)") %>%
  add_lines(x = ~time,
            y = ~in_minus_out,
            lines = list(color = "lightgoldenrod"),
            name = "Cumulative Inflow - Outflow") %>%
  layout(title = "Event 89 - Calibrating Soil Moisture",
         xaxis = list(name = "Date/Time, hourly intervals"),
         yaxis = list(name = "Water Volume, Litres"))



# Select most extreme period (2021-10-05 01:00:00 to 2021-10-06 00:00:00): 
ev89_L_extr <- ev89_L %>%
  filter(
    between(time,
            ymd_hms("2021-10-05 01:00:00", tz = "UTC"),
            ymd_hms("2021-10-06 00:00:00", tz = "UTC"))
  )

plot(ev89_L_extr$time, ev89_L_extr$rain60)

ev89_extr_smB_L <- l3_all_moist_B_int %>%
  filter(
    between(time,
            ymd_hms("2021-10-05 01:00:00", tz = "UTC"),
            ymd_hms("2021-10-06 00:00:00", tz = "UTC"))
  )

l3_outB_smooth <- l3_outHR_smooth[, c(1, 3)]
l3_outB_smooth_L <- convert_ml_l(l3_outB_smooth, 2)

ev89_extr_outB_L <- l3_outB_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2021-10-05 01:00:00", tz = "UTC"),
            ymd_hms("2021-10-06 00:00:00", tz = "UTC"))
  )

ev89_extr_calibration <- combine_data(list(ev89_L_extr, ev89_extr_outB_L, ev89_extr_smB_L))

ev89_extr_mb_calibration <- mass_balance_any_period(ev89_extr_calibration, c(7:15), 6)

for (i in 1:nrow(ev89_extr_mb_calibration)){
  ev89_extr_mb_calibration[i, "in_minus_out"] <- ev89_extr_mb_calibration[i, "cum_rain"] - ev89_extr_mb_calibration[i, "cum_out"]
}


plot_ly(data = ev89_extr_mb_calibration) %>%
  add_lines(x = ~time,
            y = ~cum_rain,
            lines = list(color = "steelblue"),
            name = "Cumulative Rainfall") %>%
  add_lines(x = ~time,
            y = ~cum_out,
            lines = list(color = "forestgreen"),
            name = "Cumulative Outflow") %>%
  add_lines(x = ~time,
            y = ~rel_sm,
            lines = list(color = "brown"),
            name = "Cumulative Change in Soil Moisture \n (Change in Soil Moisture Compared to Begining)") %>%
  add_lines(x = ~time,
            y = ~in_minus_out,
            lines = list(color = "lightgoldenrod"),
            name = "Cumulative Inflow - Outflow") %>%
  layout(title = "Event 89 (Most Extreme Period Only) - Calibrating Soil Moisture",
         xaxis = list(name = "Date/Time, hourly intervals"),
         yaxis = list(name = "Water Volume, Litres"))



# Event 35:

ev35_L <- rain60_events_L[rain60_events_L$event == 35 & !is.na(rain60_events_L$event),]

# Event period (2021-01-18 15:00:00 to 2021-01-21 10:00:00): 
ev35_L_ <- ev35_L %>%
  filter(
    between(time,
            ymd_hms("2021-01-18 15:00:00", tz = "UTC"),
            ymd_hms("2021-01-21 10:00:00", tz = "UTC"))
  )

plot(ev89_L$time, ev89_L$rain60)

ev35_smB_L <- l3_all_moist_B_int %>%
  filter(
    between(time,
            ymd_hms("2021-01-18 15:00:00", tz = "UTC"),
            ymd_hms("2021-01-21 10:00:00", tz = "UTC"))
  )

l3_outB_smooth <- l3_outHR_smooth[, c(1, 3)]
l3_outB_smooth_L <- convert_ml_l(l3_outB_smooth, 2)

ev35_outB_L <- l3_outB_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2021-01-18 15:00:00", tz = "UTC"),
            ymd_hms("2021-01-21 10:00:00", tz = "UTC"))
  )

ev35_calibration <- combine_data(list(ev35_L, ev35_outB_L, ev35_smB_L))

ev35_mb_calibration <- mass_balance_any_period(ev35_calibration, c(7:15), 6)

for (i in 1:nrow(ev35_mb_calibration)){
  ev35_mb_calibration[i, "in_minus_out"] <- ev35_mb_calibration[i, "cum_rain"] - ev35_mb_calibration[i, "cum_out"]
}


plot_ly(data = ev35_mb_calibration) %>%
  add_lines(x = ~time,
            y = ~cum_rain,
            lines = list(color = "steelblue"),
            name = "Cumulative Rainfall") %>%
  add_lines(x = ~time,
            y = ~cum_out,
            lines = list(color = "forestgreen"),
            name = "Cumulative Outflow") %>%
  add_lines(x = ~time,
            y = ~rel_sm,
            lines = list(color = "brown"),
            name = "Cumulative Change in Soil Moisture \n (Change in Soil Moisture Compared to Begining)") %>%
  add_lines(x = ~time,
            y = ~in_minus_out,
            lines = list(color = "lightgoldenrod"),
            name = "Cumulative Inflow - Outflow") %>%
  layout(title = "Event 35 - Calibrating Soil Moisture",
         xaxis = list(name = "Date/Time, hourly intervals"),
         yaxis = list(name = "Water Volume, Litres"))


# Event 5:

ev5_L <- rain60_events_L[rain60_events_L$event == 5 & !is.na(rain60_events_L$event),]

# Event period (2020-10-02 19:00:00 to 2020-10-06 03:00:00): 
ev5_L <- ev5_L %>%
  filter(
    between(time,
            ymd_hms("2020-10-02 19:00:00", tz = "UTC"),
            ymd_hms("2020-10-06 03:00:00", tz = "UTC"))
  )

plot(ev5_L$time, ev5_L$rain60)

ev5_smB_L <- l3_all_moist_B_int %>%
  filter(
    between(time,
            ymd_hms("2020-10-02 19:00:00", tz = "UTC"),
            ymd_hms("2020-10-06 03:00:00", tz = "UTC"))
  )

l3_outB_smooth <- l3_outHR_smooth[, c(1, 3)]
l3_outB_smooth_L <- convert_ml_l(l3_outB_smooth, 2)

ev5_outB_L <- l3_outB_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2020-10-02 19:00:00", tz = "UTC"),
            ymd_hms("2020-10-06 03:00:00", tz = "UTC"))
  )

ev5_calibration <- combine_data(list(ev5_L, ev5_outB_L, ev5_smB_L))

ev5_mb_calibration <- mass_balance_any_period(ev5_calibration, c(7:15), 6)

for (i in 1:nrow(ev5_mb_calibration)){
  ev5_mb_calibration[i, "in_minus_out"] <- ev5_mb_calibration[i, "cum_rain"] - ev5_mb_calibration[i, "cum_out"]
}



plot_ly(data = ev5_mb_calibration) %>%
  add_lines(x = ~time,
            y = ~cum_rain,
            lines = list(color = "steelblue"),
            name = "Cumulative Rainfall") %>%
  add_lines(x = ~time,
            y = ~cum_out,
            lines = list(color = "forestgreen"),
            name = "Cumulative Outflow") %>%
  add_lines(x = ~time,
            y = ~rel_sm,
            lines = list(color = "brown"),
            name = "Cumulative Change in Soil Moisture \n (Change in Soil Moisture Compared to Begining)") %>%
  add_lines(x = ~time,
            y = ~in_minus_out,
            lines = list(color = "lightgoldenrod"),
            name = "Cumulative Inflow - Outflow") %>%
  layout(title = "Event 5 - Calibrating Soil Moisture",
         xaxis = list(name = "Date/Time, hourly intervals"),
         yaxis = list(name = "Water Volume, Litres"))


# Event 104:

ev104_L <- rain60_events_L[rain60_events_L$event == 104 & !is.na(rain60_events_L$event),]

# Event period (2021-11-26 06:00:00 to 2021-12-03 03:00:00): 
ev104_L <- ev104_L %>%
  filter(
    between(time,
            ymd_hms("2021-11-26 06:00:00", tz = "UTC"),
            ymd_hms("2021-12-03 03:00:00", tz = "UTC"))
  )

plot(ev104_L$time, ev104_L$rain60)

ev104_smB_L <- l3_all_moist_B_int %>%
  filter(
    between(time,
            ymd_hms("2021-11-26 06:00:00", tz = "UTC"),
            ymd_hms("2021-12-03 03:00:00", tz = "UTC"))
  )

l3_outB_smooth <- l3_outHR_smooth[, c(1, 3)]
l3_outB_smooth_L <- convert_ml_l(l3_outB_smooth, 2)

ev104_outB_L <- l3_outB_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2021-11-26 06:00:00", tz = "UTC"),
            ymd_hms("2021-12-03 03:00:00", tz = "UTC"))
  )

ev104_calibration <- combine_data(list(ev104_L, ev104_outB_L, ev104_smB_L))

ev104_mb_calibration <- mass_balance_any_period(ev104_calibration, c(7:15), 6)

for (i in 1:nrow(ev104_mb_calibration)){
  ev104_mb_calibration[i, "in_minus_out"] <- ev104_mb_calibration[i, "cum_rain"] - ev104_mb_calibration[i, "cum_out"]
}



plot_ly(data = ev104_mb_calibration) %>%
  add_lines(x = ~time,
            y = ~cum_rain,
            lines = list(color = "steelblue"),
            name = "Cumulative Rainfall") %>%
  add_lines(x = ~time,
            y = ~cum_out,
            lines = list(color = "forestgreen"),
            name = "Cumulative Outflow") %>%
  add_lines(x = ~time,
            y = ~rel_sm,
            lines = list(color = "brown"),
            name = "Cumulative Change in Soil Moisture \n (Change in Soil Moisture Compared to Begining)") %>%
  add_lines(x = ~time,
            y = ~in_minus_out,
            lines = list(color = "lightgoldenrod"),
            name = "Cumulative Inflow - Outflow") %>%
  layout(title = "Event 104 - Calibrating Soil Moisture",
         xaxis = list(name = "Date/Time, hourly intervals"),
         yaxis = list(name = "Water Volume, Litres"))



# Event 21:

ev21_L <- rain60_events_L[rain60_events_L$event == 21 & !is.na(rain60_events_L$event),]

# Event period (2020-12-03 10:00:00 to 2020-12-08 16:00:00): 
ev21_L <- ev21_L %>%
  filter(
    between(time,
            ymd_hms("2020-12-03 10:00:00", tz = "UTC"),
            ymd_hms("2020-12-08 16:00:00", tz = "UTC"))
  )

plot(ev104_L$time, ev104_L$rain60)

ev21_smB_L <- l3_all_moist_B_int %>%
  filter(
    between(time,
            ymd_hms("2020-12-03 10:00:00", tz = "UTC"),
            ymd_hms("2020-12-08 16:00:00", tz = "UTC"))
  )

l3_outB_smooth <- l3_outHR_smooth[, c(1, 3)]
l3_outB_smooth_L <- convert_ml_l(l3_outB_smooth, 2)

ev21_outB_L <- l3_outB_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2020-12-03 10:00:00", tz = "UTC"),
            ymd_hms("2020-12-08 16:00:00", tz = "UTC"))
  )

ev21_calibration <- combine_data(list(ev21_L, ev21_outB_L, ev21_smB_L))

ev21_mb_calibration <- mass_balance_any_period(ev21_calibration, c(7:15), 6)

for (i in 1:nrow(ev21_mb_calibration)){
  ev21_mb_calibration[i, "in_minus_out"] <- ev21_mb_calibration[i, "cum_rain"] - ev21_mb_calibration[i, "cum_out"]
}



plot_ly(data = ev21_mb_calibration) %>%
  add_lines(x = ~time,
            y = ~cum_rain,
            lines = list(color = "steelblue"),
            name = "Cumulative Rainfall") %>%
  add_lines(x = ~time,
            y = ~cum_out,
            lines = list(color = "forestgreen"),
            name = "Cumulative Outflow") %>%
  add_lines(x = ~time,
            y = ~rel_sm,
            lines = list(color = "brown"),
            name = "Cumulative Change in Soil Moisture \n (Change in Soil Moisture Compared to Begining)") %>%
  add_lines(x = ~time,
            y = ~in_minus_out,
            lines = list(color = "lightgoldenrod"),
            name = "Cumulative Inflow - Outflow") %>%
  layout(title = "Event 21 - Calibrating Soil Moisture",
         xaxis = list(name = "Date/Time, hourly intervals"),
         yaxis = list(name = "Water Volume, Litres"))




### With L4A (unrestricted) ###

## Event 89:
# Select Event 89 data from litres dataframe:
ev89_L <- rain60_events_L[rain60_events_L$event == 89 & !is.na(rain60_events_L$event),]

# Event period (2021-09-28 14:00:00 to 2021-10-07 00:00:00)
ev89_L <- ev89_L %>%
  filter(
    between(time,
            ymd_hms("2021-09-28 14:00:00", tz = "UTC"),
            ymd_hms("2021-10-07 00:00:00", tz = "UTC"))
  )

plot(ev89_L$time, ev89_L$rain60)

ev89_smL4A_L <- l4_all_moist_A_int %>%
  filter(
    between(time,
            ymd_hms("2021-09-28 14:00:00", tz = "UTC"),
            ymd_hms("2021-10-07 00:00:00", tz = "UTC"))
  )

l4_outA_smooth <- l4_outHR_smooth[, c(1, 2)]
l4_outA_smooth_L <- convert_ml_l(l4_outA_smooth, 2)

ev89_outL4A_L <- l4_outA_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2021-09-28 14:00:00", tz = "UTC"),
            ymd_hms("2021-10-07 00:00:00", tz = "UTC"))
  )

ev89_calibration_L4A <- combine_data(list(ev89_L, ev89_outL4A_L, ev89_smL4A_L))

ev89_mb_calibration_L4A <- mass_balance_any_period(ev89_calibration_L4A, c(7:15), 6)

for (i in 1:nrow(ev89_mb_calibration_L4A)){
  ev89_mb_calibration_L4A[i, "in_minus_out"] <- ev89_mb_calibration_L4A[i, "cum_rain"] - ev89_mb_calibration_L4A[i, "cum_out"]
}


plot_ly(data = ev89_mb_calibration_L4A) %>%
  add_lines(x = ~time,
            y = ~cum_rain,
            lines = list(color = "steelblue"),
            name = "Cumulative Rainfall") %>%
  add_lines(x = ~time,
            y = ~cum_out,
            lines = list(color = "forestgreen"),
            name = "Cumulative Outflow") %>%
  add_lines(x = ~time,
            y = ~rel_sm,
            lines = list(color = "brown"),
            name = "Cumulative Change in Soil Moisture \n (Change in Soil Moisture Compared to Begining)") %>%
  add_lines(x = ~time,
            y = ~in_minus_out,
            lines = list(color = "lightgoldenrod"),
            name = "Cumulative Inflow - Outflow") %>%
  layout(title = "Event 89 - Calibrating Soil Moisture",
         xaxis = list(name = "Date/Time, hourly intervals"),
         yaxis = list(name = "Water Volume, Litres"))


# Event 35:

ev35_L <- rain60_events_L[rain60_events_L$event == 35 & !is.na(rain60_events_L$event),]

# Event period (2021-01-18 15:00:00 to 2021-01-21 10:00:00): 
ev35_L_ <- ev35_L %>%
  filter(
    between(time,
            ymd_hms("2021-01-18 15:00:00", tz = "UTC"),
            ymd_hms("2021-01-21 10:00:00", tz = "UTC"))
  )

plot(ev89_L$time, ev89_L$rain60)

ev35_smL4A_L <- l4_all_moist_A_int %>%
  filter(
    between(time,
            ymd_hms("2021-01-18 15:00:00", tz = "UTC"),
            ymd_hms("2021-01-21 10:00:00", tz = "UTC"))
  )

l4_outA_smooth <- l4_outHR_smooth[, c(1, 2)]
l4_outA_smooth_L <- convert_ml_l(l4_outA_smooth, 2)

ev35_outL4A_L <- l4_outA_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2021-01-18 15:00:00", tz = "UTC"),
            ymd_hms("2021-01-21 10:00:00", tz = "UTC"))
  )

ev35_calibration_L4A <- combine_data(list(ev35_L, ev35_outL4A_L, ev35_smL4A_L))

ev35_mb_calibration_L4A <- mass_balance_any_period(ev35_calibration_L4A, c(7:15), 6)

for (i in 1:nrow(ev35_mb_calibration_L4A)){
  ev35_mb_calibration_L4A[i, "in_minus_out"] <- ev35_mb_calibration_L4A[i, "cum_rain"] - ev35_mb_calibration_L4A[i, "cum_out"]
}


plot_ly(data = ev35_mb_calibration_L4A) %>%
  add_lines(x = ~time,
            y = ~cum_rain,
            lines = list(color = "steelblue"),
            name = "Cumulative Rainfall") %>%
  add_lines(x = ~time,
            y = ~cum_out,
            lines = list(color = "forestgreen"),
            name = "Cumulative Outflow") %>%
  add_lines(x = ~time,
            y = ~rel_sm,
            lines = list(color = "brown"),
            name = "Cumulative Change in Soil Moisture \n (Change in Soil Moisture Compared to Begining)") %>%
  add_lines(x = ~time,
            y = ~in_minus_out,
            lines = list(color = "lightgoldenrod"),
            name = "Cumulative Inflow - Outflow") %>%
  layout(title = "Event 35 - Calibrating Soil Moisture",
         xaxis = list(name = "Date/Time, hourly intervals"),
         yaxis = list(name = "Water Volume, Litres"))



# Event 5:

ev5_L <- rain60_events_L[rain60_events_L$event == 5 & !is.na(rain60_events_L$event),]

# Event period (2020-10-02 19:00:00 to 2020-10-06 03:00:00): 
ev5_L <- ev5_L %>%
  filter(
    between(time,
            ymd_hms("2020-10-02 19:00:00", tz = "UTC"),
            ymd_hms("2020-10-06 03:00:00", tz = "UTC"))
  )

plot(ev5_L$time, ev5_L$rain60)

ev5_smL4A_L <- l4_all_moist_A_int %>%
  filter(
    between(time,
            ymd_hms("2020-10-02 19:00:00", tz = "UTC"),
            ymd_hms("2020-10-06 03:00:00", tz = "UTC"))
  )

l4_outA_smooth <- l4_outHR_smooth[, c(1, 2)]
l4_outA_smooth_L <- convert_ml_l(l4_outA_smooth, 2)

ev5_outL4A_L <- l4_outA_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2020-10-02 19:00:00", tz = "UTC"),
            ymd_hms("2020-10-06 03:00:00", tz = "UTC"))
  )

ev5_calibration_L4A <- combine_data(list(ev5_L, ev5_outL4A_L, ev5_smL4A_L))

ev5_mb_calibration_L4A <- mass_balance_any_period(ev5_calibration_L4A, c(7:15), 6)

for (i in 1:nrow(ev5_mb_calibration_L4A)){
  ev5_mb_calibration_L4A[i, "in_minus_out"] <- ev5_mb_calibration_L4A[i, "cum_rain"] - ev5_mb_calibration_L4A[i, "cum_out"]
}



plot_ly(data = ev5_mb_calibration_L4A) %>%
  add_lines(x = ~time,
            y = ~cum_rain,
            lines = list(color = "steelblue"),
            name = "Cumulative Rainfall") %>%
  add_lines(x = ~time,
            y = ~cum_out,
            lines = list(color = "forestgreen"),
            name = "Cumulative Outflow") %>%
  add_lines(x = ~time,
            y = ~rel_sm,
            lines = list(color = "brown"),
            name = "Cumulative Change in Soil Moisture \n (Change in Soil Moisture Compared to Begining)") %>%
  add_lines(x = ~time,
            y = ~in_minus_out,
            lines = list(color = "lightgoldenrod"),
            name = "Cumulative Inflow - Outflow") %>%
  layout(title = "Event 5 - Calibrating Soil Moisture",
         xaxis = list(name = "Date/Time, hourly intervals"),
         yaxis = list(name = "Water Volume, Litres"))


# Event 104:

ev104_L <- rain60_events_L[rain60_events_L$event == 104 & !is.na(rain60_events_L$event),]

# Event period (2021-11-26 06:00:00 to 2021-12-03 03:00:00): 
ev104_L <- ev104_L %>%
  filter(
    between(time,
            ymd_hms("2021-11-26 06:00:00", tz = "UTC"),
            ymd_hms("2021-12-03 03:00:00", tz = "UTC"))
  )

plot(ev104_L$time, ev104_L$rain60)

ev104_smL4A_L <- l4_all_moist_A_int %>%
  filter(
    between(time,
            ymd_hms("2021-11-26 06:00:00", tz = "UTC"),
            ymd_hms("2021-12-03 03:00:00", tz = "UTC"))
  )

l4_outA_smooth <- l4_outHR_smooth[, c(1, 2)]
l4_outA_smooth_L <- convert_ml_l(l4_outA_smooth, 2)

ev104_outL4A_L <- l4_outA_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2021-11-26 06:00:00", tz = "UTC"),
            ymd_hms("2021-12-03 03:00:00", tz = "UTC"))
  )

ev104_calibration_L4A <- combine_data(list(ev104_L, ev104_outL4A_L, ev104_smL4A_L))

ev104_mb_calibration_L4A <- mass_balance_any_period(ev104_calibration_L4A, c(7:15), 6)

for (i in 1:nrow(ev104_mb_calibration_L4A)){
  ev104_mb_calibration_L4A[i, "in_minus_out"] <- ev104_mb_calibration_L4A[i, "cum_rain"] - ev104_mb_calibration_L4A[i, "cum_out"]
}



plot_ly(data = ev104_mb_calibration_L4A) %>%
  add_lines(x = ~time,
            y = ~cum_rain,
            lines = list(color = "steelblue"),
            name = "Cumulative Rainfall") %>%
  add_lines(x = ~time,
            y = ~cum_out,
            lines = list(color = "forestgreen"),
            name = "Cumulative Outflow") %>%
  add_lines(x = ~time,
            y = ~rel_sm,
            lines = list(color = "brown"),
            name = "Cumulative Change in Soil Moisture \n (Change in Soil Moisture Compared to Begining)") %>%
  add_lines(x = ~time,
            y = ~in_minus_out,
            lines = list(color = "lightgoldenrod"),
            name = "Cumulative Inflow - Outflow") %>%
  layout(title = "Event 104 - Calibrating Soil Moisture",
         xaxis = list(name = "Date/Time, hourly intervals"),
         yaxis = list(name = "Water Volume, Litres"))




## Lysimeter 7: ##


## Event 89:
# Select Event 89 data from litres dataframe:
ev89_L <- rain60_events_L[rain60_events_L$event == 89 & !is.na(rain60_events_L$event),]

# Event period (2021-09-28 14:00:00 to 2021-10-07 00:00:00)
ev89_L <- ev89_L %>%
  filter(
    between(time,
            ymd_hms("2021-09-28 14:00:00", tz = "UTC"),
            ymd_hms("2021-10-07 00:00:00", tz = "UTC"))
  )

plot(ev89_L$time, ev89_L$rain60)

ev89_smL7B_L <- l7_all_moist_B_int %>%
  filter(
    between(time,
            ymd_hms("2021-09-28 14:00:00", tz = "UTC"),
            ymd_hms("2021-10-07 00:00:00", tz = "UTC"))
  )

l7_outB_smooth <- l7_outHR_smooth[, c(1, 3)]
l7_outB_smooth_L <- convert_ml_l(l7_outB_smooth, 2)

ev89_outL7B_L <- l7_outB_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2021-09-28 14:00:00", tz = "UTC"),
            ymd_hms("2021-10-07 00:00:00", tz = "UTC"))
  )

ev89_calibration_L7B <- combine_data(list(ev89_L, ev89_outL7B_L, ev89_smL7B_L))

ev89_mb_calibration_L7B <- mass_balance_any_period(ev89_calibration_L7B, c(7:15), 6)

for (i in 1:nrow(ev89_mb_calibration_L7B)){
  ev89_mb_calibration_L7B[i, "in_minus_out"] <- ev89_mb_calibration_L7B[i, "cum_rain"] - ev89_mb_calibration_L7B[i, "cum_out"]
}


plot_ly(data = ev89_mb_calibration_L7B) %>%
  add_lines(x = ~time,
            y = ~cum_rain,
            lines = list(color = "steelblue"),
            name = "Cumulative Rainfall") %>%
  add_lines(x = ~time,
            y = ~cum_out,
            lines = list(color = "forestgreen"),
            name = "Cumulative Outflow") %>%
  add_lines(x = ~time,
            y = ~rel_sm,
            lines = list(color = "brown"),
            name = "Cumulative Change in Soil Moisture \n (Change in Soil Moisture Compared to Begining)") %>%
  add_lines(x = ~time,
            y = ~in_minus_out,
            lines = list(color = "lightgoldenrod"),
            name = "Cumulative Inflow - Outflow") %>%
  layout(title = "Event 89 - Calibrating Soil Moisture",
         xaxis = list(name = "Date/Time, hourly intervals"),
         yaxis = list(name = "Water Volume, Litres"))


# Event 35:

ev35_L <- rain60_events_L[rain60_events_L$event == 35 & !is.na(rain60_events_L$event),]

# Event period (2021-01-18 15:00:00 to 2021-01-21 10:00:00): 
ev35_L_ <- ev35_L %>%
  filter(
    between(time,
            ymd_hms("2021-01-18 15:00:00", tz = "UTC"),
            ymd_hms("2021-01-21 10:00:00", tz = "UTC"))
  )


ev35_smL7B_L <- l7_all_moist_B_int %>%
  filter(
    between(time,
            ymd_hms("2021-01-18 15:00:00", tz = "UTC"),
            ymd_hms("2021-01-21 10:00:00", tz = "UTC"))
  )

l7_outB_smooth <- l7_outHR_smooth[, c(1, 3)]
l7_outB_smooth_L <- convert_ml_l(l7_outB_smooth, 2)

ev35_outL7B_L <- l7_outB_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2021-01-18 15:00:00", tz = "UTC"),
            ymd_hms("2021-01-21 10:00:00", tz = "UTC"))
  )

ev35_calibration_L7B <- combine_data(list(ev35_L, ev35_outL7B_L, ev35_smL7B_L))

ev35_mb_calibration_L7B <- mass_balance_any_period(ev35_calibration_L7B, c(7:15), 6)

for (i in 1:nrow(ev35_mb_calibration_L7B)){
  ev35_mb_calibration_L7B[i, "in_minus_out"] <- ev35_mb_calibration_L7B[i, "cum_rain"] - ev35_mb_calibration_L7B[i, "cum_out"]
}


plot_ly(data = ev35_mb_calibration_L7B) %>%
  add_lines(x = ~time,
            y = ~cum_rain,
            lines = list(color = "steelblue"),
            name = "Cumulative Rainfall") %>%
  add_lines(x = ~time,
            y = ~cum_out,
            lines = list(color = "forestgreen"),
            name = "Cumulative Outflow") %>%
  add_lines(x = ~time,
            y = ~rel_sm,
            lines = list(color = "brown"),
            name = "Cumulative Change in Soil Moisture \n (Change in Soil Moisture Compared to Begining)") %>%
  add_lines(x = ~time,
            y = ~in_minus_out,
            lines = list(color = "lightgoldenrod"),
            name = "Cumulative Inflow - Outflow") %>%
  layout(title = "Event 35 - Calibrating Soil Moisture",
         xaxis = list(name = "Date/Time, hourly intervals"),
         yaxis = list(name = "Water Volume, Litres"))



# Event 5:

ev5_L <- rain60_events_L[rain60_events_L$event == 5 & !is.na(rain60_events_L$event),]

# Event period (2020-10-02 19:00:00 to 2020-10-06 03:00:00): 
ev5_L <- ev5_L %>%
  filter(
    between(time,
            ymd_hms("2020-10-02 19:00:00", tz = "UTC"),
            ymd_hms("2020-10-06 03:00:00", tz = "UTC"))
  )

plot(ev5_L$time, ev5_L$rain60)

ev5_smL7B_L <- l7_all_moist_B_int %>%
  filter(
    between(time,
            ymd_hms("2020-10-02 19:00:00", tz = "UTC"),
            ymd_hms("2020-10-06 03:00:00", tz = "UTC"))
  )

l7_outB_smooth <- l7_outHR_smooth[, c(1, 3)]
l7_outB_smooth_L <- convert_ml_l(l7_outB_smooth, 2)

ev5_outL7B_L <- l7_outB_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2020-10-02 19:00:00", tz = "UTC"),
            ymd_hms("2020-10-06 03:00:00", tz = "UTC"))
  )

ev5_calibration_L7B <- combine_data(list(ev5_L, ev5_outL7B_L, ev5_smL7B_L))

ev5_mb_calibration_L7B <- mass_balance_any_period(ev5_calibration_L7B, c(7:15), 6)

for (i in 1:nrow(ev5_mb_calibration_L7B)){
  ev5_mb_calibration_L7B[i, "in_minus_out"] <- ev5_mb_calibration_L7B[i, "cum_rain"] - ev5_mb_calibration_L7B[i, "cum_out"]
}



plot_ly(data = ev5_mb_calibration_L7B) %>%
  add_lines(x = ~time,
            y = ~cum_rain,
            lines = list(color = "steelblue"),
            name = "Cumulative Rainfall") %>%
  add_lines(x = ~time,
            y = ~cum_out,
            lines = list(color = "forestgreen"),
            name = "Cumulative Outflow") %>%
  add_lines(x = ~time,
            y = ~rel_sm,
            lines = list(color = "brown"),
            name = "Cumulative Change in Soil Moisture \n (Change in Soil Moisture Compared to Begining)") %>%
  add_lines(x = ~time,
            y = ~in_minus_out,
            lines = list(color = "lightgoldenrod"),
            name = "Cumulative Inflow - Outflow") %>%
  layout(title = "Event 5 - Calibrating Soil Moisture",
         xaxis = list(name = "Date/Time, hourly intervals"),
         yaxis = list(name = "Water Volume, Litres"))



# Event 104:

ev104_L <- rain60_events_L[rain60_events_L$event == 104 & !is.na(rain60_events_L$event),]

# Event period (2021-11-26 06:00:00 to 2021-12-03 03:00:00): 
ev104_L <- ev104_L %>%
  filter(
    between(time,
            ymd_hms("2021-11-26 06:00:00", tz = "UTC"),
            ymd_hms("2021-12-03 03:00:00", tz = "UTC"))
  )

plot(ev104_L$time, ev104_L$rain60)

ev104_smL7B_L <- l7_all_moist_B_int %>%
  filter(
    between(time,
            ymd_hms("2021-11-26 06:00:00", tz = "UTC"),
            ymd_hms("2021-12-03 03:00:00", tz = "UTC"))
  )

l7_outB_smooth <- l7_outHR_smooth[, c(1, 3)]
l7_outB_smooth_L <- convert_ml_l(l7_outB_smooth, 2)

ev104_outL7B_L <- l7_outB_smooth_L %>%
  filter(
    between(time,
            ymd_hms("2021-11-26 06:00:00", tz = "UTC"),
            ymd_hms("2021-12-03 03:00:00", tz = "UTC"))
  )

ev104_calibration_L7B <- combine_data(list(ev104_L, ev104_outL7B_L, ev104_smL7B_L))

ev104_mb_calibration_L7B <- mass_balance_any_period(ev104_calibration_L7B, c(7:15), 6)

for (i in 1:nrow(ev104_mb_calibration_L7B)){
  ev104_mb_calibration_L7B[i, "in_minus_out"] <- ev104_mb_calibration_L7B[i, "cum_rain"] - ev104_mb_calibration_L7B[i, "cum_out"]
}



plot_ly(data = ev104_mb_calibration_L7B) %>%
  add_lines(x = ~time,
            y = ~cum_rain,
            lines = list(color = "steelblue"),
            name = "Cumulative Rainfall") %>%
  add_lines(x = ~time,
            y = ~cum_out,
            lines = list(color = "forestgreen"),
            name = "Cumulative Outflow") %>%
  add_lines(x = ~time,
            y = ~rel_sm,
            lines = list(color = "brown"),
            name = "Cumulative Change in Soil Moisture \n (Change in Soil Moisture Compared to Begining)") %>%
  add_lines(x = ~time,
            y = ~in_minus_out,
            lines = list(color = "lightgoldenrod"),
            name = "Cumulative Inflow - Outflow") %>%
  layout(title = "Event 104 - Calibrating Soil Moisture",
         xaxis = list(name = "Date/Time, hourly intervals"),
         yaxis = list(name = "Water Volume, Litres"))
