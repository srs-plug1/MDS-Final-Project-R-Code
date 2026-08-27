# Method 1: Average over the period from the initial peak difference in measured
# soil moisture and (inflow - outflow) to the end of the event as this
# difference remains constant.
# Uses data from RO3 - Calibration of Soil Moisture (EDA)


# Event 89 (data available for lysimeters 3 and 4):
# Soil moisture spike at 2021-10-05 10:00:00
ev89_mb_calibrationS <- ev89_mb_calibration[ev89_mb_calibration$time >=
                                           ymd_hms("2021-10-05 10:00:00",
                                           tz = "UTC"), ]
ev89_L3B_scaling <- calibration_scaling(ev89_mb_calibrationS, "ev89_L3B")

ev89_mb_calibration_L4AS <- ev89_mb_calibration_L4A[ev89_mb_calibration_L4A$time >=
                                                   ymd_hms("2021-10-05 10:00:00",
                                                   tz = "UTC"), ]
ev89_L4A_scaling <- calibration_scaling(ev89_mb_calibration_L4AS, "ev89_L4A")



# Event 35 (data available for lysimeters 4 and 7):
# Soil moisture spike at 2021-01-19 21:00:00
ev35_mb_calibration_L4AS <- ev35_mb_calibration_L4A[ev35_mb_calibration_L4A$time >=
                                                     ymd_hms("2021-01-19 21:00:00",
                                                             tz = "UTC"), ]
ev35_L4A_scaling <- calibration_scaling(ev35_mb_calibration_L4AS, "ev35_L4A")

ev35_mb_calibration_L7BS <- ev35_mb_calibration_L7B[ev35_mb_calibration_L7B$time >=
                                                     ymd_hms("2021-01-19 21:00:00",
                                                             tz = "UTC"), ]
ev35_L7B_scaling <- calibration_scaling(ev35_mb_calibration_L7BS, "ev35_L7B")



# Event 5 (data available for lysimeters 3, 4, and 7):
# Soil moisture spike at 2020-10-04 02:00:00
ev5_mb_calibrationS <- ev5_mb_calibration[ev5_mb_calibration$time >=
                                             ymd_hms("2020-10-04 02:00:00",
                                                     tz = "UTC"), ]
ev5_L3B_scaling <- calibration_scaling(ev5_mb_calibrationS, "ev35_L3B")

ev5_mb_calibration_L4AS <- ev5_mb_calibration_L4A[ev5_mb_calibration_L4A$time >=
                                                     ymd_hms("2020-10-04 02:00:00",
                                                             tz = "UTC"), ]
ev5_L4A_scaling <- calibration_scaling(ev5_mb_calibration_L4AS, "ev35_L4A")

ev5_mb_calibration_L7BS <- ev5_mb_calibration_L7B[ev5_mb_calibration_L7B$time >=
                                                     ymd_hms("2020-10-04 02:00:00",
                                                             tz = "UTC"), ]
ev5_L7B_scaling <- calibration_scaling(ev5_mb_calibration_L7BS, "ev5_L7B")



# Combine all:
all_scaling <- rbind(ev89_L3B_scaling, ev89_L4A_scaling, ev35_L4A_scaling, 
                     ev35_L7B_scaling, ev5_L3B_scaling, ev5_L4A_scaling, 
                     ev5_L7B_scaling)

plot_ly() %>%
  add_boxplot(data = all_scaling,
              y = ~calibration_diff)

plot_ly() %>%
  add_boxplot(data = all_scaling,
              y = ~calibration_multiplier)

# Use multiplier to scale the change in soil moisture:
mean(all_scaling$calibration_multiplier)
sd(all_scaling$calibration_multiplier)



# Method 2: Average over the period of most intense and constant rainfall:

# Event 89 (data available for lysimeters 3 and 4):
# Intense and constant rainfall period: 2021-10-05 01:00:00 to 2021-10-05 11:00:00
# Select three hours before to average

ev89_mb_calibrationS2 <- ev89_mb_calibration %>% filter(
  between(time,
          ymd_hms("2021-10-04 22:00:00", tz = "UTC"),
          ymd_hms("2021-10-05 11:00:00", tz = "UTC")
  )
)


ev89_L3B_scaling2 <- calibration_scaling2(ev89_mb_calibrationS2, "ev89_L3B")

ev89_mb_calibration_L4AS2 <- ev89_mb_calibration_L4A %>% filter(
  between(time,
          ymd_hms("2021-10-04 22:00:00", tz = "UTC"),
          ymd_hms("2021-10-05 11:00:00", tz = "UTC")
  )
)

ev89_L4A_scaling2 <- calibration_scaling2(ev89_mb_calibration_L4AS2, "ev89_L4A")



# Event 35 (data available for lysimeters 4 and 7):
# Intense and constant rainfall period: 2021-01-19 09:00:00 to 2021-01-19 21:00:00
# Select three hours before to average

ev35_mb_calibration_L4AS2 <- ev35_mb_calibration_L4A %>% filter(
  between(time,
          ymd_hms("2021-01-19 06:00:00", tz = "UTC"),
          ymd_hms("2021-01-19 21:00:00", tz = "UTC")
  )
)

ev35_L4A_scaling2 <- calibration_scaling2(ev35_mb_calibration_L4AS2, "ev35_L4A")

ev35_mb_calibration_L7BS2 <- ev35_mb_calibration_L7B %>% filter(
  between(time,
          ymd_hms("2021-01-19 06:00:00", tz = "UTC"),
          ymd_hms("2021-01-19 21:00:00", tz = "UTC")
  )
)

ev35_L7B_scaling2 <- calibration_scaling2(ev35_mb_calibration_L7BS2, "ev35_L7B")



# Event 5 (data available for lysimeters 3, 4, and 7):
# Intense and constant rainfall period: 2020-10-03 03:00:00 to 2020-10-04 02:00:00
# Select three hours before to average

ev5_mb_calibrationS2 <- ev5_mb_calibration %>% filter(
  between(time,
          ymd_hms("2020-10-03 00:00:00", tz = "UTC"),
          ymd_hms("2020-10-04 02:00:00", tz = "UTC")
  )
)

ev5_L3B_scaling2 <- calibration_scaling2(ev5_mb_calibrationS2, "ev35_L3B")

ev5_mb_calibration_L4AS2 <- ev5_mb_calibration_L4A %>% filter(
  between(time,
          ymd_hms("2020-10-03 00:00:00", tz = "UTC"),
          ymd_hms("2020-10-04 02:00:00", tz = "UTC")
  )
)

ev5_L4A_scaling2 <- calibration_scaling2(ev5_mb_calibration_L4AS2, "ev35_L4A")

ev5_mb_calibration_L7BS2 <- ev5_mb_calibration_L7B %>% filter(
  between(time,
          ymd_hms("2020-10-03 00:00:00", tz = "UTC"),
          ymd_hms("2020-10-04 02:00:00", tz = "UTC")
  )
)

ev5_L7B_scaling2 <- calibration_scaling2(ev5_mb_calibration_L7BS2, "ev5_L7B")



# Combine all:
all_scaling2 <- rbind(ev89_L3B_scaling2, ev89_L4A_scaling2, ev35_L4A_scaling2, 
                     ev35_L7B_scaling2, ev5_L3B_scaling2, ev5_L4A_scaling2, 
                     ev5_L7B_scaling2)

plot_ly() %>%
  add_boxplot(data = all_scaling2,
              y = ~calibration_diff)

plot_ly() %>%
  add_boxplot(data = all_scaling2,
              y = ~calibration_multiplier)

# Use multiplier to scale the change in soil moisture:
mean(all_scaling2$calibration_multiplier)
sd(all_scaling2$calibration_multiplier)
