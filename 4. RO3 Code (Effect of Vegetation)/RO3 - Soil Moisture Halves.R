# The following uses data frames from the 
# "RO3 - Mass Balance Lysimeters3, 4, and 7" file:

## Examining Soil Moisture Data for All
# Soil moisture data should be available for lysimeters 3, 4, and 7 across this
# period.

# Starting conditions at beginning of dry period for lysimeters 3, 4, and 7:
# average over first 5 hours:
# Total moisture (L3):
mean(l3A_mb_l347_dry[c(1:5), "total_moisture"])

# Dry period end:
mean(l3A_mb_l347_dry[c((nrow(l3A_mb_l347_dry) - 5):nrow(l3A_mb_l347_dry)), "total_moisture"])

# Total moisture (L4):
l4B_for_mb_l347_dry <- l4B_for_mb_l347 %>%
  filter(
    between(time,
            ymd_hms("2022-06-08 11:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )
l4B_mb_l347_dry <- mass_balance_any_period_cal(l4B_for_mb_l347_dry, c(7:15), 6, 2.617265)

mean(l4B_mb_l347_dry[c(1:5), "total_moisture"])

# Dry period end:
mean(l4B_mb_l347_dry[c((nrow(l4B_mb_l347_dry) - 5):nrow(l4B_mb_l347_dry)), "total_moisture"])


# Total moisture (L7):
mean(l7A_mb_l347_dry[c(1:5), "total_moisture"])

# Dry period end:
mean(l7A_mb_l347_dry[c((nrow(l7A_mb_l347_dry) - 5):nrow(l7A_mb_l347_dry)), "total_moisture"])



# Examine total soil moisture over the whole period:
# Total (uncalibrated) moisture (unrestricted):

plot_ly(x = ~time,
        y = ~total_moisture) %>%
  add_lines(data = l3B_mb_l347,
            name = "Lysimeter 3 (Unrestricted)") %>%
  add_lines(data = l7B_mb_l347,
            name = "Lysimeter 7 (Unrestricted)") %>%
  add_lines(data = l4A_mb_l347,
            name = "Lysimeter 4 (Unrestricted)")

# Total (uncalibrated) moisture (restricted):

plot_ly(x = ~time,
        y = ~total_moisture) %>%
  add_lines(data = l3A_mb_l347,
            name = "Lysimeter 3 (Restricted)") %>%
  add_lines(data = l7A_mb_l347,
            name = "Lysimeter 7 (Restricted)") %>%
  add_lines(data = l4B_mb_l347,
            name = "Lysimeter 4 (Restricted)")


# Visualising soil moisture in the soil only:
# Total (uncalibrated) moisture (unrestricted):

plot_ly(x = ~time,
        y = ~total_moisture_soil_only) %>%
  add_lines(data = l3B_mb_l347,
            name = "Lysimeter 3 (Unrestricted)") %>%
  add_lines(data = l7B_mb_l347,
            name = "Lysimeter 7 (Unrestricted)") %>%
  add_lines(data = l4A_mb_l347,
            name = "Lysimeter 4 (Unrestricted)")
# Total (uncalibrated) moisture (restricted):

plot_ly(x = ~time,
        y = ~total_moisture_soil_only) %>%
  add_lines(data = l3A_mb_l347,
            name = "Lysimeter 3 (Restricted)") %>%
  add_lines(data = l4B_mb_l347,
            name = "Lysimeter 4 (Restricted)")  %>%
  add_lines(data = l7A_mb_l347,
            name = "Lysimeter 7 (Restricted)") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Total Moisture Content (Growing Medium Only), L"))


# Visualising soil moisture 0 cm to 25 cm vs. 25 cm to 67.5 cm:

# Restricted:
l3A_sm_halves_l347 <- soil_moisture_halves(l3A_mb_l347, 20, c(7:15), 2.617265)
plot_ly(l3A_sm_halves_l347,
        x = ~time) %>%
  add_lines(y = ~tot_sm_top,
            name = "Top 25 cm") %>%
  add_lines(y = ~tot_sm_bottom,
            name = "Bottom 42.5 cm") %>%
  layout(xaxis = list(title = "Date/Time (hourly intervals)"),
         yaxis = list(title = "Soil Moisture Content, L"))

l4B_sm_halves_l347 <- soil_moisture_halves(l4B_mb_l347, 20, c(7:15), 2.617265)
plot_ly(l4B_sm_halves_l347,
        x = ~time) %>%
  add_lines(y = ~tot_sm_top,
            name = "Top 25 cm") %>%
  add_lines(y = ~tot_sm_bottom,
            name = "Bottom 42.5 cm") %>%
  layout(xaxis = list(title = "Date/Time (hourly intervals)"),
         yaxis = list(title = "Soil Moisture Content, L"))

l7A_sm_halves_l347 <- soil_moisture_halves(l7A_mb_l347, 20, c(7:15), 2.617265)
plot_ly(l7A_sm_halves_l347,
        x = ~time) %>%
  add_lines(y = ~tot_sm_top,
            name = "Top 25 cm") %>%
  add_lines(y = ~tot_sm_bottom,
            name = "Bottom 42.5 cm") %>%
  layout(xaxis = list(title = "Date/Time (hourly intervals)"),
         yaxis = list(title = "Soil Moisture Content, L"))


# Unrestricted:
l3B_sm_halves_l347 <- soil_moisture_halves(l3B_mb_l347, 20, c(7:15), 2.617265)
plot_ly(l3B_sm_halves_l347,
        x = ~time) %>%
  add_lines(y = ~tot_sm_top) %>%
  add_lines(y = ~tot_sm_bottom)

l4A_sm_halves_l347 <- soil_moisture_halves(l4A_mb_l347, 20, c(7:15), 2.617265)
plot_ly(l4A_sm_halves_l347,
        x = ~time) %>%
  add_lines(y = ~tot_sm_top) %>%
  add_lines(y = ~tot_sm_bottom)

l7B_sm_halves_l347 <- soil_moisture_halves(l7B_mb_l347, 20, c(7:15), 2.617265)
plot_ly(l7B_sm_halves_l347,
        x = ~time) %>%
  add_lines(y = ~tot_sm_top) %>%
  add_lines(y = ~tot_sm_bottom)




## Focus on changes in soil moisture
## Compare L4 and L7, given similar starting conditions in restricted 
## (see comparison graphs for soil only moisture content)

# Full period:
plot_ly(data = l4B_sm_halves_l347,
        x = ~time) %>%
  add_lines(y = ~tot_sm_top,
            name = "Top 25 cm (Total)") %>%
  add_lines(y = ~tot_sm_bottom,
            name = "Bottom 42.5 cm (Total)") %>%
  add_lines(y = ~change_sm_top,
            name = "Top 25 cm (Change)") %>%
  add_lines(y = ~change_sm_bottom,
            name = "Bottom 42.5 cm (Change)")

plot_ly(data = l7A_sm_halves_l347,
        x = ~time) %>%
  add_lines(y = ~tot_sm_top,
            name = "Top 25 cm (Total)") %>%
  add_lines(y = ~tot_sm_bottom,
            name = "Bottom 42.5 cm (Total)") %>%
  add_lines(y = ~change_sm_top,
            name = "Top 25 cm (Change)") %>%
  add_lines(y = ~change_sm_bottom,
            name = "Bottom 42.5 cm (Change)")


# Dry period:
l4B_sm_halves_l347_dry <- soil_moisture_halves(l4B_mb_l347_dry, 20, c(7:15), 2.617265)
l7A_sm_halves_l347_dry <- soil_moisture_halves(l7A_mb_l347_dry, 20, c(7:15), 2.617265)


plot_ly(x = ~time,
        y = ~tot_sm_bottom) %>%
  add_lines(data = l4B_sm_halves_l347_dry) %>%
  add_lines(data = l7A_sm_halves_l347_dry)

plot_ly(x = ~time,
        y = ~tot_sm_top) %>%
  add_lines(data = l4B_sm_halves_l347_dry) %>%
  add_lines(data = l7A_sm_halves_l347_dry)

plot_ly(data = l4B_sm_halves_l347_dry,
        x = ~time) %>%
  add_lines(y = ~tot_sm_top,
            name = "Top 25 cm (Total)") %>%
  add_lines(y = ~tot_sm_bottom,
            name = "Bottom 42.5 cm (Total)") %>%
  add_lines(y = ~change_sm_top,
            name = "Top 25 cm (Change)") %>%
  add_lines(y = ~change_sm_bottom,
            name = "Bottom 42.5 cm (Change)")

plot_ly(data = l7A_sm_halves_l347_dry,
        x = ~time) %>%
  add_lines(y = ~tot_sm_top,
            name = "Top 25 cm (Total)") %>%
  add_lines(y = ~tot_sm_bottom,
            name = "Bottom 42.5 cm (Total)") %>%
  add_lines(y = ~change_sm_top,
            name = "Top 25 cm (Change)") %>%
  add_lines(y = ~change_sm_bottom,
            name = "Bottom 42.5 cm (Change)")


# Comparison plot 1 (changes in top). Include:
# Total moisture in soil for both
# Changes for the top

plot_ly(x = ~time) %>%
  add_lines(data = l4B_sm_halves_l347_dry,
            y = ~total_moisture_soil_only,
            name = "Total Moisture in Soil (L4)") %>%
  add_lines(data = l4B_sm_halves_l347_dry,
            y = ~change_sm_top,
            name = "Change in Moisture, Top 25 cm (L4)") %>%
  add_lines(data = l7A_sm_halves_l347_dry,
            y = ~total_moisture_soil_only,
            name = "Total Moisture in Soil (L7)") %>%
  add_lines(data = l7A_sm_halves_l347_dry,
            y = ~change_sm_top,
            name = "Change in Moisture, Top 25 cm (L7)")


# Comparison plot 2 (changes in bottom). Include:
# Total moisture in soil for both
# Changes for the top

plot_ly(x = ~time) %>%
  add_lines(data = l4B_sm_halves_l347_dry,
            y = ~total_moisture_soil_only,
            name = "Total Moisture in Soil (L4)") %>%
  add_lines(data = l4B_sm_halves_l347_dry,
            y = ~change_sm_top,
            name = "Change in Moisture, Top 25 cm (L4)") %>%
  add_lines(data = l7A_sm_halves_l347_dry,
            y = ~total_moisture_soil_only,
            name = "Total Moisture in Soil (L7)") %>%
  add_lines(data = l7A_sm_halves_l347_dry,
            y = ~change_sm_top,
            name = "Change in Moisture, Top 25 cm (L7)")

# Changes for the bottom

plot_ly(x = ~time) %>%
  add_lines(data = l4B_sm_halves_l347_dry,
            y = ~total_moisture_soil_only,
            name = "Total Moisture in Soil (L4)") %>%
  add_lines(data = l4B_sm_halves_l347_dry,
            y = ~change_sm_bottom,
            name = "Change in Moisture, Bottom 42.5 cm (L4)") %>%
  add_lines(data = l7A_sm_halves_l347_dry,
            y = ~total_moisture_soil_only,
            name = "Total Moisture in Soil (L7)") %>%
  add_lines(data = l7A_sm_halves_l347_dry,
            y = ~change_sm_bottom,
            name = "Change in Moisture, Bottom 42.5 cm (L7)")


## Plotting the proportion of change in soil moisture (in soil itself)
## explained by changes in each half

l4_percent_top_bottom <- plot_ly(x = ~time) %>%
  add_lines(data = l4B_sm_halves_l347_dry,
            y = ~percent_change_due_to_top,
            name = "Prop. of Total Moisture Loss Coming from Top 25 cm (L4)") %>%
  add_lines(data = l4B_sm_halves_l347_dry,
            y = ~percent_change_due_to_bottom,
            name = "Prop. of Total Moisture Loss Coming from Bottom 42.5 cm (L4)")



l7_percent_top_bottom <- plot_ly(x = ~time) %>%
  add_lines(data = l7A_sm_halves_l347_dry,
            y = ~percent_change_due_to_top,
            name = "Prop. of Total Moisture Loss Coming from Top 25 cm (L7)") %>%
  add_lines(data = l7A_sm_halves_l347_dry,
            y = ~percent_change_due_to_bottom,
            name = "Prop. of Total Moisture Loss Coming from Bottom 42.5 cm (L7)")


subplot(l4_percent_top_bottom, l7_percent_top_bottom, nrows = 2)

l4_percent_top_bottom <- plot_ly(x = ~time) %>%
  add_lines(data = l4B_sm_halves_l347_dry,
            y = ~percent_sm_in_top,
            name = "Prop. of Total Moisture in Top 25 cm (L4)") %>%
  add_lines(data = l4B_sm_halves_l347_dry,
            y = ~percent_sm_in_bottom,
            name = "Prop. of Total Moisture in Bottom 42.5 cm (L4)")



l7_percent_top_bottom <- plot_ly(x = ~time) %>%
  add_lines(data = l7A_sm_halves_l347_dry,
            y = ~percent_sm_in_top,
            name = "Prop. of Total Moisture in Top 25 cm (L7)") %>%
  add_lines(data = l7A_sm_halves_l347_dry,
            y = ~percent_sm_in_bottom,
            name = "Prop. of Total Moisture in Bottom 42.5 cm (L7)")


subplot(l4_percent_top_bottom, l7_percent_top_bottom, nrows = 2)


plot_ly() %>%
  add_lines(data = l4B_sm_halves_l347_dry,
            x = ~time,
            y = ~percent_sm_in_bottom,
            name = "Amenity Grass Lysimeter \n (L4, restricted)") %>%
  add_lines(data = l7A_sm_halves_l347_dry,
            x = ~time,
            y = ~percent_sm_in_bottom,
            name = "Iris sibirica Lysimeter \n (L7, restricted)") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Proportion of Soil Moisture Stored in Bottom 42.5 cm of Lysimeter"))


# From point where total moisture in growing media is truly equal (2022-06-09 19:00:00):
l4B_for_mb_l347_dry_equal_sm <- l4B_for_mb_l347_dry %>%
  filter(
    between(time,
            ymd_hms("2022-06-09 19:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )
l7A_for_mb_l347_dry_equal_sm <- l7A_for_mb_l347_dry %>%
  filter(
    between(time,
            ymd_hms("2022-06-09 19:00:00", tz = "UTC"),
            ymd_hms("2022-06-18 14:00:00", tz = "UTC"))
  )
l4B_mb_l347_dry_equal_sm <- mass_balance_any_period_cal(l4B_for_mb_l347_dry_equal_sm, c(7:15), 6, 2.617265)
l7A_mb_l347_dry_equal_sm <- mass_balance_any_period_cal(l7A_for_mb_l347_dry_equal_sm, c(7:15), 6, 2.617265)
l4B_sm_halves_l347_dry_equal_sm <- soil_moisture_halves(l4B_mb_l347_dry_equal_sm, 20, c(7:15), 2.617265)
l7A_sm_halves_l347_dry_equal_sm <- soil_moisture_halves(l7A_mb_l347_dry_equal_sm, 20, c(7:15), 2.617265)

plot_ly(x = ~time,
        y = ~total_moisture_soil_only) %>%
  add_lines(data = l4B_sm_halves_l347_dry_equal_sm,
            name = "Lysimeter 4, restricted") %>%
  add_lines(data = l7A_sm_halves_l347_dry_equal_sm,
            name = "Lysimeter 7, restricted") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Soil Moisture Content (Growing Medium), L"),
         legend = list(orientation = "h",
                       x = 0.5,
                       y = 0.15))

plot_ly(x = ~time,
        y = ~percent_sm_in_bottom) %>%
  add_lines(data = l4B_sm_halves_l347_dry_equal_sm,
            name = "Lysimeter 4, restricted") %>%
  add_lines(data = l7A_sm_halves_l347_dry_equal_sm,
            name = "Lysimeter 7, restricted") %>%
  layout(xaxis = list(title = "Date/Time, hourly intervals"),
         yaxis = list(title = "Propoertion of Soil Moisture Content in \n Bottom 42.5 cm of Growing Medium, L"),
         legend = list(orientation = "h",
                       x = 0.5,
                       y = 0.15))
