# Statistical Tests and Plots performed on the dataframes from the 
# "RO2 - Calculating Event Metrics" file.

## Full Set of Events Examined (N = 58): ##
# Peak flow attenuation
shapiro.test(out_met30$peak_flow_attenuation_A)
shapiro.test(out_met30$peak_flow_attenuation_B)
diffs <- out_met30$peak_flow_attenuation_A - out_met30$peak_flow_attenuation_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met30$peak_flow_attenuation_A,
         y = out_met30$peak_flow_attenuation_B)
cliff.delta(out_met30$peak_flow_attenuation_A, out_met30$peak_flow_attenuation_B)


# Time to peak outflow rate
shapiro.test(out_met30$time_to_peak_A)
shapiro.test(out_met30$time_to_peak_B)
diffs <- out_met30$time_to_peak_A - out_met30$time_to_peak_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met30$time_to_peak_A,
         y = out_met30$time_to_peak_B)
cliff.delta(out_met30$time_to_peak_A, out_met30$time_to_peak_B)


# Volume reduction
shapiro.test(out_met30$volume_reduction_A)
shapiro.test(out_met30$volume_reduction_B)
diffs <- out_met30$volume_reduction_A - out_met30$volume_reduction_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met30$volume_reduction_A,
         y = out_met30$volume_reduction_B)
cliff.delta(out_met30$volume_reduction_A, out_met30$volume_reduction_B)

test <- data.frame(event = ,
                   vra = out_met30$volume_reduction_A,
                   vrb = out_met30$volume_reduction_B)


# Total drainage volume (start to end)
shapiro.test(out_met30$total_drainage_volume_A)
shapiro.test(out_met30$total_drainage_volume_B)
diffs <- out_met30$total_drainage_volume_A - out_met30$total_drainage_volume_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met30$total_drainage_volume_A,
         y = out_met30$total_drainage_volume_B)
cliff.delta(out_met30$total_drainage_volume_A, out_met30$total_drainage_volume_B)


# Total drainage volume (start to 24 h  after end)
shapiro.test(out_met30$total_drainage_volume_A_24)
shapiro.test(out_met30$total_drainage_volume_B_24)
diffs <- out_met30$total_drainage_volume_A_24 - out_met30$total_drainage_volume_B_24
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met30$total_drainage_volume_A_24,
         y = out_met30$total_drainage_volume_B_24)
cliff.delta(out_met30$total_drainage_volume_A_24, out_met30$total_drainage_volume_B)


# Percent GFRR reached at max outflow rate
shapiro.test(out_met30$gf_percent_reached_at_max_A)
shapiro.test(out_met30$gf_percent_reached_at_max_B)
diffs <- out_met30$gf_percent_reached_at_max_A - out_met30$gf_percent_reached_at_max_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met30$gf_percent_reached_at_max_A,
         y = out_met30$gf_percent_reached_at_max_B)
cliff.delta(out_met30$gf_percent_reached_at_max_A, out_met30$gf_percent_reached_at_max_B)


# 10 cm (TOP), percentage of capacity reached at peak soil moisture
shapiro.test(out_met10$percent_capacity_at_peak_A)
shapiro.test(out_met10$percent_capacity_at_peak_B)
diffs <- out_met10$percent_capacity_at_peak_A - out_met10$percent_capacity_at_peak_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met10$percent_capacity_at_peak_A,
         y = out_met10$percent_capacity_at_peak_B)
cliff.delta(out_met10$percent_capacity_at_peak_A, out_met10$percent_capacity_at_peak_B)


# 40 cm (MIDDLE), percentage of capacity reached at peak soil moisture
shapiro.test(out_met40$percent_capacity_at_peak_A)
shapiro.test(out_met40$percent_capacity_at_peak_B)
diffs <- out_met40$percent_capacity_at_peak_A - out_met40$percent_capacity_at_peak_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met40$percent_capacity_at_peak_A,
         y = out_met40$percent_capacity_at_peak_B)
cliff.delta(out_met40$percent_capacity_at_peak_A, out_met40$percent_capacity_at_peak_B)


# 75 cm (BOTTOM), percentage of capacity reached at peak soil moisture
shapiro.test(out_met75$percent_capacity_at_peak_A)
shapiro.test(out_met75$percent_capacity_at_peak_B)
diffs <- out_met75$percent_capacity_at_peak_A - out_met75$percent_capacity_at_peak_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met75$percent_capacity_at_peak_A,
         y = out_met75$percent_capacity_at_peak_B)
cliff.delta(out_met75$percent_capacity_at_peak_A, out_met75$percent_capacity_at_peak_B)


## Subset of Larger Events (N = 18): ##
out_met30_large <- out_met30[out_met30$total_rain_L > 17, ]
out_met10_large <- out_met10[out_met10$total_rain_L > 17, ]
out_met40_large <- out_met40[out_met40$total_rain_L > 17, ]
out_met75_large <- out_met75[out_met75$total_rain_L > 17, ]

# Peak flow attenuation
shapiro.test(out_met30_large$peak_flow_attenuation_A)
shapiro.test(out_met30_large$peak_flow_attenuation_B)
diffs <- out_met30_large$peak_flow_attenuation_A - out_met30_large$peak_flow_attenuation_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met30_large$peak_flow_attenuation_A,
         y = out_met30_large$peak_flow_attenuation_B)
cliff.delta(out_met30_large$peak_flow_attenuation_A, out_met30_large$peak_flow_attenuation_B)


# Time to peak outflow rate
shapiro.test(out_met30_large$time_to_peak_A)
shapiro.test(out_met30_large$time_to_peak_B)
diffs <- out_met30_large$time_to_peak_A - out_met30_large$time_to_peak_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met30_large$time_to_peak_A,
         y = out_met30_large$time_to_peak_B)
cliff.delta(out_met30_large$time_to_peak_A, out_met30_large$time_to_peak_B)


# Volume reduction
shapiro.test(out_met30_large$volume_reduction_A)
shapiro.test(out_met30_large$volume_reduction_B)
diffs <- out_met30_large$volume_reduction_A - out_met30_large$volume_reduction_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met30_large$volume_reduction_A,
         y = out_met30_large$volume_reduction_B)
cliff.delta(out_met30_large$volume_reduction_A, out_met30_large$volume_reduction_B)


# Total drainage volume (start to end)
shapiro.test(out_met30_large$total_drainage_volume_A)
shapiro.test(out_met30_large$total_drainage_volume_B)
diffs <- out_met30_large$total_drainage_volume_A - out_met30_large$total_drainage_volume_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met30_large$total_drainage_volume_A,
         y = out_met30_large$total_drainage_volume_B)
cliff.delta(out_met30_large$total_drainage_volume_A, out_met30_large$total_drainage_volume_B)


# Total drainage volume (start to 24 h  after end)
shapiro.test(out_met30_large$total_drainage_volume_A_24)
shapiro.test(out_met30_large$total_drainage_volume_B_24)
diffs <- out_met30_large$total_drainage_volume_A_24 - out_met30_large$total_drainage_volume_B_24
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met30_large$total_drainage_volume_A_24,
         y = out_met30_large$total_drainage_volume_B_24)
cliff.delta(out_met30_large$total_drainage_volume_A_24, out_met30_large$total_drainage_volume_B)


# Percent GFRR reached at max outflow rate
shapiro.test(out_met30_large$gf_percent_reached_at_max_A)
shapiro.test(out_met30_large$gf_percent_reached_at_max_B)
diffs <- out_met30_large$gf_percent_reached_at_max_A - out_met30_large$gf_percent_reached_at_max_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met30_large$gf_percent_reached_at_max_A,
         y = out_met30_large$gf_percent_reached_at_max_B)
cliff.delta(out_met30_large$gf_percent_reached_at_max_A, out_met30_large$gf_percent_reached_at_max_B)


# 10 cm (TOP), percentage of capacity reached at peak soil moisture
shapiro.test(out_met10_large$percent_capacity_at_peak_A)
shapiro.test(out_met10_large$percent_capacity_at_peak_B)
diffs <- out_met10_large$percent_capacity_at_peak_A - out_met10_large$percent_capacity_at_peak_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met10_large$percent_capacity_at_peak_A,
         y = out_met10_large$percent_capacity_at_peak_B)
cliff.delta(out_met10_large$percent_capacity_at_peak_A, out_met10_large$percent_capacity_at_peak_B)


# 40 cm (MIDDLE), percentage of capacity reached at peak soil moisture
shapiro.test(out_met40_large$percent_capacity_at_peak_A)
shapiro.test(out_met40_large$percent_capacity_at_peak_B)
diffs <- out_met40_large$percent_capacity_at_peak_A - out_met40_large$percent_capacity_at_peak_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met40_large$percent_capacity_at_peak_A,
         y = out_met40_large$percent_capacity_at_peak_B)
cliff.delta(out_met40_large$percent_capacity_at_peak_A, out_met40_large$percent_capacity_at_peak_B)


# 75 cm (BOTTOM), percentage of capacity reached at peak soil moisture
shapiro.test(out_met75_large$percent_capacity_at_peak_A)
shapiro.test(out_met75_large$percent_capacity_at_peak_B)
diffs <- out_met75_large$percent_capacity_at_peak_A - out_met75_large$percent_capacity_at_peak_B
qqnorm(diffs)
qqline(diffs)
hist(diffs)

SignTest(x = out_met75_large$percent_capacity_at_peak_A,
         y = out_met75_large$percent_capacity_at_peak_B)
cliff.delta(out_met75_large$percent_capacity_at_peak_A, out_met75_large$percent_capacity_at_peak_B)





