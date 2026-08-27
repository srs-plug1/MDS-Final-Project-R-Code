## Peak Flow Attenuation:
# Time spread:
cor.test(out_met30$peak_flow_attenuation_A, out_met30$time_spread, method = "spearman")
cor.test(out_met30$peak_flow_attenuation_B, out_met30$time_spread, method = "spearman")

cor.test(out_met30$peak_flow_attenuation_A, out_met30$time_spread, method = "kendall")
cor.test(out_met30$peak_flow_attenuation_B, out_met30$time_spread, method = "kendall")
# p-values are 0.4816 (A) and 0.7226 (B)


# Average intensity:
cor.test(out_met30$peak_flow_attenuation_A, out_met30$average_intensity, method = "spearman")
cor.test(out_met30$peak_flow_attenuation_B, out_met30$average_intensity, method = "spearman")
# p-values are 0.8293 (A) and 0.669 (B)

cor.test(out_met30$peak_flow_attenuation_A, out_met30$average_intensity, method = "kendall")
cor.test(out_met30$peak_flow_attenuation_B, out_met30$average_intensity, method = "kendall")
# p-values are 0.885 (A) and 0.6498 (B)


# Total event volume:
cor.test(out_met30$peak_flow_attenuation_A, out_met30$total_rain_L, method = "spearman")
cor.test(out_met30$peak_flow_attenuation_B, out_met30$total_rain_L, method = "spearman")
# p-values are 0.6173 (A) and 0.2399 (B)

cor.test(out_met30$peak_flow_attenuation_A, out_met30$total_rain_L, method = "kendall")
cor.test(out_met30$peak_flow_attenuation_B, out_met30$total_rain_L, method = "kendall")
# p-values are 0.6248 (A) and 0.2312 (B)


## Drainage volume and volume reduction:
plot(out_met30$total_drainage_volume_A, out_met30$volume_reduction_A)
cor.test(out_met30$total_drainage_volume_A, out_met30$volume_reduction_A, na.rm = T, method = "pearson")
plot(out_met30$total_drainage_volume_B, out_met30$volume_reduction_B)
cor.test(out_met30$total_drainage_volume_B, out_met30$volume_reduction_B, na.rm = T, method = "pearson")

## Drainage Volume:
# Time spread:
cor.test(out_met30$total_drainage_volume_A, out_met30$time_spread, method = "spearman")
cor.test(out_met30$total_drainage_volume_B, out_met30$time_spread, method = "spearman")

cor.test(out_met30$total_drainage_volume_A, out_met30$time_spread, method = "kendall")
cor.test(out_met30$total_drainage_volume_B, out_met30$time_spread, method = "kendall")


# Average intensity:
cor.test(out_met30$total_drainage_volume_A, out_met30$average_intensity, method = "spearman")
cor.test(out_met30$total_drainage_volume_B, out_met30$average_intensity, method = "spearman")
# p-values are 0.6564 (A) and 0.5979 (B).

cor.test(out_met30$total_drainage_volume_A, out_met30$average_intensity, method = "kendall")
cor.test(out_met30$total_drainage_volume_B, out_met30$average_intensity, method = "kendall")
# p-values are 0.6947 (A) and 0.6104 (B).


# Total event volume:
cor.test(out_met30$total_drainage_volume_A, out_met30$total_rain_L, method = "spearman")
cor.test(out_met30$total_drainage_volume_B, out_met30$total_rain_L, method = "spearman")
# p-values are 2.307e-14 (A) and 2.2e-16 (B). Rho: 0.8101104 (A), 0.8457274 (B)

cor.test(out_met30$total_drainage_volume_A, out_met30$total_rain_L, method = "kendall")
cor.test(out_met30$total_drainage_volume_B, out_met30$total_rain_L, method = "kendall")
# p-values are 1.126e-11 (A) and 3.543e-13 (B). Tau: 0.6199311 (A), 0.6645689 (B)



## Volume Reduction:
# Time spread:
cor.test(out_met30$volume_reduction_A, out_met30$time_spread, method = "spearman")
cor.test(out_met30$volume_reduction_B, out_met30$time_spread, method = "spearman")

cor.test(out_met30$volume_reduction_A, out_met30$time_spread, method = "kendall")
cor.test(out_met30$volume_reduction_B, out_met30$time_spread, method = "kendall")


# Average intensity:
cor.test(out_met30$volume_reduction_A, out_met30$average_intensity, method = "spearman")
cor.test(out_met30$volume_reduction_B, out_met30$average_intensity, method = "spearman")
# p-values are 0.01505 (A) and 0.003166 (B). Rho: 0.3205743 (A), 0.3842676 (B)

cor.test(out_met30$volume_reduction_A, out_met30$average_intensity, method = "kendall")
cor.test(out_met30$volume_reduction_B, out_met30$average_intensity, method = "kendall")
# p-values are 0.007558 (A) and 0.002088 (B). Tau: 0.2434128 (A), 0.2805146 (B)


# Total event volume:
cor.test(out_met30$volume_reduction_A, out_met30$total_rain_L, method = "spearman")
cor.test(out_met30$volume_reduction_B, out_met30$total_rain_L, method = "spearman")
# p-values are 0.04327 (A) and 0.01555 (B). Rho: -0.2687065 (A), -0.3190982 (B)

cor.test(out_met30$volume_reduction_A, out_met30$total_rain_L, method = "kendall")
cor.test(out_met30$volume_reduction_B, out_met30$total_rain_L, method = "kendall")
# p-values are 0.03453 (A) and 0.01151 (B). Tau: -0.1928395 (A), -0.2306003  (B)




