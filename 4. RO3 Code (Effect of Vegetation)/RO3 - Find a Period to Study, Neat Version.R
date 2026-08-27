# Find a Period to Study, from:
# May, June, July, August (2021) - 2021-05-01 00:00:00 to 2021-08-31 23:00:00
# 2022 design storm timings
## Last design storm end, May: 2022-05-11 17:40:00
## Start of design storms, August: 2022-08-23 14:00:00
# May, June, July, August (2022) - 2022-05-18 18:00:00 to 2022-08-23 13:00:00


# 2021 Period
p2021_start <- ymd_hms("2021-05-01 00:00:00", tz = "UTC")
p2021_end <- ymd_hms("2021-08-31 23:00:00", tz = "UTC")

# 2022 Period:
p2022_start <- ymd_hms("2022-05-18 18:00:00", tz = "UTC")
p2022_end <- ymd_hms("2022-08-23 13:00:00", tz = "UTC")

# Outflow data:
# Unrestricted:
l3_outB_smooth_L <- convert_ml_l((l3_outHR_smooth[, c(1, 3)]), 2)
l4_outA_smooth_L <- convert_ml_l((l4_outHR_smooth[, c(1, 2)]), 2)
l7_outB_smooth_L <- convert_ml_l((l7_outHR_smooth[, c(1, 3)]), 2)
l8_outB_smooth_L <- convert_ml_l((l8_outHR_smooth[, c(1, 3)]), 2)
# Restricted:
l3_outA_smooth_L <- convert_ml_l((l3_outHR_smooth[, c(1, 2)]), 2)
l4_outB_smooth_L <- convert_ml_l((l4_outHR_smooth[, c(1, 3)]), 2)
l7_outA_smooth_L <- convert_ml_l((l7_outHR_smooth[, c(1, 2)]), 2)
l8_outA_smooth_L <- convert_ml_l((l8_outHR_smooth[, c(1, 2)]), 2)

# Join With Soil Moisture Data (hourly, interpolated, litres):
# Unrestricted
l3_out_sm_B_L <- combine_data(list(l3_outB_smooth_L, l3_all_moist_B_int))
l4_out_sm_A_L <- combine_data(list(l4_outA_smooth_L, l4_all_moist_A_int))
l7_out_sm_B_L <- combine_data(list(l7_outB_smooth_L, l7_all_moist_B_int))
l8_out_sm_B_L <- combine_data(list(l8_outB_smooth_L, l8_all_moist_B_int))

all_out_sm_unr_L <- combine_data(list(l3_out_sm_B_L, l4_out_sm_A_L,
                                      l7_out_sm_B_L, l8_out_sm_B_L))

# Restricted
l3_out_sm_A_L <- combine_data(list(l3_outA_smooth_L, l3_all_moist_A_int))
l4_out_sm_B_L <- combine_data(list(l4_outB_smooth_L, l4_all_moist_B_int))
l7_out_sm_A_L <- combine_data(list(l7_outA_smooth_L, l7_all_moist_A_int))
l8_out_sm_A_L <- combine_data(list(l8_outA_smooth_L, l8_all_moist_A_int))

all_out_sm_restr_L <- combine_data(list(l3_out_sm_A_L, l4_out_sm_B_L,
                                        l7_out_sm_A_L, l8_out_sm_A_L))


# Finding available data for 2021 period:
all_out_sm_unr_L_2021 <- all_out_sm_unr_L %>%
  filter(
    between(time,
            p2021_start,
            p2021_end)
    )
vis_miss(all_out_sm_unr_L_2021) +
  theme(plot.margin = margin(t = 40, r = 50)) +
  ggtitle("Missing Mass Balance Data in 2021 Beginning May to End August 2021 (Unrestricted)") +
  theme(axis.text.x = element_text(vjust = -0.25))

all_out_sm_restr_L_2021 <- all_out_sm_restr_L %>%
  filter(
    between(time,
            p2021_start,
            p2021_end)
  )
vis_miss(all_out_sm_restr_L_2021) +
  theme(plot.margin = margin(t = 40, r = 50)) +
  ggtitle("Missing Mass Balance Data in 2021 Beginning May to End August 2021 (Restricted)") +
  theme(axis.text.x = element_text(vjust = -0.25))

# Only L3 and L4 Can be compared in 2021
# Restricted between: 2021-05-20 11:00:00 and 2021-08-31 23:00:00
# Unrestricted between: 2021-05-20 11:00:00 and 2021-08-31 23:00:00


# Finding available data for 2022 period:
all_out_sm_unr_L_2022 <- all_out_sm_unr_L %>%
  filter(
    between(time,
            p2022_start,
            p2022_end)
  )
vis_miss(all_out_sm_unr_L_2022) +
  theme(plot.margin = margin(t = 40, r = 50)) +
  ggtitle("Missing Mass Balance Data from 18th May to 23rd August 2022 (Unrestricted)") +
  theme(axis.text.x = element_text(vjust = -0.25))

all_out_sm_unr_L_2022_no_out <- na.omit(all_out_sm_unr_L_2022)
rownames(all_out_sm_unr_L_2022_no_out) <- NULL
#complete_times(all_out_sm_unr_L_2022_no_out)

# No data for lysimeter 8, therefore no complete data for all lysimeters

all_out_sm_restr_L_2022 <- all_out_sm_restr_L %>%
  filter(
    between(time,
            p2022_start,
            p2022_end)
  )
vis_miss(all_out_sm_restr_L_2022) +
  theme(plot.margin = margin(t = 40, r = 50)) +
  ggtitle("Missing Mass Balance Data from 18th May to 23rd August 2022 (Restricted)") +
  theme(axis.text.x = element_text(vjust = -0.25))

all_out_sm_restr_L_2022_no_out <- na.omit(all_out_sm_restr_L_2022)
rownames(all_out_sm_restr_L_2022_no_out) <- NULL
vis_miss(complete_times(all_out_sm_restr_L_2022_no_out))
all_out_sm_restr_L_2022_no_out_compl_times <- complete_times(all_out_sm_restr_L_2022_no_out)

# There are three periods where data is available for all lysimeters
# (Restricted outflow):
# 2022-06-17 13:00:00 to 2022-06-19 17:00:00
# 2022-07-19 06:00:00 to 2022-07-21 12:00:00
# Too short

# Without most incomplete lysimeter (lysimeter 8):
l347_out_sm_unr_L <- combine_data(list(l3_out_sm_B_L, l4_out_sm_A_L, l7_out_sm_B_L))
l347_out_sm_restr_L <- combine_data(list(l3_out_sm_A_L, l4_out_sm_B_L, l7_out_sm_A_L))

l347_out_sm_unr_L_2022 <- l347_out_sm_unr_L %>%
  filter(
    between(time,
            p2022_start,
            p2022_end)
  )
vis_miss(l347_out_sm_unr_L_2022) +
  theme(plot.margin = margin(t = 40, r = 50)) +
  ggtitle("Missing Mass Balance Data from 18th May to 23rd August 2022 (Unrestricted, L3, L4, L7 only)") +
  theme(axis.text.x = element_text(vjust = -0.25))


l347_out_sm_unr_L_2022_no_out <- na.omit(l347_out_sm_unr_L_2022)
vis_miss(complete_times(l347_out_sm_unr_L_2022_no_out))
l347_out_sm_unr_L_2022_no_out_compl_times <- complete_times(l347_out_sm_unr_L_2022_no_out)

l347_out_sm_restr_L_2022 <- l347_out_sm_restr_L %>%
  filter(
    between(time,
            p2022_start,
            p2022_end)
  )
vis_miss(l347_out_sm_restr_L_2022) +
  theme(plot.margin = margin(t = 40, r = 50)) +
  ggtitle("Missing Mass Balance Data from 18th May to 23rd August 2022 (Unrestricted, L3, L4, L7 only)") +
  theme(axis.text.x = element_text(vjust = -0.25))


l347_out_sm_restr_L_2022_no_out <- na.omit(l347_out_sm_restr_L_2022)
vis_miss(complete_times(l347_out_sm_restr_L_2022_no_out))
l347_out_sm_restr_L_2022_no_out_compl_times <- complete_times(l347_out_sm_restr_L_2022_no_out)


# Without next most incomplete lysimeter (lysimeter 7):
l34_out_sm_unr_L <- combine_data(list(l3_out_sm_B_L, l4_out_sm_A_L))
l34_out_sm_restr_L <- combine_data(list(l3_out_sm_A_L, l4_out_sm_B_L))

l34_out_sm_unr_L_2022 <- l34_out_sm_unr_L %>%
  filter(
    between(time,
            p2022_start,
            p2022_end)
  )
vis_miss(l34_out_sm_unr_L_2022)
l34_out_sm_unr_L_2022_no_out <- na.omit(l34_out_sm_unr_L_2022)
vis_miss(complete_times(l34_out_sm_unr_L_2022_no_out))
l34_out_sm_unr_L_2022_no_out_compl_times <- complete_times(l34_out_sm_unr_L_2022_no_out)


l34_out_sm_restr_L_2022 <- l34_out_sm_restr_L %>%
  filter(
    between(time,
            p2022_start,
            p2022_end)
  )
vis_miss(l34_out_sm_restr_L_2022)
l34_out_sm_restr_L_2022_no_out <- na.omit(l34_out_sm_restr_L_2022)
vis_miss(complete_times(l34_out_sm_restr_L_2022_no_out))
l34_out_sm_restr_L_2022_no_out_compl_times <- complete_times(l34_out_sm_restr_L_2022_no_out)




# Summary of periods that can be studied:

# 2021:
# Lysimeters 3 and 4 ONLY
# Unrestricted and restricted:
# Between: 2021-05-20 11:00:00 and 2021-08-31 23:00:00
# Can potentially do more formal comparisons here.


# 2022:
# Lysimeters 3, 4, and 7 ONLY
# Unrestricted:
# Between: 2022-05-27 23:00:00 and 2022-06-10 22:00:00
# And: 2022-06-11 05:00:00 and 2022-06-18 14:00:00

# Gap of 7 hours between these periods. Interpolation could be reasonable.

# Lysimeters 3 and 7:



# Lysimeters 3 and 4:
# Restricted:
# Between: 2022-06-17 13:00:00 and 2022-07-05 03:00:00
# And: 2022-07-07 14:00:00 and 2022-08-23 13:00:00


