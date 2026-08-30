## RAINFALL
L3_rain5 <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[Rain - 5 minutes].csv")
L4_rain5 <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[Rain - 5 minutes].csv")
L7_rain5 <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[Rain - 5 minutes].csv")
L8_rain5 <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[Rain - 5 minutes].csv")
L8_rain5 <- L8_rain5[-52298,]

convert_time(c("L3_rain5", "L4_rain5", "L7_rain5", "L8_rain5"), global = T)

L3_rain5 <- L3_rain5 %>% dplyr::select(time, everything())
L4_rain5 <- L4_rain5 %>% dplyr::select(time, everything())
L7_rain5 <- L7_rain5 %>% dplyr::select(time, everything())
L8_rain5 <- L8_rain5 %>% dplyr::select(time, everything())

rain5 <- combine_data(list(L3_rain5, L4_rain5, L7_rain5, L8_rain5))

vis_miss(rain5) +
  theme(plot.margin = margin(t = 40)) +
  ggtitle("Missing Data in 5-minute Rainfall Data (Bioretention Cell Lysimeters)") +
  theme(axis.text.x = element_text(vjust = -0.25))

# Rain5 aggregated:
rainHR1 <- aggregate_data(rain5, 5, 60, 2)
rainHR2 <- aggregate_data(rain5, 5, 60, 3)
rainHR3 <- aggregate_data(rain5, 5, 60, 4)
rainHR4 <- aggregate_data(rain5, 5, 60, 5)

rainHR <- combine_data(list(rainHR1, rainHR2, rainHR3, rainHR4))



## RAINFALL HOURLY
L3_rain60 <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[Rain - Hourly Total].csv")
L4_rain60 <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[Rain - Hourly Total].csv")
L7_rain60 <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[Rain - Hourly Total].csv")
L8_rain60 <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[Rain - Hourly Total].csv")

convert_time(c("L3_rain60", "L4_rain60", "L7_rain60", "L8_rain60"), global = T)

L3_rain60 <- L3_rain60 %>% dplyr::select(time, everything())
L4_rain60 <- L4_rain60 %>% dplyr::select(time, everything())
L7_rain60 <- L7_rain60 %>% dplyr::select(time, everything())
L8_rain60 <- L8_rain60 %>% dplyr::select(time, everything())

rain60 <- combine_data(list(L3_rain60, L4_rain60, L7_rain60, L8_rain60))

vis_miss(rain60) +
  theme(plot.margin = margin(t = 40)) +
  ggtitle("Missing Data in 60-minute Rainfall Data (Bioretention Cell Lysimeters)") +
  theme(axis.text.x = element_text(vjust = -0.25))

rain60_ave <- average_columns(rain60, c(2:5), "rain60")


## RAINFALL DAILY
L3_rainDL <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[Rain - Daily Total].csv")
L4_rainDL <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[Rain - Daily Total].csv")
L7_rainDL <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[Rain - Daily Total].csv")
L8_rainDL <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[Rain - Daily Total].csv")

convert_time(c("L3_rainDL", "L4_rainDL", "L7_rainDL", "L8_rainDL"), global = T)

L3_rainDL <- L3_rainDL %>% dplyr::select(time, everything())
L4_rainDL <- L4_rainDL %>% dplyr::select(time, everything())
L7_rainDL <- L7_rainDL %>% dplyr::select(time, everything())
L8_rainDL <- L8_rainDL %>% dplyr::select(time, everything())

rainDL <- combine_data(list(L3_rainDL, L4_rainDL, L7_rainDL, L8_rainDL))

vis_miss(rainDL) +
  theme(plot.margin = margin(t = 40)) +
  ggtitle("Missing Data in Daily Rainfall Data (Bioretention Cell Lysimeters)") +
  theme(axis.text.x = element_text(vjust = -0.25))

# Average Hourly rain:
rainHR_ave <- average_columns(rainHR, c(2:5), "rainHR")


## OUTFLOW
L3_outA <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[Outflow A (ml)].csv")
L3_outB <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[Outflow B (ml)].csv")
L4_outA <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[Outflow A].csv")
L4_outB <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[Outflow B].csv")
L7_outA <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[Outflow A (ml)].csv")
L7_outB <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[Outflow B (ml)].csv")
L8_outA <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[Outflow A (ml)].csv")
L8_outB <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[Outflow B (ml)].csv")

convert_time(c("L3_outA", "L3_outB", "L4_outA", "L4_outB", "L7_outA", "L7_outB", "L8_outA", "L8_outB"), global = T)

L3_outA <- L3_outA %>% dplyr::select(time, everything())
L4_outA <- L4_outA %>% dplyr::select(time, everything())
L7_outA <- L7_outA %>% dplyr::select(time, everything())
L8_outA <- L8_outA %>% dplyr::select(time, everything())
L3_outB <- L3_outB %>% dplyr::select(time, everything())
L4_outB <- L4_outB %>% dplyr::select(time, everything())
L7_outB <- L7_outB %>% dplyr::select(time, everything())
L8_outB <- L8_outB %>% dplyr::select(time, everything())

colnames(L3_outA)[2] <- "L3_outA"
colnames(L3_outB)[2] <- "L3_outB"
colnames(L4_outA)[2] <- "L4_outA"
colnames(L4_outB)[2] <- "L4_outB"
colnames(L7_outA)[2] <- "L7_outA"
colnames(L7_outB)[2] <- "L7_outB"
colnames(L8_outA)[2] <- "L8_outA"
colnames(L8_outB)[2] <- "L8_outB"

# Outflow aggregate

out5 <- combine_data(list(L3_outA, L3_outB, L4_outA, L4_outB, L7_outA, L7_outB, L8_outA, L8_outB))

outHR3A <- aggregate_data(out5, 5, 60, 2)
outHR3B <- aggregate_data(out5, 5, 60, 3)
outHR4A <- aggregate_data(out5, 5, 60, 4)
outHR4B <- aggregate_data(out5, 5, 60, 5)
outHR7A <- aggregate_data(out5, 5, 60, 6)
outHR7B <- aggregate_data(out5, 5, 60, 7)
outHR8A <- aggregate_data(out5, 5, 60, 8)
outHR8B <- aggregate_data(out5, 5, 60, 9)

outHR <- combine_data(list(outHR3A, outHR3B, outHR4A, outHR4B, outHR7A, outHR7B, outHR8A, outHR8B))
colnames(outHR)[2:9] <- c("l3A", "l3B", "l4A", "l4B", "l7A", "l7B", "l8A", "l8B")

vis_miss(outHR) +
  theme(plot.margin = margin(t = 40)) +
  ggtitle("Missing Data in Outflow Data") +
  theme(axis.text.x = element_text(vjust = -0.25))

# Outflow smooth

out_HR_smooth_ml <- smooth_outflow(outHR, c(2:9))
out_HR_smooth_litres <- out_HR_smooth_ml
out_HR_smooth_litres[, c(2:9)] <- out_HR_smooth_litres[, c(2:9)]/1000


## SOIL MOISTURE DATA
l3_vwc5_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[A_SoilVUE_VWC_5cm].csv")
l3_vwc5_A$time <- ymd_hms(l3_vwc5_A$time)
l3_vwc10_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[A_SoilVUE_VWC_10cm].csv")
l3_vwc10_A$time <- ymd_hms(l3_vwc10_A$time)
l3_vwc20_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[A_SoilVUE_VWC_20cm].csv")
l3_vwc20_A$time <- ymd_hms(l3_vwc20_A$time)
l3_vwc30_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[A_SoilVUE_VWC_30cm].csv")
l3_vwc30_A$time <- ymd_hms(l3_vwc30_A$time)
l3_vwc40_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[A_SoilVUE_VWC_40cm].csv")
l3_vwc40_A$time <- ymd_hms(l3_vwc40_A$time)
l3_vwc50_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[A_SoilVUE_VWC_50cm].csv")
l3_vwc50_A$time <- ymd_hms(l3_vwc50_A$time)
l3_vwc60_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[A_SoilVUE_VWC_60cm].csv")
l3_vwc60_A$time <- ymd_hms(l3_vwc60_A$time)
l3_vwc75_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[A_SoilVUE_VWC_75cm].csv")
l3_vwc75_A$time <- ymd_hms(l3_vwc75_A$time)
l3_vwc100_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[A_SoilVUE_VWC_100cm].csv")
l3_vwc100_A$time <- ymd_hms(l3_vwc100_A$time)

l3_vwc5_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[B_SoilVUE_VWC_5cm].csv")
l3_vwc5_B$time <- ymd_hms(l3_vwc5_B$time)
l3_vwc10_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[B_SoilVUE_VWC_10cm].csv")
l3_vwc10_B$time <- ymd_hms(l3_vwc10_B$time)
l3_vwc20_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[B_SoilVUE_VWC_20cm].csv")
l3_vwc20_B$time <- ymd_hms(l3_vwc20_B$time)
l3_vwc30_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[B_SoilVUE_VWC_30cm].csv")
l3_vwc30_B$time <- ymd_hms(l3_vwc30_B$time)
l3_vwc40_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[B_SoilVUE_VWC_40cm].csv")
l3_vwc40_B$time <- ymd_hms(l3_vwc40_B$time)
l3_vwc50_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[B_SoilVUE_VWC_50cm].csv")
l3_vwc50_B$time <- ymd_hms(l3_vwc50_B$time)
l3_vwc60_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[B_SoilVUE_VWC_60cm].csv")
l3_vwc60_B$time <- ymd_hms(l3_vwc60_B$time)
l3_vwc75_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[B_SoilVUE_VWC_75cm].csv")
l3_vwc75_B$time <- ymd_hms(l3_vwc75_B$time)
l3_vwc100_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 3]-[B_SoilVUE_VWC_100cm].csv")
l3_vwc100_B$time <- ymd_hms(l3_vwc100_B$time)


l4_vwc5_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[A_SoilVUE_VWC_5cm].csv")
l4_vwc5_A$time <- ymd_hms(l4_vwc5_A$time)
l4_vwc10_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[A_SoilVUE_VWC_10cm].csv")
l4_vwc10_A$time <- ymd_hms(l4_vwc10_A$time)
l4_vwc20_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[A_SoilVUE_VWC_20cm].csv")
l4_vwc20_A$time <- ymd_hms(l4_vwc20_A$time)
l4_vwc30_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[A_SoilVUE_VWC_30cm].csv")
l4_vwc30_A$time <- ymd_hms(l4_vwc30_A$time)
l4_vwc40_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[A_SoilVUE_VWC_40cm].csv")
l4_vwc40_A$time <- ymd_hms(l4_vwc40_A$time)
l4_vwc50_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[A_SoilVUE_VWC_50cm].csv")
l4_vwc50_A$time <- ymd_hms(l4_vwc50_A$time)
l4_vwc60_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[A_SoilVUE_VWC_60cm].csv")
l4_vwc60_A$time <- ymd_hms(l4_vwc60_A$time)
l4_vwc75_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[A_SoilVUE_VWC_75cm].csv")
l4_vwc75_A$time <- ymd_hms(l4_vwc75_A$time)
l4_vwc100_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[A_SoilVUE_VWC_100cm].csv")
l4_vwc100_A$time <- ymd_hms(l4_vwc100_A$time)

l4_vwc5_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[B_SoilVUE_VWC_5cm].csv")
l4_vwc5_B$time <- ymd_hms(l4_vwc5_B$time)
l4_vwc10_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[B_SoilVUE_VWC_10cm].csv")
l4_vwc10_B$time <- ymd_hms(l4_vwc10_B$time)
l4_vwc20_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[B_SoilVUE_VWC_20cm].csv")
l4_vwc20_B$time <- ymd_hms(l4_vwc20_B$time)
l4_vwc30_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[B_SoilVUE_VWC_30cm].csv")
l4_vwc30_B$time <- ymd_hms(l4_vwc30_B$time)
l4_vwc40_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[B_SoilVUE_VWC_40cm].csv")
l4_vwc40_B$time <- ymd_hms(l4_vwc40_B$time)
l4_vwc50_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[B_SoilVUE_VWC_50cm].csv")
l4_vwc50_B$time <- ymd_hms(l4_vwc50_B$time)
l4_vwc60_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[B_SoilVUE_VWC_60cm].csv")
l4_vwc60_B$time <- ymd_hms(l4_vwc60_B$time)
l4_vwc75_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[B_SoilVUE_VWC_75cm].csv")
l4_vwc75_B$time <- ymd_hms(l4_vwc75_B$time)
l4_vwc100_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 4]-[B_SoilVUE_VWC_100cm].csv")
l4_vwc100_B$time <- ymd_hms(l4_vwc100_B$time)


l7_vwc5_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[A_SoilVUE_VWC_5cm].csv")
l7_vwc5_A$time <- ymd_hms(l7_vwc5_A$time)
l7_vwc10_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[A_SoilVUE_VWC_10cm].csv")
l7_vwc10_A$time <- ymd_hms(l7_vwc10_A$time)
l7_vwc20_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[A_SoilVUE_VWC_20cm].csv")
l7_vwc20_A$time <- ymd_hms(l7_vwc20_A$time)
l7_vwc30_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[A_SoilVUE_VWC_30cm].csv")
l7_vwc30_A$time <- ymd_hms(l7_vwc30_A$time)
l7_vwc40_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[A_SoilVUE_VWC_40cm].csv")
l7_vwc40_A$time <- ymd_hms(l7_vwc40_A$time)
l7_vwc50_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[A_SoilVUE_VWC_50cm].csv")
l7_vwc50_A$time <- ymd_hms(l7_vwc50_A$time)
l7_vwc60_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[A_SoilVUE_VWC_60cm].csv")
l7_vwc60_A$time <- ymd_hms(l7_vwc60_A$time)
l7_vwc75_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[A_SoilVUE_VWC_75cm].csv")
l7_vwc75_A$time <- ymd_hms(l7_vwc75_A$time)
l7_vwc100_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[A_SoilVUE_VWC_100cm].csv")
l7_vwc100_A$time <- ymd_hms(l7_vwc100_A$time)


l7_vwc5_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[B_SoilVUE_VWC_5cm].csv")
l7_vwc5_B$time <- ymd_hms(l7_vwc5_B$time)
l7_vwc10_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[B_SoilVUE_VWC_10cm].csv")
l7_vwc10_B$time <- ymd_hms(l7_vwc10_B$time)
l7_vwc20_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[B_SoilVUE_VWC_20cm].csv")
l7_vwc20_B$time <- ymd_hms(l7_vwc20_B$time)
l7_vwc30_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[B_SoilVUE_VWC_30cm].csv")
l7_vwc30_B$time <- ymd_hms(l7_vwc30_B$time)
l7_vwc40_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[B_SoilVUE_VWC_40cm].csv")
l7_vwc40_B$time <- ymd_hms(l7_vwc40_B$time)
l7_vwc50_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[B_SoilVUE_VWC_50cm].csv")
l7_vwc50_B$time <- ymd_hms(l7_vwc50_B$time)
l7_vwc60_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[B_SoilVUE_VWC_60cm].csv")
l7_vwc60_B$time <- ymd_hms(l7_vwc60_B$time)
l7_vwc75_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[B_SoilVUE_VWC_75cm].csv")
l7_vwc75_B$time <- ymd_hms(l7_vwc75_B$time)
l7_vwc100_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 7]-[B_SoilVUE_VWC_100cm].csv")
l7_vwc100_B$time <- ymd_hms(l7_vwc100_B$time)


l8_vwc5_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[A_SoilVUE_VWC_5cm].csv")
l8_vwc5_A$time <- ymd_hms(l8_vwc5_A$time)
l8_vwc10_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[A_SoilVUE_VWC_10cm].csv")
l8_vwc10_A$time <- ymd_hms(l8_vwc10_A$time)
l8_vwc20_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[A_SoilVUE_VWC_20cm].csv")
l8_vwc20_A$time <- ymd_hms(l8_vwc20_A$time)
l8_vwc30_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[A_SoilVUE_VWC_30cm].csv")
l8_vwc30_A$time <- ymd_hms(l8_vwc30_A$time)
l8_vwc40_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[A_SoilVUE_VWC_40cm].csv")
l8_vwc40_A$time <- ymd_hms(l8_vwc40_A$time)
l8_vwc50_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[A_SoilVUE_VWC_50cm].csv")
l8_vwc50_A$time <- ymd_hms(l8_vwc50_A$time)
l8_vwc60_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[A_SoilVUE_VWC_60cm].csv")
l8_vwc60_A$time <- ymd_hms(l8_vwc60_A$time)
l8_vwc75_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[A_SoilVUE_VWC_75cm].csv")
l8_vwc75_A$time <- ymd_hms(l8_vwc75_A$time)
l8_vwc100_A <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[A_SoilVUE_VWC_100cm].csv")
l8_vwc100_A$time <- ymd_hms(l8_vwc100_A$time)


l8_vwc5_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[B_SoilVUE_VWC_5cm].csv")
l8_vwc5_B$time <- ymd_hms(l8_vwc5_B$time)
l8_vwc10_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[B_SoilVUE_VWC_10cm].csv")
l8_vwc10_B$time <- ymd_hms(l8_vwc10_B$time)
l8_vwc20_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[B_SoilVUE_VWC_20cm].csv")
l8_vwc20_B$time <- ymd_hms(l8_vwc20_B$time)
l8_vwc30_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[B_SoilVUE_VWC_30cm].csv")
l8_vwc30_B$time <- ymd_hms(l8_vwc30_B$time)
l8_vwc40_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[B_SoilVUE_VWC_40cm].csv")
l8_vwc40_B$time <- ymd_hms(l8_vwc40_B$time)
l8_vwc50_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[B_SoilVUE_VWC_50cm].csv")
l8_vwc50_B$time <- ymd_hms(l8_vwc50_B$time)
l8_vwc60_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[B_SoilVUE_VWC_60cm].csv")
l8_vwc60_B$time <- ymd_hms(l8_vwc60_B$time)
l8_vwc75_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[B_SoilVUE_VWC_75cm].csv")
l8_vwc75_B$time <- ymd_hms(l8_vwc75_B$time)
l8_vwc100_B <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 8]-[B_SoilVUE_VWC_100cm].csv")
l8_vwc100_B$time <- ymd_hms(l8_vwc100_B$time)


l3_vwc5_A <- l3_vwc5_A %>% dplyr::select(time, everything())
l3_vwc5_B <- l3_vwc5_B %>% dplyr::select(time, everything())
l3_vwc10_A <- l3_vwc10_A %>% dplyr::select(time, everything())
l3_vwc10_B <- l3_vwc10_B %>% dplyr::select(time, everything())
l3_vwc20_A <- l3_vwc20_A %>% dplyr::select(time, everything())
l3_vwc20_B <- l3_vwc20_B %>% dplyr::select(time, everything())
l3_vwc30_A <- l3_vwc30_A %>% dplyr::select(time, everything())
l3_vwc30_B <- l3_vwc30_B %>% dplyr::select(time, everything())
l3_vwc40_A <- l3_vwc40_A %>% dplyr::select(time, everything())
l3_vwc40_B <- l3_vwc40_B %>% dplyr::select(time, everything())
l3_vwc50_A <- l3_vwc50_A %>% dplyr::select(time, everything())
l3_vwc50_B <- l3_vwc50_B %>% dplyr::select(time, everything())
l3_vwc60_A <- l3_vwc60_A %>% dplyr::select(time, everything())
l3_vwc60_B <- l3_vwc60_B %>% dplyr::select(time, everything())
l3_vwc75_A <- l3_vwc75_A %>% dplyr::select(time, everything())
l3_vwc75_B <- l3_vwc75_B %>% dplyr::select(time, everything())
l3_vwc100_A <- l3_vwc100_A %>% dplyr::select(time, everything())
l3_vwc100_B <- l3_vwc100_B %>% dplyr::select(time, everything())

l4_vwc5_A <- l4_vwc5_A %>% dplyr::select(time, everything())
l4_vwc5_B <- l4_vwc5_B %>% dplyr::select(time, everything())
l4_vwc10_A <- l4_vwc10_A %>% dplyr::select(time, everything())
l4_vwc10_B <- l4_vwc10_B %>% dplyr::select(time, everything())
l4_vwc20_A <- l4_vwc20_A %>% dplyr::select(time, everything())
l4_vwc20_B <- l4_vwc20_B %>% dplyr::select(time, everything())
l4_vwc30_A <- l4_vwc30_A %>% dplyr::select(time, everything())
l4_vwc30_B <- l4_vwc30_B %>% dplyr::select(time, everything())
l4_vwc40_A <- l4_vwc40_A %>% dplyr::select(time, everything())
l4_vwc40_B <- l4_vwc40_B %>% dplyr::select(time, everything())
l4_vwc50_A <- l4_vwc50_A %>% dplyr::select(time, everything())
l4_vwc50_B <- l4_vwc50_B %>% dplyr::select(time, everything())
l4_vwc60_A <- l4_vwc60_A %>% dplyr::select(time, everything())
l4_vwc60_B <- l4_vwc60_B %>% dplyr::select(time, everything())
l4_vwc75_A <- l4_vwc75_A %>% dplyr::select(time, everything())
l4_vwc75_B <- l4_vwc75_B %>% dplyr::select(time, everything())
l4_vwc100_A <- l4_vwc100_A %>% dplyr::select(time, everything())
l4_vwc100_B <- l4_vwc100_B %>% dplyr::select(time, everything())

l7_vwc5_A <- l7_vwc5_A %>% dplyr::select(time, everything())
l7_vwc5_B <- l7_vwc5_B %>% dplyr::select(time, everything())
l7_vwc10_A <- l7_vwc10_A %>% dplyr::select(time, everything())
l7_vwc10_B <- l7_vwc10_B %>% dplyr::select(time, everything())
l7_vwc20_A <- l7_vwc20_A %>% dplyr::select(time, everything())
l7_vwc20_B <- l7_vwc20_B %>% dplyr::select(time, everything())
l7_vwc30_A <- l7_vwc30_A %>% dplyr::select(time, everything())
l7_vwc30_B <- l7_vwc30_B %>% dplyr::select(time, everything())
l7_vwc40_A <- l7_vwc40_A %>% dplyr::select(time, everything())
l7_vwc40_B <- l7_vwc40_B %>% dplyr::select(time, everything())
l7_vwc50_A <- l7_vwc50_A %>% dplyr::select(time, everything())
l7_vwc50_B <- l7_vwc50_B %>% dplyr::select(time, everything())
l7_vwc60_A <- l7_vwc60_A %>% dplyr::select(time, everything())
l7_vwc60_B <- l7_vwc60_B %>% dplyr::select(time, everything())
l7_vwc75_A <- l7_vwc75_A %>% dplyr::select(time, everything())
l7_vwc75_B <- l7_vwc75_B %>% dplyr::select(time, everything())
l7_vwc100_A <- l7_vwc100_A %>% dplyr::select(time, everything())
l7_vwc100_B <- l7_vwc100_B %>% dplyr::select(time, everything())

l8_vwc5_A <- l8_vwc5_A %>% dplyr::select(time, everything())
l8_vwc5_B <- l8_vwc5_B %>% dplyr::select(time, everything())
l8_vwc10_A <- l8_vwc10_A %>% dplyr::select(time, everything())
l8_vwc10_B <- l8_vwc10_B %>% dplyr::select(time, everything())
l8_vwc20_A <- l8_vwc20_A %>% dplyr::select(time, everything())
l8_vwc20_B <- l8_vwc20_B %>% dplyr::select(time, everything())
l8_vwc30_A <- l8_vwc30_A %>% dplyr::select(time, everything())
l8_vwc30_B <- l8_vwc30_B %>% dplyr::select(time, everything())
l8_vwc40_A <- l8_vwc40_A %>% dplyr::select(time, everything())
l8_vwc40_B <- l8_vwc40_B %>% dplyr::select(time, everything())
l8_vwc50_A <- l8_vwc50_A %>% dplyr::select(time, everything())
l8_vwc50_B <- l8_vwc50_B %>% dplyr::select(time, everything())
l8_vwc60_A <- l8_vwc60_A %>% dplyr::select(time, everything())
l8_vwc60_B <- l8_vwc60_B %>% dplyr::select(time, everything())
l8_vwc75_A <- l8_vwc75_A %>% dplyr::select(time, everything())
l8_vwc75_B <- l8_vwc75_B %>% dplyr::select(time, everything())
l8_vwc100_A <- l8_vwc100_A %>% dplyr::select(time, everything())
l8_vwc100_B <- l8_vwc100_B %>% dplyr::select(time, everything())


l3_vwc_A <- combine_data(list(l3_vwc5_A, l3_vwc10_A, l3_vwc20_A, l3_vwc30_A, l3_vwc40_A, l3_vwc50_A, l3_vwc60_A, l3_vwc75_A, l3_vwc100_A))
l3_vwcHR_A <- aggregate_data(l3_vwc_A, 5, 60, c(2:10))
l3_vwc_B <- combine_data(list(l3_vwc5_B, l3_vwc10_B, l3_vwc20_B, l3_vwc30_B, l3_vwc40_B, l3_vwc50_B, l3_vwc60_B, l3_vwc75_B, l3_vwc100_B))
l3_vwcHR_B <- aggregate_data(l3_vwc_B, 5, 60, c(2:10))

l4_vwc_A <- combine_data(list(l4_vwc5_A, l4_vwc10_A, l4_vwc20_A, l4_vwc30_A, l4_vwc40_A, l4_vwc50_A, l4_vwc60_A, l4_vwc75_A, l4_vwc100_A))
l4_vwc_B <- combine_data(list(l4_vwc5_B, l4_vwc10_B, l4_vwc20_B, l4_vwc30_B, l4_vwc40_B, l4_vwc50_B, l4_vwc60_B, l4_vwc75_B, l4_vwc100_B))
l4_vwcHR_A <- aggregate_data(l4_vwc_A, 5, 60, c(2:10))
l4_vwcHR_B <- aggregate_data(l4_vwc_B, 5, 60, c(2:10))

l7_vwc_A <- combine_data(list(l7_vwc5_A, l7_vwc10_A, l7_vwc20_A, l7_vwc30_A, l7_vwc40_A, l7_vwc50_A, l7_vwc60_A, l7_vwc75_A, l7_vwc100_A))
l7_vwc_B <- combine_data(list(l7_vwc5_B, l7_vwc10_B, l7_vwc20_B, l7_vwc30_B, l7_vwc40_B, l7_vwc50_B, l7_vwc60_B, l7_vwc75_B, l7_vwc100_B))
l7_vwcHR_A <- aggregate_data(l7_vwc_A, 5, 60, c(2:10))
l7_vwcHR_B <- aggregate_data(l7_vwc_B, 5, 60, c(2:10))

l8_vwc_A <- combine_data(list(l8_vwc5_A, l8_vwc10_A, l8_vwc20_A, l8_vwc30_A, l8_vwc40_A, l8_vwc50_A, l8_vwc60_A, l8_vwc75_A, l8_vwc100_A))
l8_vwc_B <- combine_data(list(l8_vwc5_B, l8_vwc10_B, l8_vwc20_B, l8_vwc30_B, l8_vwc40_B, l8_vwc50_B, l8_vwc60_B, l8_vwc75_B, l8_vwc100_B))
l8_vwcHR_A <- aggregate_data(l8_vwc_A, 5, 60, c(2:10))
l8_vwcHR_B <- aggregate_data(l8_vwc_B, 5, 60, c(2:10))


# Convert VWC:
l3_moist5_A <- convert_vwc(l3_vwc5_A, 5)
l3_moist10_A <- convert_vwc(l3_vwc10_A, 10)
l3_moist20_A <- convert_vwc(l3_vwc20_A, 20)
l3_moist30_A <- convert_vwc(l3_vwc30_A, 30)
l3_moist40_A <- convert_vwc(l3_vwc40_A, 40)
l3_moist50_A <- convert_vwc(l3_vwc50_A, 50)
l3_moist60_A <- convert_vwc(l3_vwc60_A, 60)
l3_moist75_A <- convert_vwc(l3_vwc75_A, 75)
l3_moist100_A <- convert_vwc(l3_vwc100_A, 100)

l4_moist5_A <- convert_vwc(l4_vwc5_A, 5)
l4_moist10_A <- convert_vwc(l4_vwc10_A, 10)
l4_moist20_A <- convert_vwc(l4_vwc20_A, 20)
l4_moist30_A <- convert_vwc(l4_vwc30_A, 30)
l4_moist40_A <- convert_vwc(l4_vwc40_A, 40)
l4_moist50_A <- convert_vwc(l4_vwc50_A, 50)
l4_moist60_A <- convert_vwc(l4_vwc60_A, 60)
l4_moist75_A <- convert_vwc(l4_vwc75_A, 75)
l4_moist100_A <- convert_vwc(l4_vwc100_A, 100)

l7_moist5_A <- convert_vwc(l7_vwc5_A, 5)
l7_moist10_A <- convert_vwc(l7_vwc10_A, 10)
l7_moist20_A <- convert_vwc(l7_vwc20_A, 20)
l7_moist30_A <- convert_vwc(l7_vwc30_A, 30)
l7_moist40_A <- convert_vwc(l7_vwc40_A, 40)
l7_moist50_A <- convert_vwc(l7_vwc50_A, 50)
l7_moist60_A <- convert_vwc(l7_vwc60_A, 60)
l7_moist75_A <- convert_vwc(l7_vwc75_A, 75)
l7_moist100_A <- convert_vwc(l7_vwc100_A, 100)

l8_moist5_A <- convert_vwc(l8_vwc5_A, 5)
l8_moist10_A <- convert_vwc(l8_vwc10_A, 10)
l8_moist20_A <- convert_vwc(l8_vwc20_A, 20)
l8_moist30_A <- convert_vwc(l8_vwc30_A, 30)
l8_moist40_A <- convert_vwc(l8_vwc40_A, 40)
l8_moist50_A <- convert_vwc(l8_vwc50_A, 50)
l8_moist60_A <- convert_vwc(l8_vwc60_A, 60)
l8_moist75_A <- convert_vwc(l8_vwc75_A, 75)
l8_moist100_A <- convert_vwc(l8_vwc100_A, 100)


l3_moist5_B <- convert_vwc(l3_vwc5_B, 5)
l3_moist10_B <- convert_vwc(l3_vwc10_B, 10)
l3_moist20_B <- convert_vwc(l3_vwc20_B, 20)
l3_moist30_B <- convert_vwc(l3_vwc30_B, 30)
l3_moist40_B <- convert_vwc(l3_vwc40_B, 40)
l3_moist50_B <- convert_vwc(l3_vwc50_B, 50)
l3_moist60_B <- convert_vwc(l3_vwc60_B, 60)
l3_moist75_B <- convert_vwc(l3_vwc75_B, 75)
l3_moist100_B <- convert_vwc(l3_vwc100_B, 100)

l4_moist5_B <- convert_vwc(l4_vwc5_B, 5)
l4_moist10_B <- convert_vwc(l4_vwc10_B, 10)
l4_moist20_B <- convert_vwc(l4_vwc20_B, 20)
l4_moist30_B <- convert_vwc(l4_vwc30_B, 30)
l4_moist40_B <- convert_vwc(l4_vwc40_B, 40)
l4_moist50_B <- convert_vwc(l4_vwc50_B, 50)
l4_moist60_B <- convert_vwc(l4_vwc60_B, 60)
l4_moist75_B <- convert_vwc(l4_vwc75_B, 75)
l4_moist100_B <- convert_vwc(l4_vwc100_B, 100)

l7_moist5_B <- convert_vwc(l7_vwc5_B, 5)
l7_moist10_B <- convert_vwc(l7_vwc10_B, 10)
l7_moist20_B <- convert_vwc(l7_vwc20_B, 20)
l7_moist30_B <- convert_vwc(l7_vwc30_B, 30)
l7_moist40_B <- convert_vwc(l7_vwc40_B, 40)
l7_moist50_B <- convert_vwc(l7_vwc50_B, 50)
l7_moist60_B <- convert_vwc(l7_vwc60_B, 60)
l7_moist75_B <- convert_vwc(l7_vwc75_B, 75)
l7_moist100_B <- convert_vwc(l7_vwc100_B, 100)

l8_moist5_B <- convert_vwc(l8_vwc5_B, 5)
l8_moist10_B <- convert_vwc(l8_vwc10_B, 10)
l8_moist20_B <- convert_vwc(l8_vwc20_B, 20)
l8_moist30_B <- convert_vwc(l8_vwc30_B, 30)
l8_moist40_B <- convert_vwc(l8_vwc40_B, 40)
l8_moist50_B <- convert_vwc(l8_vwc50_B, 50)
l8_moist60_B <- convert_vwc(l8_vwc60_B, 60)
l8_moist75_B <- convert_vwc(l8_vwc75_B, 75)
l8_moist100_B <- convert_vwc(l8_vwc100_B, 100)

# Aggregating, Combining

l3_moist5_A_HR <- aggregate_data_mean(l3_moist5_A, 5, 60, 2)
colnames(l3_moist5_A_HR)[2] <- "l3_moist5_A"
l3_moist10_A_HR <- aggregate_data_mean(l3_moist10_A, 5, 60, 2)
colnames(l3_moist10_A_HR)[2] <- "l3_moist10_A"
l3_moist20_A_HR <- aggregate_data_mean(l3_moist20_A, 5, 60, 2)
colnames(l3_moist20_A_HR)[2] <- "l3_moist20_A"
l3_moist30_A_HR <- aggregate_data_mean(l3_moist30_A, 5, 60, 2)
colnames(l3_moist30_A_HR)[2] <- "l3_moist30_A"
l3_moist40_A_HR <- aggregate_data_mean(l3_moist40_A, 5, 60, 2)
colnames(l3_moist40_A_HR)[2] <- "l3_moist40_A"
l3_moist50_A_HR <- aggregate_data_mean(l3_moist50_A, 5, 60, 2)
colnames(l3_moist50_A_HR)[2] <- "l3_moist50_A"
l3_moist60_A_HR <- aggregate_data_mean(l3_moist60_A, 5, 60, 2)
colnames(l3_moist60_A_HR)[2] <- "l3_moist60_A"
l3_moist75_A_HR <- aggregate_data_mean(l3_moist75_A, 5, 60, 2)
colnames(l3_moist75_A_HR)[2] <- "l3_moist75_A"
l3_moist100_A_HR <- aggregate_data_mean(l3_moist100_A, 5, 60, 2)
colnames(l3_moist100_A_HR)[2] <- "l3_moist100_A"

l3_all_moist_A_HR <- combine_data(list(l3_moist5_A_HR, l3_moist10_A_HR, l3_moist20_A_HR, l3_moist30_A_HR, l3_moist40_A_HR, l3_moist50_A_HR, l3_moist60_A_HR, l3_moist75_A_HR, l3_moist100_A_HR))

l4_moist5_A_HR <- aggregate_data_mean(l4_moist5_A, 5, 60, 2)
colnames(l4_moist5_A_HR)[2] <- "l4_moist5_A"
l4_moist10_A_HR <- aggregate_data_mean(l4_moist10_A, 5, 60, 2)
colnames(l4_moist10_A_HR)[2] <- "l4_moist10_A"
l4_moist20_A_HR <- aggregate_data_mean(l4_moist20_A, 5, 60, 2)
colnames(l4_moist20_A_HR)[2] <- "l4_moist20_A"
l4_moist30_A_HR <- aggregate_data_mean(l4_moist30_A, 5, 60, 2)
colnames(l4_moist30_A_HR)[2] <- "l4_moist30_A"
l4_moist40_A_HR <- aggregate_data_mean(l4_moist40_A, 5, 60, 2)
colnames(l4_moist40_A_HR)[2] <- "l4_moist40_A"
l4_moist50_A_HR <- aggregate_data_mean(l4_moist50_A, 5, 60, 2)
colnames(l4_moist50_A_HR)[2] <- "l4_moist50_A"
l4_moist60_A_HR <- aggregate_data_mean(l4_moist60_A, 5, 60, 2)
colnames(l4_moist60_A_HR)[2] <- "l4_moist60_A"
l4_moist75_A_HR <- aggregate_data_mean(l4_moist75_A, 5, 60, 2)
colnames(l4_moist75_A_HR)[2] <- "l4_moist75_A"
l4_moist100_A_HR <- aggregate_data_mean(l4_moist100_A, 5, 60, 2)
colnames(l4_moist100_A_HR)[2] <- "l4_moist100_A"

l4_all_moist_A_HR <- combine_data(list(l4_moist5_A_HR, l4_moist10_A_HR, l4_moist20_A_HR, l4_moist30_A_HR, l4_moist40_A_HR, l4_moist50_A_HR, l4_moist60_A_HR, l4_moist75_A_HR, l4_moist100_A_HR))

l7_moist5_A_HR <- aggregate_data_mean(l7_moist5_A, 5, 60, 2)
colnames(l7_moist5_A_HR)[2] <- "l7_moist5_A"
l7_moist10_A_HR <- aggregate_data_mean(l7_moist10_A, 5, 60, 2)
colnames(l7_moist10_A_HR)[2] <- "l7_moist10_A"
l7_moist20_A_HR <- aggregate_data_mean(l7_moist20_A, 5, 60, 2)
colnames(l7_moist20_A_HR)[2] <- "l7_moist20_A"
l7_moist30_A_HR <- aggregate_data_mean(l7_moist30_A, 5, 60, 2)
colnames(l7_moist30_A_HR)[2] <- "l7_moist30_A"
l7_moist40_A_HR <- aggregate_data_mean(l7_moist40_A, 5, 60, 2)
colnames(l7_moist40_A_HR)[2] <- "l7_moist40_A"
l7_moist50_A_HR <- aggregate_data_mean(l7_moist50_A, 5, 60, 2)
colnames(l7_moist50_A_HR)[2] <- "l7_moist50_A"
l7_moist60_A_HR <- aggregate_data_mean(l7_moist60_A, 5, 60, 2)
colnames(l7_moist60_A_HR)[2] <- "l7_moist60_A"
l7_moist75_A_HR <- aggregate_data_mean(l7_moist75_A, 5, 60, 2)
colnames(l7_moist75_A_HR)[2] <- "l7_moist75_A"
l7_moist100_A_HR <- aggregate_data_mean(l7_moist100_A, 5, 60, 2)
colnames(l7_moist100_A_HR)[2] <- "l7_moist100_A"

l7_all_moist_A_HR <- combine_data(list(l7_moist5_A_HR, l7_moist10_A_HR, l7_moist20_A_HR, l7_moist30_A_HR, l7_moist40_A_HR, l7_moist50_A_HR, l7_moist60_A_HR, l7_moist75_A_HR, l7_moist100_A_HR))

l8_moist5_A_HR <- aggregate_data_mean(l8_moist5_A, 5, 60, 2)
colnames(l8_moist5_A_HR)[2] <- "l8_moist5_A"
l8_moist10_A_HR <- aggregate_data_mean(l8_moist10_A, 5, 60, 2)
colnames(l8_moist10_A_HR)[2] <- "l8_moist10_A"
l8_moist20_A_HR <- aggregate_data_mean(l8_moist20_A, 5, 60, 2)
colnames(l8_moist20_A_HR)[2] <- "l8_moist20_A"
l8_moist30_A_HR <- aggregate_data_mean(l8_moist30_A, 5, 60, 2)
colnames(l8_moist30_A_HR)[2] <- "l8_moist30_A"
l8_moist40_A_HR <- aggregate_data_mean(l8_moist40_A, 5, 60, 2)
colnames(l8_moist40_A_HR)[2] <- "l8_moist40_A"
l8_moist50_A_HR <- aggregate_data_mean(l8_moist50_A, 5, 60, 2)
colnames(l8_moist50_A_HR)[2] <- "l8_moist50_A"
l8_moist60_A_HR <- aggregate_data_mean(l8_moist60_A, 5, 60, 2)
colnames(l8_moist60_A_HR)[2] <- "l8_moist60_A"
l8_moist75_A_HR <- aggregate_data_mean(l8_moist75_A, 5, 60, 2)
colnames(l8_moist75_A_HR)[2] <- "l8_moist75_A"
l8_moist100_A_HR <- aggregate_data_mean(l8_moist100_A, 5, 60, 2)
colnames(l8_moist100_A_HR)[2] <- "l8_moist100_A"

l8_all_moist_A_HR <- combine_data(list(l8_moist5_A_HR, l8_moist10_A_HR, l8_moist20_A_HR, l8_moist30_A_HR, l8_moist40_A_HR, l8_moist50_A_HR, l8_moist60_A_HR, l8_moist75_A_HR, l8_moist100_A_HR))



l3_moist5_B_HR <- aggregate_data_mean(l3_moist5_B, 5, 60, 2)
colnames(l3_moist5_B_HR)[2] <- "l3_moist5_B"
l3_moist10_B_HR <- aggregate_data_mean(l3_moist10_B, 5, 60, 2)
colnames(l3_moist10_B_HR)[2] <- "l3_moist10_B"
l3_moist20_B_HR <- aggregate_data_mean(l3_moist20_B, 5, 60, 2)
colnames(l3_moist20_B_HR)[2] <- "l3_moist20_B"
l3_moist30_B_HR <- aggregate_data_mean(l3_moist30_B, 5, 60, 2)
colnames(l3_moist30_B_HR)[2] <- "l3_moist30_B"
l3_moist40_B_HR <- aggregate_data_mean(l3_moist40_B, 5, 60, 2)
colnames(l3_moist40_B_HR)[2] <- "l3_moist40_B"
l3_moist50_B_HR <- aggregate_data_mean(l3_moist50_B, 5, 60, 2)
colnames(l3_moist50_B_HR)[2] <- "l3_moist50_B"
l3_moist60_B_HR <- aggregate_data_mean(l3_moist60_B, 5, 60, 2)
colnames(l3_moist60_B_HR)[2] <- "l3_moist60_B"
l3_moist75_B_HR <- aggregate_data_mean(l3_moist75_B, 5, 60, 2)
colnames(l3_moist75_B_HR)[2] <- "l3_moist75_B"
l3_moist100_B_HR <- aggregate_data_mean(l3_moist100_B, 5, 60, 2)
colnames(l3_moist100_B_HR)[2] <- "l3_moist100_B"

l3_all_moist_B_HR <- combine_data(list(l3_moist5_B_HR, l3_moist10_B_HR, l3_moist20_B_HR, l3_moist30_B_HR, l3_moist40_B_HR, l3_moist50_B_HR, l3_moist60_B_HR, l3_moist75_B_HR, l3_moist100_B_HR))

l4_moist5_B_HR <- aggregate_data_mean(l4_moist5_B, 5, 60, 2)
colnames(l4_moist5_B_HR)[2] <- "l4_moist5_B"
l4_moist10_B_HR <- aggregate_data_mean(l4_moist10_B, 5, 60, 2)
colnames(l4_moist10_B_HR)[2] <- "l4_moist10_B"
l4_moist20_B_HR <- aggregate_data_mean(l4_moist20_B, 5, 60, 2)
colnames(l4_moist20_B_HR)[2] <- "l4_moist20_B"
l4_moist30_B_HR <- aggregate_data_mean(l4_moist30_B, 5, 60, 2)
colnames(l4_moist30_B_HR)[2] <- "l4_moist30_B"
l4_moist40_B_HR <- aggregate_data_mean(l4_moist40_B, 5, 60, 2)
colnames(l4_moist40_B_HR)[2] <- "l4_moist40_B"
l4_moist50_B_HR <- aggregate_data_mean(l4_moist50_B, 5, 60, 2)
colnames(l4_moist50_B_HR)[2] <- "l4_moist50_B"
l4_moist60_B_HR <- aggregate_data_mean(l4_moist60_B, 5, 60, 2)
colnames(l4_moist60_B_HR)[2] <- "l4_moist60_B"
l4_moist75_B_HR <- aggregate_data_mean(l4_moist75_B, 5, 60, 2)
colnames(l4_moist75_B_HR)[2] <- "l4_moist75_B"
l4_moist100_B_HR <- aggregate_data_mean(l4_moist100_B, 5, 60, 2)
colnames(l4_moist100_B_HR)[2] <- "l4_moist100_B"

l4_all_moist_B_HR <- combine_data(list(l4_moist5_B_HR, l4_moist10_B_HR, l4_moist20_B_HR, l4_moist30_B_HR, l4_moist40_B_HR, l4_moist50_B_HR, l4_moist60_B_HR, l4_moist75_B_HR, l4_moist100_B_HR))

l7_moist5_B_HR <- aggregate_data_mean(l7_moist5_B, 5, 60, 2)
colnames(l7_moist5_B_HR)[2] <- "l7_moist5_B"
l7_moist10_B_HR <- aggregate_data_mean(l7_moist10_B, 5, 60, 2)
colnames(l7_moist10_B_HR)[2] <- "l7_moist10_B"
l7_moist20_B_HR <- aggregate_data_mean(l7_moist20_B, 5, 60, 2)
colnames(l7_moist20_B_HR)[2] <- "l7_moist20_B"
l7_moist30_B_HR <- aggregate_data_mean(l7_moist30_B, 5, 60, 2)
colnames(l7_moist30_B_HR)[2] <- "l7_moist30_B"
l7_moist40_B_HR <- aggregate_data_mean(l7_moist40_B, 5, 60, 2)
colnames(l7_moist40_B_HR)[2] <- "l7_moist40_B"
l7_moist50_B_HR <- aggregate_data_mean(l7_moist50_B, 5, 60, 2)
colnames(l7_moist50_B_HR)[2] <- "l7_moist50_B"
l7_moist60_B_HR <- aggregate_data_mean(l7_moist60_B, 5, 60, 2)
colnames(l7_moist60_B_HR)[2] <- "l7_moist60_B"
l7_moist75_B_HR <- aggregate_data_mean(l7_moist75_B, 5, 60, 2)
colnames(l7_moist75_B_HR)[2] <- "l7_moist75_B"
l7_moist100_B_HR <- aggregate_data_mean(l7_moist100_B, 5, 60, 2)
colnames(l7_moist100_B_HR)[2] <- "l7_moist100_B"

l7_all_moist_B_HR <- combine_data(list(l7_moist5_B_HR, l7_moist10_B_HR, l7_moist20_B_HR, l7_moist30_B_HR, l7_moist40_B_HR, l7_moist50_B_HR, l7_moist60_B_HR, l7_moist75_B_HR, l7_moist100_B_HR))

l8_moist5_B_HR <- aggregate_data_mean(l8_moist5_B, 5, 60, 2)
colnames(l8_moist5_B_HR)[2] <- "l8_moist5_B"
l8_moist10_B_HR <- aggregate_data_mean(l8_moist10_B, 5, 60, 2)
colnames(l8_moist10_B_HR)[2] <- "l8_moist10_B"
l8_moist20_B_HR <- aggregate_data_mean(l8_moist20_B, 5, 60, 2)
colnames(l8_moist20_B_HR)[2] <- "l8_moist20_B"
l8_moist30_B_HR <- aggregate_data_mean(l8_moist30_B, 5, 60, 2)
colnames(l8_moist30_B_HR)[2] <- "l8_moist30_B"
l8_moist40_B_HR <- aggregate_data_mean(l8_moist40_B, 5, 60, 2)
colnames(l8_moist40_B_HR)[2] <- "l8_moist40_B"
l8_moist50_B_HR <- aggregate_data_mean(l8_moist50_B, 5, 60, 2)
colnames(l8_moist50_B_HR)[2] <- "l8_moist50_B"
l8_moist60_B_HR <- aggregate_data_mean(l8_moist60_B, 5, 60, 2)
colnames(l8_moist60_B_HR)[2] <- "l8_moist60_B"
l8_moist75_B_HR <- aggregate_data_mean(l8_moist75_B, 5, 60, 2)
colnames(l8_moist75_B_HR)[2] <- "l8_moist75_B"
l8_moist100_B_HR <- aggregate_data_mean(l8_moist100_B, 5, 60, 2)
colnames(l8_moist100_B_HR)[2] <- "l8_moist100_B"

l8_all_moist_B_HR <- combine_data(list(l8_moist5_B_HR, l8_moist10_B_HR, l8_moist20_B_HR, l8_moist30_B_HR, l8_moist40_B_HR, l8_moist50_B_HR, l8_moist60_B_HR, l8_moist75_B_HR, l8_moist100_B_HR))

# INTERPOLATE VWC AND MOISTURE:
l3_vwc_A_int <- interpolate_moisture(l3_vwc_A, c(2:10), 5)
l4_vwc_A_int <- interpolate_moisture(l4_vwc_A, c(2:10), 5)
l7_vwc_A_int <- interpolate_moisture(l7_vwc_A, c(2:10), 5)
l8_vwc_A_int <- interpolate_moisture(l8_vwc_A, c(2:10), 5)

l3_vwc_B_int <- interpolate_moisture(l3_vwc_B, c(2:10), 5)
l4_vwc_B_int <- interpolate_moisture(l4_vwc_B, c(2:10), 5)
l7_vwc_B_int <- interpolate_moisture(l7_vwc_B, c(2:10), 5)
l8_vwc_B_int <- interpolate_moisture(l8_vwc_B, c(2:10), 5)

l3_all_moist_A_int <- interpolate_moisture(l3_all_moist_A_HR, c(2:10), 5)
l4_all_moist_A_int <- interpolate_moisture(l4_all_moist_A_HR, c(2:10), 5)
l7_all_moist_A_int <- interpolate_moisture(l7_all_moist_A_HR, c(2:10), 5)
l8_all_moist_A_int <- interpolate_moisture(l8_all_moist_A_HR, c(2:10), 5)

l3_all_moist_B_int <- interpolate_moisture(l3_all_moist_B_HR, c(2:10), 5)
l4_all_moist_B_int <- interpolate_moisture(l4_all_moist_B_HR, c(2:10), 5)
l7_all_moist_B_int <- interpolate_moisture(l7_all_moist_B_HR, c(2:10), 5)
l8_all_moist_B_int <- interpolate_moisture(l8_all_moist_B_HR, c(2:10), 5)



# Total moisture, litres:
l3_total_moist_A_int <- sum_columns(l3_all_moist_A_int, c(2, 3, 4, 5, 6, 7, 8, 9, 10), "l3_total_moist_A_int")
l4_total_moist_A_int <- sum_columns(l4_all_moist_A_int, c(2, 3, 4, 5, 6, 7, 8, 9, 10), "l4_total_moist_A_int")
l7_total_moist_A_int <- sum_columns(l7_all_moist_A_int, c(2, 3, 4, 5, 6, 7, 8, 9, 10), "l7_total_moist_A_int")
l8_total_moist_A_int <- sum_columns(l8_all_moist_A_int, c(2, 3, 4, 5, 6, 7, 8, 9, 10), "l8_total_moist_A_int")

total_moist_A_int_all <- combine_data(list(l3_total_moist_A_int, l4_total_moist_A_int, l7_total_moist_A_int, l8_total_moist_A_int))


l3_total_moist_B_int <- sum_columns(l3_all_moist_B_int, c(2, 3, 4, 5, 6, 7, 8, 9, 10), "l3_total_moist_B_int")
l4_total_moist_B_int <- sum_columns(l4_all_moist_B_int, c(2, 3, 4, 5, 6, 7, 8, 9, 10), "l4_total_moist_B_int")
l7_total_moist_B_int <- sum_columns(l7_all_moist_B_int, c(2, 3, 4, 5, 6, 7, 8, 9, 10), "l7_total_moist_B_int")
l8_total_moist_B_int <- sum_columns(l8_all_moist_B_int, c(2, 3, 4, 5, 6, 7, 8, 9, 10), "l8_total_moist_B_int")

total_moist_B_int_all <- combine_data(list(l3_total_moist_B_int, l4_total_moist_B_int, l7_total_moist_B_int, l8_total_moist_B_int))


# TEMPERATURE
WS_temp15 <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 6]-[Air temperature].csv")
WS_solar15 <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 6]-[Solar radiation].csv")

WS_temp15 <- WS_temp15 %>% dplyr::select(time, everything())
WS_solar15 <- WS_solar15 %>% dplyr::select(time, everything())

colnames(WS_temp15)[2] <- "WS_temp15"
colnames(WS_solar15)[2] <- "WS_solar15"

convert_time(c("WS_temp15", "WS_solar15"), global = T)

WS_temp_solar_15 <- combine_data(list(WS_temp15, WS_solar15))

vis_miss(WS_temp_solar_15) +
  theme(plot.margin = margin(t = 40)) +
  ggtitle("Missing Data in Weather Station Temperature Data") +
  theme(axis.text.x = element_text(vjust = -0.25))


# Aggregating WS_temp15:
WS_tempHR <- aggregate_data_mean(WS_temp15, 15, 60, 2)
colnames(WS_tempHR)[2] <- "WS_tempHR"

# Aggregating WS_solar15:
WS_solarHR <- aggregate_data_mean(WS_solar15, 15, 60, 2)
colnames(WS_solarHR)[2] <- "WS_solarHR"

## DDF TABLE
ddf_table <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/Return Periods v2.csv")
ddf_table_ml <- convert_mm_ml(ddf_table, c(seq(3, 31)))

## EVAPOTRANSPIRATION:

l3_et15 <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 6]-[ETo (Bare earth) - 15 minutes].csv")
l3_et15 <- l3_et15 %>% dplyr::select(time, everything())
colnames(l3_et15)[2] <- "l3_et15"
l3_et15$time <- ymd_hms(l3_et15$time)

l3_etHR <- aggregate_data(l3_et15, 15, 60, 2)
colnames(l3_etHR)[2] <- "l3_etHR"


l3_et15_cum_daily <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 6]-[ETo (Bare earth) - Cumulative daily].csv")
l3_et15_cum_daily <- l3_et15_cum_daily %>% dplyr::select(time, everything())
colnames(l3_et15_cum_daily)[2] <- "l3_et15_cum_daily"
l3_et15_cum_daily$time <- ymd_hms(l3_et15_cum_daily$time)

l3_etHR_cum_daily <- aggregate_data(l3_et15_cum_daily, 15, 60, 2)
colnames(l3_etHR_cum_daily)[2] <- "l3_etHR_cum_daily"


l3_et_daily <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 6]-[Daily ETo (Bare earth)].csv")
l3_et_daily <- l3_et_daily %>% dplyr::select(time, everything())
colnames(l3_et_daily)[2] <- "l3_et_daily"
l3_et_daily$time <- ymd(l3_et_daily$time)


l4_et15 <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 6]-[ETo (Reference grass) - 15 minutes].csv")
l4_et15 <- l4_et15 %>% dplyr::select(time, everything())
colnames(l4_et15)[2] <- "l4_et15"
l4_et15$time <- ymd_hms(l4_et15$time)

l4_etHR <- aggregate_data(l4_et15, 15, 60, 2)
colnames(l4_etHR)[2] <- "l4_etHR"


l4_et15_cum_daily <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 6]-[ETo (Reference grass) - Cumulative daily].csv")
l4_et15_cum_daily <- l4_et15_cum_daily %>% dplyr::select(time, everything())
colnames(l4_et15_cum_daily)[2] <- "l4_et15_cum_daily"
l4_et15_cum_daily$time <- ymd_hms(l4_et15_cum_daily$time)

l4_etHR_cum_daily <- aggregate_data(l4_et15_cum_daily, 15, 60, 2)
colnames(l4_etHR_cum_daily)[2] <- "l4_etHR_cum_daily"


l4_et_daily <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 6]-[Daily ETo (Reference grass)].csv")
l4_et_daily <- l4_et_daily %>% dplyr::select(time, everything())
colnames(l4_et_daily)[2] <- "l4_et_daily"
l4_et_daily$time <- ymd(l4_et_daily$time)


l7.8_et15 <- read.csv("1. General Setup Code (Loading Data, All Functions)/1. Downloaded Data/ngif-[Lysimeter 6]-[Eto (Reference tall crop) - 15 minutes].csv")
l7.8_et15 <- l7.8_et15 %>% dplyr::select(time, everything())
colnames(l7.8_et15)[2] <- "l7.8_et15"
l7.8_et15$time <- ymd_hms(l7.8_et15$time)



# Hourly ET:
l7.8_etHR <- aggregate_data(l7.8_et15, 15, 60, 2)
colnames(l7.8_etHR)[2] <- "l7.8_etHR"




# In litres:
l3_et_daily_ml <- convert_mm_ml(l3_et_daily, 2)
l3_et_daily_L <- convert_ml_l(l3_et_daily_ml, 2)

l4_et_daily_ml <- convert_mm_ml(l4_et_daily, 2)
l4_et_daily_L <- convert_ml_l(l4_et_daily_ml, 2)

l3_etHR_ml <- convert_mm_ml(l3_etHR, 2)
l3_etHR_L <- convert_ml_l(l3_etHR_ml, 2)

l4_etHR_ml <- convert_mm_ml(l4_etHR, 2)
l4_etHR_L <- convert_ml_l(l4_etHR_ml, 2)

l7.8_etHR_ml <- convert_mm_ml(l7.8_etHR, 2)
l7.8_etHR_L <- convert_ml_l(l7.8_etHR_ml, 2)


# PROCESSED DATA:

# RAINFALL:
rain60_ave <- average_columns(rain60, c(2:5), "rain60")
rain60_events <- rainfall_events(rain60_ave, 24)
rain60_class <- classify_events(rain60_events, 60)
rain60_return <- return_period(rain60_class, ddf_table)
rain60_more_classifications <- other_classifications(rain60_return)
rain60_events_only <- events_only(rain60_more_classifications)

rain60_ave_L <- convert_ml_l(convert_mm_ml(rain60_ave, 2), 2)
rain60_events_L <- convert_ml_l(convert_mm_ml(rain60_events, 2), 2)

# OUTFLOW:
outHR_smooth <- smooth_outflow(outHR, c(2:9))
outHR_smooth_L <- convert_ml_l(outHR_smooth, c(2:9))

l3_outHR_smooth <- outHR_smooth[, c(1:3)]
l4_outHR_smooth <- outHR_smooth[, c(1, 4, 5)]
l7_outHR_smooth <- outHR_smooth[, c(1, 6, 7)]
l8_outHR_smooth <- outHR_smooth[, c(1, 8, 9)]

l3_outHR_smooth_A <- data.frame(time = l3_outHR_smooth$time,
                                l3A = l3_outHR_smooth$l3A)
l4_outHR_smooth_A <- data.frame(time = l4_outHR_smooth$time,
                                l4A = l4_outHR_smooth$l4A)
l7_outHR_smooth_A <- data.frame(time = l7_outHR_smooth$time,
                                l7A = l7_outHR_smooth$l7A)
l8_outHR_smooth_A <- data.frame(time = l8_outHR_smooth$time,
                                l8A = l8_outHR_smooth$l8A)


l3_outHR_smooth_B <- data.frame(time = l3_outHR_smooth$time,
                                l3B = l3_outHR_smooth$l3B)
l4_outHR_smooth_B <- data.frame(time = l4_outHR_smooth$time,
                                l4B = l4_outHR_smooth$l4B)
l7_outHR_smooth_B <- data.frame(time = l7_outHR_smooth$time,
                                l7B = l7_outHR_smooth$l7B)
l8_outHR_smooth_B <- data.frame(time = l8_outHR_smooth$time,
                                l8B = l8_outHR_smooth$l8B)



l3_outHR_smooth_L <- outHR_smooth_L[, c(1:3)]
l4_outHR_smooth_L <- outHR_smooth_L[, c(1, 4, 5)]
l7_outHR_smooth_L <- outHR_smooth_L[, c(1, 6, 7)]
l8_outHR_smooth_L <- outHR_smooth_L[, c(1, 8, 9)]