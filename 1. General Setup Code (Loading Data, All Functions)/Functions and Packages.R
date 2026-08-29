
## All Imports ##

# Define package list
packages <- c(
  "glue",
  "rlang",
  "rlist",
  "tidyverse",
  "DescTools",
  "naniar",
  "patchwork",
  "bio3d",
  "timeplyr",
  "hydroTSM",
  "corrplot",
  "forecast",
  "TTR",
  "tidyquery",
  "plotly",
  "reshape2",
  "fabricatr",
  "VGAM",
  "MASS",
  "effsize",
  "hydroeval",
  "hydroGOF",
  "Metrics",
  "fmsb"
)

# Identify and install any missing packages
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages)
}

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))


## All Functions ##

#############################################
## Downloading, Cleaning, Aggregating Data ##
#############################################

combine_data <- function(df_list){
  
  for (i in df_list){
    if ((inherits(i$time, "POSIXct")) == F){
      stop("Please provide data frames with time columns of class 'POSIXct'.")
    }
  }
  
  first_time <- list()
  last_time <- list()
  interval <- as.numeric(difftime(df_list[[1]][[2, "time"]], df_list[[1]][[1, "time"]], units = "mins"))
  
  for (j in seq_along(df_list)){
    first_time[[length(first_time)+1]] <- df_list[[j]][[1, "time"]]
    last_time[[length(last_time)+1]] <- df_list[[j]][[nrow(df_list[[j]]), "time"]]
  }
  
  from <- min(unlist(first_time))
  to <- max(unlist(last_time))
  by <- as_string(glue("{interval} mins"))
  
  time <- seq(from = as_datetime(from),
              to = as_datetime(to),
              by = by)
  df_out <- as.data.frame(time)
  
  for (k in seq_along(df_list)){
    df_out <- full_join(df_out, df_list[[k]],
                        by = "time")
  }
  
  return(df_out)
}

complete_times <- function(df_in){
  
  if ((inherits(df_in$time, "POSIXct")) == F){
    stop("Please provide a data frame with time column of class 'POSIXct'.")
  }
  
  from <- df_in[1, "time"]
  to <- df_in[nrow(df_in), "time"]
  interval <- as.numeric(difftime(df_in[[2, "time"]], df_in[[1, "time"]], units = "mins"))
  
  by <- as_string(glue("{interval} mins"))
  
  time <- seq(from = as_datetime(from),
              to = as_datetime(to),
              by = by)
  df_out <- as.data.frame(time)
  
  df_out <- full_join(df_out, df_in,
                      by = "time")
}

num_complete_cols <- function(df_in){
  df_out <- df_in
  df_out[, "complete_cases"] <- NA
  for (i in 1:nrow(df_out)){
    df_out[i, "complete_cases"] <- (4 - sum(is.na(df_out[i, c(2, 3, 4, 5)])))
  }
  return(df_out)
}

aggregate_data <- function(df_in, df_interval, new_interval, col_ind){
  time_col <- df_in$time
  rounded_time <- floor_date(time_col, unit = glue("{new_interval} minutes"))
  rows <- new_interval/df_interval
  
  if (length(col_ind) > 1){
    col_names <- as.list(colnames(df_in[, col_ind]))
    df_list <- list()
    for (i in col_ind){
      df_temp <- data.frame(time = rounded_time)
      df_temp$col <- df_in[, i]
      df_list[[length(df_list) + 1]] <- query(
        glue("SELECT 
              time,
              CASE 
                WHEN COUNT(col) < {rows} THEN NULL
                ELSE SUM(col)
                END AS col_aggr
              FROM df_temp
              GROUP BY time
              ORDER BY time;")
      )
    }
    df_out <- combine_data(df_list, col_ind)
  }
  else{
    df_out <- data.frame(time = rounded_time,
                         aggregate = df_in[, col_ind])
    df_out$time <- as.POSIXct(format(df_out$time, "%Y-%m-%d %H:%M:00"), tz = "UTC")
    df_out <- query(
      glue("SELECT 
            time,
            CASE
              WHEN COUNT(aggregate) < {rows} THEN NULL
              ELSE SUM(aggregate)
              END AS aggregated
          FROM df_out
          GROUP BY time
          ORDER BY time;")
    )
  }
  return(df_out)
}

aggregate_data_mean <- function(df_in, df_interval, new_interval, col_ind){
  time_col <- df_in$time
  rounded_time <- floor_date(time_col, unit = glue("{new_interval} minutes"))
  rows <- new_interval/df_interval
  
  if (length(col_ind) > 1){
    col_names <- as.list(colnames(df_in[, col_ind]))
    df_list <- list()
    for (i in col_ind){
      df_temp <- data.frame(time = rounded_time)
      df_temp$col <- df_in[, i]
      df_list[[length(df_list) + 1]] <- query(
        glue("SELECT 
              time,
              CASE 
                WHEN COUNT(col) < {rows} THEN CAST (NULL AS DOUBLE)
                ELSE AVG(col)
                END AS col_aggr
              FROM df_temp
              GROUP BY time
              ORDER BY time;")
      )
    }
    df_out <- combine_data(df_list, col_ind)
  }
  else{
    df_out <- data.frame(time = rounded_time,
                         aggregate = df_in[, col_ind])
    df_out$time <- as.POSIXct(format(df_out$time, "%Y-%m-%d %H:%M:00"), tz = "UTC")
    df_out <- query(
      glue("SELECT 
            time,
            CASE
              WHEN COUNT(aggregate) < {rows} THEN CAST (NULL AS DOUBLE)
              ELSE AVG(aggregate)
              END AS aggregated
          FROM df_out
          GROUP BY time
          ORDER BY time;")
    )
  }
  return(df_out)
}

convert_time <- function(df_vec, global = FALSE){
  # Input data frames' time columns are consistently named "time", but time
  # column placement is not consistent. The function therefore indexes these
  # columns by their name, "time".
  # The function changes the global variables
  if (global == TRUE){
    print(glue("Changing global variable: {df_vec}."))
    for (i in df_vec){
      df <- get(i, envir = .GlobalEnv)
      # The following if statements account for the three time formats in the data:
      if (grepl("/", substr(df[1, "time"], 1, 4)) && grepl(":", df[1, "time"])){
        df$time <- dmy_hm(df$time)
      }
      else if (grepl(":", df[1, "time"])){
        df$time <- ymd_hms(df$time)
      }
      else {
        df$time <- ymd(df$time) %>% as.POSIXct()
      }
      assign(i, df, envir = .GlobalEnv)
    }
  }
  else{
    df_list <- list()
    for (i in df_vec){
      df <- get(i, envir = .GlobalEnv)
      if (inherits(df$time, "POSIXct")){
        stop("Class of time column is already 'POSIXct' 'POSIXt'.")
      }
      # The following if statements account for the three time formats in the data:
      if (grepl("/", substr(df[1, "time"], 1, 4)) && grepl(":", df[1, "time"])){
        df$time <- dmy_hm(df$time)
        df_list[[length(df_list) + 1]] <- df
      }
      else if (grepl(":", df[1, "time"])){
        df$time <- ymd_hms(df$time)
        df_list[[length(df_list) + 1]] <- df
      }
      else {
        df$time <- ymd(df$time) %>% as.POSIXct()
        df_list[[length(df_list) + 1]] <- df
      }
    }
    return(df_list)
  }
}

average_rainfall <- function(df_in){
  df_out <- data.frame(time = df_in[, 1],
                       rain = NA)
  for(i in 1:nrow(df_in)){
    row_list <- as.numeric(as.list(unname(df_in[i, c(2, 3, 4, 5)])))
    mean_rain <- mean(row_list, na.rm = T)
    IQR_rain <- IQR(row_list, na.rm = T)
    k_index <- 0
    for(k in row_list){
      k_index <- k_index + 1
      if(((k > (mean_rain + 1.5*IQR_rain) | k < (mean_rain - 1.5*IQR_rain)) & is.na(k) == F)){
        as.list(row_list)
        row_list <- row_list[-k_index]
        as.numeric(row_list)
      }
    }
    if(is.nan(mean(row_list, na.rm = T)) == F){
      df_out[i, 2] <- mean(row_list, na.rm = T)
    }
    else{
      df_out <- df_out[-i, ]
      print(glue("Removed row {row.names(df_in[i, ])} of {nrow(df_in)} (NaN)."))
    }
  }
  return(df_out)
}

average_columns <- function(df_in, col_ind_vec, col_name){
  df_out <- data.frame(time = df_in[, 1],
                       average = NA)
  colnames(df_out)[2] <- col_name
  for(i in 1:nrow(df_in)){
    row_list <- as.numeric(as.list(unname(df_in[i, col_ind_vec])))
    mean_rain <- mean(row_list, na.rm = T)
    IQR_rain <- IQR(row_list, na.rm = T)
    k_index <- 0
    for(k in row_list){
      k_index <- k_index + 1
      if(((k > (mean_rain + 1.5*IQR_rain) | k < (mean_rain - 1.5*IQR_rain)) & is.na(k) == F)){
        as.list(row_list)
        row_list <- row_list[-k_index]
        as.numeric(row_list)
      }
    }
    if(is.nan(mean(row_list, na.rm = T)) == F){
      df_out[i, 2] <- mean(row_list, na.rm = T)
    }
    else{
      df_out <- df_out[-i, ]
      print(glue("Removed row {row.names(df_in[i, ])} of {nrow(df_in)} (NaN)."))
    }
  }
  return(df_out)
}

sum_columns <- function(df_in, col_ind_vec, col_name){
  df_out <- data.frame(time = df_in$time,
                       sum = NA)
  colnames(df_out)[2] <- col_name
  
  for (i in seq_len(nrow(df_in))){
    df_out[i, 2] <- sum(df_in[i, col_ind_vec])
  }
  return(df_out)
}


## Interpolation ##

# Testing error with interpolation for gaps of different sizes:
test_interpolation <- function(moisture_df, gaps_to_try_vec, event_df){
  
  df_out <- data.frame(gap = NA,
                       cor_coeff = NA)
  
  # Select all rows of the moisture_df that fall within an event:
  event_times <- as.vector(event_df[!is.na(event_df$event), ]$time)
  print(length(event_times))
  moisture_df_subset <- moisture_df[moisture_df$time %in% as.POSIXct(event_times), ]
  
  moisture_df_subset <- moisture_df_subset[complete.cases(moisture_df_subset), ]
  rownames(moisture_df_subset) <- NULL
  
  for (i in gaps_to_try_vec){
    # Add gaps to the data at random points in the data frame
    cor_coeff_vec <- c()
    
    for (j in 1:200){
      temp_df <- moisture_df_subset
      
      # Choose a random row:
      ind <- sample(1:(nrow(temp_df) - i), 1)
      ind_range <- c(ind:(ind + i - 1))
      
      # Make this range of rows NAs:
      temp_df[ind_range, 2] <- rep(NA, i)
      
      # Interpolate these values (linear):
      moist_col <- temp_df[[2]]
      moist_col_int <- na.approx(moist_col)
      
      # Calculate the correlation just for the whole thing?:
      
      #cor_coeff <- cor(complete_moisture_df[[2]][ind_range], moist_col_int[ind_range])
      
      cor_coeff <- sum(abs(moist_col_int[ind_range] - moisture_df_subset[[2]][ind_range]))/sum(moisture_df_subset[[2]][ind_range])
      
      # Add correlation coefficient to vector:
      cor_coeff_vec[length(cor_coeff_vec) + 1] <- cor_coeff
    }
    
    # Add this value to df_out
    temp_df_out <- data.frame(gap = rep(i, 50),
                              cor_coeff = cor_coeff_vec)
    df_out <- rbind(df_out, temp_df_out)
  } 
  return(df_out)
}

# Interpolation function to interpolate soil moisture in gaps below a specific value:
interpolate_moisture <- function(moisture_df, col_vec, max_gap){
  
  df_out <- moisture_df
  
  for (i in col_vec){
    moist_col <- moisture_df[[i]]
    moist_col_int <- na.approx(moist_col, na.rm = F, maxgap = max_gap)
    df_out[, i] <- moist_col_int
  }
  
  return(df_out)
}




#####################
## Converting Data ##
#####################

convert_mm_ml <- function(df, col_ind_vec){
  df_out <- df
  for (ind in col_ind_vec){
    df_out[, ind] <- df[, ind]*2000
  }
  return(df_out)
}

convert_ml_l <- function(df, col_ind_vec){
  df_out <- df
  for (ind in col_ind_vec){
    df_out[, ind] <- df[, ind]/1000
  }
  return(df_out)
}

convert_vwc <- function(df_in, depth){
  # Data frame with time as first column, and one column for VWC
  
  # Depths of the SoilVUE sensors:
  pos_depths <- c(0, 5, 10, 20, 30, 40, 50, 60, 75, 100)
  # index of the input df depth in the possible_depths vector:
  ind <- which(pos_depths == depth)
  
  if (colnames(df_in)[1] != "time"){
    stop("Please provide a data frame with 'time' as the first column.")
  }
  
  df_out <- data.frame(time = df_in[1],
                       soil_moisture = NA)
  if (depth == 5){
    vertical_range <- (depth - pos_depths[ind-1]) + (pos_depths[ind+1] - depth)/2
    # Divide by 100 to convert from cm to mm:
    vertical_range <- vertical_range/100
  }
  else if(depth == 100){
    prev_max <- (depth - pos_depths[ind-1])/2 + pos_depths[ind-1]
    vertical_range <- depth - prev_max
    # Divide by 100 to convert from cm to m:
    vertical_range <- vertical_range/100
  }
  else{
    # Find the maximum depth covered by previous sensor:
    prev_max <- (depth - pos_depths[ind-1])/2 + pos_depths[ind-1]
    vertical_range <- (depth - prev_max) + (pos_depths[ind+1] - depth)/2
    # Divide by 100 to convert from cm to m:
    vertical_range <- vertical_range/100
  }
  
  print(glue("this is the depth in m covered by the sensor: {vertical_range}"))
  
  for(i in 1:nrow(df_in)){
    # Calculate volume in m^3, multiply by 1000 to convert to litres:
    df_out[i, 2] <- (df_in[i, 2]/100)*(1*2*vertical_range)*1000
  }
  
  return(df_out)
}

geom_mean <- function(x){
  exp(mean(log(x)))
}

# Following function based on: https://github.com/fmcclean/ngif/blob/master/app.py
smooth_outflow <- function(df_in, col_ind_vec){
  df_out <- df_in
  for (i in col_ind_vec){
    values <- df_in[, i]
    idx <- c()
    for (j in seq_along(values)){
      idx[length(idx) + 1] <- j
      if (isTRUE(values[j] != 0)){
        values[idx] <- values[j] / length(idx)
        idx <- c()
      }
    }
    df_out[, i] <- values
  }
  return(df_out)
}


#####################
## Rainfall Events ##
#####################

rainfall_events <- function(df, MIT){
  df[, "dry"] <- ifelse(df[, 2] > 0, "Wet", "Dry")
  event_count <- 0
  df[, "timer"] <- NA
  df[, "event"] <- NA
  for (i in 1:nrow(df)){
    #if(is.nan(df[i, "dry"]) | is.na(df[i, "dry"])){
    #  next
    #}
    if(is.nan(df[i, 2]) | is.na(df[i, 2])){
      df[i, "timer"] = df[(i - 1), "timer"]
      next
    }
    if (i == 1){
      if (df[i, "dry"] %in% "Wet"){
        event_count <- event_count + 1
        df[i, "event"] = event_count
        df[i, "timer"] = 0
      }
      else{
        df[i, "timer"] = 1
      }
    }
    else {
      if (df[i, "dry"] %in% "Wet") {
        df[i, "timer"] = 0
        if (df[(i - 1), "dry"] %in% "Dry"){
          if (df[(i - 1), "timer"] >= MIT){
            event_count <- event_count + 1
            df[i, "event"] = event_count
          }
          else if (df[(i - 1), "timer"] < MIT){
            df[i, "event"] = event_count
          }
        }
        if (df[(i - 1), "dry"] %in% "Wet"){
          df[i, "event"] = event_count
        }
      }
      if (df[i, "dry"] %in% "Dry"){
        df[i, "timer"] = df[(i - 1), "timer"] + 1
      }
    }
    percent_na <- sum(is.na(df$timer))/nrow(df)
  }
  for (i in rev(seq(1:nrow(df)))){
    if (df[i, "dry"] %in% "Dry"){
      if (df[i, "timer"] >= MIT){
        df[i, "event"] = NA
      }
      else{
        if (is.na(df[(i + 1), "event"])){
          df[i, "event"] = NA
        }
        else{
          df[i, "event"] = df[(i + 1), "event"]
        }
      }
    }
  }
  return(df)
}

classify_events <- function(df, df_interval){
  # Assumes rainfall to be in column 2
  int_in_hour <- 60/df_interval
  df[, "duration"] <- NA
  df[, "event_total"] <- NA
  df[, "mean_intensity_mm.h"] <- NA
  df[, "peak_rainfall"] <- NA
  df[, "intra_dry_max"] <- NA
  df[, "ADWP"] <- NA
  
  for (i in 1:max(df$event, na.rm = T)){
    
    subset_df <- df[df$event == i & !is.na(df$event),]
    duration <- nrow(subset_df)
    event_total <- sum(subset_df[, 2], na.rm = T)
    # Multiply the mean by number of original df intervals in an hour to get
    # mean intensity in mm/h:
    mean_intensity_mm.h <- mean(subset_df[, 2], na.rm = T)*int_in_hour
    peak_rainfall <- max(subset_df[, 2], na.rm = T)
    intra_dry_max <- max(subset_df$timer, na.rm = T)
    
    
    df[df$event == i & !is.na(df$event), ]$duration <- duration
    df[df$event == i & !is.na(df$event), ]$event_total <- event_total
    df[df$event == i & !is.na(df$event), ]$mean_intensity_mm.h <- mean_intensity_mm.h
    df[df$event == i & !is.na(df$event), ]$peak_rainfall <- peak_rainfall
    df[df$event == i & !is.na(df$event), ]$intra_dry_max <- intra_dry_max
    
  }
  
  for (j in 1:nrow(df)){
    if (isTRUE(!is.na(df[j, "event"]) && is.na(df[(j-1), "event"]))){
      ev <- df[j, "event"]
      ADWP <- df[(j - 1), "timer"]
      df[df$event == ev & !is.na(df$event), ]$ADWP <- ADWP
    }
  }
  
  return(df)
}

return_period <- function(df, ddf_table){
  
  for (i in 1:nrow(df)){
    if (is.na(df[i, "event_total"]) == F){ # Loop only over event rows
      dur <- df[i, "duration"] # Duration for this event
      depth <- df[i, "event_total"] # Depth for this event
      ddf_dur <- as.vector(ddf_table[, "Duration.hours"]) # Vector of durations in table
      
      if(min(ddf_dur) <= dur & dur <= max(ddf_dur)){ # Check within table range
        ddf_ind_row <- Closest(ddf_dur, dur, which = T)
        ddf_depth <- as.vector(unlist(unname(ddf_table[ddf_ind_row, c(3:ncol(ddf_table))])))
        
        if (min(ddf_depth) <= depth & depth <= max(ddf_depth)){ # Check within table range
          ddf_ind_col <- Closest(ddf_depth, depth, which = T)
          
          if (ddf_depth[ddf_ind_col] > depth){
            upper <- names(ddf_table)[ddf_ind_col + 2]
            upper <- as.numeric(gsub("\\D", "", upper))
            lower <- names(ddf_table)[(ddf_ind_col - 1 + 2)]
            lower <- as.numeric(gsub("\\D", "", lower))
          }
          
          else{
            lower <- names(ddf_table)[ddf_ind_col + 2]
            lower <- as.numeric(gsub("\\D", "", lower))
            upper <- names(ddf_table)[(ddf_ind_col + 1 + 2)]
            upper <- as.numeric(gsub("\\D", "", upper))
          }
          
          int_df <- data.frame(rp = 1:20,
                               dpth = ddf_depth[1:20])
          int_rp <- as.numeric(approx(int_df$dpth, int_df$rp, xout = depth)$y)
          df[i, "return_period_interval"] <- glue("{lower} - {upper}")
          df[i, "return_period_LINEAR_INTERPOLATION"] <- int_rp
        }
        
        else{
          next
        }
      }
    }
  }
  return(df)
}

return_period_quartiles <- function(df, ddf_table){
  
  ddf_dur <- as.vector(ddf_table[, "Duration.hours"]) # Vector of durations in table
  
  for (i in 1:max(df[, "event"], na.rm = T)){ # Loop through events
    subset_df <- df[df$event == i & !is.na(df$event), ] # Select the event only
    
    if (!is.na(subset_df[1, "quartile"])){ # Exclude events to small to be broken up into quartiles
      for (j in 1:4){ # Loop over quartiles
        
        quartile <- subset_df[subset_df$quartile == j, ]
        
        # Indices of the start and end of the quartile in the original data frame:
        quart_start <- which(df[, "event"] == i & df[, "quartile"] == j)[1]
        quart_end <- which(df[, "event"] == i & df[, "quartile"] == j)[nrow(quartile)]
        
        dur <- nrow(quartile) # Duration of this quartile
        depth <- sum(quartile[, 2]) # Depth of this quartile
        
        if(min(ddf_dur) <= dur & dur <= max(ddf_dur)){ # Check within table range
          
          ddf_ind_row <- Closest(ddf_dur, dur, which = T)
          ddf_depth <- as.vector(unlist(unname(ddf_table[ddf_ind_row, c(3:ncol(ddf_table))])))
          
          if (min(ddf_depth) <= depth & depth <= max(ddf_depth)){ # Check within table range
            ddf_ind_col <- Closest(ddf_depth, depth, which = T)
            
            print("found one with a suitable depth")
            print(i)
            View(quartile)
            
            # Determining the upper and lower bounds for the return period:
            
            if (ddf_depth[ddf_ind_col] > depth){
              upper <- names(ddf_table)[ddf_ind_col + 2]
              upper <- as.numeric(gsub("\\D", "", upper))
              lower <- names(ddf_table)[(ddf_ind_col - 1 + 2)]
              lower <- as.numeric(gsub("\\D", "", lower))
            }
            
            else{
              lower <- names(ddf_table)[ddf_ind_col + 2]
              lower <- as.numeric(gsub("\\D", "", lower))
              upper <- names(ddf_table)[(ddf_ind_col + 1 + 2)]
              upper <- as.numeric(gsub("\\D", "", upper))
            }
            
            int_df <- data.frame(rp = 1:20,
                                 dpth = ddf_depth[1:20])
            int_rp <- as.numeric(approx(int_df$dpth, int_df$rp, xout = depth)$y)
            df[quart_start:quart_end, "quartile_return_period_interval"] <- glue("{lower} - {upper}")
            df[quart_start:quart_end, "quartile_return_period_LINEAR_INTERPOLATION"] <- int_rp
          }
          
          else{
            next
          }
        }
      }
    }
  }
  return(df)
}

# At the moment, based on: https://rainsimulator.com/guides/intensity-categories/
# Assumes hourly data:
other_classifications <- function(df){
  df[, "MET_ave_intensity"] <- NA
  df[, "MET_max_intensity"] <- NA
  df[, "quartile"] <- NA
  df[, "quartile_percent"] <- NA
  for (i in 1:max(df[, "event"], na.rm = T)){
    
    # Row indices for all rows in event:
    inds <- which(df[, "event"] == i)
    # Select the rows for this event:
    subset_df <- df[df$event == i & !is.na(df$event), ]
    
    # Reference event characteristics:
    mean_intensity <- subset_df[1, "mean_intensity_mm.h"]
    peak_rainfall <- subset_df[1, "peak_rainfall"]
    event_total <- subset_df[1, "event_total"]
    
    if (mean_intensity <= 1){
      df[inds, "MET_ave_intensity"] <- "Light"
    }
    else if (mean_intensity <= 4){
      df[inds, "MET_ave_intensity"] <- "Moderate"
    }
    else if (mean_intensity <= 32){
      df[inds, "MET_ave_intensity"] <- "Heavy"
    }
    else if (mean_intensity > 32){
      df[inds, "MET_ave_intensity"] <- "Very Heavy/Violent"
    }
    
    # Following the assumption that rainfall is in column 2 and is hourly:
    if (peak_rainfall <= 1){
      df[inds, "MET_max_intensity"] <- "Light"
    }
    else if (peak_rainfall <= 4){
      df[inds, "MET_max_intensity"] <- "Moderate"
    }
    else if (peak_rainfall <= 32){
      df[inds, "MET_max_intensity"] <- "Heavy"
    }
    else if (peak_rainfall > 32){
      df[inds, "MET_max_intensity"] <- "Very Heavy/Violent"
    }
    
    # Calculate quartile statistics only for events of > 4 hours
    if (nrow(subset_df) >= 4){
      quartiles <- split_quantile(subset_df$time, 4)
      # Assign quantile numbers to rows for this event in subset and original df:
      df[inds, "quartile"] <- quartiles
      subset_df[(1:nrow(subset_df)), "quartile"] <- quartiles
      
      # Calculate % of total event depth for each quartile:
      for (j in 1:4){
        quart_inds_sub <- which(subset_df[, "quartile"] == j)
        quartile_total <- sum(subset_df[quart_inds_sub, 2])
        quartile_percent <- quartile_total/event_total
        
        # Find the indices of these rows in the original df:
        quart_inds <- inds[quart_inds_sub]
        df[quart_inds, "quartile_percent"] <- quartile_percent
      }
    }
  }
  return(df)
}

events_only <- function(df){
  df <- df[!is.na(df$event), ]
  return(df)
}


## Rainfall Events with Threshold ##

rainfall_events_thresh <- function(df, MIT, min_rain){
  
  # Create an event counter, set it to zero
  event_count <- 0
  
  # Create: "timer" column to measure length of dry period; "event" column to 
  # record event number; "pre_event_rain" column to record cumulative rain
  # between events falling below the threshold.
  df[, "timer"] <- NA
  df[, "event"] <- NA
  df[, "pre_event_rain"] <- NA
  
  # Loop over each row in the input data frame:
  for (i in seq_len(nrow(df))){
    # Skip NA or NaN rows:
    if(is.nan(df[i, 2]) | is.na(df[i, 2])){
      df[i, "timer"] = df[(i - 1), "timer"]
      next
    }
    
    # Create three categories: no rain ("Dry"); rain below threshold ("Damp");
    # rain above threshold ("Wet"):
    if (df[i, 2] == 0){
      df[i, "dry"] <- "Dry"
    }
    else if (df[i, 2] < min_rain){
      df[i, "dry"] <- "Damp"
    }
    else{
      df[i, "dry"] <- "Wet"
    }
    
    # For the first row in the data frame (as it cannot be compared to a
    # previous row)
    # NOTE: in defining events, "Damp" and "Dry" intervals are treated the same.
    
    if (i == 1){
      if (df[i, "dry"] %in% "Wet"){
        event_count <- event_count + 1
        df[i, "event"] <- event_count
        df[i, "timer"] <- 0
      }
      else{
        df[i, "timer"] <- 1
      }
    } 
    else {
      if (df[i, "dry"] %in% "Wet") {
        # If a time interval is classed as "Wet", set dry-period timer to 0
        df[i, "timer"] <- 0
        if (df[(i - 1), "dry"] %in% "Dry" | df[(i - 1), "dry"] %in% "Damp"){
          # If the previous interval is classed as "Dry" and its dry-period timer
          # is greater than 0, this represents the start of a new event, and
          # event_count increases.
          if (df[(i - 1), "timer"] >= MIT){
            event_count <- event_count + 1
            df[i, "event"] <- event_count
          }
          # Else if the MIT has not been reached, this "Wet" interval is
          # considered part of the same event as the preceding "Wet" interval.
          else if (df[(i - 1), "timer"] < MIT){
            df[i, "event"] <- event_count
          }
        }
        # If the previous interval was also "Wet", the current "Wet" interval is
        # part of the same event as the previous.
        if (df[(i - 1), "dry"] %in% "Wet"){
          df[i, "event"] <- event_count
        }
      }
      # If the current interval is dry, the dry-period timer increases.
      if (df[i, "dry"] %in% "Dry" | df[i, "dry"] %in% "Damp"){
        df[i, "timer"] <- df[(i - 1), "timer"] + 1
      }
    }
    
    # The following keeps a record of the below-threshold rainfall that falls
    # between events:
    
    if (df[i, "dry"] %in% "Dry" | df[i, "dry"] %in% "Damp"){
      # Since there is no preceding row when i == 1:
      if (i == 1){
        df[i, "pre_event_rain"] <- df[i, 2]
      }
      else{
        df[i, "pre_event_rain"] <- df[(i - 1), "pre_event_rain"] + df[i, 2]
      }
    }
    if (df[i, "dry"] %in% "Wet"){
      # For the first interval in the event, record the depth of below-
      # threshold preceding the event in the "pre_event_rain" to aid future
      # event analysis:
      if (df[(i - 1), "dry"] %in% "Dry" && df[(i - 1), "timer"] >= MIT){
        df[i, "pre_event_rain"] <- df[(i - 1), "pre_event_rain"]
      }
      # For the rest of the event, reset the pre_event_rain value to zero:
      else{
        df[i, "pre_event_rain"] <- 0
      }
    }
  }
  
  # The following assigns the correct event number to dry periods within
  # rainfall events:
  # Going from the end to the start of the data frame:
  for (i in rev(seq(1:nrow(df)))){
    if (df[i, "dry"] %in% "Dry" | df[i, "dry"] %in% "Damp"){
      # If the interval is "Dry" and the dry-period timer value is greater or
      # equal to the MIT, it does not fall within an event.
      if (df[i, "timer"] >= MIT){
        df[i, "event"] = NA
      }
      else{
        # The timer value is less than the MIT, so this interval possibly falls
        # within an event.
        # However, if the interval following this interval (in time) has not
        # been assigned an event number, this interval must not fall within an
        # event either.
        if (is.na(df[(i + 1), "event"])){
          df[i, "event"] = NA
        }
        # Otherwise, it must fall within an event and so takes the event number
        # of the interval following it (in time).
        else{
          df[i, "event"] = df[(i + 1), "event"]
        }
      }
    }
  }
  return(df)
}

# This function is currently identical to the classify_events() function. It
# should work correctly as it uses the standard column names the 
# rainfall_events() functions give:
classify_events_thresh <- function(df, df_interval){
  # Assumes rainfall to be in column 2
  int_in_hour <- 60/df_interval
  df[, "duration"] <- NA
  df[, "event_total"] <- NA
  df[, "mean_intensity_mm.h"] <- NA
  df[, "peak_rainfall"] <- NA
  df[, "intra_dry_max"] <- NA
  df[, "ADWP"] <- NA
  
  for (i in 1:max(df$event, na.rm = T)){
    
    subset_df <- df[df$event == i & !is.na(df$event),]
    duration <- nrow(subset_df)
    event_total <- sum(subset_df[, 2], na.rm = T)
    # Multiply the mean by number of original df intervals in an hour to get
    # mean intensity in mm/h:
    mean_intensity_mm.h <- mean(subset_df[, 2], na.rm = T)*int_in_hour
    peak_rainfall <- max(subset_df[, 2], na.rm = T)
    intra_dry_max <- max(subset_df$timer, na.rm = T)
    
    
    df[df$event == i & !is.na(df$event), ]$duration <- duration
    df[df$event == i & !is.na(df$event), ]$event_total <- event_total
    df[df$event == i & !is.na(df$event), ]$mean_intensity_mm.h <- mean_intensity_mm.h
    df[df$event == i & !is.na(df$event), ]$peak_rainfall <- peak_rainfall
    df[df$event == i & !is.na(df$event), ]$intra_dry_max <- intra_dry_max
    
  }
  
  for (j in 1:nrow(df)){
    if (isTRUE(!is.na(df[j, "event"]) && is.na(df[(j-1), "event"]))){
      ev <- df[j, "event"]
      ADWP <- df[(j - 1), "timer"]
      df[df$event == ev & !is.na(df$event), ]$ADWP <- ADWP
    }
  }
  
  return(df)
}

# This function is identical to other_classifications_thresh() and likewise
# should work:
other_classifications_thresh <- function(df){
  df[, "MET_ave_intensity"] <- NA
  df[, "MET_max_intensity"] <- NA
  df[, "quartile"] <- NA
  df[, "quartile_percent"] <- NA
  for (i in 1:max(df[, "event"], na.rm = T)){
    
    # Row indices for all rows in event:
    inds <- which(df[, "event"] == i)
    # Select the rows for this event:
    subset_df <- df[df$event == i & !is.na(df$event), ]
    
    # Reference event characteristics:
    mean_intensity <- subset_df[1, "mean_intensity_mm.h"]
    peak_rainfall <- subset_df[1, "peak_rainfall"]
    event_total <- subset_df[1, "event_total"]
    
    if (mean_intensity <= 1){
      df[inds, "MET_ave_intensity"] <- "Light"
    }
    else if (mean_intensity <= 4){
      df[inds, "MET_ave_intensity"] <- "Moderate"
    }
    else if (mean_intensity <= 32){
      df[inds, "MET_ave_intensity"] <- "Heavy"
    }
    else if (mean_intensity > 32){
      df[inds, "MET_ave_intensity"] <- "Very Heavy/Violent"
    }
    
    # Following the assumption that rainfall is in column 2 and is hourly:
    if (peak_rainfall <= 1){
      df[inds, "MET_max_intensity"] <- "Light"
    }
    else if (peak_rainfall <= 4){
      df[inds, "MET_max_intensity"] <- "Moderate"
    }
    else if (peak_rainfall <= 32){
      df[inds, "MET_max_intensity"] <- "Heavy"
    }
    else if (peak_rainfall > 32){
      df[inds, "MET_max_intensity"] <- "Very Heavy/Violent"
    }
    
    # Calculate quartile statistics only for events of > 4 hours
    if (nrow(subset_df) >= 4){
      quartiles <- split_quantile(subset_df$time, 4)
      # Assign quantile numbers to rows for this event in subset and original df:
      df[inds, "quartile"] <- quartiles
      subset_df[(1:nrow(subset_df)), "quartile"] <- quartiles
      
      # Calculate % of total event depth for each quartile:
      for (j in 1:4){
        quart_inds_sub <- which(subset_df[, "quartile"] == j)
        quartile_total <- sum(subset_df[quart_inds_sub, 2])
        quartile_percent <- quartile_total/event_total
        
        # Find the indices of these rows in the original df:
        quart_inds <- inds[quart_inds_sub]
        df[quart_inds, "quartile_percent"] <- quartile_percent
      }
    }
  }
  return(df)
}

# ''
return_period_thresh <- function(df, ddf_table){
  df[, "return_period"] <- NA # Add column for return period
  
  for (i in 1:nrow(df)){
    if (is.na(df[i, "event_total"]) == F){ # Loop only over event rows
      dur <- df[i, "duration"] # Duration for this event
      depth <- df[i, "event_total"] # Depth for this event
      ddf_dur <- as.vector(ddf_table[, "Duration.hours"]) # Vector of durations in table
      
      if(min(ddf_dur) <= dur & dur <= max(ddf_dur)){ # Check within table range
        ddf_ind_row <- Closest(ddf_dur, dur, which = T)
        ddf_depth <- as.vector(unlist(unname(ddf_table[ddf_ind_row, c(3:ncol(ddf_table))])))
        
        if (min(ddf_depth) <= depth & depth <= max(ddf_depth)){ # Check within table range
          ddf_ind_col <- Closest(ddf_depth, depth, which = T)
          
          if (ddf_depth[ddf_ind_col] > depth){
            upper <- names(ddf_table)[ddf_ind_col + 2]
            upper <- as.numeric(gsub("\\D", "", upper))
            lower <- names(ddf_table)[(ddf_ind_col - 1 + 2)]
            lower <- as.numeric(gsub("\\D", "", lower))
          }
          
          else{
            lower <- names(ddf_table)[ddf_ind_col + 2]
            lower <- as.numeric(gsub("\\D", "", lower))
            upper <- names(ddf_table)[(ddf_ind_col + 1 + 2)]
            upper <- as.numeric(gsub("\\D", "", upper))
          }
          
          int_df <- data.frame(rp = 1:20,
                               dpth = ddf_depth[1:20])
          int_rp <- as.numeric(approx(int_df$dpth, int_df$rp, xout = depth)$y)
          df[i, "return_period_interval"] <- glue("{lower} - {upper}")
          df[i, "return_period_LINEAR_INTERPOLATION"] <- int_rp
        }
        
        else{
          next
        }
      }
    }
  }
  return(df)
}



##########################
## Sensitivity Analysis ##
##########################

events_depth_dur <- function(df, MIT, rain_col_ind){
  df[, "duration"] <- NA
  df[, "event_total"] <- NA
  for (i in 1:max(df$event, na.rm = T)){
    subset_df <- df[df$event == i & !is.na(df$event),]
    
    duration <- nrow(subset_df)
    event_total <- sum(subset_df[rain_col_ind], na.rm = T)
    
    df[df$event == i & !is.na(df$event), ]$duration <- duration
    df[df$event == i & !is.na(df$event), ]$event_total <- event_total
  }
  return(df)
}

test_mit <- function(df, mit_vec, df_interval){
  
  box_df <- data.frame(mit = NA,
                       depth = NA,
                       duration = NA,
                       return = NA,
                       ave_intensity = NA,
                       adwp = NA)
  
  totals_df <- data.frame(mit = NA,
                          events = NA,
                          extremes = NA,
                          prop_extreme = NA)
  
  for(mit in mit_vec){
    df_events <- classify_events(rainfall_events(df, mit), df_interval)
    df_events <- return_period(df_events, ddf_table)
    
    events <- max(df_events$event, na.rm = T)
    extremes <- length(unique(df_events[!is.na(df_events$return_period_interval), ]$event))
    temp_totals_df <- data.frame(mit = mit,
                                 events = events,
                                 extremes = extremes,
                                 prop_extreme = extremes/events)
    totals_df <- rbind(totals_df, temp_totals_df)
    
    depth <- c()
    duration <- c()
    return <- c()
    ave_intensity <- c()
    adwp <- c()
    for (i in 1:max(df_events$event, na.rm = T)){
      subset_df <- df_events[df_events$event == i & !is.na(df_events$event), ]
      depth[length(depth) + 1] <- subset_df[1, ]$event_total
      duration[length(duration) + 1] <- subset_df[1, ]$duration
      return[length(return) + 1] <- subset_df[1, ]$return_period_LINEAR_INTERPOLATION
      ave_intensity[length(ave_intensity) + 1] <- subset_df[1, ]$mean_intensity_mm.h
      adwp[length(adwp) + 1] <- subset_df[1, ]$ADWP
    }
    temp_box_df <- data.frame(mit = rep(mit, max(df_events$event, na.rm = T)),
                              depth = depth,
                              duration = duration,
                              return = return,
                              ave_intensity = ave_intensity,
                              adwp = adwp)
    box_df <- rbind(box_df, temp_box_df)
  }
  return(list(box_df, totals_df))
}

summarise_test_mit <- function(box_df, totals_df){
  mit_vals <- as.vector(totals_df$mit)
  
  mean_depth = c()
  mean_duration = c()
  mean_ave_intensity = c()
  mean_adwp = c()
  
  g_mean_depth = c()
  g_mean_duration = c()
  g_mean_ave_intensity = c()
  g_mean_adwp = c()
  
  for (mit in mit_vals){
    mean_depth[length(mean_depth) + 1] <- mean(box_df[box_df[, "mit"] == mit, "depth"])
    mean_duration[length(mean_duration) + 1] <- mean(box_df[box_df[, "mit"] == mit, "duration"])
    mean_ave_intensity[length(mean_ave_intensity) + 1] <- mean(box_df[box_df[, "mit"] == mit, "ave_intensity"])
    mean_adwp[length(mean_adwp) + 1] <- mean(box_df[box_df[, "mit"] == mit, "adwp"])
    
    g_mean_depth[length(g_mean_depth) + 1] <- geom_mean(box_df[box_df[, "mit"] == mit, "depth"])
    g_mean_duration[length(g_mean_duration) + 1] <- geom_mean(box_df[box_df[, "mit"] == mit, "duration"])
    g_mean_ave_intensity[length(g_mean_ave_intensity) + 1] <- geom_mean(box_df[box_df[, "mit"] == mit, "ave_intensity"])
    g_mean_adwp[length(g_mean_adwp) + 1] <- geom_mean(box_df[box_df[, "mit"] == mit, "adwp"])
  }
  
  print(length(mit_vals))
  print(length(totals_df$events))
  print(mean_depth)
  print(mean_duration)
  print(mean_ave_intensity)
  print(mean_adwp)
  
  df_out <- data.frame(mit = mit_vals,
                       num_events = totals_df$events,
                       mean_depth = mean_depth,
                       mean_duration = mean_duration,
                       mean_ave_intensity = mean_ave_intensity,
                       mean_adwp = mean_adwp)
  df_out_geom <- data.frame(mit = mit_vals,
                            num_events = totals_df$events,
                            g_mean_depth = g_mean_depth,
                            g_mean_duration = g_mean_duration,
                            g_mean_ave_intensity = g_mean_ave_intensity,
                            g_mean_adwp = g_mean_adwp)
  
  return(list(df_out, df_out_geom))
}

sensitivity_outflow <- function(out_df, rain_df, MIT){
  
  rain_df <- convert_mm_ml(rain_df, 2)
  out_colnames <- as.vector(colnames(out_df))
  
  rain_df <- rainfall_events(rain_df, MIT)
  rain_df <- events_depth_dur(rain_df, MIT, 2)
  rain_colnames <- as.vector(colnames(rain_df[, 2:ncol(rain_df)]))
  all_colnames <- c(rain_colnames, out_colnames)
  full_df <- combine_data(list(rain_df, out_df))
  
  full_df[, "ADWP_48"] <- NA
  full_df[, "c_out_3A"] <- NA
  full_df[, "c_out_3B"] <- NA
  full_df[, "c_out_4A"] <- NA
  full_df[, "c_out_4B"] <- NA
  full_df[, "c_out_7A"] <- NA
  full_df[, "c_out_7B"] <- NA
  full_df[, "c_out_8A"] <- NA
  full_df[, "c_out_8B"] <- NA
  
  for (j in 1:nrow(full_df)){
    if(isTRUE(full_df[(j-1), "timer"] > 48) && !is.na(full_df[j, "event"])){
      full_df[j, "ADWP_48"] <- full_df[(j-1), "timer"]
    }
  }
  
  for (k in 1:nrow(full_df)){
    # If at the start of a new event, the cumulative outflow values are equal
    # to the current outflow values.
    if (!is.na(full_df[k, "event"]) && is.na(full_df[(k-1), "event"])){
      current_event_total <- full_df[k, "event_total"]
      full_df[k, "c_out_3A"] <- full_df[k, 8]/full_df[k, "event_total"]
      full_df[k, "c_out_3B"] <- full_df[k, 9]/full_df[k, "event_total"]
      full_df[k, "c_out_4A"] <- full_df[k, 10]/full_df[k, "event_total"]
      full_df[k, "c_out_4B"] <- full_df[k, 11]/full_df[k, "event_total"]
      full_df[k, "c_out_7A"] <- full_df[k, 12]/full_df[k, "event_total"]
      full_df[k, "c_out_7B"] <- full_df[k, 13]/full_df[k, "event_total"]
      full_df[k, "c_out_8A"] <- full_df[k, 14]/full_df[k, "event_total"]
      full_df[k, "c_out_8B"] <- full_df[k, 15]/full_df[k, "event_total"]
    }
    # Otherwise, so long as the previous cumulative outflow value is not NA,
    # continue calculating the cumulative value.
    else if (k > 1 && !is.na(full_df[(k-1), "c_out_3A"])){
      full_df[k, "c_out_3A"] <- full_df[(k-1), "c_out_3A"] + full_df[k, 8]/current_event_total
      full_df[k, "c_out_3B"] <- full_df[(k-1), "c_out_3B"] + full_df[k, 9]/current_event_total
      full_df[k, "c_out_4A"] <- full_df[(k-1), "c_out_4A"] + full_df[k, 10]/current_event_total
      full_df[k, "c_out_4B"] <- full_df[(k-1), "c_out_4B"] + full_df[k, 11]/current_event_total
      full_df[k, "c_out_7A"] <- full_df[(k-1), "c_out_7A"] + full_df[k, 12]/current_event_total
      full_df[k, "c_out_7B"] <- full_df[(k-1), "c_out_7B"] + full_df[k, 13]/current_event_total
      full_df[k, "c_out_8A"] <- full_df[(k-1), "c_out_8A"] + full_df[k, 14]/current_event_total
      full_df[k, "c_out_8B"] <- full_df[(k-1), "c_out_8B"] + full_df[k, 15]/current_event_total
    }
    
  }
  return(full_df)
}

test_mit_outflow <- function(out_df, rain_df, mit_vec){
  
  # Create an empty summary data frame
  df_out <- data.frame(mit = NA,
                       depth = NA,
                       duration = NA,
                       time_to_90_3A = NA,
                       time_to_90_3B = NA,
                       time_to_90_4A = NA,
                       time_to_90_4B = NA,
                       time_to_90_7A = NA,
                       time_to_90_7B = NA,
                       time_to_90_8A = NA,
                       time_to_90_8B = NA)
  
  # Empty vectors to store values for this MIT value:
  
  # Obtaining statistics for each MIT value:
  for (mit in mit_vec){
    full_df <- sensitivity_outflow(out_df, rain_df, mit)
    
    depth <- c()
    duration <- c()
    time_to_90_3A <- c()
    time_to_90_3B <- c()
    time_to_90_4A <- c()
    time_to_90_4B <- c()
    time_to_90_7A <- c()
    time_to_90_7B <- c()
    time_to_90_8A <- c()
    time_to_90_8B <- c()
    
    # Loop through each event number:
    for (i in 1:max(full_df$event, na.rm = T)){
      # Get index of row with first occurrence of this event
      event_start_ind <- which(full_df$event == i)[1]
      # Get index of row with first occurrence of next event
      event_end_ind <- which(full_df$event == (i + 1))[1] - 1
      # Select all rows from beginning of current event up to (not including)
      # start of next event (if statement to account for NA in event_end_ind
      # when last event reached):
      if (!is.na(event_end_ind)){
        subset_df <- full_df[event_start_ind:event_end_ind, ]
        
        depth[length(depth) + 1] <- subset_df[1, ]$event_total
        duration[length(duration) + 1] <- subset_df[1, ]$duration
        
        # If there is a dry period of at least 48 hours preceding this event:
        if (isTRUE(subset_df[1, ]$ADWP_48 > 48)){
          ind <- which(subset_df$c_out_3A > 0.9)[1]
          time_to_90_3A[length(time_to_90_3A) + 1] <- subset_df[ind, ]$timer
          ind <- which(subset_df$c_out_3B > 0.9)[1]
          time_to_90_3B[length(time_to_90_3B) + 1] <- subset_df[ind, ]$timer
          
          ind <- which(subset_df$c_out_4A > 0.9)[1]
          time_to_90_4A[length(time_to_90_4A) + 1] <- subset_df[ind, ]$timer
          ind <- which(subset_df$c_out_4B > 0.9)[1]
          time_to_90_4B[length(time_to_90_4B) + 1] <- subset_df[ind, ]$timer
          
          ind <- which(subset_df$c_out_7A > 0.9)[1]
          time_to_90_7A[length(time_to_90_7A) + 1] <- subset_df[ind, ]$timer
          ind <- which(subset_df$c_out_7B > 0.9)[1]
          time_to_90_7B[length(time_to_90_7B) + 1] <- subset_df[ind, ]$timer
          
          ind <- which(subset_df$c_out_8A > 0.9)[1]
          time_to_90_8A[length(time_to_90_8A) + 1] <- subset_df[ind, ]$timer
          ind <- which(subset_df$c_out_8B > 0.9)[1]
          time_to_90_8B[length(time_to_90_8B) + 1] <- subset_df[ind, ]$timer
        }
        else{
          time_to_90_3A[length(time_to_90_3A) + 1] <- NA
          time_to_90_3B[length(time_to_90_3B) + 1] <- NA
          time_to_90_4A[length(time_to_90_4A) + 1] <- NA
          time_to_90_4B[length(time_to_90_4B) + 1] <- NA
          time_to_90_7A[length(time_to_90_7A) + 1] <- NA
          time_to_90_7B[length(time_to_90_7B) + 1] <- NA
          time_to_90_8A[length(time_to_90_8A) + 1] <- NA
          time_to_90_8B[length(time_to_90_8B) + 1] <- NA
          
        }
      }
    }
    
    print(max(full_df$event, na.rm = T))
    print(length(depth))
    print(length(duration))
    print(length(time_to_90_3A))
    print(length(time_to_90_3B))
    print(length(time_to_90_4A))
    print(length(time_to_90_4B))
    print(length(time_to_90_7A))
    print(length(time_to_90_7B))
    print(length(time_to_90_8A))
    print(length(time_to_90_8B))
    
    temp_df_out <- data.frame(mit = rep(mit, (max(full_df$event, na.rm = T) - 1)),
                              depth = depth,
                              duration = duration,
                              time_to_90_3A = time_to_90_3A,
                              time_to_90_3B = time_to_90_3B,
                              time_to_90_4A = time_to_90_4A,
                              time_to_90_4B = time_to_90_4B,
                              time_to_90_7A = time_to_90_7A,
                              time_to_90_7B = time_to_90_7B,
                              time_to_90_8A = time_to_90_8A,
                              time_to_90_8B = time_to_90_8B)
    df_out <- rbind(df_out, temp_df_out)
  }
  return(df_out)
}

test_rain_threashold <- function(df, rain_min_vec, MIT, df_interval, ddf_table){
  
  box_df <- data.frame(min_rain_threshold = NA,
                       depth = NA,
                       duration = NA,
                       return = NA,
                       ave_intensity = NA,
                       adwp = NA,
                       pre_event_rain = NA)
  
  totals_df <- data.frame(min_rain_threshold = NA,
                          events = NA,
                          extremes = NA,
                          prop_extreme = NA)
  
  for(i in rain_min_vec){
    df_events <- rainfall_events_thresh(df, MIT, i)
    df_events <- classify_events_thresh(df_events, df_interval)
    df_events <- return_period_thresh(df_events, ddf_table)
    
    events <- max(df_events$event, na.rm = T)
    extremes <- length(unique(df_events[!is.na(df_events$return_period_interval), ]$event))
    temp_totals_df <- data.frame(min_rain_threshold = i,
                                 events = events,
                                 extremes = extremes,
                                 prop_extreme = extremes/events)
    totals_df <- rbind(totals_df, temp_totals_df)
    
    depth <- c()
    duration <- c()
    return <- c()
    ave_intensity <- c()
    adwp <- c()
    pre_event_rain <- c()
    for (j in 1:max(df_events$event, na.rm = T)){
      subset_df <- df_events[df_events$event == j & !is.na(df_events$event), ]
      depth[length(depth) + 1] <- subset_df[1, ]$event_total
      duration[length(duration) + 1] <- subset_df[1, ]$duration
      return[length(return) + 1] <- subset_df[1, ]$return_period_LINEAR_INTERPOLATION
      ave_intensity[length(ave_intensity) + 1] <- subset_df[1, ]$mean_intensity_mm.h
      adwp[length(adwp) + 1] <- subset_df[1, ]$ADWP
      pre_event_rain[length(pre_event_rain) + 1] <- subset_df[1, ]$pre_event_rain
    }
    temp_box_df <- data.frame(min_rain_threshold = rep(i, max(df_events$event, na.rm = T)),
                              depth = depth,
                              duration = duration,
                              return = return,
                              ave_intensity = ave_intensity,
                              adwp = adwp,
                              pre_event_rain = pre_event_rain)
    box_df <- rbind(box_df, temp_box_df)
  }
  return(list(box_df, totals_df))
}

# This function allows you to input a range of minimum rainfall thresholds to try,
# and get the resulting data frames of classified events:
rain_threshold <- function(df, rain_min_vec, MIT, df_interval, ddf_table){
  
  df_list <- list()
  
  for (i in rain_min_vec){
    df_events <- rainfall_events_thresh(df, MIT, i)
    df_events <- classify_events_thresh(df_events, df_interval)
    df_events <- return_period_thresh(df_events, ddf_table)
    
    df_list[[length(df_list) + 1]] <- df_events
  }
  
  return(df_list)
}

# Sensitivity Test, Metrics from Stovin et al. (2025):
# Currently assumes that columns of moisture_df are in order according to lysimeter number
# Peak attenuation equation from Spraakman et al. (2020)
sensitivity_perf_metrics <- function(rain_df, moisture_df, outflow_df, mit_vec, df_interval){
  df_out <- data.frame(mit = NA,
                       l3_initial_moisture = NA,
                       l4_initial_moisture = NA,
                       l7_initial_moisture = NA,
                       l8_initial_moisture = NA,
                       l3_peak_att = NA,
                       l4_peak_att = NA,
                       l7_peak_att = NA,
                       l8_peak_att = NA,
                       retention_eff = NA)
  
  for(i in mit_vec){
    df_events <- classify_events(rainfall_events(rain_df, i), df_interval)
    df_events <- return_period(df_events, ddf_table)
    # To determine column indexes of the moisture data:
    df_events_end <- ncol(df_events)
    full_df <- combine_data(list(df_events, moisture_df, outflow_df))
    
    l3_initial_moisture <- c()
    l4_initial_moisture <- c()
    l7_initial_moisture <- c()
    l8_initial_moisture <- c()
    l3_peak_att <- c()
    l4_peak_att <- c()
    l7_peak_att <- c()
    l8_peak_att <- c()
    retention_eff <- c()
    
    for (j in 1:max(full_df$event, na.rm = T)){
      subset_df <- full_df[full_df$event == j & !is.na(full_df$event), ]
      
      if (j == 1){
        View(subset_df)
      }
      
      l3_initial_moisture[length(l3_initial_moisture) + 1] <- subset_df[1, (df_events_end + 1)]
      l4_initial_moisture[length(l4_initial_moisture) + 1] <- subset_df[1, (df_events_end + 2)]
      l7_initial_moisture[length(l7_initial_moisture) + 1] <- subset_df[1, (df_events_end + 3)]
      l8_initial_moisture[length(l8_initial_moisture) + 1] <- subset_df[1, (df_events_end + 4)]
      
      # Convert rainfall to ml:
      subset_df <- convert_mm_ml(subset_df, 2)
      
      event_in <- sum(subset_df[, 2])
      event_out <- sum(subset_df[, (df_events_end + 5)])
      l3_peak_att[length(l3_peak_att) + 1] <- (event_in - event_out) / event_in
      event_in <- sum(subset_df[, 2])
      event_out <- sum(subset_df[, (df_events_end + 6)])
      l4_peak_att[length(l4_peak_att) + 1] <- (event_in - event_out) / event_in
      event_in <- sum(subset_df[, 2])
      event_out <- sum(subset_df[, (df_events_end + 7)])
      l7_peak_att[length(l7_peak_att) + 1] <- (event_in - event_out) / event_in
      event_in <- sum(subset_df[, 2])
      event_out <- sum(subset_df[, (df_events_end + 8)])
      l8_peak_att[length(l8_peak_att) + 1] <- (event_in - event_out) / event_in
    }
    
    temp_df_out <- data.frame(mit = rep(i, max(df_events$event, na.rm = T)),
                              l3_initial_moisture = l3_initial_moisture,
                              l4_initial_moisture = l4_initial_moisture,
                              l7_initial_moisture = l7_initial_moisture,
                              l8_initial_moisture = l8_initial_moisture,
                              l3_peak_att = l3_peak_att,
                              l4_peak_att = l4_peak_att,
                              l7_peak_att = l7_peak_att,
                              l8_peak_att = l8_peak_att,
                              retention_eff = NA)
    df_out <- rbind(df_out, temp_df_out)
  }
  return(df_out)
}


###############################
## Outflow-Related Functions ##
###############################

plot_event_outflows <- function(all_event_df, smooth_out_df, event_num, time_before_after, lysimeter_name){
  # Outflow should be for one lysimeter, and outflow A in column 2, outflow B in
  # column 3.
  
  
  # Select event (time) + X hours before/after
  event_df <- all_event_df[all_event_df$event == event_num & !is.na(all_event_df$event),]
  event_start <- event_df[1, "time"]
  event_end <- event_df[nrow(event_df), "time"]
  plot_period_start <- as.POSIXct(event_start - hours(time_before_after))
  plot_period_end <- as.POSIXct(event_end + hours(time_before_after))
  
  print(event_start)
  print(event_end)
  
  # Select the correct subsets of rainfall and outflow
  event_df <- all_event_df[all_event_df$time >= plot_period_start & all_event_df$time <= plot_period_end, ]
  out_subset <- smooth_out_df[smooth_out_df$time >= plot_period_start & smooth_out_df$time <= plot_period_end, ]
  
  # Plot:
  plot <- plot_ly() %>%
    add_trace(data = event_df,
              x = ~time,
              y = ~event_df[, 2],
              type = "bar",
              name = "Rainfall",
              yaxis = "y1") %>%
    add_lines(data = out_subset,
              x = ~time,
              y = ~out_subset[, 2],
              name = "Restricted Outflow",
              yaxis = "y2",
              line = list(color = "red", dash = "dot"),
              opacity = 1) %>%
    add_lines(data = out_subset,
              x = ~time,
              y = ~out_subset[, 3],
              name = "Unrestricted Outflow",
              yaxis = "y2",
              line = list(color = "green"),
              opacity = 1,
              yaxis = "y1") %>%
    layout(title = glue("Event {event_num} Rainfall and Outflow - {lysimeter_name}"),
           yaxis = list(title = "Rainfall, mm/hour",
                        autorange = "reversed"),
           yaxis2 = list(title = "Outflow, ml/hour",
                         overlaying = "y",
                         side = "right"),
           xaxis = list(title = "Date/Time (Hourly Intervals)"))
  
  return(plot)
}

# Calculating the soil moisture in litres for "full capacity".
# Full capacity VWC = 39%
full_capacity_moisture_L_all_depth <- 1*2*1*1000*0.39
full_capacity_moisture_L_5 <- 1*2*0.075*1000*0.39
full_capacity_moisture_L_10 <- 1*2*0.075*1000*0.39
full_capacity_moisture_L_20 <- 1*2*0.1*1000*0.39
full_capacity_moisture_L_30 <- 1*2*0.1*1000*0.39
full_capacity_moisture_L_40 <- 1*2*0.1*1000*0.39
full_capacity_moisture_L_50 <- 1*2*0.1*1000*0.39
full_capacity_moisture_L_60 <- 1*2*0.125*1000*0.39
full_capacity_moisture_L_75 <- 1*2*0.2*1000*0.39
full_capacity_moisture_L_100 <- 1*2*0.125*1000*0.39

# In this version, I'm just using the hourly data:
outflow_metrics <- function(event_df, smooth_outflow_df, soil_moisture_df, vwc_df, event_num, soil_moisture_col_ind_vec, full_capacity_moisture_L, gf_runoff, extreme_event_vec){
  # event_df should be the data frame with rainfall events identified and
  # classified, i.e. rainfall data that has been passed through the 
  # rainfall_events() and classify_events() functions. Units of rainfall: mm.
  # 
  # smooth_outflow_df should contain data for one lysimeter, with outflow A in
  # column 2 and outflow B in column 3. Units of outflow: ml.
  #
  # soil_moisture_df should contain data for one lysimeter, with all 9 soil
  # soil moisture values for cell A in columns 2:10, and cell B values in
  # columns 11:19. Units soil moisture content: L.
  #
  # full_capacity_moisture_L is the full capacity in litres for the depth of soil moisture chosen
  # gf_runoff should be given in l/h
  
  # Extract indices of soil moisture columns to use
  sm_A <- soil_moisture_col_ind_vec[[1]]
  sm_B <- soil_moisture_col_ind_vec[[2]]
  
  df_out <- data.frame(event_num = event_num,
                       extreme = NA,
                       time_spread = NA,
                       total_rain_L = NA,
                       duration = NA,
                       peak_intensity = NA,
                       peak_rain_time = NA,
                       peak_rain_time_percent = NA,
                       small_event_less_17_L = NA,
                       average_intensity = NA,
                       
                       
                       total_drainage_volume_A = NA,
                       drainage_over_rain_A = NA,
                       total_drainage_volume_A_24 = NA,
                       peak_flow_rate_A = NA,
                       time_to_peak_A = NA,
                       time_to_peak_percent_A = NA,
                       peak_flow_attenuation_A = NA,
                       volume_reduction_A = NA,
                       initial_soil_moisture_A = NA,
                       event_end_soil_moisture_A = NA,
                       event_end_24_soil_moisture_A = NA,
                       peak_moisture_A = NA,
                       change_moist_A = NA,
                       change_moist_24_A = NA,
                       percent_change_moist_start_end_A = NA,
                       percent_change_moist_start_24_A = NA,
                       capacity_reached_at_peak_A = NA,
                       percent_capacity_at_peak_A = NA,
                       gf_runoff_exceeded_A = NA,
                       gf_exceedence_duration_A = NA,
                       gf_percent_reached_at_max_A = NA,
                       peak_vwc_A = NA,
                       
                       
                       total_drainage_volume_B = NA,
                       drainage_over_rain_B = NA,
                       total_drainage_volume_B_24 = NA,
                       peak_flow_rate_B = NA,
                       time_to_peak_B = NA,
                       time_to_peak_percent_B = NA,
                       peak_flow_attenuation_B = NA,
                       volume_reduction_B = NA,
                       initial_soil_moisture_B = NA,
                       event_end_soil_moisture_B = NA,
                       event_end_24_soil_moisture_B = NA,
                       peak_moisture_B = NA,
                       change_moist_B = NA,
                       change_moist_24_B = NA,
                       percent_change_moist_start_end_B = NA,
                       percent_change_moist_start_24_B = NA,
                       capacity_reached_at_peak_B = NA,
                       percent_capacity_at_peak_B = NA,
                       gf_runoff_exceeded_B = NA,
                       gf_exceedence_duration_B = NA,
                       gf_percent_reached_at_max_B = NA,
                       peak_vwc_B = NA,
                       
                       tot_drain_B_over_tot_drain_A = NA)
  
  # Convert rainfall to litres:
  event_df <- convert_mm_ml(event_df, c(2, 7, 8, 9))
  event_df <- convert_ml_l(event_df, c(2, 7, 8, 9))
  rownames(event_df) <- NULL
  
  # Convert outflow to litres:
  smooth_outflow_df <- convert_ml_l(smooth_outflow_df, c(2, 3))
  
  # Obtain the start and end times of the event:
  event_subset <- event_df[!is.na(event_df$event) & event_df$event == event_num, ]
  rownames(event_subset) <- NULL
  
  event_start <- as.POSIXct(event_subset[1, "time"])
  event_end <- as.POSIXct(event_subset[nrow(event_subset), "time"])
  # 2 hours before start of event:
  event_start_2 <- event_start - hours(2)
  # x hours after the event end:
  event_end_24 <- event_end + hours(24)
  event_end_23 <- event_end + hours(23)
  event_end_1 <- event_end + hours(1)
  
  # Select the event period up to 24 hours after, (and from 2 hours before
  # for soil moisture):
  outflow_subset_24 <- smooth_outflow_df[smooth_outflow_df$time >= event_start & smooth_outflow_df$time <= event_end_24, ]
  outflow_subset <- smooth_outflow_df[smooth_outflow_df$time >= event_start & smooth_outflow_df$time <= event_end, ]
  rownames(outflow_subset) <- NULL
  rownames(outflow_subset_24) <- NULL
  
  soil_moisture_subset <- soil_moisture_df[soil_moisture_df$time >= event_start & soil_moisture_df$time <= event_end, ]
  soil_moisture_subset_event_end <- soil_moisture_df[soil_moisture_df$time == event_end_1, ]
  soil_moisture_subset_2_before <- soil_moisture_df[soil_moisture_df$time >= event_start_2 & soil_moisture_df$time < event_start, ]
  soil_moisture_subset_last_hour <- soil_moisture_df[soil_moisture_df$time == event_end_24, ]
  rownames(soil_moisture_subset) <- NULL
  rownames(soil_moisture_subset_event_end) <- NULL
  rownames(soil_moisture_subset_2_before) <- NULL
  rownames(soil_moisture_subset_last_hour) <- NULL
  
  vwc_subset_24 <- vwc_df[vwc_df$time >= event_start & vwc_df$time <= event_end_24, ]
  
  event_subset_24 <- event_df[event_df$time >= event_start & event_df$time <= event_end_24, ]
  rownames(event_subset_24) <- NULL
  
  # Add rainfall event characteristics, convert ml to L:
  df_out$total_rain_L <- event_subset[1, "event_total"]
  df_out$peak_intensity <- event_subset[1, "peak_rainfall"]
  df_out$peak_rain_time <- (which.max(event_subset[, 2])) # in hours since event start
  df_out$time_spread <- time_spread(event_subset)
  df_out$duration <- event_subset[1, "duration"]
  df_out$average_intensity <- event_subset[1, "mean_intensity_mm.h"]
  if (event_num %in% extreme_event_vec){
    df_out$extreme <- TRUE
  }
  else{
    df_out$extreme <- FALSE
  }
  
  if (df_out$total_rain_L < 17){
    df_out$small_event_less_17_L <- TRUE
  }
  else{
    df_out$small_event_less_17_L <- FALSE
  }
  
  # Add outflow event characteristics (convert to L)
  df_out$total_drainage_volume_A <- sum(outflow_subset[, 2])
  df_out$total_drainage_volume_A_24 <- sum(outflow_subset_24[, 2])
  df_out$total_drainage_volume_B <- sum(outflow_subset[, 3])
  df_out$total_drainage_volume_B_24 <- sum(outflow_subset_24[, 3])
  
  df_out$drainage_over_rain_A <- df_out$total_drainage_volume_A / df_out$total_rain_L
  df_out$drainage_over_rain_B <- df_out$total_drainage_volume_B / df_out$total_rain_L
  
  df_out$tot_drain_B_over_tot_drain_A <- (df_out$total_drainage_volume_B / df_out$total_drainage_volume_A)
  
  df_out$peak_flow_rate_A <- (max(outflow_subset_24[, 2]))
  df_out$peak_flow_rate_B <- (max(outflow_subset_24[, 3]))
  
  df_out$time_to_peak_A <- (which.max(outflow_subset_24[, 2])) # in hours since event start
  df_out$time_to_peak_B <- (which.max(outflow_subset_24[, 3])) # in hours since event start
  
  df_out$time_to_peak_percent_A <- df_out$time_to_peak_A / df_out$duration
  df_out$time_to_peak_percent_B <- df_out$time_to_peak_B / df_out$duration
  
  if (any(outflow_subset_24[, 2] > gf_runoff, na.rm = T)){
    df_out$gf_runoff_exceeded_A <- TRUE
    df_out$gf_exceedence_duration_A <- length(which(outflow_subset_24[, 2] > gf_runoff))
    df_out$gf_percent_reached_at_max_A <- (df_out$peak_flow_rate_A) / gf_runoff
  }
  else{
    df_out$gf_runoff_exceeded_A <- FALSE
    df_out$gf_percent_reached_at_max_A <- df_out$peak_flow_rate_A / gf_runoff
  }
  
  if (any(outflow_subset_24[, 3] > gf_runoff, na.rm = T)){
    df_out$gf_runoff_exceeded_B <- TRUE
    df_out$gf_exceedence_duration_B <- length(which(outflow_subset_24[, 3] > gf_runoff))
    df_out$gf_percent_reached_at_max_B <- (df_out$peak_flow_rate_B) / gf_runoff
  }
  else{
    df_out$gf_runoff_exceeded_B <- FALSE
    df_out$gf_percent_reached_at_max_B <- df_out$peak_flow_rate_B / gf_runoff
  }
  
  
  
  # Peak flow att. and volume reduction
  df_out$peak_flow_attenuation_A <- (df_out$peak_intensity - df_out$peak_flow_rate_A)/df_out$peak_intensity
  df_out$peak_flow_attenuation_B <- (df_out$peak_intensity - df_out$peak_flow_rate_B)/df_out$peak_intensity
  df_out$volume_reduction_A <- (df_out$total_rain_L - df_out$total_drainage_volume_A)/df_out$total_rain_L
  df_out$volume_reduction_B <- (df_out$total_rain_L - df_out$total_drainage_volume_B)/df_out$total_rain_L
  
  # Add VWC characteristics:
  df_out$peak_vwc_A <- max(vwc_subset_24[, 2])
  df_out$peak_vwc_B <- max(vwc_subset_24[, 3])
  
  # Add soil moisture event characteristics (already in L)
  df_out$initial_soil_moisture_A <- mean(soil_moisture_subset_2_before[, sm_A], na.rm = T) #Average across period before event
  df_out$initial_soil_moisture_B <- mean(soil_moisture_subset_2_before[, sm_B], na.rm = T) #Average across period before event
  
  df_out$event_end_soil_moisture_A <- mean(soil_moisture_subset_event_end[, sm_A], na.rm = T) # average over the hour after the event end
  df_out$event_end_soil_moisture_B <- mean(soil_moisture_subset_event_end[, sm_B], na.rm = T) # average over the hour after the event end
  
  df_out$event_end_24_soil_moisture_A <- mean(soil_moisture_subset_last_hour[, sm_A], na.rm = T) # average over the last hour
  df_out$event_end_24_soil_moisture_B <- mean(soil_moisture_subset_last_hour[, sm_B], na.rm = T) # average over the last hour
  
  df_out$peak_moisture_A <- max(soil_moisture_subset[, sm_A])
  df_out$peak_moisture_B <- max(soil_moisture_subset[, sm_B])
  
  #df_out$time_to_peak_moist_A <- which.max(soil_moisture_subset[, sm_A])
  #df_out$time_to_peak_moist_B <- which.max(soil_moisture_subset[, sm_B])
  
  df_out$change_moist_A <- df_out$event_end_soil_moisture_A - df_out$initial_soil_moisture_A
  df_out$change_moist_B <- df_out$event_end_soil_moisture_B - df_out$initial_soil_moisture_B
  
  df_out$change_moist_24_A <- df_out$event_end_24_soil_moisture_A - df_out$initial_soil_moisture_A
  df_out$change_moist_24_B <- df_out$event_end_24_soil_moisture_B - df_out$initial_soil_moisture_B
  
  df_out$percent_change_moist_start_end_A <- df_out$change_moist_A / df_out$initial_soil_moisture_A
  df_out$percent_change_moist_start_end_B <- df_out$change_moist_B / df_out$initial_soil_moisture_B
  
  df_out$percent_change_moist_start_24_A <- df_out$change_moist_24_A / df_out$initial_soil_moisture_A
  df_out$percent_change_moist_start_24_B <- df_out$change_moist_24_B / df_out$initial_soil_moisture_B
  
  if (!is.na(df_out$peak_moisture_A) & df_out$peak_moisture_A >= full_capacity_moisture_L){
    df_out$capacity_reached_at_peak_A = TRUE
    df_out$percent_capacity_at_peak_A = df_out$peak_moisture_A / full_capacity_moisture_L
  }
  else if (is.na(df_out$peak_moisture_A)){
    df_out$capacity_reached_at_peak_A = NA
  }
  else{
    df_out$capacity_reached_at_peak_A = FALSE
    df_out$percent_capacity_at_peak_A = df_out$peak_moisture_A / full_capacity_moisture_L
  }
  
  if (!is.na(df_out$peak_moisture_B) & df_out$peak_moisture_B >= full_capacity_moisture_L){
    df_out$capacity_reached_at_peak_B = TRUE
    df_out$percent_capacity_at_peak_B = df_out$peak_moisture_B / full_capacity_moisture_L
  }
  else if (is.na(df_out$peak_moisture_B)){
    df_out$capacity_reached_at_peak_B = NA
  }
  else{
    df_out$capacity_reached_at_peak_B = FALSE
    df_out$percent_capacity_at_peak_B = df_out$peak_moisture_B / full_capacity_moisture_L
  }
  
  return(df_out)
}

outflow_metrics_multiple <- function(event_df, smooth_outflow_df, soil_moisture_df, vwc_df, event_num_vec, soil_moisture_col_ind, out_thresh, out_thresh_small, full_capacity_moisture_L, gf_runoff, extreme_event_vec, AB_ratio_thresh){
  
  df_out <- data.frame(event_num = NA,
                       extreme = NA,
                       time_spread = NA,
                       total_rain_L = NA,
                       duration = NA,
                       peak_intensity = NA,
                       peak_rain_time = NA,
                       peak_rain_time_percent = NA,
                       small_event_less_17_L = NA,
                       average_intensity = NA,
                       
                       
                       total_drainage_volume_A = NA,
                       drainage_over_rain_A = NA,
                       total_drainage_volume_A_24 = NA,
                       peak_flow_rate_A = NA,
                       time_to_peak_A = NA,
                       time_to_peak_percent_A = NA,
                       peak_flow_attenuation_A = NA,
                       volume_reduction_A = NA,
                       initial_soil_moisture_A = NA,
                       event_end_soil_moisture_A = NA,
                       event_end_24_soil_moisture_A = NA,
                       peak_moisture_A = NA,
                       change_moist_A = NA,
                       change_moist_24_A = NA,
                       percent_change_moist_start_end_A = NA,
                       percent_change_moist_start_24_A = NA,
                       capacity_reached_at_peak_A = NA,
                       percent_capacity_at_peak_A = NA,
                       gf_runoff_exceeded_A = NA,
                       gf_exceedence_duration_A = NA,
                       gf_percent_reached_at_max_A = NA,
                       peak_vwc_A = NA,
                       
                       
                       total_drainage_volume_B = NA,
                       drainage_over_rain_B = NA,
                       total_drainage_volume_B_24 = NA,
                       peak_flow_rate_B = NA,
                       time_to_peak_B = NA,
                       time_to_peak_percent_B = NA,
                       peak_flow_attenuation_B = NA,
                       volume_reduction_B = NA,
                       initial_soil_moisture_B = NA,
                       event_end_soil_moisture_B = NA,
                       event_end_24_soil_moisture_B = NA,
                       peak_moisture_B = NA,
                       change_moist_B = NA,
                       change_moist_24_B = NA,
                       percent_change_moist_start_end_B = NA,
                       percent_change_moist_start_24_B = NA,
                       capacity_reached_at_peak_B = NA,
                       percent_capacity_at_peak_B = NA,
                       gf_runoff_exceeded_B = NA,
                       gf_exceedence_duration_B = NA,
                       gf_percent_reached_at_max_B = NA,
                       peak_vwc_B = NA,
                       
                       tot_drain_B_over_tot_drain_A = NA)
  
  # Keep track of the number of events excluded:
  num_excl <- 0
  
  for (i in event_num_vec){
    temp_df <- outflow_metrics(event_df, smooth_outflow_df, soil_moisture_df, vwc_df, i, soil_moisture_col_ind, full_capacity_moisture_L, gf_runoff, extreme_event_vec)
    
    if (temp_df$total_drainage_volume_A > temp_df$total_drainage_volume_B){
      print(glue("Skipping Event Number: {i} (total drainage volume (by event end) for restricted greater than unrestricted)"))
      print(glue("Event Total Rain: {temp_df$total_rain_L} litres"))
      print(glue("Total outflow A: {temp_df$total_drainage_volume_A} litres"))
      print(glue("Total outflow B: {temp_df$total_drainage_volume_B} litres"))
      
      num_excl <- num_excl + 1
      next
    }
    
    if (temp_df$tot_drain_B_over_tot_drain_A > AB_ratio_thresh){
      print(glue("Skipping Event Number: {i} (outflow B / outflow A > {AB_ratio_thresh})"))
      print(glue("Event Total Rain: {temp_df$total_rain_L} litres"))
      print(glue("Total outflow A: {temp_df$total_drainage_volume_A} litres"))
      print(glue("Total outflow B: {temp_df$total_drainage_volume_B} litres"))
      print(glue("Ratio: {temp_df$tot_drain_B_over_tot_drain_A}"))
      
      num_excl <- num_excl + 1
      next
    }
    
    if (temp_df$total_drainage_volume_A_24 > temp_df$total_rain_L | temp_df$total_drainage_volume_B_24 > temp_df$total_rain_L){
      print(glue("Skipping Event Number: {i} (total drainage volume (up to 24 after event) greater than event depth)"))
      print(glue("Event Total Rain: {temp_df$total_rain_L} litres"))
      print(glue("Total outflow A: {temp_df$total_drainage_volume_A} litres"))
      print(glue("Total outflow B: {temp_df$total_drainage_volume_B} litres"))
      print(glue("Volume reduction, outflow A: {temp_df$volume_reduction_A}"))
      print(glue("Volume reduction, outflow B: {temp_df$volume_reduction_B} \n\n"))
      
      
      num_excl <- num_excl + 1
      next
    }
    
    #    if (temp_df$small_event_less_15_L == T){
    #      if ((temp_df$total_drainage_volume_A_24/temp_df$total_rain_L) < out_thresh_small | (temp_df$total_drainage_volume_A_24/temp_df$total_rain_L) < out_thresh_small){
    #        print(glue("Skipping Event Number: {i} (total drainage volume 24 hours after event considerably lower than event depth)"))
    #        print(glue("Volume reduction, outflow A: {temp_df$volume_reduction_A}"))
    #        print(glue("Volume reduction, outflow B: {temp_df$volume_reduction_B}"))
    #        print(glue("Event Total Rain: {temp_df$total_rain_L} litres"))
    #        print(glue("Total outflow A: {temp_df$total_drainage_volume_A} litres"))
    #        print(glue("Total outflow B: {temp_df$total_drainage_volume_B} litres \n\n"))
    #        
    #        num_excl <- num_excl + 1
    #        next
    #      }
    #    }
    #    else{
    #      if ((temp_df$total_drainage_volume_A_24/temp_df$total_rain_L) < out_thresh | (temp_df$total_drainage_volume_A_24/temp_df$total_rain_L) < out_thresh){
    #        print(glue("Skipping Event Number: {i} (total drainage volume 24 hours after event considerably lower than event depth)"))
    #        print(glue("Volume reduction, outflow A: {temp_df$volume_reduction_A}"))
    #        print(glue("Volume reduction, outflow B: {temp_df$volume_reduction_B}"))
    #        print(glue("Event Total Rain: {temp_df$total_rain_L} litres"))
    #        print(glue("Total outflow A: {temp_df$total_drainage_volume_A} litres"))
    #        print(glue("Total outflow B: {temp_df$total_drainage_volume_B} litres \n\n"))
    #        
    #        num_excl <- num_excl + 1
    #        next
    #      }
    #    }
    
    df_out <- rbind(df_out, temp_df)
  }
  
  print(glue("{num_excl} of {max(event_df$event, na.rm = T)} events excluded."))
  
  return(df_out)
}

# Precipitation time based on Bou Lahdou et al. (2019):
time_spread <- function(single_event_df){
  
  rownames(single_event_df) <- NULL
  
  sum_tp <- 0
  sum_p <- 0
  
  t_vec <- c()
  p_vec <- c()
  
  for (i in 1:nrow(single_event_df)){
    
    p <- sum(single_event_df[c(1:i), 2])
    tp <- i*p
    
    sum_tp <- sum_tp + tp
    sum_p <- sum_p + p
    
    t_vec[length(t_vec) + 1] <- i
    p_vec[length(p_vec) + 1] <- p
  }
  
  t_bar <- sum_tp/sum_p
  
  ts_numerator <- 0
  
  for (i in 1:length(p_vec)){
    ts_num_i <- ((t_vec[i] - t_bar)^2)*p_vec[i]
    
    ts_numerator <- ts_numerator + ts_num_i
  }
  
  time_spread <- sqrt(ts_numerator / sum_p)
  
  #return(single_event_df)
  return(time_spread)
}


############################
## Mass Balance Functions ##
############################

# Without calibration
mass_balance_any_period <- function(df_in, sm_col_vec, out_col){
  # df_in should contain:
  # Rainfall event data with rainfall in litres in column 2.
  # Soil moisture content for all depths in litres
  # Outflow data for the unrestricted half in litres
  
  df_out <- df_in
  
  # Calculate the total moisture content in the lysimeter in each interval:
  df_out$total_moisture <- NA
  
  for (i in 1:nrow(df_out)){
    total_moisture <- 0
    for (j in sm_col_vec){
      total_moisture <- total_moisture + df_out[i, j]
    }
    df_out[i, "total_moisture"] <- total_moisture
  }
  
  # Calculate cumulative rainfall and outflow across the period:
  df_out$cum_rain <- cumsum(df_out[, 2])
  df_out$cum_out <- cumsum(df_out[, out_col])
  
  # Calculate the change in moisture since the beginning:
  for (i in 1:nrow(df_out)){
    df_out[i, "rel_sm"] <- df_out[i, "total_moisture"] - df_out[1, "total_moisture"]
  }
  
  # Calculate the unaccounted-for water:
  # Inflow = change sm + outflow + ET
  # ET = inflow - change sm - outflow
  for (i in 1:nrow(df_out)){
    df_out[i, "cum_et"] <- df_out[i, "cum_rain"] - df_out[i, "rel_sm"] - df_out[i, "cum_out"]
  }
  
  return(df_out)
}

# Plot Mass Balance:
plot_mass_balance <- function(df_in){
  # df_in should be the output of the mass_balance_any_period with the 
  # column names used by that function
  
  plot <- plot_ly(data = df_in,
                  x = ~time) %>%
    add_lines(y = ~cum_rain,
              name = "Cumulative Rainfall") %>%
    add_lines(y = ~cum_out,
              name = "Cumulative Outflow") %>%
    add_lines(y = ~rel_sm,
              name = "Change in Soil Moisture from Plot Period Start") %>%
    add_lines(y = ~rel_sm_scaled,
              name = "Change in Soil Moisture from Plot Period Start (Calibrated)") %>%
    add_lines(y = ~cum_et,
              name = "Cumulative ET Estimated Through Mass Balance \n(Calibrated Soil Moisture Used)")
  
  return(plot)
}

# With calibration option
mass_balance_any_period_cal <- function(df_in, sm_col_vec, out_col, calibration_multiplier){
  # df_in should contain:
  # Rainfall event data with rainfall in litres in column 2.
  # Soil moisture content for all depths in litres
  # Outflow data for the unrestricted half in litres
  
  df_out <- df_in
  
  # Calculate the total moisture content in the lysimeter in each interval:
  df_out$total_moisture <- NA
  df_out$total_moisture_soil_only <- NA
  
  for (i in 1:nrow(df_out)){
    total_moisture <- 0
    for (j in sm_col_vec){
      total_moisture <- total_moisture + df_out[i, j]
    }
    df_out[i, "total_moisture"] <- total_moisture
  }
  
  for (i in 1:nrow(df_out)){
    total_moisture_soil_only <- 0
    for (j in sm_col_vec[c(1:7)]){
      total_moisture_soil_only <- total_moisture_soil_only + df_out[i, j]
    }
    df_out[i, "total_moisture_soil_only"] <- total_moisture_soil_only
  }
  
  # Calculate cumulative rainfall and outflow across the period:
  df_out$cum_rain <- cumsum(df_out[, 2])
  df_out$cum_out <- cumsum(df_out[, out_col])
  
  # Calculate the change in moisture since the beginning:
  for (i in 1:nrow(df_out)){
    df_out[i, "rel_sm"] <- df_out[i, "total_moisture"] - df_out[1, "total_moisture"]
  }
  
  # Scale the change in soil moisture:
  df_out$rel_sm_scaled <- df_out$rel_sm / calibration_multiplier
  
  # Calculate the unaccounted-for water:
  # Inflow = change sm + outflow + ET
  # ET = inflow - change sm - outflow
  for (i in 1:nrow(df_out)){
    df_out[i, "cum_et"] <- df_out[i, "cum_rain"] - df_out[i, "rel_sm_scaled"] - df_out[i, "cum_out"]
  }
  
  return(df_out)
}

calibration_scaling <- function(df_in, event_lysimeter){
  # df_in should be one of the data frames from the file
  # "RO3 - Calibration of Soil Moisture (EDA)" and subsetted as in the file
  # "RO3 - Calibration of Soil Moisture (Scaling)" (Method 1)
  # with names of the form evX_mb_calibration_X or evX_mb_calibration
  # event_lysimeter should be a string containing event and lysimeter number
  
  df_out <- data.frame(event_lysimeter = rep(event_lysimeter, nrow(df_in)))
  df_in$calibration_diff <- NA
  for (i in 1:nrow(df_in)){
    df_in[i, "calibration_diff"] <- df_in[i, "rel_sm"] - df_in[i, "in_minus_out"]
  }
  
  df_in$calibration_multiplier <- NA
  for (i in 1:nrow(df_in)){
    df_in[i, "calibration_multiplier"] <- df_in[i, "rel_sm"] / df_in[i, "in_minus_out"]
  }
  
  df_out$calibration_multiplier <- df_in$calibration_multiplier
  df_out$calibration_diff <- df_in$calibration_diff
  
  return(df_out)
}

calibration_scaling2 <- function(df_in, event_lysimeter){
  # df_in should be one of the data frames from the file
  # "RO3 - Calibration of Soil Moisture (EDA)" and subsetted as in the file
  # "RO3 - Calibration of Soil Moisture (Scaling)" (Method 2)
  # with names of the form evX_mb_calibration_X or evX_mb_calibration
  # event_lysimeter should be a string containing event and lysimeter number
  
  df_out <- data.frame(event_lysimeter = event_lysimeter)
  
  # Set row names of input data frame to NULL:
  rownames(df_in) <- NULL
  
  # Calculate the increase in measured soil moisture from the beginning of the 
  # period to the end:
  # Average over the three hours preceding the intense portion of the event:
  ave_change_sm_start <- mean(df_in[c(1:3), "rel_sm"])
  change_sm_end <- df_in[nrow(df_in), "rel_sm"]
  measured_sm_diff <- change_sm_end - ave_change_sm_start
  
  df_out$measured_sm_diff <- measured_sm_diff
  
  # Calculate the increase in (inflow - outflow) from the beginning of the 
  # period to the end:
  # Average over the three hours preceding the intense portion of the event:
  ave_change_in_out_start <- mean(df_in[c(1:3), "in_minus_out"])
  change_in_out_end <- df_in[nrow(df_in), "in_minus_out"]
  in_out_diff <- change_in_out_end - ave_change_in_out_start
  
  df_out$in_out_diff <- in_out_diff
  
  df_out$calibration_diff <- measured_sm_diff - in_out_diff
  
  df_out$calibration_multiplier <- measured_sm_diff / in_out_diff
  
  return(df_out)
}

# Function to calculate total soil moisture in two halves of the soil:
soil_moisture_halves <- function(df_in, top_half_end, sm_col_vec, calibration_multiplier){
  # df_in should contain all soil moisture data for at least all depths from
  # 5 cm down to and including 60 cm, in order.
  # sm_col_vec gives the column indices of each depth, in order.
  # top_half_end should be the last depth to be included in the top half.
  
  if (top_half_end == 5){
    rel_ind <- 1
  }
  else if (top_half_end == 10){
    rel_ind <- 2
  }
  else if (top_half_end == 20){
    rel_ind <- 3
  }
  else if (top_half_end == 30){
    rel_ind <- 4
  }
  else if (top_half_end == 40){
    rel_ind <- 5
  }
  else if (top_half_end == 50){
    rel_ind <- 6
  }
  else if (top_half_end == 60){
    rel_ind <- 7
  }
  
  top_half_end_ind <- sm_col_vec[[rel_ind]]
  
  df_out <- df_in
  
  for (i in 1:nrow(df_in)){
    df_out[i, "tot_sm_top"] <- sum(df_in[i, sm_col_vec[[1]]:top_half_end_ind])
    df_out[i, "tot_sm_bottom"] <- sum(df_in[i, (top_half_end_ind + 1):sm_col_vec[[7]]])
  }
  
  for (i in 1:nrow(df_in)){
    df_out[i, "change_sm_top"] <- (df_out[i, "tot_sm_top"] - df_out[1, "tot_sm_top"]) / calibration_multiplier
    df_out[i, "change_sm_bottom"] <- (df_out[i, "tot_sm_bottom"] - df_out[1, "tot_sm_bottom"]) / calibration_multiplier
    df_out[i, "change_sm_soil_only"] <- (df_out[i, "total_moisture_soil_only"] - df_out[1, "total_moisture_soil_only"]) / calibration_multiplier
  }
  
  #for (i in 1:nrow(df_in)){
  #  df_out[i, "percent_change_due_to_top"] <- abs(df_out[i, "change_sm_top"] / df_out[i, "change_sm_soil_only"])
  #  df_out[i, "percent_change_due_to_bottom"] <- abs(df_out[i, "change_sm_bottom"] / df_out[i, "change_sm_soil_only"])
  #}
  
  for (i in 1:nrow(df_in)){
    df_out[i, "percent_change_due_to_top"] <- (df_out[i, "change_sm_top"]) / ((df_out[i, "change_sm_top"]) + (df_out[i, "change_sm_bottom"]))
    df_out[i, "percent_change_due_to_bottom"] <- (df_out[i, "change_sm_bottom"]) / ((df_out[i, "change_sm_top"]) + (df_out[i, "change_sm_bottom"]))
  }
  
  for (i in 1:nrow(df_in)){
    df_out[i, "percent_sm_in_top"] <- df_out[i, "tot_sm_top"] / df_out[i, "total_moisture_soil_only"]
    df_out[i, "percent_sm_in_bottom"] <- df_out[i, "tot_sm_bottom"] / df_out[i, "total_moisture_soil_only"]
  }
  
  return(df_out)
}

