test_interpolation <- function(moisture_df, gaps_to_try_vec, event_df){
  
  df_out <- data.frame(gap = NA,
                       error = NA)
  
  # Select all rows of the moisture_df that fall within an event:
  event_times <- as.vector(event_df[!is.na(event_df$event), ]$time)
  print(length(event_times))
  moisture_df_subset <- moisture_df[moisture_df$time %in% as.POSIXct(event_times), ]
  
  moisture_df_subset <- moisture_df_subset[complete.cases(moisture_df_subset), ]
  rownames(moisture_df_subset) <- NULL
  
  for (i in 1:10000){
    
    temp_df <- moisture_df_subset
    
    # Choose a random row:
    ind <- sample(1:(nrow(temp_df) - max(gaps_to_try_vec)), 1)
    
    for (j in gaps_to_try_vec){
      
      # Create a gap of length j:
      ind_range <- c(ind:(ind + j - 1))
      temp_df[ind_range, 2] <- rep(NA, j)
      
      # Interpolate these values (linear):
      moist_col <- temp_df[[2]]
      moist_col_int <- na.approx(moist_col)
      
      # Exclude this range of values if they are too small:
      if (sum(moisture_df_subset[[2]][ind_range]) < 0.1){
        next
      }
      
      # Calculate the error for the interpolated values:
      error <- sum(abs(moist_col_int[ind_range] - moisture_df_subset[[2]][ind_range]))/sum(moisture_df_subset[[2]][ind_range])
      
      # Add error to df_out
      temp_df_out <- data.frame(gap = j,
                                error = error)
      df_out <- rbind(df_out, temp_df_out)
    }
  }
  return(df_out)
}


testing_interpolation <- test_interpolation(l3_moist10_B_HR, c(1:20), rain60_events)

testing_interpolation_means <- aggregate(x = testing_interpolation$error, by = list(testing_interpolation$gap), FUN = mean, na.rm = T)
testing_interpolation_medians <- aggregate(x = testing_interpolation$error, by = list(testing_interpolation$gap), FUN = median, na.rm = T)


plot_ly(data = testing_interpolation_means,
        x = ~Group.1,
        y = ~x) %>%
  layout(xaxis = list(title = "Length of Gap Interpolated Across, h"),
         yaxis = list(title = "Mean Percentage Error"))

plot_ly(data = testing_interpolation_medians,
        x = ~Group.1,
        y = ~x) %>%
  layout(xaxis = list(title = "Length of Gap Interpolated Across, h"),
         yaxis = list(title = "Median Percentage Error"))


plot_ly(data = testing_interpolation) %>%
  add_boxplot(x = ~gap,
              y = ~error)


test_int_df <- data.frame(x = sort(rnorm(100, mean=50, sd=20)),
                          y = sort(rnorm(100, mean=50, sd=20)),
                          z = sort(rnorm(100, mean=50, sd=20)))

for (i in seq_along(test_int_df[,-1])+1) {
  is.na(test_int_df[sample(seq_len(100), floor(100/10)),i]) <- TRUE
}

test_int_df_res <- interpolate_moisture(test_int_df, c(1, 2), 2)