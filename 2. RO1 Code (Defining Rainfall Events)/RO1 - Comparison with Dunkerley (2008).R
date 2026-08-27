# PLOTTING GRAPHS OF HOW EVENT CHARACTERISTICS CHANGE WITH MIT:

# Loading the processed data:

mit_comp_MDS <- read.csv("1. General Setup Code (Loading Data, All Functions)\\2. Processed Data\\MDS Project MIT Sensitivity Percentages.csv")
# The following data is taken and adapted from Dunkerley (2008):
mit_comp_Dunkerley <- read.csv("1. General Setup Code (Loading Data, All Functions)\\2. Processed Data\\Dunkerley 2008 MIT Sensitivity Percentages.csv")


plot_ly() %>%
  add_lines(data = mit_comp_MDS,
            x = ~MIT,
            y = ~num_events,
            name = "Present Study",
            line = list(color = "steelblue", dash = "solid")) %>%
  add_lines(data = mit_comp_Dunkerley,
            x = ~MIT,
            y = ~num_events,
            name = "Dunkerley (2008)",
            line = list(color = "orange", dash = "dash")) %>%
  layout(title = "Comparing Change in Number of Events, Present Study and Dunkerley (2008)",
         xaxis = list(title = "MIT Value, hours",
                      tickvals = c(1, 2, 3, 6, 12, 24)),
         yaxis = list(title = "Number of Events as Percentage of \n Number of Events When MIT = 1 h, %"))

plot_ly() %>%
  add_lines(data = mit_comp_MDS,
            x = ~MIT,
            y = ~mean_event_duration,
            name = "Present Study",
            line = list(color = "steelblue", dash = "solid")) %>%
  add_lines(data = mit_comp_Dunkerley,
            x = ~MIT,
            y = ~mean_event_duration,
            name = "Dunkerley (2008)",
            line = list(color = "orange", dash = "dash")) %>%
  layout(title = "Comparing Change in (Geometric) Mean Event Duration, Present Study and Dunkerley (2008)",
         xaxis = list(title = "MIT Value, hours",
                      tickvals = c(1, 2, 3, 6, 12, 24)),
         yaxis = list(title = "(Geometric) Mean Event Duration as Percentage of \n That Value When MIT = 1 h, %"))


plot_ly() %>%
  add_lines(data = mit_comp_MDS,
            x = ~MIT,
            y = ~mean_event_depth,
            name = "Present Study",
            line = list(color = "steelblue", dash = "solid")) %>%
  add_lines(data = mit_comp_Dunkerley,
            x = ~MIT,
            y = ~mean_event_depth,
            name = "Dunkerley (2008)",
            line = list(color = "orange", dash = "dash")) %>%
  layout(title = "Comparing Change in (Geometric) Mean Event Depth, Present Study and Dunkerley (2008)",
         xaxis = list(title = "MIT Value, hours",
                      tickvals = c(1, 2, 3, 6, 12, 24)),
         yaxis = list(title = "(Geometric) Mean Event Depth as Percentage of \n That Value When MIT = 1 h, %"))


plot_ly() %>%
  add_lines(data = mit_comp_MDS,
            x = ~MIT,
            y = ~mean_event_rain_rate,
            name = "Present Study",
            line = list(color = "steelblue", dash = "solid")) %>%
  add_lines(data = mit_comp_Dunkerley,
            x = ~MIT,
            y = ~mean_event_rain_rate,
            name = "Dunkerley (2008)",
            line = list(color = "orange", dash = "dash")) %>%
  layout(title = "Comparing Change in (Geometric) Mean Event Intensity, Present Study and Dunkerley (2008)",
         xaxis = list(title = "MIT Value, hours",
                      tickvals = c(1, 2, 3, 6, 12, 24)),
         yaxis = list(title = "(Geometric) Mean Event Intensity as Percentage of \n That Value When MIT = 1 h, %"))



plot_ly() %>%
  add_lines(data = mit_comp_MDS,
            x = ~MIT,
            y = ~mean_gap_between_events,
            name = "Present Study",
            line = list(color = "steelblue", dash = "solid")) %>%
  add_lines(data = mit_comp_Dunkerley,
            x = ~MIT,
            y = ~mean_gap_between_events,
            name = "Dunkerley (2008)",
            line = list(color = "orange", dash = "dash")) %>%
  layout(title = "Comparing Change in (Geometric) Mean Gap Between Events, Present Study and Dunkerley (2008)",
         xaxis = list(title = "MIT Value, hours",
                      tickvals = c(1, 2, 3, 6, 12, 24)),
         yaxis = list(title = "(Geometric) Mean Gap Between Events as Percentage of \n That Value When MIT = 1 h, %"))




# Subplot versions:

plot_num_ev <- plot_ly() %>%
  add_lines(data = mit_comp_MDS,
            x = ~MIT,
            y = ~num_events,
            name = "Present Study",
            line = list(color = "steelblue", dash = "solid"),
            legendgroup = "1",
            showlegend = T) %>%
  add_lines(data = mit_comp_Dunkerley,
            x = ~MIT,
            y = ~num_events,
            name = "Dunkerley (2008)",
            line = list(color = "orange", dash = "dash"),
            legendgroup = "2",
            showlegend = T)%>%
  layout(xaxis = list(title = "MIT Value, hours",
                      tickvals = c(1, 2, 3, 6, 12, 24)),
         yaxis = list(title = "Number of Events, %"))

plot_duration <- plot_ly() %>%
  add_lines(data = mit_comp_MDS,
            x = ~MIT,
            y = ~mean_event_duration,
            name = "Present Study",
            line = list(color = "steelblue", dash = "solid"),
            legendgroup = "1",
            showlegend = F) %>%
  add_lines(data = mit_comp_Dunkerley,
            x = ~MIT,
            y = ~mean_event_duration,
            name = "Dunkerley (2008)",
            line = list(color = "orange", dash = "dash"),
            legendgroup = "2",
            showlegend = F) %>%
  layout(xaxis = list(title = "MIT Value, hours",
                      tickvals = c(1, 2, 3, 6, 12, 24)),
         yaxis = list(title = "Mean Event Duration, %"),
         legendgroup = "1",
         showlegend = F)


plot_depth <- plot_ly() %>%
  add_lines(data = mit_comp_MDS,
            x = ~MIT,
            y = ~mean_event_depth,
            name = "Present Study",
            line = list(color = "steelblue", dash = "solid"),
            legendgroup = "1",
            showlegend = F) %>%
  add_lines(data = mit_comp_Dunkerley,
            x = ~MIT,
            y = ~mean_event_depth,
            name = "Dunkerley (2008)",
            line = list(color = "orange", dash = "dash"),
            legendgroup = "2",
            showlegend = F) %>%
  layout(xaxis = list(title = "MIT Value, hours",
                      tickvals = c(1, 2, 3, 6, 12, 24)),
         yaxis = list(title = "Mean Event Depth, %"),
         legendgroup = "1",
         showlegend = F)


plot_intensity <- plot_ly() %>%
  add_lines(data = mit_comp_MDS,
            x = ~MIT,
            y = ~mean_event_rain_rate,
            name = "Present Study",
            line = list(color = "steelblue", dash = "solid"),
            legendgroup = "1",
            showlegend = F) %>%
  add_lines(data = mit_comp_Dunkerley,
            x = ~MIT,
            y = ~mean_event_rain_rate,
            name = "Dunkerley (2008)",
            line = list(color = "orange", dash = "dash"),
            legendgroup = "2",
            showlegend = F) %>%
  layout(xaxis = list(title = "MIT Value, hours",
                      tickvals = c(1, 2, 3, 6, 12, 24)),
         yaxis = list(title = "Mean Event Intensity, %"),
         legendgroup = "1",
         showlegend = F)



plot_adwp <- plot_ly() %>%
  add_lines(data = mit_comp_MDS,
            x = ~MIT,
            y = ~mean_gap_between_events,
            name = "Present Study",
            line = list(color = "steelblue", dash = "solid"),
            legendgroup = "1",
            showlegend = F) %>%
  add_lines(data = mit_comp_Dunkerley,
            x = ~MIT,
            y = ~mean_gap_between_events,
            name = "Dunkerley (2008)",
            line = list(color = "orange", dash = "dash"),
            legendgroup = "2",
            showlegend = F) %>%
  layout(xaxis = list(title = "MIT Value, hours",
                      tickvals = c(1, 2, 3, 6, 12, 24)),
         yaxis = list(title = "Mean Gap Between Events, %"),
         legendgroup = "1",
         showlegend = F)


subplot(plot_num_ev, plot_duration, plot_depth, plot_intensity, plot_adwp,
        shareX = T, nrows = 2, titleY = T) %>%
  layout(legend = list(x = 0.7,
                       y = 0.15),
         showlegend = T)



