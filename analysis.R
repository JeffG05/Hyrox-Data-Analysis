library(tidyverse)
library(lubridate)
library(countrycode)
library(geosphere)

# Import cities data
city_country <- read_csv("cities.csv")

# Import and clean hyrox results dataset
# Downloaded from https://www.kaggle.com/datasets/jgug05/hyrox-results
data <- read_csv("hyrox_results.csv")
data <- data %>%
  mutate(
    gender = as.factor(gender),
    age_group = as.factor(age_group),
    nationality = as.factor(nationality),
    division = as.factor(str_to_title(str_replace_all(division, "_", " "))),
    athletes = ifelse(str_detect(division, "Doubles"), 2, ifelse(str_detect(division, "Relay"), 4, 1)),
    season = str_split(event_name, " ", simplify = TRUE)[, 1] %>% str_replace("S", "") %>% as.integer(),
    year = str_split(event_name, " ", simplify = TRUE)[, 2] %>% as.integer(),
    city = str_split(str_split(event_name, " ", 3, simplify = TRUE)[, 3], " - ", simplify = TRUE)[, 1]
  ) %>%
  mutate_if(is.double, as.duration) %>%
  filter(total_time > 0)

# Create custom ggplot theme
theme_hyrox <- function () {
  theme_minimal() %+replace%
    theme(
      text = element_text(family = "mono", color = "white"),
      axis.text = element_text(color = "white", size = 10),
      plot.background = element_rect(fill = "black"),
      plot.margin = margin(r=30,b=15, l=15),
      legend.margin = margin(l = 15, r = 30),
      panel.background = element_rect(fill = "black"),
      panel.grid = element_line(color = "grey"),
      strip.text = element_text(color = "white", size = 10),
      strip.text.y.left = element_text(angle = 0, hjust = 1, margin = margin(r=20)),
      panel.spacing.y = unit(0, "cm"),
      plot.title = element_text(size = 20, face = "bold", margin = margin(t = 20, b = 15)),
      plot.subtitle = element_text(size = 14, margin = margin(b = 30)),
      axis.title.x.bottom = element_text(margin = margin(t=10)),
      axis.title.y.left = element_text(margin = margin(r=10))
    )
}

# Helper function to convert durations into strings
format_duration <- function(x) {
  td <- seconds_to_period(x)
  ifelse(
    td < hours(),
    sprintf("%02d:%02d", minute(td), round(second(td))),
    sprintf("%02d:%02d:%02d", hour(td), minute(td), round(second(td)))
  )
}

# Code for overview stats plot
overview_stats_plot <- function () {

  # Calculate number of events
  events <- data$event_id %>%
    unique() %>%
    length() %>%
    format(big.mark = ",")

  # Calculate number of race finishers
  participants <- data$athletes %>%
    sum() %>%
    format(big.mark = ",")

  # Calculate number of cities
  cities <- data$city %>%
    unique() %>%
    length() %>%
    format(big.mark = ",")

  # Draw plot
  ggplot() +
    # Draw lines
    annotate("segment", x = 0, xend = 0, y = 0, yend = 1, color = "black") +
    annotate("segment", x = -1, xend = 1, y = 0, yend = 0, color = "white") +
    annotate("segment", x = 0, xend = 0, y = 0, yend = -1, color = "white") +

    # Draw text
    annotate("text", x = 0, y = 0.6, label = participants, family = "mono", color = "yellow", size = 12,) +
    annotate("text", x = 0, y = 0.4, label = "Race Finishers", family = "mono", color = "white", size = 6) +

    annotate("text", x = -0.5, y = -0.4, label = events, family = "mono", color = "yellow", size = 12) +
    annotate("text", x = -0.5, y = -0.6, label = "Events", family = "mono", color = "white", size = 6) +

    annotate("text", x = 0.5, y = -0.4, label = cities, family = "mono", color = "yellow", size = 12) +
    annotate("text", x = 0.5, y = -0.6, label = "Cities", family = "mono", color = "white", size = 6) +

    # Add custom theme
    theme_hyrox() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.title.y.left = element_blank(),
      axis.title.x.bottom = element_blank(),
      plot.margin = margin(l = -40, r = -40, b = -20)
    )
}

# Code for event city location plot
# c parameter specifies the continent that should be zoomed to
#  Options: ('World', 'Europe', 'North America', 'Asia')
location_plot <- function (c) {

  # Get relevant data for each city
  plot_data <- data %>%
    group_by(city) %>%
    summarise(
      events = cur_data()$event_id %>% unique() %>% length(),
      continent = countrycode(cur_group()$city, "city", "continent", custom_dict = city_country),
      lat = countrycode(cur_group()$city, "city", "lat", custom_dict = city_country),
      lng = countrycode(cur_group()$city, "city", "lng", custom_dict = city_country),
      alpha = ifelse(continent == c, 1, 0)
    )

  # Draw plot
  plot_data %>%
    ggplot() +
      # Draw base map
      geom_polygon(aes(x=long, y = lat, group = group), data = map_data("world"), fill="#262626", color = "black", linewidth = 0.1) +

      # Draw city markers
      geom_point(if (c == "World") aes(x = lng, y = lat, size = events) else aes(x = lng, y = lat, size = events, alpha = alpha), color = "yellow") +

      # Update map zoom based on continent parameter
      coord_cartesian(
        xlim = if(c == "Europe") c(-20, 50) else (if (c == "North America") c(-165, -55) else (if (c == "Asia") c(20, 180) else c(-180, 180))),
        ylim = if(c == "Europe") c(35, 70) else (if (c == "North America") c(15, 70) else (if (c == "Asia") c(-10, 70) else c(-90, 90)))
      ) +

      # Define the size of city markers
      scale_size_continuous(
        name = "Events",
        breaks = 1:max(plot_data$events),
        range = if (c == "World") c(0.1, 0.8) else c(1, 5)
      ) +

      # Enable marker alphas to be set to 0
      scale_alpha(range = c(0, 1), guide = "none") +

      # Add custom theme
      theme_hyrox() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title.x.bottom = element_blank(),
        axis.title.y.left = element_blank(),
        plot.margin = margin(l=0, b=0)
      ) +

      # Add labels
      labs(
        title = "Hyrox Locations",
        subtitle = c
      )
}

# Code for city popularity plot
popularity_plot <- function () {
  data %>%

    # Calculate city population
    group_by(city) %>%
    summarise(
      athletes = sum(athletes),
      events = cur_data()$event_id %>% unique() %>% length(),
      athlete_per_event = athletes / events
    ) %>%

    # Draw plot
    ggplot(aes(reorder(city, athlete_per_event), athlete_per_event)) +
      # Draw bars
      geom_col(width = 0.1, fill = "white") +

      # Draw lollipop head
      geom_point(size=4, color="yellow") +

      # Make bars go horizontally
      coord_flip() +

      # Add custom theme
      theme_hyrox() +
      theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
      ) +

      # Add plot labels
      labs(
        x = "Event City",
        y = "Average Athletes per Event",
        title = "City Popularity"
      )
}

# Code for travel plot
travel_map <- function () {

  # Download lat lon for each country
  country_data <- read_csv("https://raw.githubusercontent.com/albertyw/avenews/master/old/data/average-latitude-longitude-countries.csv")

  # Define custom dictionaries for unrecognised nationalities
  custom_match_code <- c(
    "ENG" = "GB",
    "GIB" = "GI",
    "SIN" = "SG",
    "TRI" = "TT",
    "TWN" = "TW"
  )
  custom_match_continent <- c(
    "ENG" = "Europe",
    "GIB" = "Europe",
    "SIN" = "Asia",
    "TRI" = "Americas",
    "TWN" = "Asia"
  )

  # Add required columns to dataset
  plot_data <- data %>%
    filter(nationality != "CIV") %>%
    mutate(
      nationality_code = countrycode(nationality, "ioc", "iso2c", custom_match = custom_match_code),
      nationality_continent = countrycode(nationality, "ioc", "continent", custom_match = custom_match_continent),

      country = countrycode(city, "city", "country", custom_dict = city_country),
      country_code = countrycode(country, "country.name", "iso2c"),

      start_lat = countrycode(city, "city", "lat", custom_dict = city_country),
      start_lng = countrycode(city, "city", "lng", custom_dict = city_country),
      end_lat = countrycode(nationality_code, "ISO 3166 Country Code", "Latitude", custom_dict = country_data),
      end_lng = countrycode(nationality_code, "ISO 3166 Country Code", "Longitude", custom_dict = country_data)
    ) %>%
    filter(!is.na(start_lat) & !is.na(end_lat)) %>%
    filter(country_code != nationality_code)

  # Draw connection graph plot
  # Code taken from https://github.com/holtzy/About-Surfers-On-Twitter/blob/76398fca64a27fff67bd35df0ea1a26b4541c2d1/Where_surfers_travel.Rmd
  summary <- plot_data %>%
    count(start_lat,start_lng, end_lat,end_lng, nationality_continent) %>%
    arrange(n)

  data_for_connection <- function (dep_lon, dep_lat, arr_lon, arr_lat, group) {
    inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=50, addStartEnd=TRUE, breakAtDateLine=F)
    inter <- data.frame(inter)
    inter$group <- NA
    diff_of_lon <- abs(dep_lon) + abs(arr_lon)

    if (diff_of_lon > 180) {
      inter$group[which(inter$lon>=0)] <- paste0(group, "A")
      inter$group[which(inter$lon<0)] <- paste0(group, "B")
    } else {
      inter$group <- group
    }

    inter
  }

  data_ready_plot <- data.frame()
  for (i in seq_len(nrow(summary))) {
    tmp <- data_for_connection(summary$start_lng[i], summary$start_lat[i], summary$end_lng[i], summary$end_lat[i] , i)
    tmp$nationality_continent <- summary$nationality_continent[i]
    tmp$n <- summary$n[i]
    data_ready_plot <- rbind(data_ready_plot, tmp)
  }
  data_ready_plot$nationality_continent <- factor(data_ready_plot$nationality_continent, levels=c("Europe", "Americas", "Asia", "Africa", "Oceania"))

  # Draw plot
  plot_data %>%
    ggplot() +
      # Draw base map
      geom_polygon(aes(x=long, y = lat, group = group), data = map_data("world"), fill="#262626", color = "black", linewidth = 0.1) +

      # Draw city markers
      geom_point(aes(x = start_lng, y = start_lat), color = "white", size = 0.1) +

      # Draw connections
      geom_line(aes(x = lon, y = lat, group = group, color = nationality_continent, alpha = n), data = data_ready_plot, linewidth = 0.2) +

      # Hide alpha from legend
      scale_alpha(guide = "none") +

      # Manually define colors for each continent
      scale_color_manual(
        name = "Home Continent",
        values = c("yellow", "springgreen", "lightpink", "greenyellow", "darkslategray1")
      ) +

      # Add custom theme
      theme_hyrox() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title.x.bottom = element_blank(),
        axis.title.y.left = element_blank(),
        plot.margin = margin(l=0, b=0),
        legend.position = "bottom",
        legend.margin = margin(b = 10, t = 0),
        legend.title = element_text(margin = margin(r = 20)),
        legend.text = element_text(margin = margin(r = 10))
      ) +

      # Add label
      labs(
        title = "Travel Map"
      )
}
