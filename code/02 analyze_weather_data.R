################################################################################
### Title:  Analyze weather data
### Author: Peter Christenson 
################################################################################

rm(list = ls()); gc()
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  lubridate,
  magrittr,
  readxl,
  writexl,
  survival,
  zoo,
  tidyr,
  stringr,
  janitor,
  readr,
  data.table,
  ggplot2,
  ggthemes,
  stargazer,
  generics,
  broom
)

##################################################
###  Globals
##################################################

# Directories
WORKING_DIRECTORY <- "/Users/peterchristenson/Desktop/Projects/NFL Weather Analysis/"
setwd(WORKING_DIRECTORY)
INPUT <- "./input/"
INTERMEDIATE <- "./intermediate/"
OUTPUT <- "./output/"

# Constants
DATE <- Sys.Date()

##################################################
###  Data
##################################################

# Game data with weather
data_game_weather <- lapply(list.files(INTERMEDIATE, full.names = T), read.csv) %>% 
  bind_rows() %>% 
  clean_names() %>% 
  select(-x)

##################################################
###  Data cleaning
##################################################

#----- Create/clean relevant columns -----#
data_game_weather_cleaned <- data_game_weather %>% 
  # Create season variable (each season ends with Super Bowl)
  mutate(
    season = lag(cumsum(ifelse(grepl("Super Bowl", teams), 1, 0)), default = 0) + 2000 # First season
  ) %>% 
  select(season, everything()) %>% 
  # Clean other variables
  mutate(
    # Location
    location = gsub(".*Location: ", "", date),
    # Date
    date = gsub(" Location.*", "", date),
    date = gsub(".*Date: ", "", date),
    date = gsub(" ,", ", ", date),
    date = gsub("Febraury", "February", date),
    date = as.Date(date, format = "%B %d, %Y"),
    # Venue
    venue = gsub(" Attendance.*", "", venue),
    venue = gsub(".*Venue: ", "", venue),
    # Teams
    teams = gsub("<title>|</title>| Game Statistics| - Pro Football Archives", "", teams),
    teams = gsub(".* Championship - |.* Game - |.*Playoff - |.*Super Bowl [IVXLCDM]+ - |.*Super Bowl 50 - ", "", teams),
    teams = ifelse(
      season %in% c(2000:2007, 2013),
      gsub(" -.*", "", teams),
      gsub(".*- ", "", teams)
    ),
    teams  = gsub("Buccanneers", "Buccaneers", teams),
    team_1 = gsub(" at.*", "", teams),
    team_2 = gsub(".*at ", "", teams),
    # Weather
    weather_type = gsub(".*Weather: ", "", weather),
    weather_type = gsub(" Temp.*| [0-9].*", "", weather_type),
    weather_type = ifelse(startsWith(weather_type, "Temp"), NA, weather_type),
    temp_f       = gsub(".*Temp: ", "", weather),
    temp_f       = gsub(" F.*", "", temp_f),
    temp_f       = as.numeric(temp_f),
    wind_mph     = str_remove_all(tolower(gsub(".*Wind: ", "", weather)), " "),
    wind_mph     = as.numeric(gsub(".*?(\\d+)mph.*", "\\1", wind_mph)),
    humidity_pct = gsub(" wind.*", "", tolower(weather)),
    humidity_pct = as.numeric(gsub(".*: |%|,", "", humidity_pct))
  ) %>% 
  # Add relevant columns
  mutate(
    indoors   = ifelse(grepl("indoors", tolower(weather)), 1, 0),
    total_pts = team_1_pts + team_2_pts
  ) %>% 
  select(-c(teams, weather))

#----- Recode errors -----#
data_game_weather_cleaned %<>%
  mutate(
    temp_f = case_when(
      temp_f == 7777 ~ 77,
      T ~ temp_f
    )
  )

##################################################
###  Hypothesis testing
##################################################

#----- Create dataframe for test -----#
indoor_outdoor <- data_game_weather_cleaned %>% 
  select(indoors, total_pts) %>% 
  mutate(
    indoors = as.character(indoors),
    indoors = recode(
      indoors,
      '1' = 'Yes',
      '0' = 'No'
    )
  )

#----- Plot -----#
ggplot(indoor_outdoor) + 
  geom_density(aes(total_pts, fill = indoors), alpha = 0.3) +
  theme_few() +
  guides(fill=guide_legend(title="Indoor Stadium")) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(
    title   = "Total Points Scored by Stadium Type",
    x       = "Total Points in Game",
    caption = "Notes:\n[1] Games played from the 2000-2022 seasons." 
  )

ggsave(paste0(OUTPUT, "points_scored_kernel_density_plot.png"), height = 5, width = 8)

# t-test -------------------------------------

# 1. Check Normality
table(indoor_outdoor$indoors)
## Since both groups have large sample sizes, can assume both are normally
## distributed

points_indoor <- indoor_outdoor$total_pts[indoor_outdoor$indoors == "Yes"]
points_non_indoor <- indoor_outdoor$total_pts[indoor_outdoor$indoors == "No"]

# 2. Check variances
var.test(points_indoor, points_non_indoor)
## Assume variances are equal since p = 0.9061 > 0.05

# 3. Peform 2 sample t-test
# Null Hypothesis        -> points scored in games played in indoor stadiums is
# not greater than points scored in games not played in non-indoor stadiums
#
# Alternative Hypothesis -> points scored in games played in indoor stadiums is
# greater than points scored in games not played in non-indoor stadiums

t.test(points_indoor, points_non_indoor, alternative = "greater", conf.level = 0.95)

## p-value = 1.685e-14 < 0.05 --> reject null hypothesis

##################################################
###  Regression on weather variables
##################################################

#----- Regression prep -----#
regression_df <- data_game_weather_cleaned %>% 
  mutate(rain   = ifelse(grepl("rain|showers|drizzle", tolower(weather_type)), '1', '0'),
         snow   = ifelse(grepl("snow", tolower(weather_type)), '1', '0'),
         season = as.character(season)) %>% 
  # Only consider games with weather data
  filter(!(is.na(temp_f) | is.na(wind_mph) | is.na(humidity_pct)),
         indoors == 0)

reg_variables <- c("season", "location", "temp_f", "team_1", "team_2", 
                   "wind_mph", "humidity_pct", "rain", "snow", "total_pts")

regression_df %<>%
  select(all_of(reg_variables))

#----- Regression -----#
reg <- lm(total_pts ~ (season*team_1) + (season*team_2) + location + rain + snow + temp_f + wind_mph + humidity_pct,
   data = regression_df)

reg2 <- lm(total_pts ~ rain + snow + temp_f + wind_mph + humidity_pct,
          data = regression_df)
