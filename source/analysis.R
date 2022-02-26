#Importing the required libraries
library(tidyverse)
library(plotly)

#Reading the dataset
csv_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#Filtering data for usage in graph
usable_data <- csv_data %>%
  filter (year >= 1990) %>%
  select(year, state, total_pop_15to64, aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64, white_pop_15to64) %>%
  mutate(white_ratio = ifelse(total_pop_15to64 == 0, 0, white_pop_15to64/total_pop_15to64)) %>%
  mutate(black_ratio = ifelse(total_pop_15to64 == 0, 0, black_pop_15to64/total_pop_15to64)) %>%
  mutate(latinx_ratio = ifelse(total_pop_15to64 == 0, 0, latinx_pop_15to64/total_pop_15to64)) %>%
  mutate(aapi_ratio = ifelse(total_pop_15to64 == 0, 0, aapi_pop_15to64/total_pop_15to64)) %>%
  mutate(native_ratio = ifelse(total_pop_15to64 == 0, 0, native_pop_15to64/total_pop_15to64))

#Used in summary table
yearly_white_ratios <- usable_data %>%
  group_by(year) %>%
  summarize(ratio = mean(white_ratio, na.rm = TRUE)) %>%
  mutate(race = "White")

#Calculating ratios of group to total population
yearly_black_ratios <- usable_data %>%
  group_by(year) %>%
  summarize(ratio = mean(black_ratio, na.rm = TRUE)) %>%
  mutate(race = "Black")

 yearly_latinx_ratios <- usable_data %>%
  group_by(year) %>%
  summarize(ratio = mean(latinx_ratio, na.rm = TRUE)) %>%
  mutate(race = "Latinx")

yearly_aapi_ratios <- usable_data %>%
  group_by(year) %>%
  summarize(ratio = mean(aapi_ratio, na.rm = TRUE)) %>%
  mutate(race = "AAPI")

yearly_native_ratios <- usable_data %>%
  group_by(year) %>%
  summarize(ratio = mean(native_ratio, na.rm = TRUE)) %>%
  mutate(race = "Native")

#Combining ratios of different races into one data frame
list_of_ratios <- rbind(yearly_black_ratios, yearly_latinx_ratios, yearly_aapi_ratios, yearly_native_ratios)

#Creating a line graph from data
trends_over_time <- ggplot(data = list_of_ratios) +
  geom_line(
    mapping = aes(x = year, y = ratio, color = race), size = 1.5) +
  ggtitle("Difference in Ratios of Minority Groups in Prison (nationally)") +
  ylab("Ratio of race population to total population") +
  xlab("Year") +
  labs(color = "Race:")


#Selecting populations
comparing <- usable_data %>%
  filter(year == 2018) %>%
  select(white_pop_15to64, black_pop_15to64)

#Creating a scatterplot
comparison <- comparing %>%
  ggplot(aes(x = white_pop_15to64, y = black_pop_15to64)) +
  geom_point(alpha=0.7) +
  ggtitle("Comparison of White vs. Black Prisoner Numbers in the US (in 2018)") + 
  xlab("White Prisoners") +
  ylab("Black Prisoners")



#Function to convert two-letter state codes to lowercase full state names
st_to_state <- function(x){
  c(state.name, 'District of Columbia')[match(x, c(state.abb, 'DC'))]
}

#Attaching ratios in the year 2018 to the map
approx_black <- usable_data %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarize(ratio = mean(black_ratio, na.rm = TRUE)) %>%
  mutate(stateFull = tolower(st_to_state(state)))

state_shape <- map_data("state") %>% 
  rename(stateFull = region) %>% 
  left_join(approx_black, by="stateFull") 

#Defining a minimalist theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

#Creating a map using ggplot
us_map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = ratio),
    color = "White",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#B57EDC", high = "Red") +
  labs(fill = "Ratio of black to total prisoners") +
  ggtitle("Map Displaying Ratio of Black to Total Prisoners by State") +
  blank_theme



# Values for summary table (explained in introduction)
black_ratio_highest <- yearly_black_ratios %>%
  filter(max(ratio) == ratio) %>%
  pull(year)

black_ratio_lowest <- yearly_black_ratios %>%
  filter(min(ratio) == ratio) %>%
  pull(year)

black_ratio_10y_range <- yearly_black_ratios %>%
  filter(year >= 2008) %>%
  select(ratio)

black_ratio_10y_dif <- tail(black_ratio_10y_range, 1) - head(black_ratio_10y_range, 1)
black_ratio_10y_difference <- black_ratio_10y_dif %>%
  pull(ratio)
black_ratio_10y_percent <- black_ratio_10y_difference * 100

white_ratio_highest <- yearly_white_ratios %>%
  filter(max(ratio) == ratio) %>%
  pull(year)

white_ratio_10y_range <- yearly_white_ratios %>%
  filter(year >= 2008) %>%
  select(ratio)

white_ratio_10y_dif <- tail(white_ratio_10y_range, 1) - head(white_ratio_10y_range, 1)
white_ratio_10y_difference <- white_ratio_10y_dif %>%
  pull(ratio)
white_ratio_10y_percent <- white_ratio_10y_difference * 100

summary_table <- data.frame(variable_purpose = c("Year of lowest ratio of black to total prisoners since 1990",
                                                 "Year of highest ratio of black to total prisoners since 1990",
                                                 "Increase in ratio in the last ten years (in %)",
                                                 "Year of highest ratio of white to total prisoners since 1990",
                                                 "Increase in ratio in the last ten years (in %)"), 
                            value = c(black_ratio_lowest, black_ratio_highest, black_ratio_10y_percent,
                                      white_ratio_highest, white_ratio_10y_percent))