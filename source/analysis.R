library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
options(timeout = max(1000, getOption("timeout")))
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)
# Which states have the highest jailed black population?
highest_black_jail_state <- incarceration_trends %>%
  drop_na() %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
  pull(state)

# Which states have the highest black population?
highest_black_pop <- incarceration_trends %>%
  drop_na() %>%
  filter(black_pop_15to64 == max(black_pop_15to64, na.rm = T)) %>%
  pull(state)

# White Population vs Black Population in NY?
white_pop_ny <- incarceration_trends %>%
  drop_na() %>%
  filter(state == "NY") %>%
  filter(white_pop_15to64 == max(white_pop_15to64)) %>%
  pull(white_pop_15to64)

white_pop_jail_ny <- incarceration_trends %>%
  drop_na() %>%
  filter(state == "NY") %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>%
  pull(white_jail_pop)

white_ratio <- white_pop_jail_ny /white_pop_ny

black_pop_ny <- incarceration_trends %>%
  drop_na() %>%
  filter(state == "NY") %>%
  filter(black_pop_15to64 == max(black_pop_15to64)) %>%
  pull(black_pop_15to64)

black_pop_jail_ny <- incarceration_trends %>%
  drop_na() %>%
  filter(state == "NY") %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  pull(black_jail_pop)

black_ratio <- black_pop_jail_ny / black_pop_ny
# Which states have the highest jailed white population?
highest_white_jail_state <- incarceration_trends %>%
  drop_na() %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = T)) %>%
  pull(state)

# Which states have the highest white population?
highest_black_pop <- incarceration_trends %>%
  drop_na() %>%
  filter(black_pop_15to64 == max(black_pop_15to64, na.rm = T)) %>%
  pull(state)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function wrangles data to create a dataset suitable for visualization
get_year_jail_pop <- function() {
  visualization_df <- incarceration_trends %>%
  select(year, total_jail_pop) %>%
  group_by(year) %>%
  summarize(total_by_year = sum(total_jail_pop, na.rm = T))
return(visualization_df)   
}

# This function calls the get_year_jail_pop() function and creates a bar chart
plot_jail_pop_for_us <- function()  {
  chart <- ggplot(data = get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total_by_year)) +
    xlim(c(1970, 2018)) +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)", 
         y = "Total Jail Population", 
         x = "Year",
         caption = "Figure 1: Change in total jail population in the U.S. by year from 1970 to 2018.")
  return(chart)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#
# creates vector of states
states <- c("CA", "WA", "NY", "TX")
# This function wrangles data to create a dataset suitable for visualization
get_jail_pop_by_states <- function(state) {
  state_df <- incarceration_trends %>%
  filter(state %in% states) %>%
  group_by(year, state) %>%
  summarize(jail_state_pop = sum(total_jail_pop, na.rm = T))
return(state_df)
}
# This function calls the get_jail_pop_by_states() function and creates a line graph
plot_jail_pop_by_states <- function(states) {
  pop_chart <- ggplot(data = get_jail_pop_by_states(states)) +
  geom_line(mapping = aes(x = year, y = jail_state_pop, color = state)) +
  xlim(c(1970, 2018)) +
    labs(title = "Increase of Jail Population in U.S. states (1970-2018)",
         y = "Total Jail Population",
         x = "Year",
         caption = "Figure 2: Change in total jail populations in U.S. states by year from 1970 to 2018.")
  return(pop_chart)
}
plot_jail_pop_by_states()
## Section 5  ---- 
#----------------------------------------------------------------------------#
# Increase of Jail Populations in U.S. by Race (1988-2018)
#----------------------------------------------------------------------------#
# This function wrangles data to create a dataset suitable for visualization 
get_jailed_pop <- function() {
  get_jail_df <- incarceration_trends %>%
  select(year, black_jail_pop, white_jail_pop, aapi_jail_pop, native_jail_pop, latinx_jail_pop) %>%
  filter(year >= 1988) %>%
  gather(key = race, value = population, -year)
return(get_jail_df)
}
# This function calls the get_jailed_pop() function and creates a stacked bar chart
plot_get_jailed_pop <- function() {
  data <- get_jailed_pop()
  chart <- ggplot(data = data) + 
  geom_col(mapping = aes(x = year, y = population, fill = race)) +
    labs(title = "Increase of Jail Populations in U.S. by Race (1988-2018)",
         y = "Total Population",
         x = "Year",
         caption = "Figure 3: Change in total jail populations in the U.S. by race from 1988 to 2018.")
return(chart)
}
plot_get_jailed_pop()
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
install.packages("usmap")
library("usmap")

get_us_map <- function() {
plot_data <- incarceration_trends %>%
  group_by(state) %>%
  summarize(black_jail_population = sum(black_jail_pop, na.rm = TRUE))
state_map <- map_data("county") %>%
  rename(state = region) %>%
  left_join(plot_data)
}

plot_map <- function() {  
pop_map <- ggplot(state) +
  geom_point(
    mapping = aes(x = long, y = lat),
    color = "red",
    alpha = .3) +
  labs(title = "Black Jail Populations in the U.S.") +
  theme(plot.margin = margin(.3, 0, 0, 0, "cm")) 
}
  




