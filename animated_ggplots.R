
library(dslabs)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(gapminder)
library(gganimate)
library(av)
library(gifski)

#download Brasil Covid19 dataset: https://data.brasil.io/dataset/covid19/caso.csv.gz

# download Brasil Covid19 dataset: https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv

#Import as a covid19_brasxxxx name

covid19_bras0407 <- read_delim("Covid/covid19_bras0407.csv", 
                               +     ";", escape_double = FALSE, col_types = cols(date = col_date(format = "%m/%d/%Y")), 
                               +     trim_ws = TRUE)

# Add new column to covid19_brasxxxx

covid19_bras0407 <- covid19_bras0407 %>% mutate(deaths_per_million = deaths / (estimated_population_2019/10^6))

as.Date(covid19_bras0407$date)

# Brazilian states Animated ggplot, showing deaths' rate and acumulated deaths evolution by state from february to july 2020, one image per date

covid19_bras2606 %>% filter(place_type == "state") %>%
  ggplot(aes(deaths_per_million, deaths, size = deaths, color = state)) +
  geom_point() +
  theme_bw() +
  scale_y_log10() +
  # gganimate specific bits:
  labs(title = 'date: {frame_time}', x = 'Deaths` Rate', y = 'Acumulated Deaths') +
  transition_time(date) +
   ease_aes('linear') 

# Brazilian states Animated ggplot, showing deaths' rate and acumulated deaths evolution by largest cities (more than 50k inhabitants) from february to july 2020, one image per date

covid19_bras2606 %>% filter(place_type == "city" & estimated_population_2019 >= 50000) %>%
  ggplot(aes(deaths_per_million, deaths, size = deaths, color = state)) +
  geom_point() +
  theme_bw() +
  scale_y_log10() +
  # gganimate specific bits:
  labs(title = 'date: {frame_time}', x = 'Deaths` Rate', y = 'Acumulated Deaths') +
  transition_time(date) +
  ease_aes('linear') 

#Data world animated ggplots, showing deaths' rate and acumulated deaths evolution by country and continents

owid_covid_data %>% filter(!is.na(continent)) %>%
  ggplot(aes(total_deaths_per_million, total_deaths, size = total_deaths, color = continent)) +
  geom_point() +
  theme_bw() +
  scale_y_log10() +
  scale_x_log10() +
  # gganimate specific bits:
  labs(title = 'date: {frame_time}', x = 'Deaths` Rate', y = 'Acumulated Deaths') +
  facet_wrap(~continent) +
  transition_time(date) +
  ease_aes('linear') 
  

  # Save at gif:
  anim_save("world Death rate evolution by continent log scale.gif")
  
  # Brazilian states Animated ggplots, showing deaths' rate and acumulated deaths evolution by selected states

  covid19_bras2606 %>% filter(state %in% c("PE", "SP", "CE", "AM", "MG", "RS") & place_type == "city" & estimated_population_2019 >= 20000) %>%
    ggplot(aes(deaths_per_million, deaths, size = deaths, color = state)) +
    geom_point() +
    theme_bw() +
    scale_y_log10() +
    # gganimate specific bits:
    labs(title = 'date: {frame_time}', x = 'Deaths` Rate', y = 'Acumulated Deaths') +
    facet_wrap(~state) +
    transition_time(date) +
    ease_aes('linear')
  
  anim_save("Brazilian cities Death rate evolution9.gif")
  
  # Make a ggplot, but add frame=date: one image per year
  covid19_bras2406 %>% filter(place_type == "state") %>%
  ggplot(aes(deaths, deaths_by_population, size = 4, colour = state)) +
    geom_point(alpha = 0.7, show.legend = FALSE) +
    scale_colour_manual(values = deaths_by_population) +
    scale_size(range = c(2, 12)) +
    scale_x_log10() +
    facet_wrap(~state) +
    # Here comes the gganimate specific bits
    labs(title = 'date: {frame_time}', x = 'deaths', y = 'rate') +
    transition_time(date) +
    ease_aes('linear')
  
  # Save at gif:
  anim_save("271-ggplot2-animated-gif-chart-with-gganimate2.gif")
