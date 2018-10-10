#load packages
library(dplyr)
library(maps)
library(ggplot2)

#load data
voter_data <- read.csv('voter_turnout.csv')
voter_data$state <- tolower(voter_data$state)
president_years <- seq(1980, 2012, 4)

voter_data <- mutate(voter_data, election_type = ifelse(year %in% president_years, "presidential", 'midterm'), 
                     perc_voting = votes / eligible_voters * 100)

states <- map_data("state")
names(states) <- c("long", "lat", "group", "order", "state", "subregion")

voter_states <- inner_join(voter_data, states, by = "state")

voter_states %>%
  filter(year == 2008 | year == 2010) %>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = perc_voting, group = group), color = "white") + 
  coord_fixed(1.3) + 
  facet_wrap(~year) +
  labs(fill = "Voter Turnout (%)") + 
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank())

ggsave('voter_turnout_08_10.png')
