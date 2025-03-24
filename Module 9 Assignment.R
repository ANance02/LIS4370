library(ggplot2)
library(dplyr)
setwd("C:/Users/Owner/Desktop/SchoolWinter2025/LIS4370")
fatalities <- read.csv("Fatalities.csv")

# Direct Comparison between Total Fatalities and Year
ggplot(fatalities, aes(year, fatal), group = year, color = year)+
  geom_bar(stat = "identity")+
  labs(x = "Year", y = "Fatalities", title = "Fatalities vs Year")+
  scale_x_continuous(breaks = seq(1982, 1988, by = 1))


# Fatality Trends for the Top 5 States
top_states <- fatalities %>%
  group_by(state) %>%
  summarise(total_fatalities = sum(fatal)) %>%
  arrange(desc(total_fatalities)) %>%
  top_n(5, total_fatalities)

top_fatalities <- fatalities %>%
  filter(state %in% top_states$state)

ggplot(top_fatalities, aes(x = as.numeric(as.character(year)), y = fatal, group = state, color = state)) +
  geom_line(size = 1) +
  geom_text(data = top_fatalities %>% filter(year == "1988"), 
            aes(label = state), 
            hjust = -0.1, size = 5) +
  labs(title = "Fatality Trends Over Time (Top 5 States)", 
       x = "Year", 
       y = "Fatalities", 
       color = "State")


# Scatter plot for Alcohol Consumption and Fatalities
ggplot(fatalities, aes(x = spirits, y = fatal)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Alcohol Consumption vs. Fatalities", x = "Spirits Consumption", y = "Fatalities")

