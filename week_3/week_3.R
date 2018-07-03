library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)

suicide_data <- read_csv("week_3/data/NCHS_-_Injury_Mortality__United_States.csv")[,1:8] %>%
  filter(`Injury intent` == "Suicide") %>%
  mutate(rate = Deaths/Population*100000)

# getting suicide rates in US, 1999-2016
suicide_data %>%
  filter((Sex == "Both sexes") & (`Age group (years)` == "All Ages") & (Race == "All races") & (`Injury mechanism` == "All Mechanisms")) %>%
  ggplot() +
  geom_point(aes(x = Year, y = rate), stat = "identity", size = 3) +
  geom_segment(aes(x = Year, xend = Year, y = rate, yend = 14.5), color = "gray") +
  ylim(9.5, 14.5) +
  labs(title = paste("Suicide rate in US, 1999-2016"), y = "Suicide rate") +
  theme_classic()

#saving the image to add last features to the chart in an image editor
ggsave("week_3/USrate.jpeg")


#checking if the growth in suicide rates is associated with firearms
suicide_data %>%
  filter((Sex == "Both sexes") & (`Age group (years)` == "All Ages") & (Race == "All races") & (`Injury mechanism` %in% c("All Mechanisms", "Firearm"))) %>%
  select(-7) %>%
  spread(key = `Injury mechanism`, value = rate) %>%
  mutate(firearm_share = (Firearm/(`All Mechanisms`))) %>%
  ggplot() +
  geom_line(aes(x = Year, y = firearm_share), stat = "identity") +
  theme_minimal()

ggsave("week_3/firearm_share.jpeg")

# no, the share is falling     

suicide_by_state <- read_tsv("week_3/data/Underlying Cause of Death, 1999-2016.txt")
