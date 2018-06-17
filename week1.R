library(readr)
library(tidyr)
library(dplyr)
library(highcharter)
library(ggplot2)
library(lubridate)

#loading the data from Worldbank

life_exp <- read_csv("data/life_expectancy.csv", skip = 4)
countries <- read_csv("data/countries.csv")


# Making data clean and long and limiting to 1990 - 2009

life_exp <- life_exp %>%
  select( -3, -4, -62, -63)

life_exp <- life_exp %>%
  filter(!is.na(life_exp$`2016`))

life_exp <- life_exp %>%
  inner_join(countries[1:3], by = "Country Code") 

life_exp <- life_exp %>%
  filter(!is.na(Region))

life_exp <- life_exp %>%
  gather(3:59, key = "Year", value = "life_exp_years")

life_exp$Year <- as.numeric(life_exp$Year)

life_exp_1990 <- life_exp %>%
  filter(between(Year, 1990, 2009))


#separate sets for Singapore,  APJ and high-income countries

life_exp_sing <- life_exp_1990 %>%
  filter(life_exp_1990$`Country Code`== "SGP")

life_exp_apj <- life_exp_1990 %>%
  filter(Region == "East Asia & Pacific")

life_exp_high_income <- life_exp_1990 %>%
  filter(IncomeGroup == "High income")

# Interactive line chart for income groups

highchart(type = "stock") %>%
  hc_add_series(data = life_exp_1990,
                type = "line",
                hcaes(x = as.Date(ISOdate(Year,1,1)),
                      y = life_exp_years,
                      group = life_exp_1990$IncomeGroup
                )
  )  %>%
  hc_add_series(data = life_exp_sing,
                type = "line",
                name = "Singapore",
                hcaes(x = as.Date(ISOdate(Year,1,1)),
                      y = life_exp_years
                )
  ) %>%
  hc_legend(enabled = TRUE)

