library(readr)
library(tidyr)
library(dplyr)
library(tibble)
library(ggplot2)


#uploading data from World Bank
import <- read_csv("week_2/data/import.csv", skip = 4)[,1:61]
export <- read_csv("week_2/data/export.csv", skip = 4)[,1:61]

# cleaning and getting import data on the USA
import_usa <- import %>%
  gather(5:61, key = "year", value = "import") %>%
  select(1,3,5,6) %>%
  filter(`Country Name` == "United States") 


# cleaning and getting export data on the USA
export_usa <- export %>%
  gather(5:61, key = "year", value = "export") %>%
  select(1,3,5,6) %>%
  filter(`Country Name` == "United States") 

#joining data to derive trade balance

balance_usa <- export_usa %>%
  inner_join(import_usa[,3:4], by = c("year"))  %>%
  transmute(year = as.numeric(year), trade_balance = (export - import)) 

balance_usa_since_2005 <- balance_usa %>%
  filter(year >= 2005)

balance_usa %>%
  ggplot(aes(x = year, y = trade_balance))+
  geom_point(stat = "summary", fun.y = mean) +
  stat_summary(fun.y = mean, geom = "line")



balance_usa_since_2005 %>%
  ggplot(aes(x = year, y = trade_balance))+
  geom_smooth(aes(color = "Trade balance"),stat = "summary", fun.y = mean, size = 2) +
  geom_smooth(aes(color = "Trendline"),method = "lm", se = FALSE, size = 1.5) +
  labs(y = "Trade balance( export - import), % of GDP") +
  scale_color_manual(values=c("black", "orange"), name = "") +
  theme_minimal() +
  theme(legend.position = "bottom") 
