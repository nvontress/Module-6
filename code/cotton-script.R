########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 25, 2019
# Edited by: Natalie Von Tress
# Last Edited: October 2, 2019
########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# 2. Read & inspect the dataset ----
cotton <- read_csv("data/cotton-usda-nass.csv")
str(cotton)
summary(cotton)
dim(cotton)

# 3.1. Create a NC data subset ----
cotton %>%
  filter(state == "NORTH CAROLINA") %>%
  select(year, state, ag_district, county, data_item, value) -> nc_cotton

# 3.2. Divide the data_item column ----
nc_cotton %>%
  separate(data_item, 
           into = c('cotton_type','measurement'),
           sep = " - ") -> nc_cotton

# 3.3. Convert the value column to numeric type ----
nc_cotton %>%
  filter(value != "(D)") -> nc_cotton
nc_cotton$value <- as.numeric(nc_cotton$value)

# 4. Visualizing trends ----
nc_cotton %>%
  ggplot(mapping = aes(x = year, y = value)) +
  geom_point() +
  facet_grid(rows = vars(measurement), cols = vars(ag_district), scales = "free_y") +
  labs(title = "Cotton production in NC", caption = "Source: USDA NASS", x = "Year", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

# 5. Summarize data from 2018 ----
nc_cotton %>%
  filter(year == "2018") %>%
  spread(key = "measurement", value = "value") %>%
  mutate(total_lbs = `ACRES HARVESTED` * `YIELD, MEASURED IN LB / ACRE`) %>%
  top_n(3, total_lbs) %>%
  select(county, total_lbs) -> nc_cotton_2018
nc_cotton_2018
