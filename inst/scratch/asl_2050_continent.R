load_all()
library(readr)

# Using country-codes dataset from
# https://datahub.io/core/country-codes#resource-country-codes_zip

data(drivers)

country_codes <- read_csv("data-raw/country-codes.csv", na = "")

continents <- select(country_codes, iso3 = "ISO3166-1-Alpha-3", continent = Continent) %>%
  mutate(continent = case_when(
    continent == "AF" ~ "Africa",
    continent == "AN" ~ "Antarctica",
    continent == "AS" ~ "Asia",
    continent == "EU" ~ "Europe",
    continent == "NA" ~ "North America",
    continent == "OC" ~ "Oceania",
    continent == "SA" ~ "South America"
  )) %>%
  mutate(continent = factor(continent))

save(continents, file = "data/continents.RData")
