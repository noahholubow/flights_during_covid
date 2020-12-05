# Column names found here: https://www.transtats.bts.gov/Fields.asp?Table_ID=259

# Import ------------------------------------------------------------------
# loading packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(janitor)
library(naniar)
library(skimr)
library(lubridate)
library(stringr)
library(devtools)
library(mapdata)
library(svglite)
library(patchwork)
library(codebook)
library(future)
library(dataMaid)
library(kableExtra)


# Initial Data Import -----------------------------------------------------

# importing data, removing column X51
yr2019 <- read_csv("Data/unprocessed/2019_ALL_carrier.csv") %>% 
  select(-(X51)) %>% 
  clean_names()

yr2020 <- read_csv("Data/unprocessed/2020_ALL_carrier_MAY.csv") %>% 
  select(-(X51)) %>% 
  clean_names() %>% 
  mutate(aircraft_type = as.character(aircraft_type))
  

# merging data sets
merged <- add_row(yr2019, yr2020)


# Initial Cleaning --------------------------------------------------------
# filtering out beyond month 4
all <- merged %>% 
  filter(month %in% c(1:5))

# filtering out columns
filtered <- all %>% 
  select(-c("airline_id", "unique_carrier_entity", "unique_carrier", "unique_carrier_name", "carrier_group_new","origin_airport_id", 
           "origin_airport_seq_id", "origin_city_market_id", "origin_wac", "dest_airport_id",
           "dest_airport_seq_id", "dest_city_market_id", "dest_wac", "quarter", "data_source"))

# Joining -----------------------------------------------------------------
# import external data sets
aircraft_type <- read_csv("Data/unprocessed/aircraft_type.csv") %>% 
  rename(aircraft_type_f = Description)

aircraft_config <- read_csv("Data/unprocessed/aircraft_config.csv") %>% 
  rename(aircraft_config_f = Description)

aircraft_group <- read_csv("Data/unprocessed/aircraft_group.csv") %>% 
  rename(aircraft_group_f = Description)

carrier_group <- read_csv("Data/unprocessed/carrier_group.csv") %>% 
  rename(carrier_Type = Description)

# join aircraft type
filtered_join <- filtered %>% 
  left_join(aircraft_type, c("aircraft_type" = "Code")) %>% 
  left_join(aircraft_group, c("aircraft_group" = "Code")) %>% 
  left_join(aircraft_config, c("aircraft_config" = "Code"))


# COVID-19 ----------------------------------------------------------------
# importing COVID-19 data set
covid_initial <- read_csv("Data/unprocessed/us-states-2020-11-16.csv") %>% 
  mutate(month = month(date)) %>% 
  filter(month == 1 | month == 2 | month == 3 | month == 4 | month == 5)

# calculate
covid_day <- covid_initial %>% 
  arrange(state, date) %>% 
  group_by(state) %>% 
  mutate(
    day_count = cases - dplyr::lag(cases, n = 1),
    day_count = if_else(is.na(day_count), cases, day_count)
    ) %>% 
  mutate(year = 2020)
covid_day

# group COVID-19 data set per month
covid <- covid_day %>% 
  group_by(state, month, fips, year) %>% 
  summarise(cases = sum(day_count))
covid


# left joining using FIPS
filtered_join_covid <- filtered_join %>% 
  left_join(covid, by = c("origin_state_fips" = "fips", "month" = "month", "year" = "year")) %>% 
  select(-"state") %>% 
  rename("origin_cases" = cases) %>% 
  left_join(covid, by = c("dest_state_fips" = "fips", "month" = "month", "year" = "year")) %>% 
  select(-"state") %>% 
  rename("dest_cases" = cases)  
filtered_join_covid


# population data
pop <- read_csv("Data/unprocessed/SCPRC-EST2019-18+POP-RES.csv")


# Cleaning ----------------------------------------------------------------
# looking at missing values
skim_without_charts(filtered_join_covid)
# why a discrepancy?
# explore missing values

view(filtered_join_covid %>% 
  filter(is.na(region)))

# reorder and remove coded columns with renamed ones
filtered_join_r <- filtered_join_covid %>% 
  select(month, year, carrier, origin, dest, departures_scheduled:origin_country_name, origin_cases, dest_city_name:dest_country_name, dest_cases, 
         aircraft_type, aircraft_type_f, aircraft_config, aircraft_config_f, aircraft_group, aircraft_group_f, 
         distance_group:class)

# making blank values NA - does this actually do anything?
filtered_join_c <- filtered_join_r %>% 
  complete()


# changing year, aircraft group/config/type to factor
filtered_join_c <-  filtered_join_c %>% 
  mutate(year = as.factor(year), 
         aircraft_group = as.factor(aircraft_group), 
         aircraft_config = as.factor(aircraft_config), 
         aircraft_type = as.factor(aircraft_type))
filtered_join_c


# EDA ---------------------------------------------------------------------
# number of total flights
filtered_join_c %>% 
  ggplot(aes(x = year)) +
  geom_bar()


# * Comparisons -------------------------------------------------------------

# ** In aggregate (Jan-Apr) -----------------------------------------------
# num pgrs - change scientific notation
filtered_join_c %>% 
  group_by(year) %>% 
  summarise(sum_psgr = sum(passengers)) %>% 
  ggplot(aes(x = year, y = sum_psgr / 1000000)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("year") +
  ylab("Passenger Count (in millions)")

# mainline carriers 2019 vs 2020
filtered_join_c %>% 
  filter(carrier_group == 3, class == "F") %>% 
  group_by(year) %>% 
  summarise(sum_psgr = sum(passengers)) %>% 
  ggplot(aes(x = year, y = sum_psgr / 1000000)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Year") +
  ylab("Passenger Count (in millions)")

# freight
# change scientific notation
filtered_join_c %>% 
  group_by(year) %>% 
  summarise(sum_frgt = sum(freight)) %>% 
  ggplot(aes(x = year, y = sum_frgt / 1000000000)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("year") +
  ylab("freight Weight (in billions)")

# payload
# num pgrs - change scientific notation
filtered_join_c %>% 
  group_by(year) %>% 
  summarise(sum_payload = sum(payload)) %>% 
  ggplot(aes(x = year, y = sum_payload / 1000000000)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("year") +
  ylab("Total payload (in billions)")


# ** Jan-Feb --------------------------------------------------------------
# JAN-FEB mainline carriers 2019 vs 2020
filtered_join_c %>% 
  filter(carrier_group == 3, class == "F", month == c(1,2)) %>% 
  group_by(year) %>% 
  summarise(sum_psgr = sum(passengers)) %>% 
  ggplot(aes(x = year, y = sum_psgr / 1000000)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("year") +
  ylab("Passenger Count (in millions)")

# MAR mainline carriers 2019 vs 2020
filtered_join_c %>% 
  filter(carrier_group == 3, class == "F", month == c(3)) %>% 
  group_by(year) %>% 
  summarise(sum_psgr = sum(passengers)) %>% 
  ggplot(aes(x = year, y = sum_psgr / 1000000)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("year") +
  ylab("Passenger Count (in millions)")

# APR mainline carriers 2019 vs 2020
filtered_join_c %>% 
  filter(carrier_group == 3, class == "F", month == c(4)) %>% 
  group_by(year) %>% 
  summarise(sum_psgr = sum(passengers)) %>% 
  ggplot(aes(x = year, y = sum_psgr / 1000000)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("year") +
  ylab("Passenger Count (in millions)")

# MAY mainline carriers 2019 vs 2020
filtered_join_c %>% 
  filter(carrier_group == 3, class == "F", month == c(5)) %>% 
  group_by(year) %>% 
  summarise(sum_psgr = sum(passengers)) %>% 
  ggplot(aes(x = year, y = sum_psgr / 1000000)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("year") +
  ylab("Passenger Count (in millions)")

# * Trendlines ------------------------------------------------------------


# **2020 TREND LINE PLOT ALL passengers
filtered_join_c %>% 
  filter(carrier_group == 3, class == "F", year == 2020) %>% 
  group_by(month) %>% 
  summarise(sum_psgr = sum(passengers)) %>% 
  ggplot(aes(x = month, y = sum_psgr / 1000000)) +
  geom_line() +
  xlab("month") +
  ylab("Passenger Count (in millions)")

# **2019 vs 2020 TREND LINE PASSENGER COUNT MAINLINE
view(filtered_join_c %>% 
  filter(carrier_group == 3, class == "F")) %>% 
  group_by(year, month) %>% 
  summarise(sum_psgr = sum(passengers) / 1000000) %>% 
  ggplot(aes(x = month, y = sum_psgr)) +
  geom_line(aes(color = year)) +
  xlab("month") +
  ylab("Passenger Count (in millions)")


# ** 2019 vs 2020 TREND LINE freight COUNT
filtered_join_c %>% 
  group_by(year, month) %>% 
  summarise(sum_frgt = sum(freight) / 1000000000) %>% 
  ggplot(aes(x = month, y = sum_frgt)) +
  geom_line(aes(color = year)) +
  xlab("month") +
  ylab("freight Weight (in billions)")

# **2019 vs 2020 TREND LINE CARGO ONLY 
filtered_join_c %>% 
  filter(class == c("G", "P")) %>% 
  group_by(year, month) %>% 
  summarise(sum_frgt = sum(freight) / 1000000000) %>% 
  ggplot(aes(x = month, y = sum_frgt)) +
  geom_line(aes(color = year)) +
  xlab("Month") +
  ylab("Cargo Count (in billions)")


# **2019 vs 2020 TREND LINE CARGO ONLY US vs. other
cargo_us_only <- filtered_join_c %>% 
  filter(class == c("G", "P"), origin_country == "US") %>% 
  group_by(year, month) %>% 
  summarise(sum_frgt = sum(freight) / 1000000000) %>% 
  ggplot(aes(x = month, y = sum_frgt)) +
  geom_line(aes(color = year)) +
  xlab("Month") +
  ylab("Cargo Count (in billions)")

cargo_intl_only <- filtered_join_c %>% 
  filter(class == c("G", "P"), origin_country != "US") %>% 
  group_by(year, month) %>% 
  summarise(sum_frgt = sum(freight) / 1000000000) %>% 
  ggplot(aes(x = month, y = sum_frgt)) +
  geom_line(aes(color = year)) +
  xlab("Month") +
  ylab("Cargo Count (in billions)")

cargo_us_only + cargo_intl_only

# split report up
  # maybe big airlines vs smaller
# add COVID data - NYTimes github COVID

# By Airline ------------------------------------------------------------
# by US airline
filtered_join_c %>% 
  filter(class == "F", carrier_group == 3) %>% 
  group_by(year, month, carrier_name) %>% 
  summarise(sum_psgr = sum(passengers) / 1000000) %>% 
  ggplot(aes(x = month, y = sum_psgr)) +
  geom_line(aes(color = year)) +
  facet_wrap(~carrier_name, scales = "free_y") +
  xlab("month") +
  ylab("Passenger Count (in millions)") 




# * Popular Routes --------------------------------------------------------

# scheduled passenger service mainline in 2019
filtered_join_c %>% 
  filter(class == "F", carrier_group == 3, year == 2019, month == c(3,4,5)) %>% 
  arrange(desc(passengers)) %>% 
  head(n = 10)

# scheduled passenger service mainline in 2020
filtered_join_c %>% 
  filter(class == "F", carrier_group == 3, year == 2020, month == c(3,4,5)) %>% 
  arrange(desc(passengers)) %>% 
  head(n = 10)


# Maps --------------------------------------------------------------------
# sample map
usa <- map_data("usa")
usa <- ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "white", color = "purple") +
  coord_fixed(1.3)
usa

# coordinates of Northwestern
northwestern <- data.frame(
  long = -87.6739,
  lat = 42.0549,
  name = "Northwestern",
  stringsAsFactors = FALSE
)

# adding Northwestern
usa +
  geom_point(data = northwestern, aes(x = long, y = lat), color = "red", size = 5) +
  geom_point(data = northwestern, aes(x = long, y = lat), color = "yellow", size = 4)

# states
states <- map_data("state")
states

# plotting states
states_plot <- ggplot(data = states) +
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") +
  coord_fixed(1.3) +
  guides(fill = FALSE) + # leaves off color legend
  geom_point(data = northwestern, aes(x = long, y = lat), color = "red", size = 5) +
  geom_point(data = northwestern, aes(x = long, y = lat), color = "purple", size = 4)
ggsave(file = "states_with_northwestern.svg", plot = states_plot, width = 10, height = 3)



# Getting number of passengers --------------------------------------------
## WANT TO SHADE BY PROPORTION OF FLIGHTS STILL FLOWN 2020 COMPARED TO 2019
# grouping flights by dest, month using semi_join in the state/coordinates
flights_states_dest_2019 <- filtered_join_c %>% 
  filter(year == 2019) %>% 
  group_by(dest_state_nm, year, month) %>% 
  mutate(dest_state_nm = tolower(dest_state_nm)) %>% 
  nest() %>% 
  semi_join(states, by = c("dest_state_nm" = "region"))
flights_states_dest_2019 %>% 
  arrange(month, dest_state_nm)

# get num passengers for a given month/state/year
get_num_psgr <- function(tibble, row_num){
  num_psgr = sum(tibble$data[[row_num]]$passengers)
  num_psgr
}

flights_states_dest_2019_1 <- flights_states_dest_2019 %>% 
  mutate(num_psgr_2019 = 0)
for(i in 1:nrow(flights_states_dest_2019_1)){
  flights_states_dest_2019_1$num_psgr_2019[[i]] = get_num_psgr(flights_states_dest_2019_1, i)
}
flights_states_dest_2019_1


# will need to replicate for 2020
flights_states_dest_2020 <- filtered_join_c %>% 
  filter(year == 2020) %>% 
  group_by(dest_state_nm, year, month) %>% 
  mutate(dest_state_nm = tolower(dest_state_nm)) %>% 
  nest() %>% 
  semi_join(states, by = c("dest_state_nm" = "region")) %>% 
  arrange(month, dest_state_nm)
flights_states_dest_2020

# repeat for 2020
flights_states_dest_2020_1 <- flights_states_dest_2020 %>% 
  mutate(num_psgr_2020 = 0)
for(i in 1:nrow(flights_states_dest_2020_1)){
  flights_states_dest_2020_1$num_psgr_2020[[i]] = get_num_psgr(flights_states_dest_2020_1, i)
}
flights_states_dest_2020_1

# merge using left join
flights_states_dest_prop <- flights_states_dest_2019_1 %>% 
  select(-"data") %>% 
  left_join(flights_states_dest_2020_1, by = "dest_state_nm") %>% 
  select(-c("data", "month.y", "year.y")) %>% 
  mutate(prop_2020_to_2019 = num_psgr_2020 / num_psgr_2019) %>% 
  rename("month" = "month.x", "year" = "year.x") %>% 
  select(-"year")


# looks like Delaware is infinite
flights_states_dest_prop %>% 
  filter(prop_2020_to_2019 == Inf)

# removing Delaware
flights_states_dest_prop_c <- flights_states_dest_prop %>% 
  filter(dest_state_nm != "delaware")
flights_states_dest_prop_c



# NUMBER OF PASSENGERS ORIGIN ---------------------------------------------
## WANT TO SHADE BY PROPORTION OF FLIGHTS STILL FLOWN 2020 COMPARED TO 2019
# grouping flights by dest, month using semi_join in the state/coordinates
flights_states_origin_2019 <- filtered_join_c %>% 
  filter(year == 2019) %>% 
  group_by(origin_state_nm, year, month) %>% 
  mutate(origin_state_nm = tolower(origin_state_nm)) %>% 
  nest() %>% 
  semi_join(states, by = c("origin_state_nm" = "region"))
flights_states_origin_2019 %>% 
  arrange(month, origin_state_nm)


flights_states_origin_2019_1 <- flights_states_origin_2019 %>% 
  mutate(num_psgr_2019 = 0)
for(i in 1:nrow(flights_states_origin_2019_1)){
  flights_states_origin_2019_1$num_psgr_2019[[i]] = get_num_psgr(flights_states_origin_2019_1, i)
}
flights_states_origin_2019_1


# will need to replicate for 2020
flights_states_origin_2020 <- filtered_join_c %>% 
  filter(year == 2020) %>% 
  group_by(origin_state_nm, year, month) %>% 
  mutate(origin_state_nm = tolower(origin_state_nm)) %>% 
  nest() %>% 
  semi_join(states, by = c("origin_state_nm" = "region")) %>% 
  arrange(month, origin_state_nm)
flights_states_origin_2020

# repeat for 2020
flights_states_origin_2020_1 <- flights_states_origin_2020 %>% 
  mutate(num_psgr_2020 = 0)
for(i in 1:nrow(flights_states_origin_2020_1)){
  flights_states_origin_2020_1$num_psgr_2020[[i]] = get_num_psgr(flights_states_origin_2020_1, i)
}
flights_states_origin_2020_1

# merge using left join
flights_states_origin_prop <- flights_states_origin_2019_1 %>% 
  select(-"data") %>% 
  left_join(flights_states_origin_2020_1, by = "origin_state_nm") %>% 
  select(-c("data", "month.y", "year.y")) %>% 
  mutate(prop_2020_to_2019 = num_psgr_2020 / num_psgr_2019) %>% 
  rename("month" = "month.x", "year" = "year.x") %>% 
  select(-"year")


# looks like Delaware is infinite
flights_states_origin_prop %>% 
  filter(prop_2020_to_2019 == Inf)

# removing Delaware
flights_states_origin_prop_c <- flights_states_origin_prop %>% 
  filter(origin_state_nm != "delaware")
flights_states_origin_prop_c


# Get number of freight ---------------------------------------------------
## WANT TO SHADE BY PROPORTION OF FLIGHTS STILL FLOWN 2020 COMPARED TO 2019
# grouping flights by dest, month using semi_join in the state/coordinates
# get num passengers for a given month/state/year
get_amt_frgt <- function(tibble, row_num){
  amt_frgt = sum(tibble$data[[row_num]]$freight)
  amt_frgt
}

flights_states_dest_2019_1_f <- flights_states_dest_2019 %>% 
  mutate(amt_frgt_2019 = 0)
for(i in 1:nrow(flights_states_dest_2019_1_f)){
  flights_states_dest_2019_1_f$amt_frgt_2019[[i]] = get_amt_frgt(flights_states_dest_2019_1_f, i)
}
flights_states_dest_2019_1_f


# will need to replicate for 2020
# repeat for 2020
flights_states_dest_2020_1_f <- flights_states_dest_2020 %>% 
  mutate(amt_frgt_2020 = 0)
for(i in 1:nrow(flights_states_dest_2020_1_f)){
  flights_states_dest_2020_1_f$amt_frgt_2020[[i]] = get_amt_frgt(flights_states_dest_2020_1_f, i)
}
flights_states_dest_2020_1_f

# merge using left join
flights_states_dest_prop_f <- flights_states_dest_2019_1_f %>% 
  select(-"data") %>% 
  left_join(flights_states_dest_2020_1_f, by = "dest_state_nm") %>% 
  select(-c("data", "month.y", "year.y")) %>% 
  mutate(prop_2020_to_2019_f = amt_frgt_2020 / amt_frgt_2019) %>% 
  rename("month" = "month.x", "year" = "year.x")


# looks like Delaware is infinite
flights_states_dest_prop_f %>% 
  filter(prop_2020_to_2019_f == Inf)

# removing Delaware
flights_states_dest_prop_f_c <- flights_states_dest_prop_f %>% 
  filter(dest_state_nm != "delaware") %>% 
  select(-"year")
flights_states_dest_prop_f_c



# * States with Flights ---------------------------------------------------
# creating flights by state APRIL
flights_by_state <- filtered_join_c %>% 
  filter(year == 2020) %>% 
   group_by(dest_state_nm, month) %>% 
  summarise(sum_psgr = sum(passengers)) %>% 
  mutate(dest_state_nm = tolower(dest_state_nm)) %>% 
  nest() %>% 
  right_join(states, by = c("dest_state_nm" = "region"))
#view(flights_by_state)

# mapping flights by state APRIL
flights_by_state_apr <- flights_by_state %>% 
  unnest(data) %>% 
  filter(month == 4)
flights_by_state_apr_plot <- flights_by_state_apr %>% 
    ggplot() +
    geom_polygon(aes(x = long, y = lat, fill = sum_psgr, group = group), color = "white") +
    coord_fixed(1.3) +
    geom_point(data = northwestern, aes(x = long, y = lat), color = "red", size = 5) +
    geom_point(data = northwestern, aes(x = long, y = lat), color = "purple", size = 4)
flights_by_state_apr_plot

# mapping flights by state MAY
flights_by_state_may <- flights_by_state %>% 
  unnest(data) %>% 
  filter(month == 5)
flights_by_state_may_plot <- flights_by_state_may %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, fill = sum_psgr, group = group), color = "white") +
  coord_fixed(1.3) +
  geom_point(data = northwestern, aes(x = long, y = lat), color = "red", size = 5) +
  geom_point(data = northwestern, aes(x = long, y = lat), color = "purple", size = 4)

flights_by_state_apr_plot + flights_by_state_may_plot

# mapping flights by state ALL MONTHS
flights_by_state_all <- flights_by_state %>% 
  unnest(data)
flights_by_state_all_plot <- flights_by_state_all %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, fill = sum_psgr, group = group), color = "white") +
  coord_fixed(1.3) +
  geom_point(data = northwestern, aes(x = long, y = lat), color = "red", size = 5) +
  geom_point(data = northwestern, aes(x = long, y = lat), color = "purple", size = 4) +
  facet_wrap(~month)
flights_by_state_all_plot
flights_by_state_all


# states by proportion of previous flights by month PASSENGERS
flights_by_prop <- flights_states_dest_prop_c %>% 
  right_join(states, by = c("dest_state_nm" = "region"))
flights_by_prop

flights_by_prop_plot <- flights_by_prop %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, fill = prop_2020_to_2019, group = group), color = "white") +
  coord_fixed(1.3) +
  geom_point(data = northwestern, aes(x = long, y = lat), color = "red", size = 5) +
  geom_point(data = northwestern, aes(x = long, y = lat), color = "purple", size = 4) +
  facet_wrap(~month)
flights_by_prop_plot

flights_states_dest_prop_c %>% 
  filter(month == c(3,4,5)) %>% 
  arrange(desc(prop_2020_to_2019)) %>% 
  head(5)


# states by proportion of previous flights by month FREIGHT
flights_by_prop_f <- flights_states_dest_prop_f_c %>% 
  right_join(states, by = c("dest_state_nm" = "region"))
flights_by_prop_f

flights_by_prop_f_plot <- flights_by_prop_f %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, fill = prop_2020_to_2019_f, group = group), color = "white") +
  coord_fixed(1.3) +
  geom_point(data = northwestern, aes(x = long, y = lat), color = "red", size = 5) +
  geom_point(data = northwestern, aes(x = long, y = lat), color = "purple", size = 4) +
  facet_wrap(~month)
flights_by_prop_f_plot





# comparing total and prop


# by prop VS by COVID cases

flights_states_dest_prop_c




# states by proportion of previous flights in APRIL by LOWERING PROP THRESHOLD
flights_by_prop %>% 
  filter(month == 4) %>% 
  filter(prop_2020_to_2019 <= 0.60) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, fill = prop_2020_to_2019, group = group), color = "white") +
  coord_fixed(1.3) +
  geom_point(data = northwestern, aes(x = long, y = lat), color = "red", size = 5) +
  geom_point(data = northwestern, aes(x = long, y = lat), color = "purple", size = 4) 




# * States with COVID-19 --------------------------------------------------
# adding COVID-19 cases to states AND population data
covid_states <- covid %>% 
  left_join(pop, by = c("state" = "NAME")) %>% 
  select(-c("SUMLEV", "STATE", "REGION", "DIVISION", "POPEST18PLUS2019", "PCNT_POPEST18PLUS")) %>% 
  mutate(state = tolower(state)) %>% 
  rename("popestimate2019" = "POPESTIMATE2019") %>% 
  mutate(covid_per_cap = cases / popestimate2019) %>% 
  left_join(states, by = c("state" = "region"))
covid_states

pop

# plotting covid states
states_covid_plot <- ggplot(data = covid_states) +
  geom_polygon(aes(x = long, y = lat, fill = covid_per_cap, group = group), color = "white") +
  coord_fixed(1.3) +
  geom_point(data = northwestern, aes(x = long, y = lat), color = "red", size = 5) +
  geom_point(data = northwestern, aes(x = long, y = lat), color = "purple", size = 4) +
  facet_wrap(~month)
states_covid_plot

# flights vs. cases total for DEST
flights_states_dest_2020_1 %>% 
  left_join(covid_states, by = c("dest_state_nm" = "state", "month" = "month")) %>% 
  ggplot() +
    geom_point(aes(x = covid_per_cap, y = num_psgr_2020))

# flights vs. cases total for DEST - looking for outliers
flights_states_dest_2020_1 %>% 
  left_join(covid_states, by = c("dest_state_nm" = "state", "month" = "month")) %>% 
  filter(covid_per_cap >= 0.00375 & covid_per_cap <= 0.0042) %>% 
  arrange(desc(num_psgr_2020))



# flights vs. cases total for ORIGIN
flights_states_origin_2020_1 %>% 
  left_join(covid_states, by = c("origin_state_nm" = "state", "month" = "month")) %>% 
  ggplot() +
  geom_point(aes(x = covid_per_cap, y = num_psgr_2020))

# flights vs. cases total for ORIGIN - looking for outliers
flights_states_origin_2020_1 %>% 
  left_join(covid_states, by = c("origin_state_nm" = "state", "month" = "month")) %>% 
  filter(covid_per_cap >= 0.00375 & covid_per_cap <= 0.0042) %>% 
  arrange(desc(num_psgr_2020))


# distance Group ----------------------------------------------------------
# plotting COMMERCIAL MAINLINE carrierS BY TRIP LENGTH
filtered_join_c %>% 
  filter(class == "F", carrier_group == 3) %>% 
  group_by(distance_group, year) %>% 
  summarise(sum_psgr = sum(passengers)) %>%
  ggplot(aes(x = distance_group, y = sum_psgr / 1000000, fill=(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "year",
                      breaks = c(2019, 2020),
                      labels = c("2019", "2020")) +
  ylab("Passenger Count (in millions)") +
  scale_x_continuous(breaks = round(seq((0), 20, by = 1)), "Trip Length")
  
# plotting freight carrierS BY TRIP LENGTH
filtered_join_c %>% 
  filter(class == c("G", "P")) %>% 
  group_by(distance_group, year) %>% 
  summarise(sum_freight = sum(freight)) %>%
  ggplot(aes(x = distance_group, y = sum_freight / 1000000, fill=(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "year",
                      breaks = c(2019, 2020),
                      labels = c("2019", "2020")) +
  ylab("Passenger Count (in millions)") +
  scale_x_continuous(breaks = round(seq((0), 20, by = 1)), "Trip Length")



# region ------------------------------------------------------------------
# Atlantic vs. Pacific by region each month
filtered_join_c %>% 
  filter(region == c("A", "P")) %>% 
  group_by(region, year, month) %>% 
  summarise(sum_passengers = sum(freight) / 1000000) %>% 
  ggplot(aes(x = region, y = sum_passengers, fill = (year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "year",
                      breaks = c(2019, 2020),
                      labels = c("2019", "2020)")) +
  ylab("freight Count (in millions)") +
  facet_wrap(~month, scales = "free_y")
  

# Atlantic vs. Pacific by region each month for freight as BAR
filtered_join_c %>% 
  filter(region == c("A", "P"), class == c("G", "P")) %>% 
  group_by(region, year, month) %>% 
  summarise(sum_freight = sum(freight) / 1000000) %>% 
  ggplot(aes(x = region, y = sum_freight, fill = (year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "year",
                      breaks = c(2019, 2020),
                      labels = c("2019", "2020)")) +
  ylab("Freight Weight (in millions)") +
  facet_wrap(~month, scales = "free_y")

# Atlantic vs. Pacific by region each month for freight as LINE INBOUND
filtered_join_c %>% 
  filter(region == c("A", "P"), class == c("G", "P"), dest_country == "US") %>% 
  group_by(region, year, month) %>% 
  summarise(sum_freight = sum(freight) / 1000000) %>% 
  ggplot(aes(x = month, y = sum_freight)) +
  geom_line(aes(color = year)) +
  xlab("Month") +
  ylab("freight Weight (in millions)") +
  facet_wrap(~region, scales = "free_y")


filtered_join_c %>% 
  filter(region == c("A", "P"), class == c("G", "P"), dest_country == "US") %>% 
  group_by(region, year, month) %>% 
  summarise(sum_freight = sum(freight) / 1000000) %>% 
  ggplot(aes(x = month, y = sum_freight)) +
  geom_line(aes(color = year)) +
  xlab("Month") +
  ylab("Freight Weight (in millions)") +
  facet_wrap(~region, scales = "free_y")

# Atlantic vs. Pacific by region each month for freight as LINE OUTBOUND
filtered_join_c %>% 
  filter(region == c("A", "P"), class == c("G", "P"), origin_country == "US") %>% 
  group_by(region, year, month) %>% 
  summarise(sum_freight = sum(freight) / 1000000) %>% 
  ggplot(aes(x = month, y = sum_freight)) +
  geom_line(aes(color = year)) +
  xlab("Month") +
  ylab("freight Count (in millions)") +
  facet_wrap(~region, scales = "free_y")


# * Jet Engines -----------------------------------------------------------
# By year, by jet engines over months
filtered_join_c %>% 
  filter(aircraft_group == c("6","7","8"), class == c("G", "P")) %>% 
  group_by(aircraft_group, year, month) %>%
  summarise(num_flights = n()) %>% 
  ggplot(aes(x = month, y = num_flights)) +
  geom_line(aes(color = year)) +
  xlab("month") +
  ylab("Count") +
  facet_wrap(~aircraft_group, scales = "free_y")


# distance -----------------------------------------------
filtered_join_c %>% 
  group_by(year, month) %>% 
  summarise(num_flights = n(), distance = distance) %>% 
  ggplot(aes(x = distance, y = num_flights )) +
  geom_line(aes(color = year)) +
  xlab("month") +
  ylab("Count") +
  facet_wrap(~month, scales = "free_y")


# Codebook ----------------------------------------------------------------
# dataMaid::makeCodebook(filtered_join_c, replace = TRUE)

# Creating RDS ------------------------------------------------------------
write_rds(filtered_join_c, "Data/processed/processed_flight_data.rds")
write_csv(filtered_join_c, "Data/processed/processed_flight_data.csv")