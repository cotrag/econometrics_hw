county_mobility_data <- read_dta("online_table2-2.dta")

head(county_mobility_data)


county_housing_prices <- read_csv("county_housing data.csv")

head(county_housing_prices)

county_housing_prices$`1/31/21`
county_housing_prices$`12/31/20`


county_housing_prices_long <- county_housing_prices %>% 
  pivot_longer(!c(RegionID, SizeRank, RegionName, RegionType, StateName, State,
                 Metro, StateCodeFIPS, MunicipalCodeFIPS), 
               names_to = "date", values_to = "price")

county_housing_prices_long$date

county_housing_prices_long$date <- mdy(county_housing_prices_long$date)


county_housing_prices_long$date

county_housing_prices_long <- county_housing_prices_long %>% 
  filter(date > "2010-01-01" & date < "2020-01-01")

county_housing_prices_long$date

county_housing_prices_long$RegionName <- str_remove(county_housing_prices_long$RegionName,
                                                    "County")

county_housing_prices_long$RegionName <- str_replace(county_housing_prices_long$RegionName,
                                                     " ", "")





county_housing_prices_ohio <- county_housing_prices_long %>% 
  filter(State == "OH")


county_mobility_data_ohio <- county_mobility_data %>% 
  filter(stateabbrv == "OH")



joined_ds_oh <- stringdist_join(county_housing_prices_ohio, county_mobility_data_ohio,
                          by = c("RegionName" = "county_name"))

joined_ds_oh

dataset_merge_function <- function(state_for_function) {
  
  filtered_price_function <-  county_housing_prices_long %>% 
    filter(State == state_for_function)
  
  filtered_mobility_function <- county_mobility_data %>% 
    filter(stateabbrv == state_for_function)
  
  joined_function_ds <- stringdist_join(filtered_price_function, filtered_mobility_function,
                                  by = c("RegionName" = "county_name"))
  
  return(joined_function_ds)
  
}


dataset_merge_function("IL")


state_vector_paper_final <- unique(county_housing_prices_long$State)

state_vector_paper_final



mapped_dataset <- map(state_vector_paper_final, dataset_merge_function)



binded_dataset <- do.call("rbind", mapped_dataset)

binded_dataset$pct_causal_p75_kir26_m


binded_dataset <- as.data.frame(binded_dataset)

binded_dataset$pct_causal_p75_kir26_m

binded_dataset_nas <- binded_dataset %>% 
  filter(is.na(pct_causal_p75_kir26_m))


missing_counties <- unique(binded_dataset_nas$RegionName)

missing_counties

# there are some missing values-- presumably because the counties are too small


# Just build a table for now


binded_dataset$pct_causal_p25_kir26

binded_dataset$price

binded_dataset_mean_price <- binded_dataset %>% 
  group_by(RegionName) %>% 
  mutate(avg_price = mean(price, na.rm = TRUE))

binded_dataset_mean_price$RegionID



binded_dataset_mean_price <- binded_dataset %>% 
 # group_by(State)
  group_by(RegionID) %>% 
  summarise(mean_price = mean(price, na.rm = TRUE), 
            causal_exposure_effs_25th = pct_causal_p25_kir26,
            state = State,
            county = RegionName,
            causal_exposure_effs_75th = pct_causal_p75_kir26)

binded_dataset_mean_price

# 

mean(binded_dataset$price, na.rm = TRUE)


binded_dataset_ungroup <- ungroup(binded_dataset)



binded_dataset_ungroup %>% 
 # group_by(RegionID) %>% 
  summarize("Mean Price" = mean(price, na.rm = TRUE),
            "Mean Estimated Change in Child Income at age 26 by Spending 1 year in 
            Given County, p25" = 
              round(mean(pct_causal_p25_kir26, na.rm = TRUE), 3),
            "Mean Estimated Change in Child Income at age 26 by Spending 1 year in 
            Given County, p75" = 
              round(mean(pct_causal_p75_kir26, na.rm = TRUE), 3)) %>% 
  knitr::kable() %>% 
  kable_styling("striped", "hover", full_width=F)


binded_dataset_mean_price$causal_exposure_effs_25th

ggplot(binded_dataset_mean_price, aes(x = mean_price, y = causal_exposure_effs_25th)) +
  geom_point()

binded_dataset

ggplot(binded_dataset, aes(x = date, y = price)) +
  geom_point()


# randomly select 5 lines to graph

set.seed(4532)

sample_selection <- sample_n(binded_dataset_ungroup, 5)

sample_selection


sample_data <- binded_dataset_ungroup %>% 
  filter(RegionID == c(2261, 1961, 1182, 2803, 259))



sample_data

ggplot(sample_data, aes(x = date, y = price, color = RegionName)) +
  geom_line() +
  labs(title = "ZHVI Over Time",
       x = "Date",
       y = "Price",
       caption = "Source: Zillow Research Data") +
  theme_classic() +
  scale_y_continuous(labels = comma) +
  scale_color_discrete(name = "County")
