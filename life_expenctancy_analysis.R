# Google Data Analytics Capstone Project
# The goal of this analysis is to identify correlations between characteristics
# of census tracts and their life expectancy
# Created: 03Febuary2025
# Author: Jacob Van Der Vaart

# packages
library(tidyverse)
library(janitor)


# data
# Social determinants of health data for 2010 to 2015 was grabbed from
# the Agency of Healthcare Research and Quality 
# https://www.ahrq.gov/sdoh/data-analytics/sdoh-data.html

sdoh_2010 <- readxl::read_xlsx("sdoh/sdoh_2010_tract_1_0.xlsx") 
sdoh_2011 <- readxl::read_xlsx("sdoh/sdoh_2011_tract_1_0.xlsx")
sdoh_2012 <- readxl::read_xlsx("sdoh/sdoh_2012_tract_1_0.xlsx")
sdoh_2013 <- readxl::read_xlsx("sdoh/sdoh_2013_tract_1_0.xlsx")
sdoh_2014 <- readxl::read_xlsx("sdoh/sdoh_2014_tract_1_0.xlsx")
sdoh_2015 <- readxl::read_xlsx("sdoh/sdoh_2015_tract_1_0.xlsx")

glimpse(sdoh_2010)
glimpse(sdoh_2011)
glimpse(sdoh_2012)
glimpse(sdoh_2013)
glimpse(sdoh_2014)
glimpse(sdoh_2015)

# remove capitals from column names
sdoh_2010 <- clean_names(sdoh_2010)
sdoh_2011 <- clean_names(sdoh_2011)
sdoh_2012 <- clean_names(sdoh_2012)
sdoh_2013 <- clean_names(sdoh_2013)
sdoh_2014 <- clean_names(sdoh_2014)
sdoh_2015 <- clean_names(sdoh_2015)

# Life Expectency data from CDC https://www.cdc.gov/nchs/data-visualization/life-expectancy/index.html
life_exp <- read_csv("U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015.csv")
glimpse(life_exp)

# clean column names to remove capitals and spaces
life_exp <- clean_names(life_exp)
colnames(life_exp)

# Select only columns that are present in each sdoh dataset and
# select only tracts that are present in each sdoh dataset
# identify columns
sdoh_dfs <- list(sdoh_2010,sdoh_2011,sdoh_2012,sdoh_2013,sdoh_2014,sdoh_2015)
common_variables <- Reduce(intersect, lapply(sdoh_dfs, colnames))

# identify tracts
sdoh_fips <- sdoh_dfs <- list(sdoh_2010$tractfips,sdoh_2011$tractfips,
                              sdoh_2012$tractfips,sdoh_2013$tractfips,
                              sdoh_2014$tractfips,sdoh_2015$tractfips)
common_tracts <- Reduce(intersect, lapply(sdoh_dfs, unique))

# select columns and filter rows
# arrange to be in the same order
sdoh_2010 <- sdoh_2010 |> select(all_of(common_variables))|> 
  filter(tractfips %in% common_tracts) |> 
  arrange(tractfips)
sdoh_2011 <- sdoh_2011 |>select(all_of(common_variables))|> 
  filter(tractfips %in% common_tracts)|> 
  arrange(tractfips)
sdoh_2012 <- sdoh_2012 |>select(all_of(common_variables))|> 
  filter(tractfips %in% common_tracts)|> 
  arrange(tractfips)
sdoh_2013 <- sdoh_2013 |>select(all_of(common_variables))|> 
  filter(tractfips %in% common_tracts)|> 
  arrange(tractfips)
sdoh_2014 <- sdoh_2014 |>select(all_of(common_variables))|> 
  filter(tractfips %in% common_tracts)|> 
  arrange(tractfips)
sdoh_2015 <- sdoh_2015 |>select(all_of(common_variables))|> 
  filter(tractfips %in% common_tracts)|> 
  arrange(tractfips)

# check order matches
unique(sdoh_2010$tractfips == sdoh_2011$tractfips & 
         sdoh_2010$tractfips == sdoh_2012$tractfips & 
         sdoh_2010$tractfips == sdoh_2013$tractfips &
         sdoh_2010$tractfips == sdoh_2014$tractfips &
         sdoh_2010$tractfips == sdoh_2015$tractfips)
########### 271 variables seems to be too many
# manually review and parse down later, or perhaps take only the most correlated? 


# create new dataframe that is the average for each variable across the 6 years.
common_variables
glimpse(sdoh_2010)
# first 8 variables are all identifiers
# hrsa_mua_census_tract is binary and does not need to be averaged.

# add identifiers for new dataframe, do not need col 1 (year) 
sdoh_avg <- sdoh_2010[2:8]

# find only numeric columns.
numeric_cols <- select(sdoh_2010,where(is.numeric)) |> colnames()
sdoh_2010[,numeric_cols] |> glimpse()

# remove year and territory before calculating averages
numeric_cols <- numeric_cols[-c(1,2)]
sdoh_2010[,numeric_cols] |> glimpse()


sdoh_dfs <- list(sdoh_2010,sdoh_2011,sdoh_2012,sdoh_2013,sdoh_2014,sdoh_2015)

# Ensure all dataframes only include numeric columns for averaging
sdoh_numeric_list <- map(sdoh_dfs, ~ select(.x, all_of(numeric_cols)))

# Compute the mean for numeric columns
sdoh_means <- reduce(sdoh_numeric_list, `+`) / length(sdoh_dfs)

sdoh_avg <- cbind(sdoh_avg,sdoh_means)

# clean census tract_number column to match TRACTFIPS
# life expectancy data frame lacks the leading digits that correspond to the state
# and county FIPS
# The end FIPS should be 11 digits SSCCCTTTTTT. where s = state c = county and t = tract

# remove decimal in tract number as it is not in sdoh data 
life_exp_clean <- life_exp |> 
  mutate(census_tract_number = str_remove(census_tract_number,"\\.")) 

# prepare life expectancy and sdoh data for merging
# create matching tract FIPS in sdoh data
sdoh_avg <- sdoh_avg |> 
  mutate(census_tract_number = str_sub(tractfips,-6))

# check 
unique(nchar(sdoh_avg$census_tract_number))
sdoh_avg$census_tract_number

# create matching county column. remove state name from life_exp county
life_exp_clean <- mutate(life_exp_clean, county = str_remove(county, ",.*"))

# merge life expectancy data with sdoh
sdoh_life_exp_df <- merge(sdoh_avg, life_exp_clean, by = c("state","county","census_tract_number"))

# calculate the population weighted average life expectancy across all census blocks
# remove nas and zero population census blocks
sdoh_life_exp_df <- sdoh_life_exp_df |>  
  filter(acs_tot_pop_wt > 0,
         !is.na(acs_tot_pop_wt),
         !is.na(life_expectancy))

avg_life_exp <- sum(sdoh_life_exp_df$life_expectancy * sdoh_life_exp_df$acs_tot_pop_wt)/sum(sdoh_life_exp_df$acs_tot_pop_wt)

# calculate life expectancy as difference from average
sdoh_life_exp_df <- sdoh_life_exp_df |> 
  mutate(life_exp_dif = round(life_expectancy - avg_life_exp,1),
         life_exp_pct_dif = round((life_expectancy - avg_life_exp)/avg_life_exp*100,1))

# calculate correlations with life_exp for each variable
# initialize matrix with number of rows for faster processing
correlation_df <- matrix( nrow = length(numeric_cols), ncol = 2)

# loop through all numeric variables and get correlation
# save variable name and correlation to matrix
for(i in 1:length(numeric_cols)){
  var <- numeric_cols[i]
  correlation_df[i,2] <- cor(sdoh_life_exp_df$life_expectancy,sdoh_life_exp_df[var], use = "complete.obs")
  correlation_df[i,1] <- var 
}

# convert matrix and correlation to dataframe and sort by correlation
correlation_df <- data.frame(correlation_df)
colnames(correlation_df) <- c("var","cor")
correlation_df <- arrange(correlation_df,desc(cor)) |> 
  mutate(cor = round(as.numeric(cor),2)) 
glimpse(correlation_df)



# save cleaned data to csv
write.csv(sdoh_life_exp_df,"cleaned_sdoh_and_life_exp.csv")

# subset to a smaller number of variables for analysis
cols_of_interest <- c("acs_median_hh_inc","acs_pct_bachelor_dgr","acs_pct_graduate_dgr",
                      "acs_per_capita_inc", "acs_pct_employed","acs_pct_10units",
                      "acs_pct_asian_comb","acs_pct_black_comb", "acs_pct_white_comb",
                      "acs_pct_hispanic","acs_pct_hh_limit_english", "acs_pct_hu_no_veh", 
                      "acs_pct_non_citizen","acs_pct_owner_hu","acs_pct_unemploy",
                      "pos_dist_clinic_tract","pos_dist_ed_tract", "cen_popdensity_tract")
life_exp_subset <- sdoh_life_exp_df |> 
  select(state:region,life_expectancy,life_exp_dif, life_exp_pct_dif, acs_tot_pop_wt,
         all_of(cols_of_interest) )

# subset Correlation
correlation_df |> filter(var %in% all_of(cols_of_interest))
write.csv("Correlation_data.csv")

# convert to a long format, placing measures of interest under a measure column
life_exp_subset_long <- life_exp_subset |> 
  pivot_longer(cols = all_of(cols_of_interest), names_to = "measure", values_to = "value")

write_csv(life_exp_subset_long,"life_expectancy_long.csv")


