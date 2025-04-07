library(zipcodeR)
library(tidygeocoder)
library(ecmwfr)
library(ncdf4)
library(raster)
library(dplyr)
library(ggplot2)
library(DescTools)
library(weathermetrics)
library(ThermIndex)
library(tigris)
library(sf)
library(viridis)
library(USA.state.boundaries)
library(gridExtra)

#remove extra characters
zip_codes <- data_wide %>%
  mutate(TX_CTR_ZIP = normalize_zip(TX_CTR_ZIP)) %>% 
  group_by(TX_CTR_ZIP) %>%
  summarize(count = n())

#obtain geodata
zip_codes<- reverse_zipcode(zip_codes$TX_CTR_ZIP)
zip_codes<- zip_codes %>% mutate(country="US")
zip_codes<- zip_codes %>% dplyr::select(zipcode, major_city, county, state, country)

#geocode, no county due to incomplete data
coordinates<- geocode(zip_codes, address=NULL, postalcode=zipcode, city=major_city, state=state, country=country)

#round to match coordinates (approximate)
coordinates_adjusted <- coordinates %>%
  mutate(
    long_adjusted = RoundTo(long, multiple = 0.50, FUN = round),
    lat_adjusted =  RoundTo(lat, multiple = 0.50, FUN = round)
  )

#climate data 1, read file from ERA5
grib_file_path <- "84546023019c2a189c27789c9464ed19.grib" 
grib_data <- brick(grib_file_path)
df <- as.data.frame(grib_data, xy = TRUE)
climate <- df %>% filter (y>=17 & y<=49)
climate <- climate %>% filter (x>=-159 & x<=-65)
rm(grib_file_path, df, grib_data)

climate <- climate %>%
  mutate(across(3:356, ~ . - 273.15))

# merge
climate_centers <- coordinates_adjusted %>%
  inner_join(climate, 
             by = c("long_adjusted" = "x", 
                    "lat_adjusted" = "y"))
rm(climate)

#climate data 2
grib_file_path <- "ce4067f9d7f35f007baa0255ee171dcf.grib"  
grib_data <- brick(grib_file_path)
df <- as.data.frame(grib_data, xy = TRUE)
climate <- df %>% filter (y>=17 & y<=49)
climate <- climate %>% filter (x>=-159 & x<=-65)
rm(grib_file_path, df, grib_data)

#merge
climate_centers_final <- climate_centers %>%
  inner_join(climate, 
             by = c("long_adjusted" = "x", 
                    "lat_adjusted" = "y"))
rm(climate, climate_centers)

colnames(climate_centers_final) <- gsub("\\.", "_", colnames(climate_centers_final))
colnames(climate_centers_final) <- gsub("_+", "_", colnames(climate_centers_final))
colnames(climate_centers_final) <- gsub("_{2,}", "_", colnames(climate_centers_final))
colnames(climate_centers_final) <- gsub(".*?(Relative_humidity|Temperature)", "\\1", colnames(climate_centers_final))
colnames(climate_centers_final) <- gsub("SFC_Ground_or_water_surface_", "", colnames(climate_centers_final))

climate_centers_simplified <- climate_centers_final[, -c(364:717)]

heat_index_counter <- 1

# Loop through the combinations of C_# and RH_#
for (i in 1:177) {
  temp_col <- paste0("2_metre_temperature_C_", i)
  dp_col <- paste0("2_metre_dewpoint_temperature_C_", i)
  
  # Check if both columns exist
  if (temp_col %in% colnames(climate_centers_simplified) && dp_col %in% colnames(climate_centers_simplified)) {
    temperature <- climate_centers_simplified[[temp_col]]
    dewpoint <- climate_centers_simplified[[dp_col]]
    
    # Initialize heat_index vector
    heat_index <- temperature  # default to temp
    
    # Identify where temp is ≥ 27
    high_temp_idx <- which(temperature >= 27)
    
    # Compute heat index only for those rows
    if (length(high_temp_idx) > 0) {
      heat_index_calc <- heat.index(
        t = temperature[high_temp_idx],
        dp = dewpoint[high_temp_idx],
        temperature.metric = "celsius",
        output.metric = "celsius",
        round = 2
      )
      heat_index[high_temp_idx] <- heat_index_calc
    }

    # Save the result in a new column
    climate_centers_simplified[[paste0("heat_index", heat_index_counter)]] <- heat_index
    
    # Increment the counter
    heat_index_counter <- heat_index_counter + 1
  }
}

#average daily HI
cols_to_average <- heat_indices[, 2:46473]
num_groups <- ncol(cols_to_average) %/% 8 
averages_matrix <- matrix(ncol = num_groups, nrow = nrow(cols_to_average))

for (i in seq(1, num_groups * 8, by = 8)) {
  avg_group <- rowMeans(cols_to_average[, i:min(i + 7, ncol(cols_to_average))], na.rm = TRUE)
  
  averages_matrix[, (i - 1) %/% 8 + 1] <- avg_group
}

averages_df <- as.data.frame(averages_matrix)
colnames(averages_df) <- paste0("heat_index_", seq(1, ncol(averages_df)))
heat_indices_substituted <- cbind(heat_indices[, 1, drop = FALSE], averages_df)

heat_index_dates <- seq(from = as.Date("2009-01-01"), to = as.Date("2024-11-26"), by = "day")
period_mapping <- data.frame(
  period = 1:length(heat_index_dates),
  date = heat_index_dates
)

period_mapping <- period_mapping %>% mutate(
  year= year(date),
  month = month(date),
  day=day(date),
) %>% dplyr::select(-date)

heat_indices_long <- heat_indices_substituted %>%
  pivot_longer(
    cols = starts_with("heat_index_"),
    names_to = "period",
    names_pattern = "heat_index_(\\d+)",
    values_to = "heat_index_value"
  ) %>%
  mutate(period = as.numeric(period))

heat_indices_accumulated <- heat_indices_long %>% group_by (zipcode) %>% summarize(overall_HI=sum(heat_index_value), HEAT_30_daily=sum(heat_index_value>=30)) 
heat_indices_accumulated$overall_HI_quartile <- cut(heat_indices_accumulated$HEAT_30_daily, 
                                                    breaks = quantile(heat_indices_accumulated$HEAT_30_daily, probs = seq(0, 1, 0.25), na.rm = TRUE), 
                                                    include.lowest = TRUE, 
                                                    labels = c("Q1", "Q2", "Q3", "Q4"))

