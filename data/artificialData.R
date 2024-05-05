library(dplyr)
library(mvtnorm)
library(sf)
library(spdep)
library(spatialreg)
library(sf)
library(gstat)
set.seed(123)

create_data <- function(n_counties = 3244) {
  columns_to_drop <- c("DIABETES_CrudePrev", "BPHIGH_CrudePrev", "OBESITY_CrudePrev", 
                       "LPA_CrudePrev", "CSMOKING_CrudePrev", "avg_temp", "Median.Household.Income", "NatWalkInd")
  
  
  ## Artificial Data using Correlation Matrix
  simulation_data <- select(new_spatial_dataset, -one_of(columns_to_drop))
  
  cov_matrix <- cov(new_spatial_dataset %>% select(columns_to_drop), use = "complete.obs")  
  
  simulated_data <- rmvnorm(n = nrow(simulation_data), mean = c(mean(mean(new_spatial_dataset$BPHIGH_CrudePrev),mean(new_spatial_dataset$OBESITY_CrudePrev),
                                                                mean(new_spatial_dataset$LPA_CrudePrev),mean(new_spatial_dataset$CSMOKING_CrudePrev),mean(new_spatial_dataset$avg_temp),
                                                                mean(new_spatial_dataset$Median.Household.Income),mean(new_spatial_dataset$NatWalkInd)), sigma = cov_matrix))
  colnames(simulated_data) <- columns_to_drop
  simulated_data <- data.frame(simulated_data)
  

  
  
  
  tibble(
    CountyFIPS = simulation_data$CountyFIPS,
    NatWalkInd = simulated_data$NatWalkInd,
    StateAbbr = simulation_data$StateAbbr,
    DIABETES_CrudePrev = simulated_data$DIABETES_CrudePrev,
    BPHIGH_CrudePrev = simulated_data$BPHIGH_CrudePrev,
    OBESITY_CrudePrev = simulated_data$OBESITY_CrudePrev,
    LPA_CrudePrev = simulated_data$LPA_CrudePrev,
    CSMOKING_CrudePrev = simulated_data$CSMOKING_CrudePrev,
    AvgSummerTemp = simulated_data$avg_temp,
    MedianHHIncome =  simulated_data$Median.Household.Income ,
    INTPTLAT = simulation_data$INTPTLAT,
    INTPTLON = simulation_data$INTPTLON,
    geometry = simulation_data$geometry

  )
}


create_data2 <- function( new_spatial_dataset) {

  # Artificial Data using Correlation Matrix
  simulation_data <- new_spatial_dataset
  
  # Calculate the coefficients for each row based on the latitude and longitude
  coefficients <- t(apply(simulation_data[, c("INTPTLAT", "INTPTLON")], 1, function(row) {
    create_coefficients(row[1], row[2])
  }))
  
  # Combine the coefficients with the covariates to calculate the diabetes prevalence
  diabetes_prevalence <- rowSums(coefficients * simulation_data[, c("NatWalkInd", "BPHIGH_CrudePrev",
                                                                    "OBESITY_CrudePrev", "LPA_CrudePrev",
                                                                    "CSMOKING_CrudePrev", "avg_temp",
                                                                    "Median.Household.Income")])
  # 
  # # Add the diabetes prevalence to the simulated data
  simulation_data$DIABETES_CrudePrev_new <- diabetes_prevalence
  
  variable_names <- c("NatWalkInd", "BPHIGH_CrudePrev", "OBESITY_CrudePrev", 
                      "LPA_CrudePrev", "CSMOKING_CrudePrev", "avg_temp",
                      "Median.Household.Income")
  colnames(coefficients) <- paste0("coef_", variable_names)
  simulation_data <- cbind(simulation_data, coefficients)
  

  
  
  tibble(
    CountyFIPS = simulation_data$CountyFIPS,
    NatWalkInd = simulation_data$NatWalkInd,
    StateAbbr = simulation_data$StateAbbr,
    DIABETES_CrudePrev = simulation_data$DIABETES_CrudePrev,
    DIABETES_CrudePrev_new = simulation_data$DIABETES_CrudePrev_new,
    BPHIGH_CrudePrev = simulation_data$BPHIGH_CrudePrev,
    OBESITY_CrudePrev = simulation_data$OBESITY_CrudePrev,
    LPA_CrudePrev = simulation_data$LPA_CrudePrev,
    CSMOKING_CrudePrev = simulation_data$CSMOKING_CrudePrev,
    AvgSummerTemp = simulation_data$avg_temp,
    MedianHHIncome =  simulation_data$Median.Household.Income ,
    walk_coef = simulation_data$coef_NatWalkInd,
    bp_coef = simulation_data$coef_BPHIGH_CrudePrev,
    obesity_coef = simulation_data$coef_OBESITY_CrudePrev,
    lpa_coef = simulation_data$coef_LPA_CrudePrev,
    smoke_coef = simulation_data$coef_CSMOKING_CrudePrev,
    temp_coef = simulation_data$coef_avg_temp,
    income_coef = simulation_data$coef_Median.Household.Income,
    INTPTLAT = simulation_data$INTPTLAT,
    INTPTLON = simulation_data$INTPTLON,
    geometry = simulation_data$geometry
    
  )
  
  
}


create_coefficients <- function(lat,long) {

  lat <- as.numeric(lat)
  long <- as.numeric(long)
  walk_coef = 0.005*lat + 0.02*long
  bp_coef = 0.002*lat + 0.001*long
  obesity_coef = 0.001*long + (0.002 * lat)
  lpa_coef = (0.0005 * lat) + (0.002*long)
  smoke_coef = (0.0092*lat) + 0.01
  temp_coef = (0.01*long) + (0.001*lat)
  income_coef = (0.004 *lat) + (0.02*long)
  
  # Adjust coefficients to ensure the sum of products is under 100
  coefficients <- c(walk_coef, bp_coef, obesity_coef, lpa_coef, smoke_coef, temp_coef, income_coef)
  return(coefficients)

}


create_data_SAR <- function(n_counties = 3244) {
  
  
  
  ## Artificial Data using SAR Model
  new_spatial_dataset <- st_as_sf(new_spatial_dataset)
  w <- poly2nb(new_spatial_dataset)
  w <- nb2listw(w, style = "W")
  simulated_data<- new_spatial_dataset
  sar_model <- lagsarlm(DIABETES_CrudePrev ~ 1, data = new_spatial_dataset, listw = w)
  simulated_data$DIABETES_CrudePrev <- predict(sar_model, type = "response")
  sar_model <- lagsarlm(BPHIGH_CrudePrev ~ 1, data = new_spatial_dataset, listw = w)
  simulated_data$BPHIGH_CrudePrev <- predict(sar_model, type = "response")
  sar_model <- lagsarlm(OBESITY_CrudePrev ~ 1, data = new_spatial_dataset, listw = w)
  simulated_data$OBESITY_CrudePrev <- predict(sar_model, type = "response")
  sar_model <- lagsarlm(LPA_CrudePrev ~ 1, data = new_spatial_dataset, listw = w)
  simulated_data$LPA_CrudePrev <- predict(sar_model, type = "response")
  sar_model <- lagsarlm(CSMOKING_CrudePrev ~ 1, data = new_spatial_dataset, listw = w)
  simulated_data$CSMOKING_CrudePrev <- predict(sar_model, type = "response")
  sar_model <- lagsarlm(Median.Household.Income ~ 1, data = new_spatial_dataset, listw = w)
  simulated_data$MedianHHIncome <- predict(sar_model, type = "response")
  sar_model <- lagsarlm(avg_temp ~ 1, data = new_spatial_dataset, listw = w)
  simulated_data$AvgSummerTemp <- predict(sar_model, type = "response")
  sar_model <- lagsarlm(NatWalkInd ~ 1, data = new_spatial_dataset, listw = w)
  simulated_data$NatWalkInd <- predict(sar_model, type = "response")
  
  
  
  tibble(
    CountyFIPS = new_spatial_dataset$CountyFIPS,
    NatWalkInd = simulated_data$NatWalkInd,
    StateAbbr = new_spatial_dataset$StateAbbr,
    DIABETES_CrudePrev = simulated_data$DIABETES_CrudePrev,
    BPHIGH_CrudePrev = simulated_data$BPHIGH_CrudePrev,
    OBESITY_CrudePrev = simulated_data$OBESITY_CrudePrev,
    LPA_CrudePrev = simulated_data$LPA_CrudePrev,
    CSMOKING_CrudePrev = simulated_data$CSMOKING_CrudePrev,
    AvgSummerTemp = simulated_data$avg_temp,
    MedianHHIncome =  simulated_data$Median.Household.Income ,
    INTPTLAT = new_spatial_dataset$INTPTLAT,
    INTPTLON = new_spatial_dataset$INTPTLON,
    geometry = new_spatial_dataset$geometry
    
  )
}

# Function to create spatially varying functions
create_spatial_functions <- function(existing_sf_dataset) {
  
  if (!inherits(data, "sf")) {
    existing_sf_dataset <- prepare_sf(existing_sf_dataset)
  }
  out <- as(existing_sf_dataset, "Spatial")
  # Loop through each attribute in the dataset
  for (attribute in colnames(existing_sf_dataset)) {
    # Skip geometry column
    if (attribute == "geometry") next
    
    # Extract attribute values and polygon geometries
    attribute_values <- existing_sf_dataset[[attribute]]
    polygon_geometries <- st_geometry(existing_sf_dataset)
    
    # Perform spatial interpolation (e.g., kriging)
    # Adjust the interpolation method as needed
    interpolated_values <- krige(attribute_values ~ 1, locations = polygon_geometries)
    
    # Store interpolated values as spatially varying function
    existing_sf_dataset[[paste0(attribute, "_interpolated")]] <- interpolated_values$var1.pred
  }
  
  # Return modified dataset with spatially varying functions
  return(out)
}




