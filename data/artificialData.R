library(dplyr)
library(mvtnorm)
library(sf)
library(spdep)
library(spatialreg)
library(sf)
library(gstat)
set.seed(123)



## This function generates artifical spatially varying coefficients for an existing dataset for all the covariates and returns it
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

## This function generates spatially varying coefficients
create_coefficients <- function(lat,long) {

  lat <- as.numeric(lat)
  long <- as.numeric(long)
  walk_coef = 0.005*lat + 0.02*long
  bp_coef = 0.002*lat + 0.001*long
  obesity_coef = 0.001*long + (0.002 * lat)
  lpa_coef = (0.0005 * lat) + (0.002*long)
  smoke_coef = (0.0092*lat) + 0.01
  temp_coef = (0.01*long) + (0.001*lat)
  income_coef = (0.0000004 *lat) + (0.0000002*long)
  coefficients <- c(walk_coef, bp_coef, obesity_coef, lpa_coef, smoke_coef, temp_coef, income_coef)
  return(coefficients)

}






