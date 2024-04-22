library(sf)
library(sp)
library(GWmodel)

prepare_sf <- function(data) {
  st_as_sf(data, coords = c("INTPTLON", "INTPTLAT"), crs = 4326, remove = FALSE)
}

fit_artificial_gwr <- function(data) {
  if (!inherits(data, "sf")) {
    data <- prepare_sf(data)
  }
  data_sp <- as(data, "Spatial")
  # Include new covariates in the model formula
  merged_gwr_bw <- bw.gwr(DIABETES_CrudePrev ~ NatWalkInd + OBESITY_CrudePrev + BPHIGH_CrudePrev + LPA_CrudePrev + CSMOKING_CrudePrev + AvgSummerTemp + MedianHHIncome,
                          data = data_sp,
                          kernel = "exponential",
                          adaptive = TRUE)
  # Include new covariates in the model fitting
  gwr.basic(DIABETES_CrudePrev ~ NatWalkInd + OBESITY_CrudePrev + BPHIGH_CrudePrev + LPA_CrudePrev + CSMOKING_CrudePrev + AvgSummerTemp + MedianHHIncome,
            data = data_sp,
            bw = merged_gwr_bw,
            kernel = "exponential")
}


