library(sf)
library(sp)
library(GWmodel)
set.seed(123)
## Helper function to fit the GWR
prepare_sf <- function(data) {
  st_as_sf(data, coords = c("INTPTLON", "INTPTLAT"), crs = 4326, remove = FALSE)
}


## Fits the GWR Model on the artificial dataset
fit_artificial_gwr <- function(data) {
  if (!inherits(data, "sf")) {
    data <- prepare_sf(data)
  }
  data_sp <- as(data, "Spatial")

  merged_gwr_bw <- bw.gwr(DIABETES_CrudePrev_new ~ NatWalkInd + OBESITY_CrudePrev + BPHIGH_CrudePrev + LPA_CrudePrev + CSMOKING_CrudePrev + AvgSummerTemp + MedianHHIncome,
                          data = data_sp,
                          kernel = "exponential",
                          adaptive = TRUE)
  gwr.basic(DIABETES_CrudePrev_new ~ NatWalkInd + OBESITY_CrudePrev + BPHIGH_CrudePrev + LPA_CrudePrev + CSMOKING_CrudePrev + AvgSummerTemp + MedianHHIncome,
            data = data_sp,
            bw = merged_gwr_bw,
            kernel = "exponential")
  

}


