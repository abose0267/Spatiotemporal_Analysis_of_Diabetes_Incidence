library(dplyr)

create_data <- function(n_counties = 3244) {
  tibble(
    CountyFIPS = sprintf("%05d", 1:n_counties),
    NatWalkInd = runif(n_counties, 4, 7),
    StateAbbr = sample(c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                         "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                         "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                         "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                         "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"), size = n_counties, replace = TRUE),
    DIABETES_CrudePrev = runif(n_counties, 10, 20),
    BPHIGH_CrudePrev = runif(n_counties, 30, 50),
    OBESITY_CrudePrev = runif(n_counties, 25, 45),
    LPA_CrudePrev = runif(n_counties, 20, 45),
    CSMOKING_CrudePrev = runif(n_counties, 15, 30),
    INTPTLAT = runif(n_counties, 25, 49),
    INTPTLON = runif(n_counties, -124, -66),
    AvgSummerTemp = runif(n_counties, 70, 100),
    MedianHHIncome = runif(n_counties, 30000, 100000) 
  )
}
