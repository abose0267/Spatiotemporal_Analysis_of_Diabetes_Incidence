
loadData <- function() {
  data <- read.csv("../../Data/data.csv")
  data2 <- read.csv("../../Data/500cities.csv")
  spatial_data <- st_read("../../Data/shapes/us_counties")
  return(list( original_walkability= data, original_diabetes = data2, spatial_data=spatial_data))
}


clean_walkability <- function(data) {
  
  data$STATEFP <- sprintf("%02d", as.numeric(data$STATEFP))
  data$COUNTYFP <- sprintf("%03d", as.numeric(data$COUNTYFP))
  data$CountyFIPS <- sprintf("%05d",as.numeric(paste0(data$STATEFP, data$COUNTYFP)))
  
  cleaned_walk_data <- data %>%
    group_by(CountyFIPS) %>%
    summarise(NatWalkInd = mean(NatWalkInd, na.rm = TRUE)) %>%
    ungroup()
  
  return(cleaned_walk_data)
}

clean_diabetes <- function(data) {
  data$CountyFIPS <- sprintf("%05s", paste0(data$CountyFIPS))
  
    cleaned_diabetes <- data %>%
    group_by(CountyFIPS) %>%
    summarise(DIABETES_CrudePrev = mean(DIABETES_CrudePrev, na.rm = TRUE),BPHIGH_CrudePrev= mean(BPHIGH_CrudePrev, na.rm = TRUE),OBESITY_CrudePrev=mean(OBESITY_CrudePrev, na.rm = TRUE),LPA_CrudePrev=mean(LPA_CrudePrev, na.rm = TRUE),CSMOKING_CrudePrev=mean(CSMOKING_CrudePrev, na.rm = TRUE),StateAbbr=StateAbbr) %>%
    ungroup() %>%
    unique() 
  
    cleaned_diabetes <- cleaned_diabetes %>% 
    select(CountyFIPS, StateAbbr, DIABETES_CrudePrev, BPHIGH_CrudePrev, OBESITY_CrudePrev, LPA_CrudePrev, CSMOKING_CrudePrev)
  
  return(cleaned_diabetes)
  
}

clean_spatial <- function(data) {
  data$CountyFIPS <- sprintf("%05s", paste0(data$GEOID))
  
  cleaned_spatial = data.frame()
  
  
  selected_columns <- data[, c("CountyFIPS","INTPTLAT","INTPTLON","geometry")]
  cleaned_spatial <- data.frame(selected_columns) %>% na.omit()
  
  return(cleaned_spatial)
}


plot_correlation_matrix <- function(data) {
  
  # Calculate correlation matrix
  cor_matrix <- cor(data %>% select(-CountyFIPS, -StateAbbr), use = "complete.obs")  # Handling missing values by using complete observations
  # Make correlation matrix
  plot<- corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
           tl.col = "black", tl.srt = 45,  # Text label color and rotation
           addCoef.col = "orange")  # Add correlation coefficients to plot
  return(plot)
}


test_train_split <- function(data,train) {
  
  set.seed(123)  
  train_indices <- sample(1:nrow(data), train * nrow(data))
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  return(list(test_data=test_data,train_data=train_data))
}

fit_gwr <- function(data) {
  
  merged_gwr_bw <- bw.gwr(DIABETES_CrudePrev ~ OBESITY_CrudePrev + BPHIGH_CrudePrev + LPA_CrudePrev + CSMOKING_CrudePrev,
                          data = data,
                          kernel = "exponential",
  )
  
  merged_gwr <- gwr.basic(DIABETES_CrudePrev ~ OBESITY_CrudePrev + BPHIGH_CrudePrev + LPA_CrudePrev + CSMOKING_CrudePrev,
                          data = data,
                          bw = merged_gwr_bw,
                          kernel = "exponential",
  )
  
  
  return(merged_gwr)
}


test_model <- function(model,data) {
  
  return(predict(model,data))
}



rmse <- function(predicted_values,actual_values) {
  return(sqrt(mean((predicted_values - actual_values)^2)))
}


plotDiabetes <- function(model_results,new_dataset) {
  
  # Plot the spatial distribution of DIABETES_CrudePrev
  ggplot() +
    geom_sf(data = new_dataset, aes(fill = DIABETES_CrudePrev), color="black",size=0.2) +
    scale_fill_viridis_c(name = "Diabetes Crude Prev") +
    labs(title = "Spatial Distribution of Diabetes Crude Prevalence")
  
  # Plot the spatial distribution of GWR results
  ggplot() +
    geom_sf(data = model_results, aes(fill = yhat), color = "black", size = 0.2) +
    scale_fill_viridis_c(name = "Predicted Diabetes Crude Prev") +
    labs(title = "GWR Predicted Diabetes Crude Prevalence using GWR") +
    theme_bw()
}

plotGWR <- function(model, value) {
  

}

plotValue <- function(data, value) {

}








