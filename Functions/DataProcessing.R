
library(randomForest)
library(caret)
library(sp)


loadData <- function() {
  data <- read.csv("./Data/data.csv")
  data2 <- read.csv("./Data/500cities.csv")
  spatial_data <- st_read("./Data/shapes/us_counties")
  temp_data <-read.csv("./Data/tempData.csv")
  income_data <-read.csv("./Data/incomeData.csv")
  return(list( original_walkability= data, original_diabetes = data2, spatial_data=spatial_data, temp_data=temp_data,income_data=income_data))
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

clean_income <- function(data) {
  data$CountyFIPS <- sprintf("%02d%03d", data$State.FIPS.Code, data$County.FIPS.Code)
  data$Median.Household.Income <- gsub(",", "", data$Median.Household.Income)
  data$Median.Household.Income <- as.numeric(data$Median.Household.Income)
  cleaned_income <- subset(data, select = c(CountyFIPS ,Median.Household.Income))
  return(cleaned_income)
}

cleaned_weather <- function(temp_data) {
  temp_data$GEOID <- as.character(temp_data$GEOID)
  temp_data$GEOID <- sprintf("%011s", temp_data$GEOID)
  temp_data$CountyFIPS <- substr(temp_data$GEOID, start = 1, stop = 5)
  temp_data$avg_temp <- temp_data$Summer.Average.Land.Surface.Temperature..F.
  selected_columns <- temp_data[, c("CountyFIPS", "avg_temp")]
  cleaned_weather <- selected_columns[complete.cases(selected_columns), ] %>% group_by(CountyFIPS) %>%
    summarise(avg_temp = mean(avg_temp, na.rm = TRUE)) %>%
    ungroup()
  return(cleaned_weather)
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
  
  merged_gwr_bw <- bw.gwr(DIABETES_CrudePrev ~ NatWalkInd + OBESITY_CrudePrev + BPHIGH_CrudePrev + LPA_CrudePrev + CSMOKING_CrudePrev + Median.Household.Income + avg_temp,
                          data = data,
                          kernel = "exponential",
  )
  
  merged_gwr <- gwr.basic(DIABETES_CrudePrev ~ NatWalkInd + OBESITY_CrudePrev + BPHIGH_CrudePrev + LPA_CrudePrev + CSMOKING_CrudePrev+ Median.Household.Income + avg_temp,
                          data = data,
                          bw = merged_gwr_bw,
                          kernel = "exponential",
  ) 
  
  
  return(merged_gwr)
}

fit_gwr2<-function(data) {
  
  merged_gwr_bw2 <- bw.gwr(DIABETES_CrudePrev ~ NatWalkInd + OBESITY_CrudePrev + BPHIGH_CrudePrev + LPA_CrudePrev + CSMOKING_CrudePrev + Median.Household.Income + avg_temp,
                          data = data,
                          kernel = "gaussian",
  )
  
  merged_gwr2 <- gwr.basic(DIABETES_CrudePrev ~ NatWalkInd + OBESITY_CrudePrev + BPHIGH_CrudePrev + LPA_CrudePrev + CSMOKING_CrudePrev+ Median.Household.Income + avg_temp,
                          data = data,
                          bw = merged_gwr_bw2,
                          kernel = "gaussian",
  ) 
  
  return(merged_gwr2)
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


runRandomForest <- function(data) {
  set.seed(123) # for reproducibility
  
  merged_df <- as.data.frame(data)
  
  index <- createDataPartition(merged_df$DIABETES_CrudePrev, p = 0.8, list = FALSE)
  train_df <- merged_df[index, ]
  test_df <- merged_df[-index, ]
  
  rf_train <- train(
    x = train_df[, c("NatWalkInd", "OBESITY_CrudePrev", "BPHIGH_CrudePrev", "LPA_CrudePrev", "CSMOKING_CrudePrev", "avg_temp", "Median.Household.Income")],
    y = train_df$DIABETES_CrudePrev,
    method = "rf"
  )
  
  rf_pred <- predict(rf_train, newdata = test_df[, c("NatWalkInd", "OBESITY_CrudePrev", "BPHIGH_CrudePrev", "LPA_CrudePrev", "CSMOKING_CrudePrev", "avg_temp", "Median.Household.Income")])
  
  return_frame = data.frame( test_df, predictedValues = rf_pred)
  return(return_frame)
}




plotGWR <- function(model, value) {
  
  
}

plotValue <- function(data, value) {
  
}







