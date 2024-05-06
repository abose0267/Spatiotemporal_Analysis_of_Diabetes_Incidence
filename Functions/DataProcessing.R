
library(randomForest)
library(caret)
library(sp)



## This function loads in and returns all the datasets from the data folder
loadData <- function() {
  data <- read.csv("./Data/data.csv")
  data2 <- read.csv("./Data/500cities.csv")
  spatial_data <- st_read("./Data/shapes/us_counties")
  temp_data <-read.csv("./Data/tempData.csv")
  income_data <-read.csv("./Data/incomeData2.csv")
  return(list( original_walkability= data, original_diabetes = data2, spatial_data=spatial_data, temp_data=temp_data,income_data=income_data))
}

## This function takes in the walkability dataset, and cleans it and returns the relevant columns
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

## This function takes in the income dataset, and cleans it and returns the relevant columns
clean_income <- function(data) {
  data$CountyFIPS <- sprintf("%02d%03d", data$State.FIPS.Code, data$County.FIPS.Code)
  data$Median.Household.Income <- gsub(",", "", data$Median.Household.Income)
  data$Median.Household.Income <- log(as.numeric(data$Median.Household.Income))
  cleaned_income <- subset(data, select = c(CountyFIPS ,Median.Household.Income))
  return(cleaned_income)
}


## This function takes in the temperature dataset, and cleans it and returns the relevant columns
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



## This function takes in the diabetes dataset, and cleans it and returns the relevant columns
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

## This function takes in a dataset, and extracts and returns the spatials columns
clean_spatial <- function(data) {
  data$CountyFIPS <- sprintf("%05s", paste0(data$GEOID))
  
  cleaned_spatial = data.frame()
  
  
  selected_columns <- data[, c("CountyFIPS","INTPTLAT","INTPTLON","geometry")]
  cleaned_spatial <- data.frame(selected_columns) %>% na.omit()
  
  return(cleaned_spatial)
}

## This function uses the corrplot packages to plot a correlation matrix
plot_correlation_matrix <- function(data, method = "square", order = "hclust", tl.col = "black", tl.srt = 45, addCoef.col = "orange", plot_width = 8, plot_height = 8) {
  data <- data %>% select(-CountyFIPS, -StateAbbr)
  colnames(data) <- c("National Walkability Index", "Diabetes Crude Prevalence", "High Blood Pressure Crude Prevalence", "Obesity Crude Prevalence", "Low Physical Activity Cruede Prevalence" , "Smoking Prevalence","Average Temperature", "Median Household Income")
  cor_matrix <- cor(data, use = "complete.obs")  # Handling 
  png("correlation_plot.png", width = 800, height = 800)
  # Make correlation matrix plot
 corrplot(cor_matrix, method = method, type = "upper", order = order,
                   tl.col = tl.col, tl.srt = tl.srt, addCoef.col = addCoef.col)
 dev.off()
}



## This function fits a gwr model on a spatial dataset using an ideal bandwidth, and returns the results
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

## This function fits a mixed gwr model on a spatial dataset with an optimal bandwidth and returns the results
fit_gwr_mixed <- function(data) {
  
  merged_gwr_bw <- bw.gwr(DIABETES_CrudePrev ~ NatWalkInd + OBESITY_CrudePrev + BPHIGH_CrudePrev + LPA_CrudePrev + CSMOKING_CrudePrev + Median.Household.Income + avg_temp,
                          data = data,
                          kernel = "exponential",
  )
  merged_gwr <- gwr.mixed(DIABETES_CrudePrev ~ NatWalkInd + OBESITY_CrudePrev + BPHIGH_CrudePrev + LPA_CrudePrev + CSMOKING_CrudePrev+ Median.Household.Income + avg_temp,
                          data = data,
                          fixed.vars = c("NatWalkInd"),
                          intercept.fixed = TRUE,
                          bw = merged_gwr_bw,
                          kernel = "exponential",
                          ) 
  
  
  return(merged_gwr)
}













