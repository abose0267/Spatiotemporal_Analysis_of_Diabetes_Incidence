source("./data/artificialData.R")
source("./Functions/simulationStudyFunctions.R")
source("./Functions/DataProcessing.R")
library(GWmodel)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyverse)
library(knitr)
library(webshot)
library(dplyr)
library(corrplot)
library(sp)
library(RColorBrewer)
library(spgwr)
set.seed(123)


loadedData <- loadData()


## Cleans all the data and merges into a spatial dataset
cleaned_walkability = loadedData$original_walkability %>% clean_walkability()
cleaned_diabetes = loadedData$original_diabetes %>% clean_diabetes()
spatial_data = loadedData$spatial_data %>% clean_spatial()
temp_data = loadedData$temp_data %>% cleaned_weather()
income_data = loadedData$income_data %>% clean_income()
new_dataset = merge(cleaned_walkability, cleaned_diabetes, by = "CountyFIPS", all.x = TRUE, all.y = TRUE) %>% subset( !(StateAbbr %in% c("AK", "HI"))) %>% merge(temp_data, by = "CountyFIPS", all.x = TRUE, all.y = TRUE)
merged_income = merge(new_dataset,income_data,by = "CountyFIPS", all.x = TRUE, all.y = TRUE ) %>% na.omit()
new_spatial_dataset = merge(merged_income,spatial_data, by = "CountyFIPS", all.x = TRUE, all.y = TRUE) %>% na.omit()
summary(new_spatial_dataset)


# Creates a correlation matrix of the response and covariates
plot_correlation_matrix(merged_income)


new_spatial_dataset <- na.omit(new_spatial_dataset)
sf_spatial_data<- st_as_sf(new_spatial_dataset)
sf_spatial_data <- as(sf_spatial_data, "Spatial")

gwr_model_results = fit_gwr(sf_spatial_data)



## Adjusts P-Values to determine significant vs insignificant regions for plotting purposes with insignificant values having a coefficient of 0
adjusted_tvalues <- gwr.t.adjust(gwr_model_results)$SDF %>% as("sf")
selected_columns <- c(grep("_p$", colnames(adjusted_tvalues), value = TRUE),"geometry")
subset_df <- adjusted_tvalues[, selected_columns]
model_df <- gwr_model_results$SDF %>% as("sf")
for (i in seq_len(nrow(model_df))) {
  # Check values in subset_df
  for (col_name in names(model_df)) {
    # Construct the corresponding column name in subset_df
    col_name_subset <- paste0(col_name, "_p")
    
    # Check if the column exists in subset_df
    if (col_name_subset %in% names(subset_df)) {
      if (subset_df[[col_name_subset]][i] < 0.05) {
        # Set corresponding value in model_df to 0
        model_df[i, col_name] <- 0
      }
    }
  }
}

# Runs a monte carlo simulation using the same bandwidth as original model
gwr_mc <- gwr.montecarlo(DIABETES_CrudePrev ~ NatWalkInd + OBESITY_CrudePrev + BPHIGH_CrudePrev + LPA_CrudePrev + CSMOKING_CrudePrev + Median.Household.Income + avg_temp, sf_spatial_data, nsims=99, kernel="exponential",adaptive=F, gwr_model_results$GW.arguments$bw)





## Creates a table to show all the P-Values from the Monte Carlo Simulation
gwr_mc2 <- data.frame(
  "Variable" = c("(Intercept)", "NatWalkInd", "OBESITY_CrudePrev", "BPHIGH_CrudePrev",
                 "LPA_CrudePrev", "CSMOKING_CrudePrev", "Median.Household.Income", "avg_temp"),
  "Coefficient" = gwr_mc
)




# Create a mapping of old abbreviations to new ones
abbreviations <- c(
  "(Intercept)" = "Intercept",
  "NatWalkInd" = "National Walkability Index",
  "OBESITY_CrudePrev" = "Obesity Prevalence",
  "BPHIGH_CrudePrev" = "High Blood Pressure Prevalence",
  "LPA_CrudePrev" = "Low Physical Activity Prevalence",
  "CSMOKING_CrudePrev" = "Current Smoking Prevalence",
  "Median.Household.Income" = "Median Household Income",
  "avg_temp" = "Average Temperature"
)

# Replace the abbreviations in the Variable column
gwr_mc2$Variable <- abbreviations[gwr_mc2$Variable]

out <- data.frame(gwr_mc2$Variable,gwr_mc2$p.value)

out <- out %>%rename(Covariates = gwr_mc2.Variable)
out <- out %>% rename(P_Value = gwr_mc2.p.value)
mc_table <- knitr::kable(out, caption = "Results of Monte Carlo Simulation")



#Conducts a Moran's I Test on GWR Results

w <- poly2nb(sf_spatial_data, queen = TRUE)
w <- nb2listw(w)

# Perform Global Moran's I test on the residuals
moran_result <- moran.test(model_df$residual, w)


# Creates a table to display results of Moran's I Test
out <- data.frame(moran_result$estimate)
out <- out %>%rename( "Value"  = 	moran_result.estimate)
val <- knitr::kable(out, caption = "Moran's I Test Results")



adjusted_tvalues <- gwr.t.adjust(gwr_model_results)$SDF %>% as("sf")
selected_columns <- c(grep("_p$", colnames(adjusted_tvalues), value = TRUE),"geometry")
subset_df <- adjusted_tvalues[, selected_columns]
model_df <- gwr_model_results$SDF %>% as("sf")
for (i in seq_len(nrow(model_df))) {
  # Check values in subset_df
  for (col_name in names(model_df)) {
    # Construct the corresponding column name in subset_df
    col_name_subset <- paste0(col_name, "_p")
    
    # Check if the column exists in subset_df
    if (col_name_subset %in% names(subset_df)) {
      if (subset_df[[col_name_subset]][i] < 0.05) {
        # Set corresponding value in model_df to 0
        model_df[i, col_name] <- 0
      }
    }
  }
}

## Calculates VIF Score for all covariates
# gwr.collin.diagno(DIABETES_CrudePrev ~ NatWalkInd + OBESITY_CrudePrev + BPHIGH_CrudePrev + LPA_CrudePrev + CSMOKING_CrudePrev + Median.Household.Income + avg_temp, sf_spatial_data, gwr_model_results$GW.arguments$bw, kernel="exponential",adaptive=FALSE)


png("residual_plot.png")
plot(model_df$residual)
dev.off()


## Generates impact and facet plot by multiplying observed values * predicted coefficients
plots = data.frame(CountyFIPS = new_spatial_dataset$CountyFIPS,Walkability_Obs = new_spatial_dataset$NatWalkInd,HBP_Obs = new_spatial_dataset$BPHIGH_CrudePrev,walkability_coef= model_df$NatWalkInd, income_impact = new_spatial_dataset$Median.Household.Income * model_df$Median.Household.Income,temp_impact = new_spatial_dataset$avg_temp * model_df$avg_temp, walk_impact = new_spatial_dataset$NatWalkInd * model_df$NatWalkInd, bp_impact = new_spatial_dataset$BPHIGH_CrudePrev * model_df$BPHIGH_CrudePrev, lpa_impact = new_spatial_dataset$LPA_CrudePrev*model_df$LPA_CrudePrev,obesity_impact=new_spatial_dataset$OBESITY_CrudePrev * model_df$OBESITY_CrudePrev, smoking_impact = new_spatial_dataset$CSMOKING_CrudePrev * model_df$CSMOKING_CrudePrev, INTPTLAT=new_spatial_dataset$INTPTLAT, INTPTLON = new_spatial_dataset$INTPTLON, geometry =  new_spatial_dataset$geometry  ) %>%   pivot_longer(cols = c(bp_impact,lpa_impact,smoking_impact,obesity_impact, income_impact, temp_impact),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      names_to = "Impact_Type",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      values_to = "Impact_Value") %>% st_sf()
plot1 <- plots %>%
  ggplot() +
  geom_sf(aes(fill = Impact_Value), color = scales::alpha("black", 0.1), ) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,name = "Impact") +
  facet_wrap(~Impact_Type, labeller = labeller(Impact_Type = c(
    "walk_impact" = "Walk Impact",
    "lpa_impact" = "LPA Impact",
    "smoking_impact" = "Smoking Impact",
    "obesity_impact" = "Obesity Impact",
    "income_impact" = "Income Impact",
    "temp_impact" = "Temperature Impact",
    "bp_impact" = "High Blood Pressure Impact"
  ))) +
  theme(legend.position = "bottom")

ggsave("facet_plot.png", plot1, width = 8, height = 6, units = "in", dpi = 300)


plot2 <- plots %>%
  ggplot() +
  geom_sf(aes(fill = walk_impact), color = scales::alpha("black", 0.1)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Impact") +
  theme(legend.position = "bottom")
ggsave("impact_plot.png", plot2, width = 8, height = 6, units = "in", dpi = 300)