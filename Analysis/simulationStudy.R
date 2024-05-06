source("./data/artificialData.R")
source("./Functions/simulationStudyFunctions.R")
set.seed(123)

# Creates a dataset with spatially varying coefficients
artificial_data = create_data2(new_spatial_dataset)



# Convert data to spatial format and fit GWR
gwr_result_sim <- fit_artificial_gwr(artificial_data)
gwr_result_sim



artificial_data<- na.omit(artificial_data)
residuals <- gwr_result_sim$SDF$residual 


insert_position <- 2
artificial_data <- cbind(artificial_data[, 1:insert_position], residuals = residuals, artificial_data[, (insert_position + 1):ncol(artificial_data)])


sf_artificial_data<- st_as_sf(artificial_data)
# Creates a residual plot of the GWR on simulation study response values
sim_residual_plot <- ggplot() +
  geom_sf(data = sf_artificial_data, aes(fill = residuals),color = scales::alpha("black", 0.1)) +
  scale_fill_gradient2(name = "Residuals", midpoint = 0) 

ggsave("sim_residual_plot.png", sim_residual_plot, width = 8, height = 6, units = "in", dpi = 300)


# Creates a table of Mean Absolute Errors for each predicted coefficient
coefficients <- gwr_result_sim$lm$coefficients

names <- c("Intercept","NatWalkInd","OBESITY_CrudePrev","BPHIGH_CrudePrev","LPA_CrudePrev","CSMOKING_CrudePrev", "AvgSummerTemp","MedianHHIncome")

coefficient_df <- data.frame(names,c(coefficients)) 

coefficient_df <- coefficient_df[-1,]

coefficient_df <- pivot_wider(coefficient_df, names_from = names, values_from = c.coefficients.)
artificial_data$walk_coef_residual = abs(artificial_data$walk_coef - coefficient_df$NatWalkInd)
artificial_data$obesity_coef_residual = abs(artificial_data$obesity_coef - coefficient_df$OBESITY_CrudePrev)
artificial_data$bp_coef_residual = abs(artificial_data$bp_coef - coefficient_df$BPHIGH_CrudePrev)
artificial_data$lpa_coef_residual = abs(artificial_data$lpa_coef - coefficient_df$LPA_CrudePrev)
artificial_data$smoke_coef_residual = abs(artificial_data$smoke_coef - coefficient_df$CSMOKING_CrudePrev)
artificial_data$temp_coef_residual = abs(artificial_data$temp_coef - coefficient_df$AvgSummerTemp)
artificial_data$income_coef_residual = abs(artificial_data$income_coef - coefficient_df$MedianHHIncome)

MAE_df = data.frame(Covariates=c("National Walkability Index", "Obesity Crude Prevalence", "High Blood Pressure Crude Prevalence", "Low Physical Activity Crude Prevalence", "Smoking Crude Prevalence", "Average Temperature", "Median Household Income"))
vals <- c(sum(artificial_data$walk_coef_residual)/nrow(artificial_data), sum(artificial_data$obesity_coef_residual)/nrow(artificial_data), sum(artificial_data$bp_coef_residual)/nrow(artificial_data), sum(artificial_data$lpa_coef_residual)/nrow(artificial_data), sum(artificial_data$smoke_coef_residual)/nrow(artificial_data), sum(artificial_data$temp_coef_residual)/nrow(artificial_data), sum(artificial_data$income_coef_residual)/nrow(artificial_data))

MAE_df$values <- vals
MAE_df <- MAE_df %>% rename("Mean Absolute Error" = values )
MAE_table <- knitr::kable(MAE_df, caption = "Mean Absolute Error of Coefficient Predictions for Simulated Data")


