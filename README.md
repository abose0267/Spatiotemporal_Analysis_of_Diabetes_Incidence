# Understanding Walkability: Calculating the Index and Investigating Its Connection to Diabetes Prevalence


## Abstract

The diabetes epidemic in the United States presents a nuanced public health challenge, shaped by factors such as socioeconomic status and climate. While the influence of these factors on diabetes is well-established, the role of walkability in managing diabetes prevalence remains contested. This study revisits the relationship between walkability and diabetes in the U.S., using walkability indexes calculated from CDC data. Contrary to some studies suggesting that increased walkability reduces diabetes prevalence, our findings, analyzed through Geographically Weighted Regression (GWR), reveal that walkability is not a significant predictor of diabetes prevalence and exhibits notable regional anomalies. Further analysis using Monte Carlo simulations, Global I Moran's Test, and Variance Inflation Ratio (VIR) supports these results. Our study also critiques the current methods of calculating the walkability index, proposing a revised model that incorporates additional relevant variables from the CDC. This nuanced understanding underscores the need for region-specific urban planning and public health strategies that recognize the complex interplay between walkability, environmental, and socioeconomic factors.



## Organization

- Report.qmd: Used to render the pdf version of the report
- Poster Folder:
  - Poster.qmd: This file contains the code to render the poster that we made for the report
  - Images Folder: Store the images that go in the poster
- Analysis Folder:
  - gwrAnalysis.R: This file contains the code for running a gwr model on artifical data
- Functions Folder:
  - DataProcessing.R: This file contains the code used to render the report. This includes data cleaning, fitting models, and running diagnostics
- Data Folder:
  - This folder is used to store all the necessary datasets
    - 500cities.csv: This dataset contains health factor data from the CDC
    - artificalData.R: This script is used to generate artificial data for the simulation study portion of the report
    - incomeData2.csv: This dataset contains the Median Household Income data that is used in the report
    - Shapes folder: This contains shapefiles that are used for spatial analysis
    - Tempdata.csv: This dataset contains the average temperature used in the report