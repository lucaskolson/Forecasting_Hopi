# Forecasting_Hopi
A demgraphic forecasting project for partial completion of a graduate certificate in quantitative methods from the University of Washington.

## Table of Contents

- [Project Overview] (#project-overview)
- [Data Sources] (#data-sources)
- [Tools] (#tools)
- [Data Cleaning/Preparation] (#data-cleaning/preparation)
- [Data Analysis] (#data-analysis)
- [Results/Findings] (#results/findings)
- [Recommendations] (#recommendations)
- [Limitations] (#limitations)
- [References] (#references)


### Project Overview
This demographic forecasting project recreates and improves on the demographic forecasting models used by US courts for water resource management with Native nations. This project is based on a published case study by demographer David Swanson of the Hopi Nation’s court case to determine water rights over the subsequent three decades based on increasing demand from a growing population. Forecasting with small populations, particularly Native nations having a contentious relationship with the US census, presents numerous methodological challenges. Swanson develops a forecasting model using the Cohort Component Method (CCM) for 1937-2017, tested on Tribal enrollment data from 1992-2017, and then extended through 1937.

I test the reliability of Swanson’s model by recreating his results using modified mortality with uncertainty and multiple fertility models to generate prediction intervals for future population growth. In lieu of mortality tables for the general US population created by the Social Security Administration, I develop Brass relational models with UN model lifetables of developing countries. Furthermore, I fit three different fertility models to demonstrate the sensitivity of the CCM model to these assumptions about fertility, which were not clear in the original article. In total, I simulated 1,000 forecasts through the period 1990-2010 for each of the three fertility scenarios.

This project was completed as a partial requirement of the Graduate Certificate in Quantitative Methods by the University of Washington’s Center for Statistics and the Social Sciences (CSSS).

### Data Sources
The data for the original article’s model came from the 1937 Tribal Census and the Tribal enrollment data from 1992-2017. The rest of the data is modeled using SSA life tables and an unspecified geometric fertility model between 1937 and 2010. I replace the SSA life tables with that of the United Nations for developing countries (see references for link) while referencing, when available, Native age-specific death rates for each period from the National Reports on US Vital Statistics and the Indian Health Services.

## Tools
Raw data was compiled and organized in Excel, while the data analysis and modeling was completed in R. Specific libraries used in R for this project include:

•	tidyverse
•	readxl
•	popReconstruct
•	pander

### Data Cleaning/Preparation
The initial goal for new mortality data was to use age-specific values for Pueblo or Native aggregates between 1940-2010, but careful investigation revealed these values were not available. Consequently, I selected the appropriate UN model lifetables for each decade, and conducted linear imputation of the death rate for each age bracket for the intervening five year periods. I also had to standardize the age bracket for 75+ due to changes in measurement between multiple decades.

### Data Analysis
For mortality, I created Brass relational models for each sex and each 5-year period. 
 

For fertility, I created three different models to impute values between 1950 and 2010 (fertility was assumed to be constant between 1940 and 1960 based on secondary literature, similar to the original article).

 

### Results/Findings
Using the three different fertility models combined with the Brass models with uncertainty, I created 3,000 simulations between 1990-2010 to compare to the original forecast and the Tribal Enrolment data for this period. These results demonstrate that the official Tribal enrollment counts can be recreated using alternative assumptions regarding both mortality and fertility, as compared to Swanson’s model. Furthermore, forecasts are particularly sensitive to assumptions regarding fertility, and these alternate assumptions would lead to significantly different results for forecasts in 1937 or beyond.  

### Recommendations
Social Security Administration lifetables for the general US population do not accurately represent mortality for Native peoples. While imperfect, UN model life tables for developing countries provide a better approximation for projections. Furthermore, assumptions about fertility models can be more important and should be transparently conducted to ensure both accuracy and accountability.

### Limitations
Demographic forecasting for Native populations is a necessary, though potentially treacherous activity, given the lack of quality data at regular intervals. This project, as well as the original Swanson’s article, aim for the best possible modeling given the frequent absence of measured data. However, these projections do not address the complex history of US blood quantum policies other than to reference the Hopi Tribal Membership Ordinance 33, which states that membership is based on lineal descent from those people listed on the 1937 Census Roll. 

### References
- Connolly, Michele, and Bette Jacobs. "Counting Indigenous American Indians and Alaska Natives in the US census." Statistical Journal of the IAOS 36, no. 1 (2020): 201-210.
- Indian Health Services, “Division of Program Statistics, Publications.” Accessed at: https://www.ihs.gov/dps/publications/
- “IPUMS NHGIS,” Accessed at: https://www.nhgis.org/
- National Center for Health Statistics, “National Vital Statistics Reports.” Accessed at: https://www.cdc.gov/nchs/products/nvsr.htm 
- “Social Explorer,” Accessed at: https://www.socialexplorer.com/explore-tables
- Swanson, David A. "Forecasting a Tribal Population Using the Cohort-Component Method: A Case Study of the Hopi." Population Research and Policy Review (2022): 1-22.
- US Census Bureau, “1940 Census of Population: Characteristics of the Nonwhite Population by Race,” 1940. Accessed at: https://www.census.gov/library/publications/1943/dec/population-nonwhite.html
- US Census Bureau, “1950 Census of Population: Volume 4. Special Reports,” 1950. Accessed at: https://www.census.gov/library/publications/1953/dec/population-vol-04.html
- US Census Bureau, “1960 Census of Population: Subject Reports: Nonwhite Population by Race,” 1960. Accessed at: https://www.census.gov/library/publications/1965/dec/population-pc-2-1c.html
- US Census Bureau, “1970 Census of Population, Supplementary Report: Race of the Population of the United States, by States: 1970,” 1970. Accessed at: https://www.census.gov/library/publications/1972/dec/pc-s1-11.html
- UN, “MODEL LIFE TABLES FOR DEVELOPING COUNTRIES,” 1982. Accessed at: https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwihtKzAlO_3AhWFk4kEHbWUBt4QFnoECA4QAQ&url=https%3A%2F%2Fwww.un.org%2Fdevelopment%2Fdesa%2Fpd%2Fsites%2Fwww.un.org.development.desa.pd%2Ffiles%2Ffiles%2Fdocuments%2F2020%2FJan%2Fun_1982_model_life_tables_for_developing_countries.pdf&usg=AOvVaw2_btxXmZ3brBmq82tOAcNb. 
