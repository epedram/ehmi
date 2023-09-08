---
title: "Spatio-temporal Interpolation and Delineation of Extreme Heat Events in California between 2017-2021"
output: html_document
---
## Abstract
Robust spatio-temporal delineation of extreme climate events and accurate identification of areas that are impacted by an event is a prerequisite for studying the epidemiology of climate change. In prior research, climate attributes such as temperature and humidity have often been linearly assigned to the population of the study unit from the closest weather station. This could result in inaccurate event delineation and biased assessment of extreme heat exposure. We have developed a spatio-temporal model to dynamically delineate boundaries for Extreme Heat Events (EHE) across space and over time, using a relative measure of Apparent Temperature (AT). Our surface interpolation approach offers a higher spatio-temporal resolution compared to the standard nearest-station assignment method. We show that the proposed approach can provide at least 80.8 percent improvement in identification of areas and populations impacted by extreme heat events. This improvement in average adjusts the misclassification of about one million Californians per day of an extreme event, who would be either unidentified or misidentified under extreme heat events between 2017 and 2021.

The results of this study have been published as an open access research article at [Environmental Research Journal](https://www.sciencedirect.com/science/article/pii/S0013935123017887).

## Computational R Scripts

This repository provides scripts for collecting and processing historical weather variables from the National Oceanic and Atmospheric Administration's Integrated Surface Data Set (NOAA ISD). 


The latest version of the computation codes can be retrieved from the project github repository at:
[here](https://github.com/epedram/ehmi).

- **station_data_preprocessing/01_download_isd_web_files.R**
This R script utilizes the NOAA ISD (Integrated Surface Dataset) web interface to generate a list of weather stations for a given period and download the corresponding yearly weather data in parallel. The data includes information such as temperature, precipitation, wind speed, and dew point temperature. It also create a list of stations that were missing for a certain year.

- **station_data_preprocessing/02_compile_daily_values**
This R script is developed to compile daily values from NOAA ISD hourly and sub-hourly data tables. The script is optimized to run in parallel, making it efficient at handling large amounts of data. The script processes the data by first reading in the hourly data tables, then it groups and aggregates the data by min, max and average daily values and saves all the resulting daily data in a new yearly file. 

- **station_data_preprocessing/03_impute_missing_daily_records.R**
This script visualize and handles missing daily values by using imputation techniques to fill in any missing data points. Once the missing values have been imputed, the script then proceeds to calculate the apparent temperature. This code uses vectorized *sapply()* operation to process the data.

- **extreme_events_identification/extreme_events_identification.R**
This R script performs an analysis on historical weather data at a station level to identify Extreme Heat Events (EHEs) using the definition provided by Sheridan et al. The script first calculates the excess heat factor (EHF) for each station by multiplying the excess heat and an acclimatization term. It then iterates through the data and tags each daily record that meets the criteria for an EHE as "1" and non-extreme conditions as "0". For each episode of EHE, the script generates a Unique Identifier (UID) for the consecutive days of the event and calculates the ordinal position of each day within the EHE. Finally, the script introduces and calculates the Extreme Heat Magnitude Indicator (EHMI) as the min-max normalized product of the intensity (EHF) and duration of independent events, and uses a spatial interpolation approach to estimate a continuous fine-resolution (600m) surface representing the EHMI value at daily intervals. This script process the data and provide EHMI maps of high resolution at daily intervals.

This repository is published under the MIT License.
