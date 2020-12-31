# Extreme Heat and Cold Event Magnitude Indicators (EHMI and ECMI) Surface Estimation

### Introduction

Extreme climate events are increasingly identified as a significant factor for hospitalization and health outcomes. The risk that is raised by an extreme heat or cold event is often amplified by environmental and socioeconomic conditions of the impacted area and its population demographics. We have developed a spatiotemporal methodology to delineate boundaries for Extreme Heat and Cold Events across space and time. Hence, our method helps to quantify the potential hospitalization risk of such extreme events in a scalable manner.

### Results

In an exploratory work, we have compiled climatological data from year 2010 to 2020 from California to interpolate the geostatistical distribution of average daily Apparent Temperature (AT) for more than 120 NOAA stations using an Inverse Distance Weighting (IDW) technique. In 2019, for example, we have identified 317 extreme heat events (with an average and max duration of 2.18 and 4 days, respectively), and 274 extreme cold events (with an average and max duration of 1.68 and 3 days). Detailed results of this analysis can be found [here](articles/Extreme_Events_Visualization.html).
During 50 distinct days of heat events, 98.3% of California’s land surface was impacted by at least one extreme heat event. Figure 1 illustrates the extent of these heat events derived from the stacking spatial temporal interpolated surfaces.

<img src="./images/figure_1.png">
<figcaption>Figure 1 - Extent of the area impacted by one or more heat events (2019)</figcaption>

### Methods

Building upon a known methodology for identifying extreme heat events (EHE), [3,4] we performed a multi-step approach to integrate data from different sources using spatial and temporal benchmarks.

#### Step 1: Identifying the Events
We leveraged historical weather station data and adapted the extreme heat event (EHE) and extreme cold event (ECE) definitions[5] to identify extreme heat/cold events at each station by aggregating hourly weather records. In absence of a unified definition for such extreme events [1], researchers often incorporate relative measures of apparent temperature [6] to identify days in which human thermoregulation is stressed [7–10]. Accounting for antecedent climate conditions is also suggested, as acclimatization can be a critical factor to impact the heat and cold events hazards.[11] Sheridan et al. [5] define an extreme heat event (EHE) over a certain period of time, as an episode when the excess heat factor (EHF) exceeds the 85th percentile of all the recorded positive EHF values. EHF is derived as the product of the excess heat (EH) and an acclimatization term (EHaccl) [2]: 

<img src="./images/eq_3_ehf.png" width="300" height="30">

Where:

<img src="./images/eq_1_eh.png" width="360" height="80">

<img src="./images/eq_2_ehaccl.png" width="360" height="80">


Early studies on the effect of heat/cold exposure suggested effect differences on mortality related to the timing, duration, and intensity of the events. [11,13,14] At each station, we applied a temporal clustering algorithm to compute the extreme events duration and their seasonal precedence. The algorithm iterates through temporally ordered apparent temperature data and tag each and every daily record that satisfies extreme heat/cold event criteria. Non-extreme conditions are also coded as “0”, heat events as “1” and cold events as “2”. We assigned code “3” for the unlikely situations that the both events occur on the same day. Then, for each episode of the extreme heat/cold, a Unique Identifier (UID) was generated for those consecutive days that the event persists. By aggregating all the daily records based on the UIDs, duration and seasonal precedence of the events was identified. Finally, reiterating through updated records, the ordinal position of each day within the extreme heat/cold event was calculated. We introduced the Extreme Heat and Cold Magnitude Indicators (EHMI and ECMI) as normalized products of the intensity and duration of the event.

#### Step 2a: Delineating the Boundaries of the Events

Using the spatio-temporal extreme events data, we dynamically estimated the geographic boundary for each event by synthesizing neighboring stations' status. As spatially disaggregate features, weather stations represent hypothetical area-wide territories with varying sizes and imaginary shapes. We proposed a 2-step approach for delineating the boundaries of extreme cold and heat events. First, we used a spatial interpolation approach to estimate a continuous surface representing EHMI and ECMI value at daily intervals. Such an estimation is sensitive to underneath conditions such as spatial distribution of the stations, topography, vegetation and other locational factors. Areas with limited data records also introduce uncertainty to estimates.[15] At this study we employed the Inverse Distance Weighting (IDW) method to estimate the surface. Applying a statistically-driven threshold, we generated iso-intensity closed contours and extracted those areas that have been impacted by extreme events. This resulted in a set of estimated zones that captures the spatial extent of dynamic climate events robustly. Figure 2 presents the evolution of an extreme heat event in Los Angeles county over a 4-day span in September of 2019.
<img src="./images/figure_2.jpg">
<figcaption>Figure 2 - Example of an extreme heat event in Los Angeles county (September 2019)</figcaption>

#### Step 2b: Machine Learning Model to adjust EHMI and ECMI (Future Work)
In our fututre work, to assure the accuracy of estimated surfaces, we will use a machine learning model trained on the built environment features (e.g., land cover type, building footprint coverage, land surface temperature, and tree canopy and impervious surface coverage) to adjust EHMI and ECMI around each weather station (30 m pixels in 1000 m radius). The urban heat literature identifies three main methods to measure heat exposure: (a) using land surface temperature (LST) driven from satellite images, [16,17] (b) using atmospheric temperature drive from weather stations [2,18], and (c) using mean radiant temperature (MRT).[19] Each method has advantages and disadvantages, depending on the purpose and scale of the research. Atmospheric climate variables such as air temperature and relative humidity are appropriate for measuring the public health impact of urban microclimate extremes. Whereas LST is suitable for evaluating how regional landscape characteristics drive the urban heat islands effect. Therefore, LST can be used to measure the disparities of urban heat at city-levels. To combine atmospheric data with land surface temperature and built environment features, we will build a predictive model that estimates EHMI and ECMI to adjust the data of the closest weather station for each cell in zip code polygons. In this approach the variability of temperatures in urban areas due to urban form configurations will be taken into account. Also, in this framework, we can use climate zones that are already developed for the continental U.S. [20].

## References

51. Kim Y-M, Kim S, Cheong H-K, Kim E-H. Comparison of Temperature Indexes for the Impact Assessment of Heat Stress on Heat-Related Mortality. Environ Health Toxicol. 2011;26. doi:10.5620/eht.2011.26.e2011009


75. Heris MP, Middel A, Muller B. Impacts of form and design policies on urban microclimate: Assessment of zoning and design guideline choices in urban redevelopment projects. Landsc Urban Plan. 2020;202:103870.


78. Sheridan SC, Lee CC, Allen MJ. The Mortality Response to Absolute and Relative Temperature Extremes. International Journal of Environmental Research and Public Health. 2019;16(9):1493. doi:10.3390/ijerph16091493


79. Sheridan SC, Lee CC, Smith ET. A comparison between station observations and reanalysis data in the identification of extreme temperature events. doi:10.1002/essoar.10502708.1


87. Sheridan SC, Lee CC, Allen MJ. The Mortality Response to Absolute and Relative Temperature Extremes. International Journal of Environmental Research and Public Health. 2019;16(9):1493. doi:10.3390/ijerph16091493


88. Steadman RG. A Universal Scale of Apparent Temperature. J Climate Appl Meteor. 1984;23(12):1674-1687.


89. Epstein Y, Moran DS. Thermal Comfort and the Heat Stress Indices. Ind Health. 2006;44(3):388-398.


90. Urban A, Kyselý J. Comparison of UTCI with Other Thermal Indices in the Assessment of Heat and Cold Effects on Cardiovascular Mortality in the Czech Republic. Int J Environ Res Public Health. 2014;11(1):952-967.


91. Ng CFS, Ueda K, Ono M, Nitta H, Takami A. Characterizing the effect of summer temperature on heatstroke-related emergency ambulance dispatches in the Kanto area of Japan. Int J Biometeorol. 2013;58(5):941-948.


92. Allen MJ, Sheridan SC. Mortality risks during extreme temperature events (ETEs) using a distributed lag non-linear model. Int J Biometeorol. 2015;62(1):57-67.


93. Anderson GB, Brooke Anderson G, Bell ML. Heat Waves in the United States: Mortality Risk during Heat Waves and Effect Modification by Heat Wave Characteristics in 43 U.S. Communities. Environmental Health Perspectives. 2011;119(2):210-218. doi:10.1289/ehp.1002313


94. Nairn JR, Fawcett RJB. The Excess Heat Factor: A Metric for Heatwave Intensity and Its Use in Classifying Heatwave Severity. Int J Environ Res Public Health. 2014;12(1):227-253.


95. Ellis FP, Nelson F, Pincus L. Mortality during heat waves in New York City July, 1972 and August and September, 1973. Environ Res. 1975;10(1):1-13.


96. Schuman SH. Patterns of urban heat-wave deaths and implications for prevention: data from New York and St. Louis during July, 1966. Environ Res. 1972;5(1):59-75.


97. Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A. Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology. 2005;25(15):1965-1978. doi:10.1002/joc.1276


99. Anderson M. As Rising Heat Bakes U.S. Cities, The Poor Often Feel It Most. NPR. https://www.npr.org/2019/09/03/754044732/as-rising-heat-bakes-u-s-cities-the-poor-often-feel-it-most. Published September 3, 2019. Accessed October 18, 2020.


100. Mirzaei PA. Recent challenges in modeling of urban heat island. Sustainable Cities and Society. 2015;19:200-206.


101. Kolokotroni M, Giridharan R. Urban heat island intensity in London: An investigation of the impact of physical characteristics on changes in outdoor air temperature during summer. Solar Energy. 2008;82(11):986-998.


102. Crank PJ, Middel A, Wagner M, Hoots D, Smith M, Brazel A. Validation of seasonal mean radiant temperature simulations in hot arid urban climates. Sci Total Environ. 2020;749:141392.


103. Demuzere M, Hankey S, Mills G, Zhang W, Lu T, Bechtel B. Combining expert and crowd-sourced training data to map urban form and functions for the continental US. Sci Data. 2020;7(1):264.
