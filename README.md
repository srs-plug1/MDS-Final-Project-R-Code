# MDS-Final-Project-R-Code

## Report Abstract

Sustainable drainage systems, or "SuDS", are a nature-based alternative to traditional drainage systems and consist of an area of soil planted with vegetation to store and attenuate the flow of stormwater from urban areas. They are increasingly being used to manage urban flood risk as climate change increases the frequency of high-intensity rainfall events, but research on the various components of their design is needed to better understand how SuDS can be effectively implemented. This study uses field data collected from purpose-built bioretention cells -- a type of SuDS -- in Newcastle to investigate the role of vegetation in contributing to water losses through evapotranspiration (ET) and to understand how restricting the outflow from the bioretention cell into traditional drainage systems influences its performance. Attention is also given to the appropriate definition of rainfall events for the context of the study. In defining events, sensitivity analyses were conducted to select an appropriate value of the minimum inter-event time (MIT) that should separate events. Statistical tests of difference were used to compare the hydrological performance of restricted and unrestricted outflow. Soil moisture sensor data was used in a mass-balance analysis to estimate evapotranspiration from the differently-vegetated bioretention cells, with the differences quantified using Nash-Sutcliffe efficiency. An MIT of 24 h was found to be appropriate for the study’s context. In terms of outflow restriction, small but significant differences were found between the hydrological performance of restricted and unrestricted outflow and restricting outflow was not found to cause issues with bioretention cell capacity. The *Iris sibirica*-planted bioretention cell was found to be significantly different to an unplanted bioretention cell in terms of ET, whilst amenity grass was not. However, limitations in the amount of data available to study ET led to the conclusion that further investigation is required to validate these findings.


## Instructions

The files "Functions and Packages.R" and "Loading Data (23.08.2026).R" contain all the data, functions, and packages needed to run all other files.

Some of the files for each research objective may need to be run before others can be. The recommended order is as follows:

### RO1 (order not critical):
RO1 - Sensitivity Analysis Part A (test_mit)
RO1 - Sensitivity Analysis Part B (Performance Metrics)
RO1 - Comparison with Dunkerley (2008)

### RO2:
RO2 - Calculating Event Metrics [Must be run before the following]
RO2 - Statistical Tests (Final Version)
RO2 - Effect of Event Characteristics, Correlation Tests
RO2 - All Figures, In Report

### RO3:
RO3 - Find a Period to Study, Neat Version
RO3 - Calibration of Soil Moisture (EDA)
RO3 - Calibration of Soil Moisture (Scaling)
RO3 - Mass Balance Lysimeters 3, 4, and 7
RO3 - Soil Moisture Halves
