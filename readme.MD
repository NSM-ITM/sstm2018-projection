# Shanghai Science and Technology Museum - Visitor Prediction Prototype
SSTM - National Science Museum, Thailand Staff Exchange Program 2018

## Prediction Pipeline
Prediction pipeline in this application can be depiected as follows:

```
Weather Data Website 
        \
          [Web Scrapper - PY] 
                \
              Weather Data --
                              \
                               --> [Data Mapping - PY] --> [Data Modeling & Prediction - R]
                              /
        Real Visitor Data ---
```
                 
This predictive analysis prototype demonstrates the possibility to utilize collected data and create the basic future projection of some specific data dimensions. Also, it is an example of data science product development with rapid prototyping methodology which involves various kinds of data and multidisciplinary skills; delivered as part of SSTM-NSM staff exchange program 2018.

For live preview, please visit: [Prediction App](https://wachiraphc.shinyapps.io/SSTM2018-VisitorProjection/) - usually takes 1 minutes for first launch/refresh

## Data Preparation
### Weather Data Scraper

Hypothetical assumption of this analysis is that weather condition could affect the number of museum visitors. A web scraper tool was create to automatically retrieve weather from following website.
```
http://www.tianqihoubao.com/lishi/shanghai
```
Web scraper was written in Python 3. Please refer to Jupyter Notebook *"Weather Scraper.ipynb"* for more information. Daily weather data of each year would be saved in each file win following format:

```
Scraped Weather Data
---------------------------------------------------------------------
| Date |  Weather 1  | Weather 2 | Min Temp | Max Temp | Wind Speed |
---------------------------------------------------------------------
```

### Data Mapping & Reformat
Real visitor data CSV file is in following format.
```
Real Visitor Data
-------------------------
| Date |  Visitor Count |
-------------------------
```

Mapping script was written in Python 3. Please refer to Jupyter Notebook *"DataPrep-Mapping.ipynb"* for more information. Final data for modeling process is composed of following columns.
```
date
minTemp
maxTemp 
clear
cloudy
rain
snow
storm
wind 
weekend
holiday
visitorTotal
visitorMale 
visitorFemale
visitorKids
VisitorTeenage
VisitorAdult
VisitorElderly
OverMonthlyAverage
```
Some columns were prepared for future implementation, thus, currently unused by the predictive analysis.

## Data Modeling
### Model Creation

In order to project future value of a dataset from a given set od features, many regression models can be used with different characteristics and properties. Following regression models were selected in this prototype due to their readability to work on prepared dataset without much modification.

* Linear Regression
* Recursive Partitioning and Regression Trees
* Random Forest
* Artificial Neural Network

### Prediction

All models will be newly generated each time the application is launched. As of now, according to the available data (3 years), the start up process normally takes less than 1 minute. Once models are generated *"Model Preview"* shows the overall prediction result from each model in comparison with real visitor data. User can manually input the data for individual prediction in *"Model Projection"* . For statistical reliability analysis, technical information of each model is shown in *"Model Description"*.

* As per the philosophy of rapid prototyping schema, this prototype is merely created for a demonstration of basic predictive analytic pipeline. Therefore, models are not, by any means, finely configured for better accuracy or tuned for better prediction performance.

