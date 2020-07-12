## Exploratory Data Analysis of Accidental Drug Deaths in Connecticut
For this project I performed an exploratory data analysis on accidental drug related deaths in Connecticut from 2012-2018. The dataset was taken from Data.gov and contains information on accidental drug related deaths in Connecticut from 2012 to 2018, excluding 2015 and 2017. The dataset can be found at: https://catalog.data.gov/dataset/accidental-drug-related-deaths-january-2012-sept-2015/resource/44580a89-a260-4844-8ead-35736f395389

The first part of the project consists of data cleaning, as to make the data tidy. The second part contains descriptive graphs showing trends in age, sex, race, drug type, and more. The last part of the project integrates spacial data using `tidycensus`. The map created can be viewed in `Leaflet` - `OpenStreetMap`. 

### Observations

* Far more males than females die from drug overdoses.
* Heroin, fentanyl, and cocaine and their combinations are the most common causes of death.
* Cocaine deaths occur later than heroin and fentanyl deaths. 
* White people constitute the majority of the deaths.
* Black people die from cocaine more than the other drugs.
* The fewest deaths occur in January and the most deaths occur in November.
* Deaths from fentanyl increased significantly in 2016 and 2018.
* In decreasing frequency, death locations include a person's own residence, a hospital, another person's residence, hotels/motels, and vehicles.
* About 36% of injuries were due to the abuse of prescribed medications.

### Example Graphs from Project:
![Top 5 Drugs vs Death Count Per Year](https://github.com/AleahGoldstein/EDA_AccidentalDrugDeaths_Connecticut/blob/master/Accidental_Drug_Related_Deaths_Analysis_files/figure-html/unnamed-chunk-30-1.png)

![Boxplot of Drug Type vs Age](https://github.com/AleahGoldstein/EDA_AccidentalDrugDeaths_Connecticut/blob/master/Accidental_Drug_Related_Deaths_Analysis_files/figure-html/unnamed-chunk-22-1.png)

![Violin Plot of Drug Type vs Age](https://github.com/AleahGoldstein/EDA_AccidentalDrugDeaths_Connecticut/blob/master/Accidental_Drug_Related_Deaths_Analysis_files/figure-html/unnamed-chunk-23-1.png)

### Requirments:
* Please make sure you have your Census API key set in your environment for use with `tidycensus`. See here for more details: https://walker-data.com/tidycensus/reference/census_api_key.html
* Download and install R and RStudio
* Install libraries: `tidyverse`, `ggplot2`, `dplyr`, `forcats`, `stringr`, `tools`, `tidycensus`, `RColorBrewer`, `viridis`, `leaflet`, `sf`, `widgetframe`, `here`

