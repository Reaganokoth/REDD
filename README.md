# REDD
REDD package 
This package has two main purposes:

The first one is to provide information about REDD projects around the world; including definitions, forest data and maps. 
The second one is to provide tools for calculating CO2 emissions due to deforestation from the data of a land cover change sample. 

.

For the moment, it contains three functions: 

"definitions", which provides some relevant definitions in REDD+ context and forest facts collected from un-redd.org. These are basic concepts that may change between the countries or projects.

"REDDworld", that shows a map of the situation of forest cover or emissions in the countries of REDD+ projects with data from  CIFOR– CEC – CIRAD – IFRI, 2015 (http://www.reddprojectsdatabase.org/view/countries.php)

"defor_emiss", which calculates the CO2 emissions (in Ton CO2) related to deforestation (forestland that changes to other land cover).

.

The following packages are needed for functions in this package to work:

stringr,
 
 httr, 
 
 xml2, 
  
 magrittr, 
 
 pbapply, 
 
 magick,
 
 tmap,
 
 dplyr

In case these packages are not automatically installed when installing or loading this package, please install them manually.
