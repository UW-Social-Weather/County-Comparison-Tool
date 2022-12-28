County Comparison Tool
=====

Initialized December 28 2022

Code associated with the UW PHI & Social Weather "County Comparison Tool."

## Project Purpose
-----

The County Comparison Tool organizes counties according to Social Vulnerability and population size in order to facilitate accurate comparisons across various health indicators. The dashboard can be accessed here [https://rsc.csde.washington.edu/content/72927f50-3d4f-4343-b145-add0a248a9a1].

## Basic Organization
-----

Folders contain different sections necessary to run the dashboard including:

## 1. data/
 This folder includes both data that is used in the analysis and the scripts in R and Python that were used to prepare the files.

## 2. healthdown/
 Original code that was adapated to create this dashboard. 

## 3. rsconnect/
 This folder contains the necessary metadata to publish the dashboard to the existing server at the University of Washington.

## 4. shapes/
 This folder contains the shapefiles necessary to create the map visualizations.

## 5. www/
 This folder contains the necessary HTML files that are used to modify the appearance of the dashboard.

## Key Considerations
-----
* Currently the dashboard is hosted on a server at the University of Washington. You can also run the dashboard locally as the code included here is self-contained. 
Future edits and modifications to the public dashboard will require publishing the dashboard to a new R Shiny Server. 

* 
