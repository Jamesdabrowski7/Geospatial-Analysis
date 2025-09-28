Overview

This project uses R to analyze crime data in relation to zip codes, communities, and schools. It identifies high- and low-crime areas, evaluates school exposure to criminal damage, and visualizes findings with maps and bar charts.

Technologies

R Packages: sf, dplyr, ggplot2, tmap

Data: Shapefiles (.shp) and CSV (crimes1.csv)

Data

zip_codes.shp – Zip code boundaries

communities.shp – Community boundaries

schools.shp – School locations

crimes1.csv – Crime incidents with coordinates

Tasks
Task 1: Crime by Zip Code & Community

Count crimes per zip code and community using st_within().

Identify high- and low-risk areas based on median/mean crime counts.

Map Output:




Task 2: School Exposure

Focus on CRIMINAL DAMAGE TO PROPERTY.

Create 0.5- and 1-mile buffers around schools.

Identify crimes within buffers and visualize results.
