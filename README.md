
# smhimeteo

**smhimeteo** is an R package for downloading meteorological data from the Swedish Meteorological and Hydrological Institute (SMHI). It allows users to list available parameters, explore stations and time periods, download meteorological datasets, and find nearby stations using spatial coordinates.

## Installation

You can install the development version of `smhimeteo` from GitHub using:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install smhimeteo from GitHub
devtools::install_github("AlbertMorera/smhimeteo")

library(smhimeteo)

```

## Basic Usage

### 1. List available meteorological parameters

```r
get_smhi_parameters()
```

This function returns a data frame with all available meteorological parameters, such as temperature, precipitation, wind, etc (names are in Swedish).

### 2. List stations for a given parameter

```r
get_smhi_station("1")
```

Replace `"1"` with the key of the parameter you are interested in (e.g., temperature).

### 3. List available time periods for a station and parameter

```r
get_smhi_period("188790", "1")
```

This returns the available time resolutions (e.g., daily, hourly, monthly) for the specified station (`"188790"`) and parameter (`"1"`).

### 4. Download meteorological data

```r
data <- get_smhi_data(
  station_key = "188790",
  parameter_key = "1",
  period = "latest-hour"
)
```

This retrieves the most recent hourly data for temperature at station 188790.

## Working with Spatial Data

### 1. Create a spatial point using `sf`

```r
library(sf)
library(tibble)

point <- st_as_sf(
  tibble(x = 16.43, y = 59.69),
  coords = c("x", "y"),
  crs = 4326
)
```

### 2. Find the closest stations for specific parameters and a given point

```r
get_closest_stations(point, c("1", "5"), "latest-months", n = 3)
```

This function finds the 3 closest stations for parameters "1" and "5" (e.g., temperature and precipitation) based on the spatial coordinates of the point.

### 3. Get the closest station using `get_smhi_closest()`

```r
get_smhi_closest(point, "1", "latest-months", verbose = TRUE, n = 1, max_radius = 100)
```

This retrieves the closest station for parameter "1" (e.g., temperature) within a 100 km radius.

## Cite this package
Morera, A. (2025). smhimeteo: an R package for accessing meteorological data from SMHI. GitHub repository URL. https://github.com/AlbertMorera/smhimeteo
