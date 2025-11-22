# Anthropocene Landscape Succession API

This R/Shiny application computes aquifer levels, land‑use history, and rainfall/irrigation volumes for any polygon, whether preset by the developer or drawn by the user. For each polygon, it returns three Plotly charts suitable for embedding in external applications (e.g., OpenLayers).

## What the app does
- Computes water‑level trends using cleaned Kansas well data  
- Computes PRISM rainfall converted to acre‑feet  
- Computes irrigated vs. non‑irrigated land area  
- Computes land‑use composition per year (BLULC rasters)  
- Returns three interactive Plotly charts  
- Auto‑caches results for instant future access  
- Supports county & GMD presets or arbitrary polygons  

==============================

## DEPLOYMENT

--Download the data folder <a href="https://www.dropbox.com/scl/fi/5rhcmsp4897aotyj5ygzk/data.zip?rlkey=wgpsoyyefj6y0cvty1x1llmxd&st=gtfwweae&dl=0" target="_blank">here</a>.

--Unzip the data folder and put it together with app.R and R/ folder. 

--View with R Studio, or place the entire folder in a shiny-server folder.

--See the current state of the project online:

<a href="https://hidinginplainssight.org" target="_blank">https://hidinginplainssight.org</a>


## Calling the app from an external client  
The app responds to HTTPS GET requests at:

https://flows.rivertoday.org/aquifer_plots/

### Supported query parameters
**Preset polygon**  
?preset=COUNTY:14
?preset=GMD:3

**Bounding box**  
?bbox=minLon,minLat,maxLon,maxLat

**GeoJSON polygon**  
?geojson={...}

**WKT polygon**  
?wkt=POLYGON((...))

============================

## MODULE OVERVIEW (R/ folder)

To facilitate review, the app has been divided into modules launched by app.R. The modules are found in the R/ folder. A brief description of each one follows.

### **preset_utils.R**
Loads county/GMD presets, resolves `preset=COUNTY:x`, and provides polygon + label.  
Also includes the full prewarm engine (`HP_PREWARM=COUNTY/GMD/ALL`).

### **query_parser.R**
Parses incoming URL parameters and extracts a valid sf polygon.

### **cache_utils.R**
Simple disk‑based caching under `data/hp_cache`.  
Keys stable across runs.

### **raster_utils.R**
Low‑level PRISM raster handling, cropping, extraction.

### **landuse_utils.R**
Extracts BLULC land‑use fractions yearly using exactextractr.

### **irrigation_utils.R**
Loads irrigation mask and computes irrigated area.

### **aquifer_utils.R**
Loads and filters water‑level time series.

### **compute_result.R**
Central pipeline that produces:
- area_km2  
- aquifer time series  
- landuse fractions  
- rainfall AF  
- irrigation AF  
- combined year-wise dataframe  

### **plot_ppt.R / plot_landuse.R / plot_aquifer.R**
Generate Plotly visualizations for final outputs.

## Prewarming how-to:
Run inside the app directory:

HP_PREWARM=COUNTY Rscript app.R
HP_PREWARM=GMD Rscript app.R
HP_PREWARM=ALL Rscript app.R

Cached outputs saved in `data/hp_cache`.

## License
MIT
