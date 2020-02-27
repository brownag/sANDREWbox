---
title: "DEM to Block Diagram - Rayshader Demo"
author: "Andrew Brown"
date: "Last updated: April 5th, 2019"
---
<center>

![LiDAR-derived (resampled to 5m x 5m resolution) 3D landscape with SSURGO MUSYM as thematic attribute. Table Mountain, Tuolumne County, California](sample.png "LiDAR-derived (resampled to 5m x 5m resolution) 3D landscape with SSURGO MUSYM as thematic attribute. Table Mountain, Tuolumne County, California")

</center>

# Get the script

This document explains how to use the `rayshader` R package to create 3-D thematic block diagrams from digital elevation models and a thematic shapefile.

Download the __R__ script here: [dem-to-block_diagram.R](dem-to-block_diagram.R)

# Install dependencies

If you don't already have the necessary packages, install them:

```{r eval=F}

install.packages('rayshader','rgl','raster',
                  'rgdal','gstat','viridis',
                  'sf','fasterize')
```

# Setup

There are a few options you will need to customize to run the script with your own data.

## Thematic Shapefile

Set the path to the shapefile/feature class you want to overlay on your 3D landscape.

```{r, eval=F}
## 1. read shapefile for overlay (must cover full extent of elevation .TIF)
#       for example, ssurgo data symbolized on musym
thematic_shp <- readOGR(dsn='C:/PATH/TO/A/GEODATABASE.gdb',
                        layer="YOURLAYERNAME", stringsAsFactors = FALSE)
```

## Thematic Attribute
Set the _thematic attribute_ -- the column name in shapefile attribute table containing what you would like to symbolize.  If necessary, specify any levels to omit with `omit.groups`.

```{r, eval=F}
## 2. thematic attribute - the column name in shapefile attribute table 
mu.col <- "MUSYM"

# OPTIONAL: levels in mu.col to omit from result (uncomment if needed)
# omit.groups <- c("W","8034","7078","7076","7079","7083","7085")
```

## Digital Elevation Model
Load a raster digital elevation model (TIFF, or other raster-compatible format) for a chunk of space. You can easily create this for a desired extent by panning to area in ArcMap, and _Data_ > _Export Data_ > _By Data Frame_.

```{r, eval=F}
## 3. digital elevation model (TIFF, or other raster-compatible format) for a chunk of space
#     e.g. pan to desired area in ArcMap, and Data > Export Data > By Data Frame
elev_orig <- fasterize::raster('YOURDEM.tif')
```

### OPTIONAL: Resampling and Interpolating DEM input

If needed, you can resample your elevation raster to some other resolution/grid size. Note that the DEM/hillshade and any derived overlays/shadows will all be in the _same resolution_ as the elevation matrix. 

If you have very detailed/large rasters, creating the elevation derivatives will take a long time and _rgl_ (3D visualization package) will run slow. It will be to your benefit to reduce resolution in this case.

```{r, eval=F}
## 4. OPTIONAL: resample raster input, default is same as DEM
target_resolution <- c(5,5) # target is 5m x 5m grid
```

Depending on DEM origin/level of detail there may be value in performing some interpolaton on the resampled result -- to remove artefacts or unnecessary detail. Set `idw_smooth` to TRUE to use this feature. By default, the 7x7 focal median is taken following the interpolation. Also, select the percentage of the input DEM to use in the IDW interpolation.

```{r, eval=F}
## 5. Apply inverse-distance weighting interpolation to minimize DEM artefacts?
idw_smooth <- TRUE
focal_length <- 7 # size of focal window (an n x n square)
pct_dem_train <- 15 # percentage of DEM to use in spatial interpolation (100% = exact match)
```

## Setting Custom Colors
After the input thematic shapefile has been rasterized, _viridis_ colors are assigned. By default, a set of colors that spans the color ramp that matches the number of unique levels in `mu.col` is used to render the map.

There is a section of commented-out code that allows you to modify the default set of viridis colors.  Here is the code that you would need to edit.

## Changelog:

 * _2019/03/12_ - initial commit
 
 * _2019/04/05_ - replaced `raster::rasterize()` with `fasterize::fasterize()` for making thematic raster much faster
 
 * _2020/02/27_ - updates to conform with new `rayshader`; removed _FedData_ and _imager_ dependencies; added gstat IDW interpolation option; added group omission option