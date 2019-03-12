---
title: "DEM to Block Diagram - Rayshader Demo"
author: "Andrew Brown"
date: "Last updated: March 12, 2019"
---

# Get the script

This document explains how to use the `rayshader` R package to create 3-D thematic block diagrams from digital elevation models and a thematic shapefile.

Download the __R__ script here: [dem-to-block_diagram.R](dem-to-block_diagram.R)

# Install dependencies

If you don't already have the necessary packages, install them:

```{r, eval=F}
install.packages('rayshader','rgl','raster','rgdal','imager','FedData','viridis')

#note: FedData is only used for creating polygon from rectangular extent
#note: viridis is only used to generate a colorblind friendly color scheme
```

# Setup

There are a few options you will need to customize to run the script with your own data.

## Thematic Shapefile

Set the path to the shapefile/feature class you want to overlay on your 3D landscape.

```{r, eval=F}
thematic_shp <- readOGR(dsn = 'L:/PATH/TO/GEODATABASE/OR/FOLDER.gdb',
                        layer = "yourfeatureclass")
```

## Thematic Attribute
Set the _thematic attribute_ -- the column name in shapefile attribute table containing what you would like to symbolize. 

```{r, eval=F}
mu.col <- "MUSYM"
```

## Digital Elevation Model
Load a raster digital elevation model (TIFF, or other raster-compatible format) for a chunk of space. You can easily create this for a desired extent by panning to area in ArcMap, and _Data_ > _Export Data_ > _By Data Frame_.

```{r, eval=F}
elev <- raster('C:/path/to/your_dem.tif')
```

### OPTIONAL: Resample Raster Input

If needed, you can resample your elevation raster to some other resolution/grid size. Note that the DEM/hillshade and any derived overlays/shadows will all be in the _same resolution_ as the elevation matrix. 

If you have very detailed/large rasters, creating the elevation derivatives will take a long time and _rgl_ (3D visualization package) will run slow.

```{r, eval=F}
## copy elevation raster
elev_template <- elev

## change raster resolution in template (leaving all else the same)
res(elev_template) <- c(5, 5) #5 m by 5 m cells (from 1m x 1m)

## resample elevation raster to desired template 
elev <- resample(elev, elev_template)

## save to file
writeRaster(elev, filename='lidar_resampled_5m.tif')
```

## Setting Custom Colors
After the input thematic shapefile has been rasterized, _viridis_ colors are assigned. By default, a set of colors that spans the color ramp that matches the number of unique levels in `mu.col` is used to render the map.

There is a section of commented-out code that allows you to modify the default set of viridis colors.  Here is the code that you would need to edit.

```{r, eval=F}
# OPTIONAL: set SPECIFIC COLORS for certain theme levels

# calculate number of groups
n.grp <- length(unique(values(theme)))

# generate color palette with n.grp colors
first.colors <- viridis(n.grp)

# set 5th symbol to black (for example)
first.colors[5] <- "#000000" 
## ETC.

alt.cols <- col2rgb(first.colors[values(theme)])

# remove/comment out other code to create `cols` (vector of colors)...

cols <- alt.cols
```