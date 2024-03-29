# dem-to-block_diagram.R

## Last Revised: 06/09/2023
## Authors: Andrew Brown, Andrew Paolucci, Dylan Beaudette
<center>
<img src="sample.png" alt="LiDAR-derived (resampled to 5m x 5m resolution) 3D landscape with SSURGO MUSYM as thematic attribute. Dredge tailings and erosional remnants near Jenny Lind, CA" width="50%">
</center>

# Get the script

This document explains how to use the `rayshader` R package to create 3-D thematic block diagrams from digital elevation models and a thematic shapefile.

Download the __R__ script here: [dem-to-block_diagram-v2-ecosite.R](dem-to-block_diagram-v2-ecosite.R)

This script folder contains *demo_data.zip* which houses two demonstration inputs:

 * _demo_dem.tif_ - LiDAR DEM (1m x 1m resolution) for small extent
 
 * _demo_ssurgo.shp_ (plus accessory files) - SSURGO shapefile corresponding to _demo_dem.tif_ extent
 
Try running the script with these files to see a quick example and make sure all essential components of the script are working.

# Install dependencies

Install the required packages. 

```
install.packages(c('rayshader', 'rgl', 'terra', 'soilDB', 'raster'))
```

# Setup

There are a few options you will need to customize to run the script with your own data. 

The most important are your thematic shapefile (what are you plotting) and the DEM you will use as your base. There are also other options related to the extent of plotting, groups and color palettes.

## Digital Elevation Model

Load a raster digital elevation model (.TIF) for a chunk of space. You can easily create this for a desired extent by panning to area in ArcGIS and exporting the data for current map frame.

If needed, define additional extent constraints (default uses full extent of DEM)

### OPTIONAL: Resampling and Interpolating DEM input

Note that any derived overlays/shadows will all be in the _same resolution_ as the parent DEM. If needed, you can resample your DEM to some other resolution/grid size.

If you have very detailed/large rasters, creating the derivatives will take a long time and _rgl_ (3D visualization package) will run slow. It will be to your benefit to reduce resolution in this case.

```
target_resolution <- c(5, 5) # target is 5m x 5m grid
```

## Thematic Shapefile

The default example uses the extent of your DEM to download SSURGO geometry. It then pulls corresponding ecological site information and symbolizes the map based on `"ecoclassid"`.

Alternately, you can use any feature class you want to overlay on your 3D landscape, for example:

```
thematic_shp <- st_read('my_extent.shp', stringsAsFactors = FALSE)
```

## Thematic Attribute
Set the _thematic attribute_ -- the column name in shapefile attribute table containing what you would like to symbolize.  

```r
## thematic attribute - the column name in shapefile attribute table 
mu.col <- "MUSYM"
```

If necessary, specify any levels to omit with `omit.groups`.

```r
# group levels in mu.col to omit from result
omit.groups <- c("W")
omit.color <- NA
```
<!--
Depending on DEM origin/level of detail there may be value in performing some interpolation on the resampled result. The DEM will be resampled first. To interpolate by inverse distance weighting, set `idw_smooth` to `TRUE`. By default, a 7x7 focal median is taken to reduce any noise introduced by the interpolation. You also must set the percentage of the input DEM to sample for supporting interpolation -- a higher percentage will result in greater match between your interpolated result and your input data.

This routine could be used to remove artifacts in low-relief, low-detail areas _OR_ smooth spurious/unnecessary detail in high-detail datasets. This option may be challenging to use/have unintended effects in low-detail DEMs with strongly contrasting relief. Future efforts may try to use a relief "threshold" for applying interpolation to only a subset of the DEM (e.g. only where "contour" spacing is greater than X meters).

```
## 5. Apply inverse-distance weighting interpolation to minimize DEM artifacts?
idw_smooth <- FALSE
focal_length <- 7 # size of focal window (an n x n square)
pct_dem_train <- 15 # random % of DEM to use in spatial interpolation (100% = exact match)
gstat.nmax <- 5 # number of neighbors to use in making prediction
```
-->

## Setting Custom Colors
After the input thematic shapefile has been rasterized, colors are assigned. By default, a set of colors that spans the color ramp that matches the number of unique levels in `mu.col` is used to render the map.

There is a section of commented-out code that allows you to modify the default set.  Here is the code that you would need to edit. Note you will need to account for any groups you are omitting in the indexing of your color vector.

```r
# replace individual colors (Optional) 
## RGB Method new.colors[4] <- rgb(0,0,132/255)
#new.colors[1] <- "#E4A358" 
#new.colors[2] <- "#A0B7CB" 
#new.colors[3] <- "#A1CC7D" 
#new.colors[4] <- "#FFFFB3" 
#new.colors[5] <- "#FDBF6F" 
#new.colors[6] <- "#999999" 
```

## Changelog:

 * _2019/03/12_ - initial commit
 
 * _2019/04/05_ - replaced `raster::rasterize()` with `fasterize::fasterize()` for making thematic raster much faster
 
 * _2020/02/28_ - updates to conform with new _rayshader_ 0.13.x; removed _FedData_, _imager_, _viridis_ dependencies; added _gstat_ IDW interpolation option; added _group.omit_ option; better color selection code and implementation of masking from Andy Paolucci; switched over to using _sf_ for most spatial operations
 
 * _2022/04/29_ - various updates (unpublished) to make rayshader, terra etc. work together with most recent API. Uses 
 
 * _2023/06/09_ - version 2 released; uses rayshader, sf, terra; ecological site example of adding ancillary thematic data to SSURGO polygon geometry
