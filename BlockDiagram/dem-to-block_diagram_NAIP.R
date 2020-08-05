# dem-to-block_diagram_NAIP.R

# revised for use with NAIP as overlay

# last revised: 08/04/2020
# @authors: andrew paolucci, andrew brown, dylan beaudette

library(rgl)       # 3D rendering
library(av)        # MPEG output
library(scales)    # Scaling of RGB matrix
library(raster)    # Raster data manipulation
library(rayshader) # Rayshading

#### SETUP

# I created these TIF files in QGIS. You can do it the ~same way in ESRI products
#   - Pan to desired area
#   - Right click DEM in Browser and select "Export" >> "Save As..."
#   - Set CRS and resolution to common target (0.6m is NAIP resolution; 3m is good for computation)
#   - Set Extent to "Map view extent"
#   - Repeat with identical parameters and extent for NAIP .sid 

# Default assumption is both of these are in same resolution/CRS/extent
elev_orig <- raster('demo_dem2.tif')
names(elev_orig) <- "elev"

theme <- stack("naip_overlay.tif")
names(theme) <- c("r","g","b")

# stack the elevation and theme rasters
# if this does not work, then you need to be more careful matching up your exports
dat <- try(stack(elev_orig, theme))
if (inherits(dat, 'try-error'))
  stop("check to make sure CRS, resolution and bounding box are IDENTICAL", call. = FALSE)

agg.factor <- 5 # 5 * 0.6m = 3m resolution
dat.agg <- aggregate(dat, agg.factor)

# convert elevation raster -> matrix
elmat <- rayshader::raster_to_matrix(dat.agg$elev)

# convert NAIP into RGB matrix
r_cropped <- rayshader::raster_to_matrix(dat.agg$r)
g_cropped <- rayshader::raster_to_matrix(dat.agg$g)
b_cropped <- rayshader::raster_to_matrix(dat.agg$b)

rgb_array <- array(0, dim = c(nrow(r_cropped), ncol(r_cropped), 3))

rgb_array[,,1] <- r_cropped / 255 
rgb_array[,,2] <- g_cropped / 255 
rgb_array[,,3] <- b_cropped / 255 

rgb_array <- aperm(rgb_array, c(2,1,3))
rgb_contrast <- scales::rescale(rgb_array, to = c(0,1))

# inspect 2D NAIP arrays
plot_map(rgb_array)
plot_map(rgb_contrast)

if (!dir.exists("NAIP/animation/static/"))
  dir.create("NAIP/animation/static/", recursive = TRUE)

# animate
rayshader::plot_3d(rgb_contrast, elmat, zscale = 2, fov = 0, theta = -45,
          zoom = 0.7, phi = 25, windowsize = c(1000,800), lineantialias = TRUE)
rayshader::render_camera(theta = -45, zoom = 0.7, phi = 25)
angles <- seq(0, 360, length.out = 1441)[-1]
for (i in 1:length(angles)) {
  rayshader::render_camera(theta = -45 + angles[i], zoom = 0.7, phi = 25)
  rayshader::render_snapshot(filename = sprintf("NAIP/animation/static/ca649_huntersvalley_%i.png", i), 
                  title_text = "Mariposa County, CA (Hunters Valley) | NAIP 2018 + NED 10m DEM",
                  title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
}
rgl::rgl.close()

av::av_encode_video(sprintf("NAIP/animation/static/ca649_huntersvalley_%i.png", 1:length(angles)), 
                    framerate = 30,
                    output = "NAIP/animation/ca649_huntersvalley.mp4")


