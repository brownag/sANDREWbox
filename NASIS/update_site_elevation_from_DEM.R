library(aqp)
library(soilDB)
library(raster)

f <- get_site_data_from_NASIS_db()
coordinates(f) <- ~ x_std + y_std
proj4string(f) <- "+proj=longlat +datum=WGS84"

r <- raster("L:/NRCS/MLRAShared/Geodata/project_data/MUSum_10m_SSR2/SSR2_DEM10m_AEA.tif")

f$elev_new <- extract(r, f)
plot(f$elev_field ~f$elev_new)

write.table(data.frame(siteiid=f$siteiid, new_elev=f$elev_new), 
            row.names = FALSE, col.names = FALSE, sep = "|", 
            file = "C:/data/updtelev.txt")
