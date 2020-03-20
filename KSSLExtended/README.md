## Geochemical, Optical and X-Ray Data w/ `fetchKSSL()` 
### Author: Andrew G. Brown
### Last updated: 2020/03/19

With a new argument to `fetchKSSL`, `returnGeochemicalData = TRUE`, the result is a _list_, rather than a `SoilProfileCollection`. This is similar, and complementary to, the `returnMorphologicData` argument that retrieves NASIS pedons linked to each KSSL `pedlabsampnum`.

##### **For each call to `fetchKSSL(..., returnGeochemicalData = TRUE)` you get a named list with following elements:**

 - _SoilProfileCollection_ in `$SPC` 
 
 - _Geochemical/Elemental Analysis_ in `$geochem`
 
 - _Optical Glass/Grain Counts_ in `$optical`
 
 - _X-ray Diffraction and Thermal_ in `$xrd_thermal`
 
### Contents

##### **There are three demos currently in this folder; one for each of the "extended" KSSL tables.**
  
__1. Geochem:__ Trace Heavy Metals Comparison By MLRA (127, 147, 148) - [fetchKSSL_heavymetals_demo.R](fetchKSSL_heavymetals_demo.R)

__2. X-Ray:__ Comparison of Coefficient of Linear Extensibility & Montmorillonite XRD Peak Size for Selected Southern U.S. MLRAs (85, 86A, 86B, 133A, 135A, 135B) - [fetchKSSL_xrd-cole_demo.R](fetchKSSL_xrd-cole_demo.R)

__3. Optical:__ Total Volcanic Glass Percentage Comparison By MLRA (1, 3, 22A, 22B) - [fetchKSSL_glasscount_demo.R](fetchKSSL_glasscount_demo.R)


### Future Directions

##### **Eventually, these demos will be polished and turned into more complete "tutorials".**

Making the geochemical, optical and X-Ray data available in this fashion has opened up many, many new avenues for analysis. These new data sources will complement more typical investigations of field and lab data, allowing us to be more explicit about our models, assumptions and underlying data.

Using examples like the ones above, generalizations to the workflows will be identified. Then, helper functions can be created to deal with the complexities. 

The author is open to suggestions on how to make the data more accessible. Please feel free to contact Andrew Brown with any questions [andrew.g.brown@usda.gov](andrew.g.brown@usda.gov)
