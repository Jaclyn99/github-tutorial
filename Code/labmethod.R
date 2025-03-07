library(lidR)
library(terra)
library(tidyverse)

# Set Working Directory
setwd("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2015")
wd <- "C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2015"

#Create LAScatalog object from 2015 las tiles
# Create a LAScatalog object by reading all LAS files
cat_2015 <- readLAScatalog("LAS/clipped")

# Check the validity and consistency of the LAScatalog object
las_check(cat_2015)

# Generate a summary of the LAScatalog object, providing key statistics and information
summary(cat_2015)

# Plot the LAScatalog to visualize the spatial distribution of LAS tiles
plot(cat_2015)

#Set the output directory for the filtered .las data
opt_output_files(cat_2015) <- paste(wd, "/Filtered/filtered_2015_{ID}", sep = "")
cat_2015 <- filter_duplicates(cat_2015) #remove duplicate points and speed up processing


#read filtered .las into LAScatalog
filtered_cat_2015 <- readLAScatalog("Filtered")

# Generate a summary of the filtered LAScatalog object
summary(filtered_cat_2015)

# Plot the filtered LAScatalog to visualize the spatial distribution of LAS tiles
plot(filtered_cat_2015)


# Load required library
library(sf)


#Create DEM
dem_allLAS_2015 <- rasterize_terrain(filtered_cat_2015, 2, tin())

#Create color palette
col_1 <- height.colors(50) 

#Plot DEM using color palette
plot(dem_allLAS_2015, col = col_1) #plot in 2D
#plot_dtm3(dem_allLAS_2015) #plot in 3D **DO NOT RUN IF YOUR COMPUTER IS SLOW**


#define LAScatalog engine options
opt_output_files(filtered_cat_2015) <- paste(wd, "/Normalized/norm_2015_{ID}", sep = "")

#normalize all tiles in cat_2015 with the DEM 
norm_tiles_2015 <- normalize_height(filtered_cat_2015, dem_allLAS_2015) #check your folder when complete 


#read normalized las into catalog to continue processing
norm_cat_2015 <- readLAScatalog("Normalized")

#ensure the entire study area was processed
plot(norm_cat_2015)
summary(norm_cat_2015)

#Create CHM for all normalized 2015 Tiles
chm_2015 <- rasterize_canopy(norm_cat_2015, 2, p2r()) 
plot(chm_2015, col = col_1) #plot in 2D


# Load the boundary shapefile (region of interest)
boundary_shapefile_2015 <- st_read("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2015/ubcv_ExportFeatures.shp")

# Convert the boundary shapefile from sf to SpatVector (terra compatibility)
boundary_sp_2015 <- vect(boundary_shapefile_2015)

# Clip DEM based on the boundary region using mask from terra
dem_clipped_2015 <- mask(dem_allLAS_2015, boundary_sp_2015,inverse= TRUE)

# Plot the clipped DEM using color palette
col_1 <- height.colors(50)
plot(dem_clipped_2015, col = col_1)  # Plot in 2D

# Clip CHM based on the boundary region using mask from terra
chm_clipped_2015 <- mask(chm_2015, boundary_sp_2015,inverse= TRUE)

png("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2015/chm_clipped_2015.png", width = 1200, height = 800, res = 150)

# Plot the clipped CHM using color palette
plot(chm_clipped_2015, col = col_1, main = "UBC Vancouver Campus Canopy Height Model (CHM) - 2015", cex.main = 1)

# par(xpd = TRUE)

# Add building polygons
plot(boundary_shapefile_2015["geometry"], add = TRUE, col = "pink", border = "black")

legend("bottomleft", legend = c("UBCV Buildings"), 
       fill = "pink", border = "black", 
       bty = "y", title = "Legend", cex = 0.5, inset = c(0.27, 0.04))  

# Reset par to default after plotting
par(xpd = FALSE)

dev.off()  # Close the graphics device to save the image


# Save the CHM as a GeoTIFF
writeRaster(chm_clipped_2015, filename = "C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2015/chm_clipped_2015.tif", overwrite = TRUE)



# Read the shapefile for the area of interest (AOI)
neighbour_shapefile <- st_read("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/FinalResults/Neighbourhoods/Stadium.shp")

# Convert to SpatVector for compatibility with terra
neighbour_sp <- vect(neighbour_shapefile)

# Load the boundary shapefile (region of interest)
neighbourhoodBuilding_2015 <- st_read("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2015/neighbourhood_buildingclip/Stadium_b.shp")

neighbour_sp <- project(neighbour_sp, crs(chm_clipped_2015))

# Clip the CHM using the boundary shapefile
chm_neighbour_clipped <- mask(chm_clipped_2015, neighbour_sp)

# Get the bounding box of the boundary shapefile
bbox <- st_bbox(neighbour_sp)
# Reset par to default after plotting
# par(xpd = TRUE)

png("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2015/neighbourhood_buildingclip/Stadium.png", width = 1200, height = 800, res = 150)
# Plot the clipped CHM and overlay clipped building polygons (even after removing building point clouds)
plot(chm_neighbour_clipped, col = col_1, main = "Canopy Height Model (CHM) in the Stadium - 2015", cex.main = 1, 
     xlim = c(bbox["xmin"], bbox["xmax"]),
     ylim = c(bbox["ymin"], bbox["ymax"]))

# Add clipped building polygons (for visual representation)
plot(neighbourhoodBuilding_2015["geometry"], add = TRUE, col = "pink", border = "black")  # Add clipped buildings as polygons

legend("bottomleft", legend = c("UBCV Buildings"), 
       fill = "pink", border = "black", 
       bty = "y", # No box around the legend
       title = "Legend", cex = 0.5, inset = c(0.12, 0.02))  # Adjust inset to move it outside the plot area

# Reset par to default after plotting
par(xpd = FALSE)

dev.off()  # Close the graphics device to save the image


# Save the CHM as a GeoTIFF
writeRaster(chm_neighbour_clipped, filename = "C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2015/neighbourhood_buildingclip/Stadium.tif", overwrite = TRUE)







# Set Working Directory
setwd("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2021")
wd <- "C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2021"

#Create LAScatalog object from 2021 las tiles
# Create a LAScatalog object by reading all LAS files
cat_2021 <- readLAScatalog("LAS/clipped")

# Check the validity and consistency of the LAScatalog object
las_check(cat_2021)

# Generate a summary of the LAScatalog object, providing key statistics and information
summary(cat_2021)

# Plot the LAScatalog to visualize the spatial distribution of LAS tiles
plot(cat_2021)

#Set the output directory for the filtered .las data
opt_output_files(cat_2021) <- paste(wd, "/Filtered/filtered_2021_{ID}", sep = "")
cat_2021 <- filter_duplicates(cat_2021) #remove duplicate points and speed up processing


#read filtered .las into LAScatalog
filtered_cat_2021 <- readLAScatalog("Filtered")

# Generate a summary of the filtered LAScatalog object
summary(filtered_cat_2021)

# Plot the filtered LAScatalog to visualize the spatial distribution of LAS tiles
plot(filtered_cat_2021)


# Load required library
library(sf)


#Create DEM
dem_allLAS_2021 <- rasterize_terrain(filtered_cat_2021, 2, tin())

#Create color palette
col_1 <- height.colors(50) 

#Plot DEM using color palette
plot(dem_allLAS_2021, col = col_1) #plot in 2D
#plot_dtm3(dem_allLAS_2021) #plot in 3D **DO NOT RUN IF YOUR COMPUTER IS SLOW**


#define LAScatalog engine options
opt_output_files(filtered_cat_2021) <- paste(wd, "/Normalized/norm_2021_{ID}", sep = "")

#normalize all tiles in cat_2021 with the DEM 
norm_tiles_2021 <- normalize_height(filtered_cat_2021, dem_allLAS_2021) #check your folder when complete 


#read normalized las into catalog to continue processing
norm_cat_2021 <- readLAScatalog("Normalized")

#ensure the entire study area was processed
plot(norm_cat_2021)
summary(norm_cat_2021)

#Create CHM for all normalized 2021 Tiles
chm_2021 <- rasterize_canopy(norm_cat_2021, 2, p2r()) 
plot(chm_2021, col = col_1) #plot in 2D


# Load the boundary shapefile (region of interest)
boundary_shapefile_2021 <- st_read("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2021/ubcv_ExportFeatures.shp")

# Convert the boundary shapefile from sf to SpatVector (terra compatibility)
boundary_sp_2021 <- vect(boundary_shapefile_2021)

# Clip DEM based on the boundary region using mask from terra
dem_clipped_2021 <- mask(dem_allLAS_2021, boundary_sp_2021,inverse= TRUE)

# Plot the clipped DEM using color palette
col_2 <- height.colors(50)
plot(dem_clipped_2021, col = col_2)  # Plot in 2D

# Clip CHM based on the boundary region using mask from terra
chm_clipped_2021 <- mask(chm_2021, boundary_sp_2021,inverse= TRUE)

png("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2021/chm_clipped_2021.png", width = 1200, height = 800, res = 150)

# Plot the clipped CHM using color palette
plot(chm_clipped_2021, col = col_2, main = "UBC Vancouver Campus Canopy Height Model (CHM) - 2021", cex.main = 1)

# Add building polygons
plot(boundary_shapefile_2021["geometry"], add = TRUE, col = "pink")  # Existing buildings

# Correct legend syntax
legend("bottomleft", legend = c("UBCV Buildings"), 
       fill = "pink", border = "black", 
       bty = "y", title = "Legend", cex = 0.5, inset = c(0.27, 0.04))  

# Reset par to default after plotting
par(xpd = FALSE)


dev.off()  # Close the graphics device to save the image

# Save the CHM as a GeoTIFF
writeRaster(chm_clipped_2021, filename = "C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2021/chm_clipped_2021.tif", overwrite = TRUE)





# Read the shapefile for the area of interest (AOI)
neighbour_shapefile <- st_read("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/FinalResults/Neighbourhoods/University_Blvd.shp")

# Convert to SpatVector for compatibility with terra
neighbour_sp <- vect(neighbour_shapefile)

# Load the boundary shapefile (region of interest)
neighbourhoodBuilding_2021 <- st_read("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2021/neighbourhood_buildingclip/University_Blvd_b.shp")

neighbour_sp <- project(neighbour_sp, crs(chm_clipped_2021))

# Clip the CHM using the boundary shapefile
chm_neighbour_clipped <- mask(chm_clipped_2021, neighbour_sp)

# Get the bounding box of the boundary shapefile
bbox <- st_bbox(neighbour_sp)
# Reset par to default after plotting
# par(xpd = TRUE)

png("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2021/neighbourhood_buildingclip/University_Blvd.png", width = 1200, height = 800, res = 150)
# Plot the clipped CHM and overlay clipped building polygons (even after removing building point clouds)
plot(chm_neighbour_clipped, col = col_1, main = "Canopy Height Model (CHM) in the University Blvd - 2021", cex.main = 1, 
     xlim = c(bbox["xmin"], bbox["xmax"]),
     ylim = c(bbox["ymin"], bbox["ymax"]))

# Add clipped building polygons (for visual representation)
plot(neighbourhoodBuilding_2021["geometry"], add = TRUE, col = "pink", border = "black")  # Add clipped buildings as polygons

legend("bottomleft", legend = c("UBCV Buildings"), 
       fill = "pink", border = "black", 
       bty = "y", # No box around the legend
       title = "Legend", cex = 0.5, inset = c(0.013, 0.))  # Adjust inset to move it outside the plot area

# Reset par to default after plotting
par(xpd = FALSE)

dev.off()  # Close the graphics device to save the image


# Save the CHM as a GeoTIFF
writeRaster(chm_neighbour_clipped, filename = "C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2021/neighbourhood_buildingclip/University_Blvd.tif", overwrite = TRUE)












chm_clipped_2021 <- project(chm_clipped_2021, chm_clipped_2015)


# Set NoData values to 0 in chm_clipped_2015 and chm_clipped_2021
chm_clipped_2015[is.na(chm_clipped_2015)] <- 0
chm_clipped_2021[is.na(chm_clipped_2021)] <- 0

# Now calculate the CHM difference
chm_diff <- chm_clipped_2021 - chm_clipped_2015


# Define a color palette for visualization (e.g., red for loss, blue for growth)
col_palette <- colorRampPalette(c("red", "white", "blue"))(100)

# Load new buildings shapefile (Keep as sf object)
newBuildings <- st_read("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/newBuidlings.shp")

png("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/difference_chm.png", 
    width = 1200, height = 800, res = 150)

# Plot CHM difference with the adjusted color scale
plot(chm_diff, col = col_palette, main = "UBC Vancouver Campus Canopy Height Change (2016 - 2021)", 
     cex.main = 1)

# Add new buildings layer
plot(newBuildings["geometry"], add = TRUE, col = "yellow")  

# Add legend
legend("bottomleft", legend = c("Canopy Loss", "Canopy No Change", "Canopy Growth", "New Buildings (2016-2021)"), 
       fill = c("red", "white", "blue", "yellow"), border = "black", 
       bty = "y", cex = 0.51, inset = c(0.27, 0.04))  

par(xpd = FALSE)  # Reset

dev.off()  # Close the graphics device to save the image

# Save the CHM as a GeoTIFF
writeRaster(chm_diff, filename = "C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/chm_diff.tif", overwrite = TRUE)




# Read the shapefile for the area of interest (AOI)
neighbour_shapefile <- st_read("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/FinalResults/Neighbourhoods/Wesbrook_Place.shp")

# Convert to SpatVector for compatibility with terra
neighbour_sp <- vect(neighbour_shapefile)

# Load the boundary shapefile (region of interest)
neighbourhoodBuilding_all_2021 <- st_read("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2021/neighbourhood_buildingclip/Wesbrook_Place_b.shp")

# Load the boundary shapefile (region of interest)
neighbourhoodBuilding_2021 <- st_read("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/2021/neighbourhood_buildingclip/Wesbrook_Place_b_new.shp")

neighbour_all_sp <- project(neighbour_sp, crs(neighbourhoodBuilding_all_2021))
neighbour_sp <- project(neighbour_sp, crs(neighbourhoodBuilding_2021))

# Clip the CHM using the boundary shapefile
chm_neighbour_clipped <- mask(chm_diff, neighbour_sp)

# Get the bounding box of the boundary shapefile
bbox <- st_bbox(neighbour_sp)

# Define a color palette for visualization (e.g., red for loss, blue for growth)
col_palette <- colorRampPalette(c("red", "white", "blue"))(100)

png("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/diff/Wesbrook_Place.png", 
    width = 1200, height = 800, res = 150)

# Plot the clipped CHM first to make sure it is at the bottom
plot(chm_neighbour_clipped, col = col_palette, main = "Canopy Height Change in the Wesbrook Place (2016 - 2021)", 
     cex.main = 1, 
     xlim = c(bbox["xmin"], bbox["xmax"]),
     ylim = c(bbox["ymin"], bbox["ymax"]))

# Plot existing buildings with black border and no fill
plot(neighbourhoodBuilding_all_2021["geometry"], add = TRUE, col = NA, border = "black")  

# Plot new buildings with green border and no fill
plot(neighbourhoodBuilding_2021["geometry"], add = TRUE, col = NA, border = "green", ldw = 3)  

# Add legend
legend("bottomleft", legend = c("Canopy Loss", "Canopy Growth", "Buildings", "New Buildings (2016 - 2021)"), 
       fill = c("red", "blue", NA, NA), 
       border = c("black", "black", "black", "green"), 
       bty = "y", cex = 0.51, inset = c(0.17, 0.04))  

dev.off()  # Close the graphics device to save the image

# Save the CHM as a GeoTIFF
writeRaster(chm_neighbour_clipped, filename = "C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/diff/Wesbrook_Place.tif", overwrite = TRUE)




# Load required libraries
library(terra)
library(sf)
library(ggplot2)

# Load canopy height rasters
chm_thres_2015 <- rast("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/CHM/2015/Hawthorn_Place.tif")
chm_thres_2021 <- rast("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/CHM/2021/Hawthorn_Place.tif")

# Load the neighborhood shapefile
chm_shp <- st_read("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/FinalResults/Neighbourhoods/Hawthorn_Place.shp")

# Calculate the total area of the shapefile in square meters
shp_area <- as.numeric(st_area(chm_shp))# Ensure it's numeric

# Define a range of threshold values (e.g., from 5m to 65m in increments of 1m)
thresholds <- seq(5, 65, by = 1)

# Initialize vectors to store canopy cover percentages
canopy_cover_2015_values <- numeric(length(thresholds))
canopy_cover_2021_values <- numeric(length(thresholds))

# Set cell area (2m × 2m = 4 m²)
cell_area <- 4  

# Calculate canopy cover for each threshold
for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  
  # Create binary rasters (TRUE for pixels above threshold, FALSE otherwise)
  binary_chm_2015 <- chm_thres_2015 > threshold
  binary_chm_2021 <- chm_thres_2021 > threshold
  
  # Count the number of tree-covered cells
  tree_cells_2015 <- sum(values(binary_chm_2015), na.rm = TRUE)
  tree_cells_2021 <- sum(values(binary_chm_2021), na.rm = TRUE)
  
  # Compute canopy cover percentage using shapefile area
  canopy_cover_2015_values[i] <- (tree_cells_2015 * cell_area) / shp_area * 100
  canopy_cover_2021_values[i] <- (tree_cells_2021 * cell_area) / shp_area * 100
}

# Create a data frame for plotting
canopy_cover_df <- data.frame(
  Threshold = rep(thresholds, 2),
  Canopy_Cover = c(canopy_cover_2015_values, canopy_cover_2021_values),
  Year = rep(c("2015", "2021"), each = length(thresholds))
)

# Plot the results
ggplot(canopy_cover_df, aes(x = Threshold, y = Canopy_Cover, color = Year)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Comparison of Canopy Cover Percentages in 2015 and 2021 for Varying Canopy Height Thresholds\n\nHawthorn Place",
    x = "Canopy Height Threshold (m)",
    y = "Canopy Cover Percentage (%)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red")) +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.title.position = "plot",
    plot.title.margin = margin(b = 20)  # Adds extra space below the title
  )

ggsave("C:/Users/jiahui99.stu/OneDrive - UBC/Desktop/initial_results/canopycover_plot/Hawthorn_Place.png",
       width = 10, height = 6, dpi = 300)


