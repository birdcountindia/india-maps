# see https://github.com/ashwinv2005/create-eBird-data-temp/blob/master/functions.R

## requires shapefiles and several packages - path1 = India; path2 = India States; 
## path3 = India Districts
## provide path to folder and name of file within

## this can be edited for more flexibility with grid sizes; current default is 25,50,100,200

## current default args are c("India","India_2011","India States","IndiaStates_2011","India Districts","IndiaDistricts_2011")

## saves a workspace image called "maps.RData"

createmaps = function(g1=25,g2=50,g3=100,g4=200,path1="India",name1="India_2011",path2="India States",
                      name2="IndiaStates_2011",path3="India Districts",name3="IndiaDistricts_2011")
{
  require(tidyverse)
  require(rgdal)
  require(sp)
  require(sf)
  
  # reading maps
  
  assign("indiamap",readOGR(path1,name1),.GlobalEnv)
  assign("statemap",readOGR(path2,name2),.GlobalEnv)
  assign("districtmap",readOGR(path3,name3),.GlobalEnv)
  
  # creating SPDF grids below that can be intersected with various maps and overlaid on to data
  
  bb = bbox(indiamap) # creates a box with extents from map
  cs = c(g1*1000/111111,g1*1000/111111)  # cell size g1 km x g1 km
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd) # create required grids
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd))) # create spatial grid data frame
  nb4g1 = gridIndex2nb(sp_grd, maxdist = sqrt(1), fullMat = TRUE) # creates list of neighbours
  nb8g1 = gridIndex2nb(sp_grd, maxdist = sqrt(2), fullMat = TRUE) # creates list of neighbours
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame") # SGDF to SPDF
  assign("nb4g1",nb4g1,.GlobalEnv)
  assign("nb8g1",nb8g1,.GlobalEnv)
  assign("gridmapg1",sp_grd_poly,.GlobalEnv)
  
  bb = bbox(indiamap)
  cs = c(g2*1000/111111,g2*1000/111111) 
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  nb4g2 = gridIndex2nb(sp_grd, maxdist = sqrt(1), fullMat = TRUE)
  nb8g2 = gridIndex2nb(sp_grd, maxdist = sqrt(2), fullMat = TRUE)
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame")
  assign("nb4g2",nb4g2,.GlobalEnv)
  assign("nb8g2",nb8g2,.GlobalEnv)
  assign("gridmapg2",sp_grd_poly,.GlobalEnv)
  
  bb = bbox(indiamap)
  cs = c(g3*1000/111111,g3*1000/111111) 
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  nb4g3 = gridIndex2nb(sp_grd, maxdist = sqrt(1), fullMat = TRUE)
  nb8g3 = gridIndex2nb(sp_grd, maxdist = sqrt(2), fullMat = TRUE)
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame")
  assign("nb4g3",nb4g3,.GlobalEnv)
  assign("nb8g3",nb8g3,.GlobalEnv)
  assign("gridmapg3",sp_grd_poly,.GlobalEnv)
  
  bb = bbox(indiamap)
  cs = c(g4*1000/111111,g4*1000/111111) 
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  nb4g4 = gridIndex2nb(sp_grd, maxdist = sqrt(1), fullMat = TRUE)
  nb8g4 = gridIndex2nb(sp_grd, maxdist = sqrt(2), fullMat = TRUE)
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame")
  assign("nb4g4",nb4g4,.GlobalEnv)
  assign("nb8g4",nb8g4,.GlobalEnv)
  assign("gridmapg4",sp_grd_poly,.GlobalEnv)
  
  # indiamap = spTransform(indiamap,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  # not required here, CRS is NA
  
  x = as(indiamap,"sf") %>% sf::st_buffer(dist=0)
  # to calculate total number of grids of each size
  c1 = st_intersection(as(gridmapg1,"sf"), x)
  areag1 = data.frame(id = as.character(c1$id), area = round(st_area(c1)*12345.65))
  c1 = as(c1,"Spatial")
  c2 = st_intersection(as(gridmapg2,"sf"), x)
  areag2 = data.frame(id = as.character(c2$id), area = round(st_area(c2)*12345.65))
  c2 = as(c2,"Spatial")
  c3 = st_intersection(as(gridmapg3,"sf"), x)
  areag3 = data.frame(id = as.character(c3$id), area = round(st_area(c3)*12345.65))
  c3 = as(c3,"Spatial")
  c4 = st_intersection(as(gridmapg4,"sf"), x)
  areag4 = data.frame(id = as.character(c4$id), area = round(st_area(c4)*12345.65))
  c4 = as(c4,"Spatial")
  area = round(st_area(x)*12345.65)
  
  
  totalcells = c(length(unique(fortify(c1)$id)),length(unique(fortify(c2)$id)),
                 length(unique(fortify(c3)$id)),length(unique(fortify(c4)$id)))
  
  assign("areag1",areag1,.GlobalEnv)
  assign("areag2",areag2,.GlobalEnv)
  assign("areag3",areag3,.GlobalEnv)
  assign("areag4",areag4,.GlobalEnv)
  assign("totalcells",totalcells,.GlobalEnv)
  assign("area",area,.GlobalEnv)
  assign("gridlevels",c(g1,g2,g3,g4),.GlobalEnv)
  
  rm(list=setdiff(ls(envir = .GlobalEnv), c("districtmap", "statemap", "indiamap", "gridmapg1", 
                                            "gridmapg2", "gridmapg3", "gridmapg4","nb4g1",
                                            "nb4g2", "nb4g3", "nb4g4", "nb8g1", "nb8g2", "nb8g3", 
                                            "nb8g4", "totalcells", "gridlevels","area",
                                            "areag1","areag2","areag3","areag4")), 
     pos = ".GlobalEnv")
  
  save.image("maps.RData")
  
  rm(list=setdiff(ls(envir = .GlobalEnv), c("nb4g1", "nb4g2", "nb4g3", "nb4g4", 
                                            "nb8g1", "nb8g2", "nb8g3", "nb8g4",
                                            "totalcells","gridlevels","area",
                                            "areag1","areag2","areag3","areag4")), 
     pos = ".GlobalEnv")
  
  save.image("neighbours.RData")
  
  load("maps.RData", envir = globalenv())
  
  rm(list=setdiff(ls(envir = .GlobalEnv), c("districtmap", "statemap", "indiamap", "gridmapg1", "gridmapg2", 
                                            "gridmapg3", "gridmapg4",
                                            "totalcells","gridlevels","area",
                                            "areag1","areag2","areag3","areag4")), 
     pos = ".GlobalEnv")
  
  save.image("maps.RData")
  
  rm(districtmap,statemap,indiamap,gridmapg1,gridmapg2, 
     gridmapg3,gridmapg4,
     totalcells,gridlevels,area,
     areag1, areag2, areag3, areag4, pos = ".GlobalEnv")
}