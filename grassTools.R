#grass tools
#utility for getting attribute tables from grass to a R data frame type
#for use with v.select
grassTableToDF=function(grassDF){
  return (read.table(text=grassDF,header=T,sep="|",stringsAsFactors = F))
}

#initialize a re-useable workspace based on a raster.  Defaults set for RMNP area
InitGrass_byRaster=function(rasterPath="C:/Users/sam/Documents/spatial/r_workspaces/findLakeInfluence/loggerSites/Inputs/BigDemWGS84.tif",grassRasterName="dem",grassPath="C:/Program Files/GRASS GIS 7.4.1"){
  
  #'read' dem into R w/ rgdal
  #This doesnt actually load the raster into memory, but it will as needed
  dem=readGDAL(fname=rasterPath)
  
  #I want to work in the projection of the raster - save projection definition as p4
  p4=proj4string(dem)
  
  ##these are necessary to fully trash a pervious grass session
  unlink_.gislock()
  remove_GISRC()
  
  #initialize grass
  initGRASS(grassPath,override=TRUE,mapset="PERMANENT",remove_GISRC = T)
  #note that has no info (yet) about projection, extent, or resolution
  
  #set projection from RGDAL's interpetation of raster proj4string
  execGRASS("g.proj",proj4=p4, flags="c")
  
  #import raster from file, and set extent based on raster.  
  #This DOES NOT set the resolution, why I do not know
  execGRASS("r.in.gdal",input=rasterPath,output=grassRasterName, flags=c("e","o","quiet"))
  
  #this sets the resolution
  execGRASS("g.region",raster=grassRasterName,flags="quiet")
  
  #we can see the region as lat-long or xy
  execGRASS("g.region", flags='l')
  execGRASS("g.region", flags='p')
  
  #set current region as default before creating new mapset
  execGRASS("g.region",flags=c("s","quiet"))
  
  #set up new mapset as workspace, leaving only the dem in PERMANENT mapset
  execGRASS("g.mapset",mapset="wkspace",flags=c("c","quiet"))
  
  #this is kinda pointless here as i am just going to trash the whole grass session and rebuild it every time i need it
  #however, mapsets are like folders which can be used to organize spatial objects
  #mapsets can have different extents and resolutions, but I believe they must have the same projection
  
}

#calculate lengths and heading(s) (segmentwise) of a sp::spatialLinesDataFrame
segLengthHeading=function(feature,smoothScope=1){
  getSegHeadings=function(seg,smoothScope){
    n=nrow(seg@Lines[[1]]@coords)
    if(n>1){
      coordDif=seg@Lines[[1]]@coords[1,]-seg@Lines[[1]]@coords[n,]
      thisHeading=atan2(y=coordDif[2],x=coordDif[1])
      # for(i in 1:(nrow(seg@Lines[[1]]@coords)-1)){
      #   coordDif=seg@Lines[[1]]@coords[(i+1),]-seg@Lines[[1]]@coords[i,]
      #   thisHeading=atan2(y=coordDif[2],x=coordDif[1])
    }
    return(thisHeading*180/pi)
  }
  
  lengths=sapply(feature@lines,getSegHeadings,smoothScope=smoothScope)
  feature$heading_deg=lengths
  return(feature)
}

