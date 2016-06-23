xyConv <- function(df, xy = c('long_x', 'lat_y'), CRSin = '+proj=longlat',
                   CRSout = '+proj=utm +zone=11') {
  df <- df[complete.cases(df[, xy]), ]
  coord <- data.frame(df[, xy])
  colnames(coord) <- c('x', 'y') 
  coord[, 1] <- as.numeric(coord[, 1])
  coord[, 2] <- as.numeric(coord[, 2])
  conv <- SpatialPoints(coordinates(coord),
                        proj4string = CRS(CRSin))
  conv <- spTransform(conv, CRS(CRSout))
  conv <- data.frame(conv)
  colnames(conv) <- c('x', 'y')
  df <- cbind(df, conv)
  return(df)
}

intPlot <- function(dat, xval, yval, colval, type, fillval = colval,
                    groupval = NULL, facetval = NULL) {
  gg <- ggplot(dat, aes_string(x = xval, y = yval))
  
  if (type == 'Scatter') {
    gg <- gg + geom_point(aes_string(color = colval)) +
      scale_color_gdocs()
  } else if (type == 'Box') {
    gg <- gg + geom_boxplot(aes_string(fill = fillval)) +
      scale_fill_gdocs()
  } else if (type == 'Bar') {
    gg <- gg + geom_bar(aes_string(color = NULL, fill = fillval)) +
      scale_fill_gdocs()
  } else if (type == 'Violin') {
    gg <- gg + geom_violin(aes_string(fill = fillval)) +
      scale_fill_gdocs()
  }
  
  gg <- gg + theme_bw()
  
  if (facetval != 'None') {
    gg <- gg + facet_wrap(facetval)
  }
  
  return(gg)
}