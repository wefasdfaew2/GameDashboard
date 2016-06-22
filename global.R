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