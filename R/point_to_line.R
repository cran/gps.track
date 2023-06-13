#' @title
#' Data.frame with coordinates of points to sf with coordinates of line
#'
#' @description
#' Allows converting data.frame with coordinates of points into sf with coordinates of line.
#'
#' @param data Data frame containing coordinates of points to convert to lines
#' @param col_long String containing the name of the column containing the longitude
#' @param col_lat String containing the name of the column containing the latitude
#' @param crs_proj String containing the proj4string
#'
#'
#' @return
#' returns a sf object with coordinates of line.
#'
#' @export
#'
#'
#'
#' @examples
#' path.file.ex <- base::system.file("extdata", "df_gps.csv", package = "gps.track")
#' df.gps <- read.table(path.file.ex,h=TRUE)
#'
#'
#' df.gps.line <-
#' point_to_line(
#'   data = df.gps,
#'   col_long = "long",
#'   col_lat = "lat",
#'   crs_proj = "+proj=longlat +datum=WGS84"
#' )
#'
#'


point_to_line <- function(data = NULL,
                       col_long = "long",
                       col_lat = "lat",
                       crs_proj = "+proj=longlat +datum=WGS84"){

  l2 <-list()
  for (i in 2:nrow(data)-1){
    line_obj <- sp::Line(cbind(data[col_long][i:(i+1),],data[col_lat][i:(i+1),]))
    lines_obj <- sp::Lines(list(line_obj),ID=i)

    l2[[length(l2)+1]]<-lines_obj
  }

  firstLine <- sp::SpatialLines(l2,
                                proj4string = sp::CRS(crs_proj))
  dfli <- data.frame(data[,!(names(data) %in% c(col_long,col_lat))])
  rownames(dfli) <- NULL

  line_df <- sp::SpatialLinesDataFrame(firstLine,dfli)
  arqline <- sf::st_as_sf(line_df)

  return(arqline)

}


