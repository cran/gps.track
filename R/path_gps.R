#' @title
#' GPS track point information extraction.
#'
#' @description
#' Allows extracting and generating new information from track points data collected with GPS.
#'
#' @param filename string designating the file .gpx or .shp path in geographic coordinates
#' @param layer string that designates the layer with the trackpoint data when arq_type = "gpx", in case arq_type = "shp" layer is ignored
#' @param time_zone string designating the gps default time zone
#' @param zone_correction string designating the time zone for correction
#' @param arq_type string that designates whether the file is type "shp" or "gpx", "shp" default
#'
#' @return
#' returns a data frame with information about time, coordinates, elevation, distance, speed, elevation difference and azimuth (always calculated in relation to the later point)
#'
#' @export
#'
#'
#' @examples
#'
#'
#' path.file.ex <- base::system.file("extdata", "trajeto_teste.shp", package = "gps.track")
#' df.gps <-
#' path_gps(
#'   filename = path.file.ex,
#'   time_zone = "Etc/GMT-0",
#'   zone_correction = "Etc/GMT+3",
#'   arq_type = "shp"
#' )
#'
#'



path_gps <- function(
    filename = NULL,
    layer = "track_points",
    time_zone = "Etc/GMT-0",
    zone_correction = "Etc/GMT+3",
    arq_type = c("shp","gpx")

  ){

  tryCatch({
    if(length(arq_type>1)){arq_type = "shp"}

    #lendo de acordo com o arquivo
    if(arq_type=="gpx"){
      mygps <- sf::st_read(filename,
                           layer = layer)
      coords <- data.frame(sf::st_coordinates(mygps))
      coords$Z <- mygps$ele
    }else{ #==shp
      mygps <-sf::st_read(filename)
      coords <- data.frame(sf::st_coordinates(mygps))
      #pode ser que o "ele" nao exista, mas que o "Z" exista no "coords", por isso nao tem problema ignorar esse erro nesse momento.
      tryCatch({coords$Z <- mygps$ele}, error=function(e){})
    }

    #salvando e configurando data e tempo
    time <-
      data.frame(X1 = do.call("rbind",
                              strsplit(mygps$time, ".000", fixed = T)))


    time$X1 <- as.POSIXct(time$X1, usetz = T, tz = time_zone)
    time$X1 <- format(time$X1, usetz = TRUE, tz = zone_correction)

    #primeira versão dos dados, tudo de interesse extraido
    res_gps <- data.frame(
      data = time$X1,
      long = coords$X,
      lat = coords$Y,
      elev = coords$Z
    )

    #criando pontos posteriores de lat e long
    res_gps$next_long <- res_gps$long[(1+1:length(res_gps$long))]
    res_gps$next_long[length(res_gps$long)] <- res_gps$long[length(res_gps$long)]

    res_gps$next_lat <- res_gps$lat[(1+1:length(res_gps$lat))]
    res_gps$next_lat[length(res_gps$lat)] <- res_gps$lat[length(res_gps$lat)]

    res_gps$next_elev <- res_gps$elev[(1+1:length(res_gps$elev))]
    res_gps$next_elev[length(res_gps$elev)] <- res_gps$elev[length(res_gps$elev)]
    res_gps$diff_elev <- res_gps$next_elev-res_gps$elev

    #calculo da distancia em metros apos pontos posteriores                                                                               lonlat = T))})
    res_gps$"dist.pont(m)" <- apply(res_gps, 1, FUN = function (row) {

      raster::pointDistance(c(as.numeric(row["next_long"]),
                              as.numeric(row["next_lat"])),
                            c(as.numeric(row["long"]),
                              as.numeric(row["lat"])),
                            lonlat = T)


    })

    res_gps$azimuth <-
      apply(res_gps, 1, FUN = function (row) {
        nngeo::st_azimuth(
          sf::st_point(
            c(as.numeric(row["long"]),
              as.numeric(row["lat"]))
          ),
          sf::st_point(
            c(as.numeric(row["next_long"]),
              as.numeric(row["next_lat"]))
          )
        )
      })


    #criando coluna do proximo tempo
    res_gps$next_time <- res_gps$data[(1+1:length(res_gps$data))]
    res_gps$next_time[length(res_gps$next_time)] <- res_gps$data[length(res_gps$data)]

    #gerando coluna com diferença de tempo entre os pontos
    res_gps$"dif_time(s)" <- as.numeric(difftime(res_gps$next_time, res_gps$data))
    #gerando coluna com a velocidade de descolamento entre os pontos
    res_gps$"vel_(m/s)" <- res_gps$`dist.pont(m)`/res_gps$`dif_time(s)`


    #removendo informações que foram usadas somente para calculo
    res_gps$next_elev <- NULL
    res_gps$next_long <- NULL
    res_gps$next_lat <- NULL
    res_gps$next_time <- NULL


    return(res_gps)
  },
 error=function(err)
 {
   message(paste("ERROR: ", err))
 })
}

