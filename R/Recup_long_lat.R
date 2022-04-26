#' Fonction qui permet de récupérer la longitude et la latitude des frontières d'une zone d'emploi
#'
#' @param indice integer
#' @param bdd_zese data frame bdd_zese
#'
#' @return data frame
#' @export

recup.longlat <- function(indice, bdd_zese){
  list.long <- vector(length = 0)
  list.lat <- vector(length = 0)
  for (k in 1:length(bdd_zese[[35]][[indice]])){
    list.long <- c(list.long, bdd_zese[[35]][[indice]][[k]][[1]][,1])
    list.lat <- c(list.lat, bdd_zese[[35]][[indice]][[k]][[1]][,2])
  }
  coord.test <- data.frame(Longitude = list.long, Latitude = list.lat)
}
