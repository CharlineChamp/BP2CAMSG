#' Carte affichant la densité des banques pour une zone d'emploi
#'
#' @param ze character
#' @param type character (Lucrative/Coopérative/Tout)
#' @param bdd_zese data frame
#' @param bdd_coordonnees_banques2022 data frame
#'
#' @return carte
#' @importFrom graphics points
#' @importFrom spatstat.core density.ppp
#' @importFrom spatstat.geom owin
#' @importFrom spatstat.geom ppp
#' @export

banq.dens <- function(ze,type, bdd_zese, bdd_coordonnees_banques2022){
  ind.ze <- which(bdd_zese$`Zone d'emploi 2020`==ze)
  coord.ze <- recup.longlat(ind.ze, bdd_zese)
  ow <- owin(poly = list(x = coord.ze$Longitude,y = coord.ze$Latitude))
  if(type == "Lucrative"){
    ind.lucr <- which(bdd_coordonnees_banques2022$Type=='Lucrative')
    banq <- bdd_coordonnees_banques2022[ind.lucr,]
    long <- banq$Longitude
    lat <- banq$Latitude
    mypattern <- ppp(long, lat,c(min(long),max(long)), c(min(lat),max(lat)))
  }
  if(type=="Coopérative"){
    ind.coop <- which(bdd_coordonnees_banques2022$Type=='Coopérative')
    banq <- bdd_coordonnees_banques2022[ind.coop,]
    long <- banq$Longitude
    lat <- banq$Latitude
    mypattern <- ppp(long, lat,c(min(long),max(long)), c(min(lat),max(lat)))
  }
  if(type=="Tout"){
    banq <- bdd_coordonnees_banques2022
    long <- banq$Longitude
    lat <- banq$Latitude
    mypattern <- ppp(long, lat,c(min(long),max(long)), c(min(lat),max(lat)))
  }
  banque.ze <- mypattern[ow]
  plot(density.ppp(banque.ze))
  points(banque.ze,cex=.4)
}
