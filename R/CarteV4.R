#' Version 4 de la carte en ggplot qui afficher uniquement par zone d'emploi
#'
#' @param bdd_zese data frame
#' @param bdd_coordonnees_ze_banques2022 data frame
#' @param type character
#' @param ze character
#'
#' @return carte ggplot
#' @export
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 scale_fill_viridis_c
#' @importFrom ggplot2 geom_sf_text
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 aes

gg_map_ze <- function(bdd_zese,bdd_coordonnees_ze_banques2022,type,ze){

  # ligne correspondant à la zone d'emploi sélectionnée
  l <- which(bdd_zese$`Zone d'emploi 2020`==ze)

  # lignes correspondants aux banques présentes dans la zone d'emploi sélectionnée
  l1 <- which(bdd_coordonnees_ze_banques2022$ze==ze)
  banque <- bdd_coordonnees_ze_banques2022[l1,]

  # Construction de la map en ggplot
  Longitude <- banque$Longitude
  Latitude <- banque$Latitude
  Banque <- banque$Banque
  Type <- banque$Type
  if(type==FALSE){
    map <- ggplot()+
      geom_sf(data=bdd_zese[l,], aes(geometry=bdd_zese[l,]$geometry),color='black',size=.2)+
      scale_fill_viridis_c(option = 'E')+
      theme_minimal()+
      theme(panel.background = element_rect(fill = "light blue"))+
      geom_point(data=banque,aes(x=Longitude,y=Latitude,group=Banque,color=Banque,label=Banque),size=.6)
  }
  else{
    map <- ggplot()+
      geom_sf(data=bdd_zese[l,], aes(geometry=bdd_zese[l,]$geometry),color='black',size=.2)+
      scale_fill_viridis_c(option = 'E')+
      theme_minimal()+
      theme(panel.background = element_rect(fill = "light blue"))+
      geom_point(data=banque,aes(x=Longitude,y=Latitude,group=Type,color=Type,label=Banque),size=.6)
  }
  map
}
