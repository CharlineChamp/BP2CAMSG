#' Version 2 de la carte en ggplot
#'
#' @param bdd_zese data frame
#' @param crit integer
#' @param label character
#' @param point interger
#' @param data data frame
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

gg_map <- function(bdd_zese,crit,label,point,data){
  map <- ggplot(NULL)
  if(crit!=0){
    map <- map+geom_sf(data=bdd_zese,aes(fill=bdd_zese[,crit],geometry=bdd_zese$geometry),size=.2,color=NA)+labs(fill = label)
  }else{
    map <-map+geom_sf(data=bdd_zese,aes(geometry=bdd_zese$geometry),size=.2,color="blue",fill="black")
  }
  map <- map+
    scale_fill_viridis_c(option = 'E')+
    theme_minimal()+
    theme(panel.background = element_rect(fill = "light blue"))+
    geom_sf_text(data=bdd_zese,aes(label=bdd_zese$`Zone d'emploi 2020`,geometry=bdd_zese$geometry,color="white"),size=2)
  if(point==0){
    map <- map+
      geom_point(data=data,aes(x=data$Longitude,y=data$Latitude,group=data$Banque,color=data$Banque),size=.5)
  }else if(point==1){
    map <- map+
      geom_point(data=data,aes(x=data$Longitude,y=data$Latitude,group=data$Type,color=data$Type),size=.5)
  }else{
    map <- map +
      geom_point(data=data,aes(x=data$Longitude,y=data$Latitude),size=.5,colour="white")
  }
  map
}
