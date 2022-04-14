#' Version 3 de la carte en plotly
#'
#' @param bdd_coordonnees_banques2022 data frame
#'
#' @return carte plotly
#' @export
#' @importFrom plotly plot_mapbox
#' @importFrom plotly layout
#' @importFrom plotly config
#' @import dplyr

plotly_map <- function(bdd_coordonnees_banques2022){
  # API mapbox
  mapboxToken <- paste("pk.eyJ1IjoiZ3JhbnQyNDk5IiwiYSI6ImNremZ6enYweDJjbjAybm8xejVqN3IwemQifQ.UTVkE6hkSjPESfp-0CPD7Q", collapse="")
  Sys.setenv("MAPBOX_TOKEN" = mapboxToken)

  # Positionnement des banques sur la carte
  fig <- plot_mapbox(data = bdd_coordonnees_banques2022,lon=~Longitude,lat=~Latitude,
                     split=~bdd_coordonnees_banques2022,
                     size=1,
                     mode = 'markers', hoverinfo='text',
                     marker = list(size = 5, opacity = .5))

  fig <- fig %>% layout(title = 'Banques en France',
                        font = list(color='white'),
                        plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
                        mapbox = list(style = 'dark'),
                        legend = list(orientation = 'h',
                                      font = list(size = 8)),
                        margin = list(l = 25, r = 25,
                                      b = 25, t = 25,
                                      pad = 2))
  fig <- fig %>% config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

  fig
}
