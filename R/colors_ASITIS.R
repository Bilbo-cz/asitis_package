#' Colors for ASITIS company visual style for data visualization
#' 
#' Function returns complete list of 6 solitaire colors and 11 color vectors (palettes) or one color vector of given color number
#' @param n Requested count of colors in color vector, if not provided, returns complete list of available colors
#' @param dark only is used if requested color count is specified, if TRUE, colors will be selected from pallet "Osmibarevná paleta tmavá" instead od "Osmibarevná paleta světlá"
#' @return list of vectors of HEX color codes or one vector if number of colors is specified
#' @examples
#' get_colors_ASITIS();
#' get_colors_ASITIS(8, dark = TRUE);
#' @export
get_colors_ASITIS <- function(n = NA, dark = F) {
  colors_ASITIS <- list(
    "základní modrá světlá" = "#6297cc",
    "neutrální barva světlá" = "#9e9fa0",
    "kontrastní oranžová světlá" = "#db7f3d",
    "základní modrá tmavá" = "#2d5274",
    "neutrální barva tmavá" = "#58595b",
    "kontrastní oranžová tmavá" = "#cc5803",
    "Čtyřbarevná paleta světlá" = c("#6297cc", "#db7f3d", "#facd4a", "#68c59d"),
    "Čtyřbarevná paleta tmavá" = c("#2d5274", "#cc5803", "#eabe3b", "#389672"),
    "Osmibarevná paleta světlá" = c("#6297cc", "#db7f3d", "#facd4a", "#68c59d", "#d37098", "#645789", "#8aa62e", "#4c5d6b"),
    "Osmibarevná paleta tmavá" = c("#2d5274", "#cc5803", "#eabe3b", "#389672", "#d37098", "#645789", "#8aa62e", "#4c5d6b"),
    "Modro-červená světlá" = c("#6297cc", "#c85354"),
    "Modro-červená tmavá" = c("#2d5274", "#a12e36"),
    "Zeleno-červená světlá" = c("#68c59d", "#c85354"),
    "Zeleno-červená tmavá" = c("#389672", "#a12e36"),
    "Čtyřbarevná paleta (alternativní kombinace)" = c("#2d5274", "#cc5803", "#eabe3b", "#68c59d"),
    "Ordinální čtyřbarevná paleta" = c("#002435", "#2d5274", "#6297cc", "#b3e2ff"),
    "Barevná škála" = c("#D46616", "#d8645a", "#bb5b83", "#7b6da0", "#457db0", "#389672", "#8aa62e", "#eabe3b")
  )
  
  if(is.na(n)){
    return(colors_ASITIS)
  } else if (!dark & n > 0 & n < 9) {
    return(colors_ASITIS$`Osmibarevná paleta světlá`[1:n])
  } else if (dark & n > 0 & n < 9) {
    return(colors_ASITIS$`Osmibarevná paleta tmavá`[1:n])
  } else {
    stop("The required number of colors is not available. Choose a value between 1 and 8 or leave the value blank to get the full list of palettes.")
  }
}


#' @title Colours for ASITIS company visual style for data visualization
#' @description Function returns complete list of 6 solitaire colours and 11 colour vectors (palletes) or one colour vector of given colour number
#' @param n Requested number of colours in colour vector, if not provided, returns complete list of available colours
#' @param dark only is used if requested colour count is specified, if TRUE, colours will be selected from pallet "Osmibarevná paleta tmavá" instead od "Osmibarevná paleta světlá"
#' @return list of vectors of HEX color codes or one vector if number of colors is specified
#' @examples
#' get_colors_ASITIS();
#' get_colors_ASITIS(8, dark = TRUE);
#' @export
get_colours_ASITIS <- function(n = NA, dark = F){
  return(get_colors_ASITIS(n = n, dark = dark))
}

#' @title Colors for ASITIS company visual style for marketing and general use
#' @description Function returns complete list of 2 solitaire colors and 7 color vectors (palettes)
#' @return list of vectors of HEX color codes
#' @examples
#' get_colors_ASITIS_marketing();
#' @export
get_colors_ASITIS_marketing <- function(){
  colors_ASITIS_marketing <- list(
    "hlavní modrá" = "#00344b",
    "hlavní oranžová" = "#d46515",
    "Doplňkové barvy produktových značek" = c("#5ec3ad", "#19abde", "#ec548d"),
    "Doplňkové barvy zeleno-modré" = c("#5ec3ad", "#4bc3c8", "#35c3e1", "#19abde", "#186ba1", "#142458"),
    "Doplňkové barvy růžovo-modré" = c("#e97369", "#ec548d", "#cb539f", "#9657a3", "#754b9e", "#2e2468"),
    "Doplňkové barvy žluto-červené" = c("#eabe3b", "#ee9a3a", "#ef7e32", "#de542d", "#bf2324", "#801517"),
    "Barvy webových stránek oranžové" = c("#fbf0e8", "#edc09f", "#e6a677", "#db7f3d", "#d46515", "#94470f", "#813e0d"),
    "Barvy webových stránek modré" = c("#e6ebed", "#96acb5", "#6b8997", "#2b576a", "#00344b", "#002435", "#00202e"),
    "Barvy webových stránek neutrální" = c("#eeeeef", "#bbbbbc", "#9e9fa0", "#747577", "#58595b", "#3e3e40", "#363638")
  )
  return(colors_ASITIS_marketing)
}

#' @title Colours for ASITIS company visual style for marketing and general use
#' @description Function returns complete list of 2 solitaire colours and 7 colour vectors (palletes)
#' @return list of vectors of HEX color codes
#' @examples
#' get_colors_ASITIS_marketing();
#' @export
get_colours_ASITIS_marketing <- function(){
  return(get_colors_ASITIS_marketing())
}
