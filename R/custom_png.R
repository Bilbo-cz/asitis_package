#' custom_png
#' modification of png function from grDevices
#'
#' default type is "Cairo"
#' default width and height is 1200×700 px
#' for more details see function png {grDevices}
#' @examples 
#' png(filename = "Rplot%03d.png",
#' width = 1200, height = 700, units = "px", pointsize = 12,
#' bg = "white", res = NA, family = "", restoreConsole = TRUE,
#' type = "cairo", antialias, symbolfamily="default")
#' @export
custom_png <- function(filename = "Rplot%03d.png",
                       width = 1200, height = 700, units = "px", pointsize = 12,
                       bg = "white", res = NA, family = "", restoreConsole = TRUE,
                       type = "cairo", antialias = c("default", "none", "cleartype", "gray", "subpixel"),
                       symbolfamily="default"){
  grDevices::png(filename = filename,
                 width = width, height = height, units = units, pointsize = pointsize,
                 bg = bg, res = res, family = family, restoreConsole = restoreConsole,
                 type = type, antialias = antialias,
                 symbolfamily=symbolfamily)
}