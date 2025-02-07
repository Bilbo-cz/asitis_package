#' ASITIS company ggplot theme
#' version 1.2.1 old style modification
#'
#' Modification of standard style theme_ASITIS to replicate old plot style used before summer 2024.
#' Use with standard print function, not with printA intended for use with theme_ASITIS.
#' 
#' @param angle.x boolean, if true, values on x axis will be 15° slanted
#' @param title.on.center boolean, if true, plot title will be aligned to the center
#' @param no.grid boolean, if true, plot will not have visible grid
#' @param double.axis boolean, if true, plot will have both right (red colored) and left (blue colored) y axes
#' @param double.axis.reverse boolean, if true, plot will have both right (blue colored) and left (red colored) y axes
#' @return ggplot theme
#' @examples 
#' p + theme_ASITIS();
#' p + theme_ASITIS(angle.x = T, title.on.center = T);
#' @export
theme_ASITIS_old <- function(angle.x = FALSE,
                         title.on.center = FALSE,
                         no.grid = FALSE,
                         double.axis = FALSE,
                         double.axis.reverse = FALSE){
  th <- ggplot2::theme_bw()+
    ggplot2::theme(axis.title.x = element_text(size = 20),
                   axis.title.y = element_text(size = 20),
                   strip.text.x = element_text(size = 30),
                   axis.text=element_text(size=15),
                   strip.placement = "outside",
                   text = element_text(family = "nunito", color = "#00344B"),
                   plot.title = element_text(size = 25, lineheight = 1.25),
                   plot.subtitle = element_text(size = 20),
                   plot.background = element_blank(),
                   legend.position = "bottom",
                   legend.title = element_blank(),
                   legend.text = element_text(size = 18),
                   legend.justification="left",
                   legend.direction = "vertical",
                   legend.key = element_blank(),
                   legend.key.spacing.y = unit(0.15, "lines"),
                   legend.key.size = unit(1.25, "lines"),
                   legend.background = element_blank()
    )
  
  if(title.on.center){
    th <- th + ggplot2::theme(plot.title = element_text(hjust = 0.5),
                              plot.title.position = "plot")  
  }
  
  if(angle.x){
    th <- th + ggplot2::theme(axis.text.x = element_text(angle = 15, vjust = 0.5 ))
  }
  
  if(no.grid){
    th <- th + ggplot2::theme(panel.grid = element_blank())
  }
  
  if(double.axis){
    th <- th + ggplot2::theme(axis.title.y.right = element_text(angle = 90, color = "#801517"),
                              axis.title.y.left = element_text(angle = 90, color = "#00344B"))
  }
  
  if(double.axis.reverse){
    th <- th + ggplot2::theme(axis.title.y.left = element_text(angle = 90, color = "#801517"),
                              axis.title.y.right = element_text(angle = 90, color = "#00344B"))
  }
  
  
  return(th)
}