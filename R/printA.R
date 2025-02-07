#' printA
#' Printing ggplot with rectangular background with rounded corners
#' Use it instead of standard print function
#' Should be used preferably with ggplot style theme_ASITIS
#'
#' @param p ggplot object
#' @examples 
#' printA(ggplot(df <- data.frame(x <- 1:2, y = 1:2), aes(x = x))+geom_line(aes(y = y))+theme_ASITIS())
#' @export
printA <- function(p = NA){
  
  obd <- grid::roundrectGrob(
    x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
    width = grid::unit(1, "npc"), height = grid::unit(1, "npc"),
    r = grid::unit(0.2, "npc"), gp = grid::gpar(fill = "#e6ebed", col = "#e6ebed")
  )
  
  obd1 <- grid::roundrectGrob(
    x = grid::unit(0.25, "npc"), y = grid::unit(0.75, "npc"),
    width = grid::unit(0.5, "npc"), height = grid::unit(0.5, "npc"),
    r = grid::unit(0.1, "npc"), gp = grid::gpar(fill = "#e6ebed", col = "#e6ebed")
  )
  
  obd2 <- grid::roundrectGrob(
    x = grid::unit(0.75, "npc"), y = grid::unit(0.25, "npc"),
    width = grid::unit(0.5, "npc"), height = grid::unit(0.5, "npc"),
    r = grid::unit(0.1, "npc"), gp = grid::gpar(fill = "#e6ebed", col = "#e6ebed")
  )
  
  p <- p + ggplot2::theme(plot.margin = margin(t = 0.05, r = 0.07, b = 0.03, l = 0.035, unit = "npc"),
                 panel.border = element_rect(color = "#e6ebed"),
                 plot.background = element_blank(),
                 plot.title = element_text(vjust = 2),
  )
  
  if(is.null(dev.list())){
    png("temp")
    showtext::showtext_begin()
    pa <- ggplot2::ggplotGrob(p)
    showtext::showtext_auto()
    dev.off()
  }
  else {
    showtext::showtext_begin()
    pa <- ggplot2::ggplotGrob(p)
    showtext::showtext_auto()
  }
  
  grid::grid.newpage()
  kombo <- grid::gTree(children = grid::gList(obd, obd1, obd2, pa))
  grid::grid.draw(kombo)
}