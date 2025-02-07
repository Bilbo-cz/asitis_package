library(ggplot2)
library(showtext)
library(readxl)
library(grid)
#library(Cairo)

# několik pokusů jak nastavit Cairo globálně
### stále nevím, který dělá přesně co

#install.packages("Cairo")
#install.packages("knitr")
#knitr::opts_knit$set(dev.args = list(type = "cairo"))


#trace(grDevices:::png, quote({
 # if (missing(type) && missing(antialias)) {
  #  type <- "cairo-png"
   # antialias <- "subpixel"
  #}
#}), print = FALSE)


options(bitmapType = "cairo")


# běžný setup
###

data_vse <- read_excel("test_data_R.xlsx")
data_vse


# inicializace písma Nunito
font_add_google("Nunito", "nunito")
font_add_google("Open sans", "opensans")
showtext_auto()
showtext_opts(dpi = 96)


# definice vzhledu pro graf
# verze 1.1
theme_ASITIS <- function(angle.x = FALSE){
  theme_bw()+
    theme(axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          strip.text.x = element_text(size = 30),
          axis.text=element_text(size=15),
          strip.placement = "outside",
          text = element_text(family = "nunito", color = "#00344B"),
          plot.title = element_text(size = 30),
          plot.background = element_blank(),
          #panel.background = element_rect(fill = "white"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          legend.justification="left",
          legend.direction = "vertical",
          axis.title.y.right = element_text(angle = 90, color = "#19ABDE")
    )+
    if(angle.x){
      theme(axis.text.x = element_text(angle = 15, vjust = 0.5 ))  
    }
  
}


obec = data_vse$uzemi_txt[1]

print(
  ggplot(data_vse[which(data_vse$hodnota >-1),], aes(x = obdobi_txt)) +
    geom_col(aes(y = hodnota), fill = ifelse(data_vse$obdobi_txt == "Nezjištěno", "#9e9fa0", "#6297cc"), linewidth = 0.5, position = "dodge")+
    geom_text(aes(y = hodnota, label = hodnota), position = position_stack(vjust = 0.5, reverse = T), colour = ifelse(data_vse$obdobi_txt == "Nezjištěno", "black", "black"), size = 5)+
    ggtitle(paste("Období výstavby domů v obci", obec))+
    ylab("Počet domů")+
    xlab("Období výstavby")+
    theme_ASITIS(angle.x = T)
)

p <- ggplot(data_vse[which(data_vse$hodnota >-1),], aes(x = obdobi_txt)) +
  geom_col(aes(y = hodnota), fill = ifelse(data_vse$obdobi_txt == "Nezjištěno", "#9e9fa0", "#6297cc"), linewidth = 0.5, position = "dodge")+
  geom_text(aes(y = hodnota, label = hodnota), position = position_stack(vjust = 0.5, reverse = T), colour = ifelse(data_vse$obdobi_txt == "Nezjištěno", "black", "black"), size = 5)+
  ggtitle(paste("Období výstavby domů v obci", obec))+
  ylab("Počet domů")+
  xlab("Období výstavby")+
  theme_ASITIS(angle.x = T)


# printA1
### verze s oranžovým puntíkem
printA <- function(p = NA){
  #library(grid)
  #library(showtext)
  
  #font_add_google("Nunito", "nunito")
  
  c <- circleGrob(x = 0.065, y = 0.973, r = 0.180,
                       gp = gpar(fill = "#D46515", col = "#D46515", lwd = 2))
  
  if(is.null(dev.list())){
    png("temp")
    showtext_begin()
    pa <- ggplotGrob(p)
    showtext_auto()
    dev.off()
  }
  else {
    showtext_begin()
    pa <- ggplotGrob(p)
    showtext_auto()
  }
    
  
  grid.newpage()
  kombo <- gTree(children = gList(c, pa))
  grid.draw(kombo)
  #print(p)
}


printA(p)


print(p)

png(paste0("test.png"), width = 1200, height = 700)
printA1(p)
#pdf(paste0("test.pdf"))
dev.off()
custom_png("test2.png")


# vlastní fukce png, která přepíše původní
### zajišťuje, aby se vždy jako png device type použil Cairo

custom_png <- function(filename = "Rplot%03d.png",
    width = 1200, height = 700, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = "cairo", antialias = c("default", "none", "cleartype", "gray", "subpixel"),
    symbolfamily="default"){
  grDevices::png(filename = filename,
           width = width, height = height, units = units, pointsize = 2*pointsize,
           bg = bg, res = res, family = family, restoreConsole = restoreConsole,
           type = type, antialias = antialias,
           symbolfamily=symbolfamily)
}

assign("png", custom_png, envir = .GlobalEnv)



# printA1
### verze s zakulaceným obdélníkem
printA1 <- function(p = NA){
  
  obd <- roundrectGrob(
    x = unit(0.5, "npc"), y = unit(0.5, "npc"),
    width = unit(1, "npc"), height = unit(1, "npc"),
    r = unit(0.2, "npc"), gp = grid::gpar(fill = "#e6ebed", col = "#e6ebed")
  )
  
  obd1 <- roundrectGrob(
    x = unit(0.25, "npc"), y = unit(0.75, "npc"),
    width = unit(0.5, "npc"), height = unit(0.5, "npc"),
    r = unit(0.1, "npc"), gp = grid::gpar(fill = "#e6ebed", col = "#e6ebed")
  )
  
  obd2 <- roundrectGrob(
    x = unit(0.75, "npc"), y = unit(0.25, "npc"),
    width = unit(0.5, "npc"), height = unit(0.5, "npc"),
    r = unit(0.1, "npc"), gp = grid::gpar(fill = "#e6ebed", col = "#e6ebed")
  )
  
  p <- p + theme(plot.margin = margin(t = 0.04, r = 0.07, b = 0.03, l = 0.035, unit = "npc"),
                 text = element_text(family = "nunito", color = "black"),
                 panel.border = element_rect(color = "#e6ebed"),
                 axis.ticks = element_line(color = "#96acb5"),
                 axis.text = element_text(color = "#58595b")
                   )
  
  if(is.null(dev.list())){
    png("temp")
    showtext_begin()
    pa <- ggplotGrob(p)
    showtext_auto()
    dev.off()
  }
  else {
    showtext_begin()
    pa <- ggplotGrob(p)
    showtext_auto()
  }
  
  
  grid.newpage()
  kombo <- gTree(children = gList(obd, obd1, obd2, pa))
  grid.draw(kombo)
}

printA1(p)



