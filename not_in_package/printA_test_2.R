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
# verze 1.2.1
theme_ASITIS <- function(angle.x = FALSE,
                         title.on.center = FALSE){
  theme_bw()+
    theme(axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          strip.text.x = element_text(size = 30),
          axis.text=element_text(size=15, color = "#58595b"),
          strip.placement = "outside",
          text = element_text(family = "nunito", color = "black"),
          panel.border = element_rect(color = "#96acb5"),
          axis.ticks = element_line(color = "#96acb5"),
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
    {if(title.on.center)
      theme(plot.title = element_text(hjust = 0.5),
            plot.title.position = "plot")  
    }+
    {if(angle.x)
      theme(axis.text.x = element_text(angle = 15, vjust = 0.5 ))  
    }
  
}





obec = data_vse$uzemi_txt[1]

printA(
  ggplot(data_vse[which(data_vse$hodnota >-1),], aes(x = obdobi_txt)) +
    geom_col(aes(y = hodnota), fill = ifelse(data_vse$obdobi_txt == "Nezjištěno", "#9e9fa0", "#6297cc"), linewidth = 0.5, position = "dodge")+
    geom_col(aes(y = hodnota), fill = ifelse(data_vse$obdobi_txt == "1991-2000", "#0090d6", 
                                             ifelse(data_vse$obdobi_txt == "Nezjištěno", "#9e9fa0", "#6297cc")),
                                             linewidth = 0.5, position = "dodge")+
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



# printA
### verze s zakulaceným obdélníkem
printA <- function(p = NA){
  
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

printA(p)




#-------------------------------------------------------------------------------
# test konkrétních grafů

fitted = runif(30, min = 300, max = 500)

df <- data.frame(Rok = 2000:2029, 
                  demo = runif(30, min = 300, max = 500),
                  fitted = fitted,
                  upper95 = fitted * 1.05,
                  lower95 = fitted * 0.95,
                  upper80 = fitted * 1.20,
                  lower80 = fitted * 0.8,
                  fac = rep(c("A", "B", "C"), 10),
                  fac2 = rep(c("A", "B"), 15)
         )



Obec = obec
top_border <- max(c(max(df$demo, na.rm = T)*1.33, max(df$upper95, na.rm = T)))

printA(ggplot(df, aes(x = Rok, y = demo)) +
        #geom_col(aes(y = demo), fill = "#6297cc")+
        geom_line(aes(y = demo),show.legend = T, size = 2, color = "#3e3e40")+
         
        #geom_line(aes(y = fitted), size = 2, color = "#cc5803", linetype = 2)+
        geom_line(aes(y = demo),show.legend = T, size = 2, color = ifelse(df$fitted > df$demo, "#2d5274", "#cc5803"))+
        #geom_ribbon(aes(ymin = lower80, ymax = upper80), fill = "brown", alpha = 0.2)+
        #geom_ribbon(aes(ymin = lower95, ymax = upper95), fill = "brown", alpha = 0.2)+
        ggtitle(paste("Demografický vývoj v obci", Obec ))+
        scale_y_continuous("Počet obyvatel", limits = c(0, top_border), n.breaks = 10)+
        scale_x_continuous(n.breaks = 10)+
        xlab("Rok")+
        theme_ASITIS()
)

ASITIScols <- c("#457db0", "#DE542D", "#EABE3B", "#5EC3AD")

printA(ggplot(df, aes(x = Rok))+
         geom_col(aes(y = fitted, fill = fac2))+
         scale_colour_manual(values = ASITIScols, aesthetics = c("fill"))+
         theme_ASITIS()
         
       )
