library(readxl)
library(ggplot2)
library(showtext)


# inicializace fontu
font_add_google("Open Sans", "opensans")
showtext_auto()
showtext_opts(dpi = 96)



update_geom_defaults("line", list(linewidth = 2))

data_vse = data.frame(x = 1:40,
                      y1 = round(runif(n = 40, min = 1, max = 10), 0),
                      y2 = round(runif(n = 40, min = 5, max = 30), 0),
                      y3 = round(runif(n = 40, min = 0, max = 20), 0),
                      y4 = round(runif(n = 40, min = 5, max = 30), 2),
                      f = rep(c("A", "B", "C", "D"), 10),
                      f2 = rep(c("A", "B"), 20),
                      f8 = rep(c("A", "B", "C", "D", "E", "F", "G", "H"), 5)
)


# definice vzhledu pro graf
theme_ASITIS <- function(){
  theme_bw()+
    theme(axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          #axis.title = element_text(color = "#D46515"),
          strip.text.x = element_text(size = 30),
          axis.text=element_text(size=15),
          strip.placement = "outside",
          text = element_text(family = "opensans", color = "#00344B"),
          #panel.border = element_rect(colour = "#00344B", fill = NA, linewidth = 2),
          plot.title = element_text(size = 30, color = "#00344B"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          legend.justification="left",
          legend.direction = "vertical",
          axis.title.y.right = element_text(angle = 90, color = "#19ABDE"))
}


ASITIScols <- c("#00344B", "#D46515", "#EABE3B", "#bb5b83", "#2E2468", "#DE542D", "#EABE3B", "#5EC3AD") ## 4 BARVY
ASITIScols <- c("#db7f3d", "#6297cc", "#5EC3AD", "#9e302d", "#645789", "#389672", "#bb5b83", "#4c5d6b") ## 4 BARVY
ASITIScols <- c("#00344B", "#6297cc", "#E56399", "#7FD1B9", "#645789", "#389672", "#bb5b83", "#4c5d6b") ## 4 BARVY




# sloupcové grafy

ggplot(data = data_vse, aes(x = x))+
  geom_col(aes(y = y1, fill = "A"))+
  ggtitle("Čárový graf s jednou proměnnou")+
  xlab("Tady je nějaký název osy")+
  scale_colour_manual(values = ASITIScols, aesthetics = c("fill", "colour"))+
  theme_ASITIS()

ggplot(data = data_vse, aes(x = f8))+
  geom_col(aes(y = y1, fill = f2), position = "dodge")+
  ggtitle("Čárový graf se dvěmi proměnnými")+
  scale_colour_manual(values = ASITIScols, aesthetics = c("fill", "colour"))+
  theme_ASITIS()

ggplot(data = data_vse, aes(x = f))+
  geom_col(aes(y = y1, fill = f))+
  ggtitle("Čárový graf se čtyřmi proměnnými")+
  scale_colour_manual(values = ASITIScols, aesthetics = c("fill", "colour"))+
  theme_ASITIS()

ggplot(data = data_vse, aes(x = f8))+
  geom_col(aes(y = y1, fill = f8))+
  ggtitle("Čárový graf s osmi proměnnými")+
  scale_colour_manual(values = ASITIScols, aesthetics = c("fill", "colour"))+
  theme_ASITIS()


# čárové grafy

ggplot(data = data_vse, aes(x = x))+
  geom_line(aes(y = y1, colour = "A"))+
  ggtitle("Čárový graf s jednou proměnnou")+
  scale_colour_manual(values = ASITIScols, aesthetics = c("fill", "colour"))+
  theme_ASITIS()

ggplot(data = data_vse, aes(x = x))+
  geom_line(aes(y = y1, colour = f2))+
  ggtitle("Čárový graf se dvěmi proměnnými")+
  scale_colour_manual(values = ASITIScols, aesthetics = c("fill", "colour"))+
  theme_ASITIS()

ggplot(data = data_vse, aes(x = x))+
  geom_line(aes(y = y1, colour = f))+
  ggtitle("Čárový graf se čtyřmi proměnnými")+
  scale_colour_manual(values = ASITIScols, aesthetics = c("fill", "colour"))+
  theme_ASITIS()

ggplot(data = data_vse, aes(x = x))+
  geom_line(aes(y = y1, colour = f8))+
  ggtitle("Čárový graf s osmi proměnnými")+
  scale_colour_manual(values = ASITIScols, aesthetics = c("fill", "colour"))+
  theme_ASITIS()


options('device')
