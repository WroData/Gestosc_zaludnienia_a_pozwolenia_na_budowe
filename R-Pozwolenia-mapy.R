library(ggmap)
library(sp)
library(maptools)
library(rgdal)

library(ggplot2)
library(plyr)
library(rgeos)


library(extrafont) #do czcionek
#font_import() #importuje czcionki



#Pozwolenia - mapy
setwd("D:/KK/OneDrive/Wroclaw w Liczbach/Gotowe projekty/Pozwolenia na budowe")

#excel z wspólrzednymi bydynków
Pozwolenia <- read.csv2("wspolrzedne - budynki mieszkalne cz1.csv",  header = TRUE, stringsAsFactors=FALSE)  

#usuniêcie punktów które z jakiœ przyczyn wypad³y poza Wroc³aw (nie ma na ulicy google maps wiec ¿le dopasowa³o)
Pozwolenia<-Pozwolenia[Pozwolenia$lat<51.21,]
Pozwolenia<-Pozwolenia[Pozwolenia$lon<17.164,]
Pozwolenia<-Pozwolenia[Pozwolenia$lon>16.9 | Pozwolenia$lat>51.1,]




#ZAludnienie2008 - obróbka
Zaludnienie2008<- readOGR("D:/KK/OneDrive/Wroclaw w Liczbach/Gotowe projekty/Pozwolenia na budowe/dem-rejurb-rejstat", layer = "REJURB_20081231") 
Zaludnienie2008@proj4string # note projection details
Zaludnienie2008_2 <- spTransform(Zaludnienie2008, CRS("+proj=longlat +datum=WGS84"))
Zaludnienie2008_2@proj4string # and after transforming
Zaludnienie2008_2@data$id <- rownames(Zaludnienie2008_2@data)



#ZAludnienie2014 - obróbka
Zaludnienie2014<- readOGR("D:/KK/OneDrive/Wroclaw w Liczbach/Gotowe projekty/Pozwolenia na budowe/dem-rejurb-rejstat", layer = "REJURB_20141231") 
Zaludnienie2014@proj4string # note projection details
Zaludnienie2014_2 <- spTransform(Zaludnienie2014, CRS("+proj=longlat +datum=WGS84"))
Zaludnienie2014_2@proj4string # and after transforming
Zaludnienie2014_2@data$id <- rownames(Zaludnienie2014_2@data)
Zaludnienie2014_2.df <- fortify(Zaludnienie2014_2)

#wyliczanie ró¿nicy gêstosi zaludnienia
roznica_Gestosci_zaludnienia_2008_2014<-data.frame(Zaludnienie2014_2@data[,17]-Zaludnienie2008_2@data[,17])
roznica_Gestosci_zaludnienia_2008_2014<-cbind(roznica_Gestosci_zaludnienia_2008_2014,seq(0,76,1))
names(roznica_Gestosci_zaludnienia_2008_2014)<-c("Roznica_Gêstosci_zaludnienia", "id")
r_g_zal<-join(Zaludnienie2014_2.df, roznica_Gestosci_zaludnienia_2008_2014, by="id")
r_g_zal<-r_g_zal[order(r_g_zal$Roznica_Gêstosci_zaludnienia),]
r_g_zal$kat<-ifelse(r_g_zal$Roznica_Gêstosci_zaludnienia>250, "500",ifelse(r_g_zal$Roznica_Gêstosci_zaludnienia>-250, "0", ifelse(r_g_zal$Roznica_Gêstosci_zaludnienia>-750, "-500",ifelse(r_g_zal$Roznica_Gêstosci_zaludnienia>-1250, "-1000","-1500"))))
r_g_zal$kat<-factor(r_g_zal$kat, c("-1500","-1000","-500","0", "500"))

                    
#granice osiedli
mapa2<- readOGR("D:/KK/OneDrive/Wroclaw w Liczbach/Gotowe projekty/Pozwolenia na budowe/GraniceOsiedli", layer = "GraniceOsiedli") #katalog pierwszy argument. Drugi - nazwa plików shp

mapa2@proj4string # note projection details
mapa3 <- spTransform(mapa2, CRS("+proj=longlat +datum=WGS84"))
mapa3@proj4string # and after transforming
area.points <- fortify(mapa3)

#Nazwy osiedli
trueCentroids = gCentroid(mapa3,byid=TRUE)
Œrodki_osiedli <- data.frame(trueCentroids)
Nazwy_Osiedli<-data.frame((as.character(mapa3$NAZWAOSIED)))
names(Nazwy_Osiedli)<-c("Nazwy" )
Nazwy_Osiedli[,1]<-(as.character((Nazwy_Osiedli[,1])))
Encoding(Nazwy_Osiedli[,1]) <- "UTF-8"
Œrodki_osiedli<-cbind(Œrodki_osiedli, Nazwy_Osiedli )
Œrodki_osiedli <- Œrodki_osiedli[order(Œrodki_osiedli$Nazwy), ]
Œrodki_osiedli<-cbind(Œrodki_osiedli, seq(1,48,1))
names(Œrodki_osiedli)<-c("long", "lat", "Nazwy", "id" )
robocze<-data.frame(cbind(c(17.31, 17.31), c(max(area.points$lat),min(area.points$lat))))
names(robocze)<-c("long", "lat")


loadfonts(device="win")
czcionka="Corbel" 





#Przeskalowanie Y
Skalar <- 1  /   ((18.27/13.86)*((max(area.points$lat)-min(area.points$lat))/(max(area.points$long)-min(area.points$long))))
Pozwolenia$lat <- Pozwolenia$lat * Skalar
r_g_zal$lat <- r_g_zal$lat * Skalar
area.points$lat <- area.points$lat * Skalar
Œrodki_osiedli$lat <- Œrodki_osiedli$lat * Skalar
robocze$lat <- robocze$lat * Skalar

kolory<-c("#FFD5B1", "#FFBF85", "#F99D4A", "#F78731", "#CD5C06") #skala kolorów 
kolor_punktow <- "#6B0000" #  "#00CCBE"
Kolor_tla <- "#FFFFFF" #  "#f6f6f6" 


mapa <- ggplot()+ 
  geom_polygon(aes(fill = kat, x = long, y = lat, group = group),  data = r_g_zal, alpha = 0.5,  color =  NA, size = 8) +
  geom_polygon(aes(x = long, y = lat, group = group), data = area.points,  color = "gray95", fill = NA, alpha = 0.1) +
  coord_equal() + 
  theme_bw()+
  labs(title="\nPozwolenia na budowê budynków mieszkalnych a ró¿nica gêstoœci zaludnienia")+
  #theme_nothing(legend = TRUE) +
  geom_point(data = Pozwolenia, aes(x = lon, y = lat,colour="Pozwolenie"),  size=1.5,  alpha=0.2, col=kolor_punktow )+ ##TU KOLOR
  annotate("text", x=17.185, y=max(Pozwolenia$lat)-Skalar*0.007*(1:48)+5*0.007,
           label=paste(Œrodki_osiedli$id,". ",Œrodki_osiedli$Nazwy,sep=""), size=4, hjust=0, family=czcionka,fontface="bold") + 
  guides(colour = guide_legend(labels ="Pozytywnie wydana zgoda na budowe \n budynku mieszkalnego ",title = NULL,override.aes = list(alpha = 1, size=3)))+
  scale_fill_manual(values = kolory, guide = guide_legend(title = "Ró¿nica w gêstosci\nzaludnienia [os/km2]"))+
  geom_point(data = robocze, aes(x = long, y = lat), size=1,  alpha=0.01 ,color = "gray")+
  
  geom_point( aes(x = min(Pozwolenia$lon)-0.01, y = max(Pozwolenia$lat)-0.01,colour="Pozwolenie"),  size=2,  alpha=0.5, col=kolor_punktow )+ ##TU KOLOR
  #annotate("text", x=min(Pozwolenia$lon), y=max(Pozwolenia$lat)-0.01,
  #         label="Pozytywnie wydana zgoda na \nbudowe budynku mieszkalnego \n", size=4, hjust=0, family=czcionka,fontface="bold") +
  
  geom_text(data=Œrodki_osiedli, aes(x=long, y=lat, label=id),family=czcionka,  size=6, color= "gray30")  +
  theme(panel.background = element_rect(fill = Kolor_tla),
        plot.background=element_rect(fill=Kolor_tla), #t³o najbardziej na zwen¹trz
        panel.grid.major = element_blank(),#element_line(linetype="solid",color="#e0e0e0"), #wiêksza siatka
        panel.grid.minor = element_blank(), #mniejsza siatka
        panel.border = element_blank(), #osie
        panel.border=element_rect(color="black"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        plot.title=element_text( size=25,family=czcionka,color="black"),
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines"),
        legend.background=element_rect(fill=Kolor_tla,linetype="solid",color="black"),
        legend.position = c(0.155, 0.20),
        legend.text=element_text(family=czcionka, size=13),
        legend.title=element_text(family=czcionka, size=15),
        legend.key=element_blank()) 
  
mapa


#
#






png(filename="D:/KK/OneDrive/Wroclaw w Liczbach/Gotowe projekty/Pozwolenia na budowe/mapa_wrzutka.png", bg=Kolor_tla, width = 14, height = 10, units = 'in', res = 800)

plot(mapa)

dev.off()
