#------------------------------------------------------------------------
library(raster)
library(sf)
library(ggplot2)
library(ggspatial)
#------------------------------------------------------------------------
Tampo       <- st_read ("SHP/Tampo.shp")
Carre       <- st_read ("SHP/Via_interocianica.shp")
Rio         <- st_read ("SHP/Rio.shp")
CCNN        <- st_read ("SHP/CCNN_TAMPO.shp")
Predios     <- st_read ("SHP/Predios.shp")
Conservacion<- st_read ("SHP/Conservacion.shp")
Madera      <- st_read ("SHP/Madera_MDD.shp")
Ecoturismo  <- st_read ("SHP/Ecoturismo.shp")
Castaña     <- st_read ("SHP/Castaña.shp")
MDD         <- st_read ("SHP/MDD.shp")
library(grid)
library(png)
img <- readPNG("PNG\\Madera.png", FALSE)
g <- rasterGrob(img, x = unit(0.1, "npc"),y = unit(0.1, "npc"), width = unit(0.18, "npc"))
img1 <- readPNG("PNG\\Conservacion.png", FALSE)
g1 <- rasterGrob(img1, x = unit(0.1, "npc"),y = unit(0.4, "npc"), width = unit(0.18, "npc"))
img2 <- readPNG("PNG\\ecotursimo.png", FALSE)
g2 <- rasterGrob(img2, x = unit(0.1, "npc"),y = unit(0.65, "npc"), width = unit(0.18, "npc"))
img3 <- readPNG("PNG\\Predios.png", FALSE)
g3 <- rasterGrob(img3, x = unit(0.9, "npc"),y = unit(0.1, "npc"), width = unit(0.18, "npc"))
img4 <- readPNG("PNG\\Castaña.png", FALSE)
g4 <- rasterGrob(img4, x = unit(0.9, "npc"),y = unit(0.55, "npc"), width = unit(0.18, "npc"))

Tampo_carre   <- st_intersection(Carre, Tampo)
Tampo_Rio_MD  <- st_intersection(Rio, Tampo)
Tampo_Predios <- st_intersection(Predios, Tampo)
Tampo_Conser  <- st_intersection(Conservacion, Tampo)
Tampo_Madera  <- st_intersection(Madera, Tampo)
Tampo_Ecotur  <- st_intersection(Ecoturismo, Tampo)
Tampo_Castaña <- st_intersection(Castaña, Tampo)
#------------------------------------------------------------------------
MADRE_DIOS =ggplot()+
  geom_sf(data = MDD , fill="seagreen1", color="seagreen4", size=0.7)+
  geom_sf(data = Tampo, color="seagreen", fill="palegreen3")+
  annotate(geom = "text", x = 370000, y = 8690000, label = "MADRE DE DIOS", family="candara", 
           color = "black", size = 3,fontface = "bold")+
  theme_bw()+
  theme(legend.text =element_text(size=9, family="serif"),
        legend.title = element_text(size=9, family="serif"),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "seashell2"),
        panel.border = element_rect(size = 2))
MDD.grob        <- ggplotGrob(MADRE_DIOS)  
#------------------------------------------------------------------------
Map =ggplot()+
  geom_sf(data = MDD , fill=NA, color="gray70", size=0.7)+
  geom_sf(data = Tampo, color ="gray70", size=0.8, fill=NA)+
  geom_sf(data = Tampo_Conser, color= "plum4", fill="plum2")+
  geom_sf(data = Tampo_Predios,fill="springgreen4", alpha=0.3, color="springgreen4")+
  geom_sf(data = Tampo_Rio_MD, color="paleturquoise3", size=0.5)+
  geom_sf(data = Tampo_carre, color="royalblue")+
  geom_sf(data = Tampo_Madera, color="navajowhite4", fill="moccasin")+
  geom_sf(data = Tampo_Ecotur, color="lightsalmon4", fill="lightsalmon")+
  geom_sf(data = Tampo_Castaña , color="palevioletred4", fill="palevioletred2")+
  geom_sf(data = Tampo, color ="orangered4", size=0.8, fill=NA)+
  coord_sf(xlim = c(220000 ,550000), ylim = c(8540000, 8810000),expand = FALSE)+
  theme_bw()+
  annotate(geom = "text", x = 440000, y = 8780000, label = "Unidad de Gestion \nForestal y Fauna Silvestre", 
           family="candara", color = "seagreen", size = 6,fontface = "bold")+
  annotate(geom = "text", x = 440000, y = 8760000, label = "Tambopata", family="candara", 
           color = "seagreen", size = 8,fontface = "bold")+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme(panel.grid.major = element_line(color = gray(.5),linetype = "solid", size = 0.5),
        legend.text =element_text(size=9, family="serif"),
        panel.background = element_rect(fill = "seashell2"),
        legend.title = element_text(size=9, family="serif"),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(size = 2))+
  annotation_custom(MDD.grob , xmin = 490000, xmax = 550000, ymin =8720000, ymax=8800000)+
  annotation_custom(g)+
  annotation_custom(g1)+
  annotation_custom(g2)+
  annotation_custom(g3)+
  annotation_custom(g4)

ggsave(plot = Map,"MAPAS/MDD.png",
       units = "cm", width = 29,height = 21, dpi = 900)# guardar grafico



