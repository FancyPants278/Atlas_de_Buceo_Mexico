library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggthemes)
library(hrbrthemes)
library(patchwork)

spdf_mx <- st_transform(st_as_sf(ne_countries(country = 'mexico')), crs = 4326)


mpa_layer <- st_read("shp/old/final_mpa_layer.shp")


mpa_metadata <- read.csv("shp/old/MPA_metadata.csv", na.strings = "")

mpa_layer <- mpa_layer %>% 
        select(ANP, ID, nombre_1)

final_mpa <- merge(mpa_layer, mpa_metadata, by = c("ID", "ANP"))

final_mpa$Pesca <- replace_na(final_mpa$Pesca, "NA")

final_mpa$Pesca <- factor(final_mpa$Pesca, levels = c("NA", "ND", "NO", "SI"), labels = c("Sin manejo", "No especificado", "Prohibida", "Permitida"))

final_mpa$Buceo <- replace_na(final_mpa$Buceo, "NA")

final_mpa$Buceo <- factor(final_mpa$Buceo, levels = c("NA", "ND", "NO", "SI"), labels = c("Sin manejo", "No especificado", "Prohibido", "Permitido"))


dive_sites <- st_read('shp/dive_sites.shp')



test <- st_join(dive_sites, final_mpa)

test <- test %>% 
        mutate(protection_level = ifelse(is.na(ANP), "Sin Protección", "Into MPA"), 
               core_zone = ifelse(Pesca == "Prohibida", "Pesca Prohibida", "Amortiguamiento"),
               protection_level = ifelse(is.na(core_zone), "Sin Protección", core_zone)) %>% 
        select(-core_zone) %>% 
        mutate(protection_level = factor(protection_level, levels = c("Sin Protección", "Amortiguamiento", "Pesca Prohibida"))) 










########## BY AREA SUBZONES --------
(p1 <- final_mpa %>% 
         mutate(Pesca = factor(Pesca, levels = c("No especificado", "Sin manejo", "Prohibida", "Permitida"))) %>% 
         ggplot() +
         geom_sf(data = spdf_mx, 
                 fill = "gray90", 
                 col = NA) +
         geom_sf(data = final_mpa, 
                 aes(fill = Pesca), 
                 col = NA) +
         coord_sf(xlim = c(-118, -85), 
                  ylim = c(13, 32), 
                  expand = TRUE) +
         facet_wrap(~Pesca) +
         scale_fill_manual(values = c( "gray30", "#fee391", "firebrick", "#005a32"), aesthetics = c("color", "fill"), labels = c("No especificado", "Prohibida", "Permitida", "Sin Manejo")) +
         #scale_color_manual(values = c(, "#fc4e2a", "#005a32", "black"), labels = c("No especificado", "Prohibido", "Permitido", "Sin Manejo")) +
         labs(subtitle = "Pesca")+
         theme_bw(base_family = "Lato") +
         theme_map() +
         theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
               strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
               axis.title = element_blank(),
               strip.text.y = element_text(hjust = .5, size = 12),
               panel.grid.minor.x = element_line(color = NA),
               panel.grid.major.y = element_line(color = NA), 
               legend.position = ""))

(p2 <- final_mpa %>% 
         select(ANP, Pesca, Buceo) %>% 
         as.data.frame() %>% 
         group_by(Pesca) %>% 
         count() %>% 
         mutate(Pesca = factor(Pesca, levels = c("No especificado", "Sin manejo", "Prohibida", "Permitida"))) %>% 
         ggplot(aes(x= reorder(Pesca, n), y = n,  fill = Pesca)) +
         geom_col() +
         scale_fill_manual(values = c("#fee391", "gray30", "firebrick", "#005a32"), name = "Nivel de Protección") +
         theme_ipsum() +
         labs(x = "", y = "Numero de subzonas en AMP") +
         theme(strip.text.x = element_text(angle = 0, hjust = .5),
               axis.text.x = element_blank(), 
               legend.position = "bottom"))



(p3 <- test %>% 
                select(ANP, Pesca, Buceo) %>% 
                as.data.frame() %>% 
                group_by(Pesca) %>% 
                count() %>% 
                filter(!is.na(Pesca)) %>% 
                mutate(Pesca = factor(Pesca, levels = c("No especificado", "Sin manejo", "Prohibida", "Permitida"))) %>% 
                ggplot(aes(x= reorder(Pesca, n), y = n,  fill = Pesca)) +
                geom_col() +
                scale_fill_manual(values = c( "#fee391", "gray30","firebrick", "#005a32"), name = "Nivel de Protección") +
                theme_ipsum() +
                labs(x = "", y = "Numero de sitios de buceo") +
                theme(strip.text.x = element_text(angle = 0, hjust = .5),
                      axis.text.x = element_blank(), 
                      legend.position = ""))
p1/(p2 + p3)

ggsave('figs/donde_se_permite_pesca.png', dpi = 300, height = 10, width = 8)

p1+(p3 / p2)

ggsave('figs/donde_se_permite_pesca2.png', dpi = 300, height = 8, width = 12)

(p1 <- final_mpa %>% 
                mutate(Buceo = factor(Buceo, levels = c("No especificado", "Sin manejo", "Prohibido", "Permitido"))) %>% 
                ggplot() +
                geom_sf(data = spdf_mx, 
                        fill = "gray90", 
                        col = NA) +
                geom_sf(data = final_mpa, 
                        aes(fill = Buceo), 
                        col = NA) +
                coord_sf(xlim = c(-118, -85), 
                         ylim = c(13, 32), 
                         expand = TRUE) +
                facet_wrap(~Buceo) +
                scale_fill_manual(values = c( "gray30", "#fee391", "firebrick", "#005a32"), aesthetics = c("color", "fill"), labels = c("No especificado", "Prohibido", "Permitido", "Sin Manejo")) +
                #scale_color_manual(values = c(, "#fc4e2a", "#005a32", "black"), labels = c("No especificado", "Prohibido", "Permitido", "Sin Manejo")) +
                labs(subtitle = "Buceo")+
                theme_bw(base_family = "Lato") +
                theme_map() +
                theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
                      strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
                      axis.title = element_blank(),
                      strip.text.y = element_text(hjust = .5, size = 12),
                      panel.grid.minor.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA), 
                      legend.position = ""))

(p2 <- final_mpa %>% 
                select(ANP, Buceo, Buceo) %>% 
                as.data.frame() %>% 
                group_by(Buceo) %>% 
                count() %>% 
                mutate(Buceo = factor(Buceo, levels = c("No especificado", "Sin manejo", "Prohibido", "Permitido"))) %>% 
                ggplot(aes(x= reorder(Buceo, n), y = n,  fill = Buceo)) +
                geom_col() +
                scale_fill_manual(values = c("#fee391", "gray30", "firebrick", "#005a32"), name = "Nivel de Protección") +
                theme_ipsum() +
                labs(x = "", y = "Numero de subzonas en AMP") +
                theme(strip.text.x = element_text(angle = 0, hjust = .5),
                      axis.text.x = element_blank(), 
                      legend.position = "bottom"))



(p3 <- test %>% 
                select(ANP, Buceo, Buceo) %>% 
                as.data.frame() %>% 
                group_by(Buceo) %>% 
                count() %>% 
                filter(!is.na(Buceo)) %>% 
                mutate(Buceo = factor(Buceo, levels = c("No especificado", "Sin manejo", "Prohibido", "Permitido"))) %>% 
                ggplot(aes(x= reorder(Buceo, n), y = n,  fill = Buceo)) +
                geom_col() +
                scale_fill_manual(values = c( "#fee391", "gray30","firebrick", "#005a32"), name = "Nivel de Protección") +
                theme_ipsum() +
                labs(x = "", y = "Numero de sitios de buceo") +
                theme(strip.text.x = element_text(angle = 0, hjust = .5),
                      axis.text.x = element_blank(), 
                      legend.position = ""))
p1/(p2 + p3)


ggsave("figs/donde_se_permite_buceo.png", dpi = 300, height = 10, width = 8)

p1+(p3 / p2)

ggsave('figs/donde_se_permite_buceo2.png', dpi = 300, height = 8, width = 12)


test %>% 
        select(ANP, site_name, Buceo) %>% 
        filter(Buceo == "Prohibido")





## Map of conflict areas Diving Fisheries

final_mpa %>% 
        filter(Pesca %in% c("Permitida", "Sin manejo", "No especificado"), Buceo %in% c("No especificado", "Sin manejo", "Permitido")) %>% 
        as.data.frame() %>% 
        group_by(Pesca, Buceo) %>% 
        count() %>% 
        ungroup() %>% 
        summarise(n=sum(n))

final_mpa %>% 
        filter(Pesca == "Prohibida", Buceo %in% c("No especificado", "Sin manejo", "Permitido")) %>% 
        as.data.frame() %>% 
        group_by(Pesca, Buceo) %>% 
        count() %>% 
        ungroup() %>% 
        summarise(n=sum(n))


(p1 <- data.frame(
        Class = c("Sitio", "Sitio"),
        Type = c("Conflicto", "Sinergia"),
        value = c(247, 100)) %>%
        ggplot(aes(x=Type, y = value, fill = Type))+
        geom_col() +
        scale_fill_manual(values = c("firebrick", "#005a32", "gray90"), aesthetics = c("color", "fill")) +
        theme_ipsum() +
        labs(x = "", y = "Numero de sitios de buceo") +
        theme(strip.text.x = element_text(angle = 0, hjust = .5),
              legend.position = ""))

(p2 <- final_mpa %>% 
                filter(Pesca %in% c("Permitida", "Sin manejo", "No especificado"), Buceo %in% c("No especificado", "Sin manejo", "Permitido")) %>% 
                ggplot() +
                geom_sf(data = spdf_mx, 
                        fill = "gray90", 
                        col = NA) +
                geom_sf(
                        fill = "firebrick", 
                        col = NA) +
                coord_sf(xlim = c(-118, -85), 
                         ylim = c(13, 32), 
                         expand = TRUE) +
                labs(subtitle = "Conflicto") +
                theme_bw(base_family = "Lato") +
                theme_map() +
                theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
                      strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
                      axis.title = element_blank(),
                      strip.text.y = element_text(hjust = .5, size = 12),
                      panel.grid.minor.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA), 
                      legend.position = ""))


(p3 <- final_mpa %>% 
                filter(Pesca == "Prohibida", Buceo %in% c("No especificado", "Sin manejo", "Permitido")) %>% 
                ggplot() +
                geom_sf(data = spdf_mx, 
                        fill = "gray90", 
                        col = NA) +
                geom_sf(
                        fill = "darkgreen", 
                        col = NA) +
                coord_sf(xlim = c(-118, -85), 
                         ylim = c(13, 32), 
                         expand = TRUE) +
                labs(subtitle = "Sinergia") +
                theme_bw(base_family = "Lato") +
                theme_map() +
                theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
                      strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
                      axis.title = element_blank(),
                      strip.text.y = element_text(hjust = .5, size = 12),
                      panel.grid.minor.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA), 
                      legend.position = ""))

(p2 + p3) / p1

ggsave('figs/mpas_buceo_vs_pesca.png', dpi = 300, height = 4, width = 5)













