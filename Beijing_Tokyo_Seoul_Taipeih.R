#Datenpaket instalieren 
install.packages("readxl")
#Paket laden
library(readxl)
library(tidyverse)

#Daten aus Excel-Tabelle einlesen 
pm25 <- read_excel("01_data/who_ambient_air_quality_database_version_2024_(v6.1).xlsx")

#alle Tabellenblätter anzeigen 
excel_sheets("01_data/who_ambient_air_quality_database_version_2024_(v6.1).xlsx")

#richtiges Sheet einlesen
pm25 <- read_excel(
  "01_data/who_ambient_air_quality_database_version_2024_(v6.1).xlsx",
  sheet = "Update 2024 (V6.1)")

#Kontroll-Check
colnames(pm25)
head(pm25)

#Städte filtern
library(tidyverse)

staedte <- c("Beijing", "Seoul", "Tokyo", "Taipei")

pm25_sel <- pm25 %>%
  filter(
    city %in% staedte,
    year >= 2010
  )

#Kontrolle wie viele Jahre die ausgewähltenStädte haben
count(pm25_sel, city)

#vgl. der echte Städte Namen
unique(pm25$city)

#die Liste ist sehr lang deshalb 
unique(pm25$city)[1:50]

#richtige Städtenamen finden 
pm25 %>%
  filter(str_detect(city, "Beijing|Seoul|Tokyo|Taipei")) %>%
  distinct(city)

#erneut Filtern
library(stringr)

pm25_sel <- pm25 %>%
  filter(
    str_detect(city, "Beijing/CHN|Seoul/KOR|Tokyo/JPN|Taipei/CHN"),
    year >= 2010)

#testen
count(pm25_sel, city)

#Daten bereinigen 
pm25_sel <- pm25_sel %>%
  mutate(
    city_clean = str_remove(city, "/.*")
 )

#testen
unique(pm25_sel$city_clean)

library(ggplot2)

ggplot(pm25_sel, aes(x = year, y = pm25_concentration, color = city_clean)) +
  geom_line(size = 1.1) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Entwicklung der PM2.5-Konzentrationen in ausgewählten ostasiatischen Hauptstädten",
    x = "Jahr",
    y = "PM2.5-Konzentration (µg/m³)",
    color = "Stadt" )

#N.A entfernen
pm25_plot <- pm25_sel %>%
  filter(!is.na(pm25_concentration))

pm25_sel <- pm25_sel %>%
  mutate(
    pm25_concentration = as.numeric(pm25_concentration))

pm25_plot <- pm25_sel %>%
  filter(!is.na(pm25_concentration))

#Kontrolle
str(pm25_plot$pm25_concentration)

#Erste Plot
ggplot(pm25_plot, aes(x = year, y = pm25_concentration, color = city_clean)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  scale_x_continuous(
    breaks = seq(2010, 2020, 2)
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Entwicklung der PM2.5-Konzentrationen in ostasiatischen Hauptstädten (2010–2020)",
    x = "Jahr",
    y = "PM2.5-Konzentration (µg/m³)",
    color = "Stadt")

#Speichern
ggsave(
  "pm25_trends_ostasien.png",
  width = 8,
  height = 5,
  dpi = 300)

#Karten erstellen
pm25_map <- pm25_plot %>%
  filter(year == max(2010))

library(maps)

world <- map_data("world")

ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "grey90",
    color = "white"
  ) +
  geom_point(
    data = pm25_map,
    aes(
      x = longitude,
      y = latitude,
      color = pm25_concentration,
      size = pm25_concentration
    ),
    alpha = 0.8
  ) +
  scale_color_viridis_c(name = "PM2.5 (µg/m³)") +
  coord_quickmap(xlim = c(110, 145), ylim = c(20, 45)) +
  theme_minimal() +
  labs(
    title = paste("PM2.5-Konzentrationen in ostasiatischen Hauptstädten", max(pm25_map$year)),
    x = "", y = ""
  )

nrow(pm25_map)

pm25_map <- pm25_plot %>%
  filter(year == 2019)

summary(pm25_map$latitude)
summary(pm25_map$longitude)

pm25_map <- pm25_map %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  filter(
    !is.na(latitude),
    !is.na(longitude)
  )

nrow(pm25_map)

ggplot() +
  borders("world", colour = "gray80") +
  geom_point(
    data = pm25_map,
    aes(x = longitude, y = latitude),
    color = "red",
    size = 4
  ) +
  coord_quickmap(xlim = c(110, 145), ylim = c(20, 45))

ggplot() +
  borders("world", colour = "gray90", fill = "gray95") +
  geom_point(
    data = pm25_map,
    aes(
      x = longitude,
      y = latitude,
      color = pm25_concentration,
      size = pm25_concentration
    ),
    alpha = 0.8
  ) +
  scale_color_viridis_c(name = "PM2.5 (µg/m³)") +
  coord_quickmap(xlim = c(110, 145), ylim = c(20, 45)) +
  theme_minimal() +
  labs(
    title = paste("PM2.5-Konzentrationen in ostasiatischen Hauptstädten", unique(pm25_map$year)),
    x = "longitude", y = "latitude")


#Darstellung von mehreren Jahren 
pm25_map_multi <- pm25_plot %>%
  filter(year %in% c(2012, 2014, 2018, 2020))

library(tidyverse)
library(maps)

pm25_map_multi <- pm25_plot %>%
  filter(year %in% c(2010, 2015, 2020)) %>%
  mutate(
    longitude = as.numeric(longitude),
    latitude  = as.numeric(latitude)
  ) %>%
  filter(
    !is.na(longitude),
    !is.na(latitude))

nrow(pm25_map_multi)

ggplot(pm25_map_multi) +
  borders("world", fill = "gray95", colour = "gray80") +
  geom_point(
    aes(
      x = longitude,
      y = latitude,
      color = pm25_concentration,
      size  = pm25_concentration
    ),
    alpha = 0.8
  ) +
  scale_color_viridis_c(name = "PM2.5 (µg/m³)") +
  facet_wrap(~ 2010, 2015, 2020) +
  theme_minimal() +
  labs(
    title = "PM2.5-Konzentrationen in ostasiatischen Hauptstädten",
    x = "longitude", y = "latitude")
