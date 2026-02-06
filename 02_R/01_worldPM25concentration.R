#1. Pakete installieren und laden
#Datenpaket ifür Excel-Dateien nstallieren 
install.packages("readxl")
#Paket laden
library(readxl) #zum Einlesen von Excel-Dateien
library(tidyverse) #Sammlung von Paketen für Datenverarbeitung und Visualisierung 

#2- Excel-Daten einlesen
#Datei aus Excel-Tabelle einlesen 
pm25 <- read_excel("01_data/who_ambient_air_quality_database_version_2024_(v6.1).xlsx")

#alle Tabellenblätter anzeigen 
excel_sheets("01_data/who_ambient_air_quality_database_version_2024_(v6.1).xlsx")

#richtiges Sheet einlesen
pm25 <- read_excel(
  "01_data/who_ambient_air_quality_database_version_2024_(v6.1).xlsx",
  sheet = "Update 2024 (V6.1)")

#Erster Kontroll-Check der Spaltenamen und der ersten Zeilen 
colnames(pm25)
head(pm25)

#3. Zeitreihen für WHO-Regionen erstellen
#Zeitstrahl für dieunterschiedlichen Kontinente erstellen
library(tidyverse)

#Daten bereinigen, auf 2010-2020 beschränken, NA-Werte entfernen
pm25_ts <- pm25 %>%
  mutate(pm25_concentration = as.numeric(pm25_concentration)) %>% #sicherstellen, dass PM2.5 numerisch ist
  filter(
    year >= 2010,
    year <= 2020,
    !is.na(pm25_concentration),
    !is.na(who_region)) %>%
  group_by(who_region, year) %>% #Gruppierung nach WHO-Regionen und Jahr
  summarise(
    pm25_mean = mean(pm25_concentration, na.rm = TRUE), #jährlicher Mittelwert
    .groups = "drop")

#Kontrolle
head(pm25_ts)

#4. Visualisierung: PM2.5 nach Regionen
ggplot(pm25_ts, aes(x = year, y = pm25_mean, color = who_region)) +
  geom_line(linewidth = 1.2) + #Linien für Zeitverlauf
  geom_point(size = 1.5) + #Punkte für einzelne Jahre
  theme_minimal(base_size = 12) + #minimalistisches Thema
  labs(
    title = "Globale Entwicklung der PM2.5-Belastung nach WHO-Regionen",
    subtitle = "Jährliche Mittelwerte, 2010–2020",
    x = "Jahr",
    y = "PM2.5-Konzentration (µg/m³)",
    color = "WHO-Region",
    caption = "Quelle: WHO Ambient Air Quality Database (v6.1), eigene Berechnung"
  ) +
  geom_smooth(color = "grey") #Glättungslinie für Überblick

# 5. Lineale Regression pro Region 
library(broom) #für Modellübersicht

#Lineares Modell für jede Region 
regression_results <- pm25_ts %>%
  group_by(who_region) %>%
  do(tidy(lm(pm25_mean ~ year, data = .)))

#Nur Koeffizienten extrahieren
coefficients <- regression_results %>%
  select(who_region, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  rename(intercept = `(Intercept)`,
    slope = year)

coefficients #zeigt Intercept und Slope pro Region 
#Slope zeigt die jährliche VErteilung der PM2.5 Konzentration pro WHO-Region

#6. Visualisierung der Trends und Slopes
ggplot(pm25_ts, aes(x = year, y = pm25_mean, color = who_region)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(title = "Globale Entwicklung der PM2.5-Belastung mit linearen Trends",
    x = "Jahr",
    y = "PM2.5 (µg/m³)",
    color = "WHO-Region")

#Balkendiagramm der jährlichen Veränderung 
ggplot(coefficients,
       aes(x = reorder(who_region, slope),
           y = slope)) +
  geom_col() +
  coord_flip() + #horizontale Darstellung
  theme_minimal() +
  labs(title = "Jährliche Veränderung der PM2.5-Belastung nach WHO-Region",
    x = "WHO-Region",
    y = "Slope (µg/m³ pro Jahr)")

#Weitere Variante Slope + Intercept zu visualisieren
# Dotplot
coefficients_long <- regression_results %>%
  filter(term %in% c("(Intercept)", "year")) %>%
  mutate(term = recode(term,
                       "(Intercept)" = "Intercept",
                       "year" = "Slope"))

ggplot(coefficients_long,
       aes(x = estimate,
           y = reorder(who_region, estimate))) +
  geom_point(size = 3) +
  facet_wrap(~term, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Intercepts und Steigungen der linearen Regression nach WHO-Region",
    x = "Koeffizientenwert",
    y = "WHO-Region")

#Darstellung nur der Slopes
coefficients_slope <- coefficients %>%
  select(who_region, slope)

ggplot(coefficients_slope,
       aes(x = reorder(who_region, slope),
           y = slope)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Jährliche Veränderung der PM2.5-Belastung nach WHO-Region",
    x = "WHO-Region",
    y = "Slope (µg/m³ pro Jahr)")

#Dastellung der Slopes direkt im Zeitstrahl
last_year <- max(pm25_ts$year)

label_data <- pm25_ts %>%
  filter(year == last_year) %>%
  left_join(coefficients, by = "who_region")

ggplot(pm25_ts, aes(x = year, y = pm25_mean, color = who_region)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  geom_text(
    data = label_data,
    aes(
      x = year,
      y = pm25_mean,
      label = round(slope, 2)
    ),
    hjust = -0.2,
    show.legend = FALSE
  ) +
  theme_minimal() +
  labs(
    title = "PM2.5-Entwicklung mit linearen Trends",
    x = "Jahr",
    y = "PM2.5 (µg/m³)")

#Weitere Visualisierungsversion 
library(tidyverse)

coeff_long <- coefficients %>%
  pivot_longer(
    cols = c(intercept, slope),
    names_to = "term",
    values_to = "estimate")

coeff_long

ggplot(coeff_long,
       aes(x = estimate,
           y = reorder(who_region, estimate))) +
  geom_point(size = 3) +
  facet_wrap(~term, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Intercepts und Steigungen der linearen Regression nach WHO-Region",
    x = "Koeffizientenwert",
    y = "WHO-Region")

ggplot(coefficients,
       aes(x = slope,
           y = reorder(who_region, slope))) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Jährliche Veränderung der PM2.5-Belastung nach WHO-Region",
    x = "Slope (µg/m³ pro Jahr)",
    y = "WHO-Region")

#Visualisierung 
# Intercept als Punkt am Startjahr 
start_points <- pm25_ts %>%
  group_by(who_region) %>%
  filter(year == min(year)) %>%
  ungroup()

end_points <- pm25_ts %>%
  group_by(who_region) %>%
  filter(year == max(year)) %>%
  ungroup()

ggplot(pm25_ts, aes(x = year, y = pm25_mean, color = who_region)) +
  geom_line(linewidth = 1.1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  
  
  geom_point(
    data = start_points,
    size = 4,
    shape = 21,
    fill = "white",
    stroke = 1.2) +
  geom_point(
    data = end_points,
    size = 4,
    shape = 19) +
   theme_minimal() +
  labs(title = "Globale Entwicklung der PM2.5-Belastung mit Start- und Endpunkten",
    x = "Jahr",
    y = "PM2.5 (µg/m³)",
    color = "WHO-Region")

#Visualisierung anpassen - Ziel ist es den Start und den Endpunkt in ein Diagramm mit den Regionen zu bringen 
#Start/Endpunkte zusammenführen
points_df <- bind_rows( start_points %>% mutate(point_type = "Start"),
  end_points   %>% mutate(point_type = "Ende"))

#Plot mit gemeinsamer Legende 
ggplot(pm25_ts, aes(x = year, y = pm25_mean, color = who_region)) +
  
  geom_line(aes(linetype = "Entwicklung"), linewidth = 1.1) +
  
  geom_smooth(
    aes(linetype = "Regression"),
    method = "lm",
    se = FALSE) +
  geom_point(
    data = points_df,
    aes(shape = point_type),
    size = 3,
    stroke = 1.2) +
  scale_shape_manual(
    name = "Punkte",
    values = c("Start" = 21, "Ende" = 19)) +
   scale_linetype_manual(
    name = "Linien",
    values = c(
      "Entwicklung" = "solid",
      "Regression" = "dashed" )) +
   theme_minimal() +
  labs(
    title = "Globale PM2.5-Entwicklung mit Start- und Endpunkten",
    x = "Jahr",
    y = "PM2.5 (µg/m³)",
    color = "WHO-Region")

#7. Globale Mittelwerte berechnen
#Globalen Trend mit Regission berechnen 
#Nutzung der Länder-Jahreswerte
pm25_country_year <- pm25 %>%
  filter(year >= 2010, year <= 2020) %>%
  filter(!is.na(pm25_concentration)) %>%
  group_by(country_name, year) %>%
  summarise(
    pm25_mean = mean(pm25_concentration, na.rm = TRUE),
    .groups = "drop")

#globles Mittel berechnen -hier: ungewichteter Mittelwert der Länder
pm25_global <- pm25_country_year %>%
  group_by(year) %>%
  summarise(
    global_pm25 = mean(pm25_mean, na.rm = TRUE),
    .groups = "drop")

#Globale Regression und Visualiserung 
#Globale Regression berechnen im linealem Modell für globalen Mittelwert
model_global <- lm(global_pm25 ~ year, data = pm25_global)

summary(model_global)


#Globalen Zeitstrahl plotten
ggplot(pm25_global, aes(x = year, y = global_pm25)) +
  geom_line(linewidth = 1.2, color = "#2c7bb6") +
  geom_point(size = 3, color = "#2c7bb6") +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linetype = "dashed",
    color = "#d7191c") +
  theme_minimal() +
   labs(
    title = "Globale Entwicklung der PM2.5-Belastung (2010–2020)",
    subtitle = "Lineare Regression der globalen Jahresmittelwerte",
    x = "Jahr",
    y = "PM2.5 (µg/m³)",
    caption = "Quelle: WHO Ambient Air Quality Database 2024 (V6.1); eigene Berechnungen")

#Modellwerte extrahieren
model_global <- lm(global_pm25 ~ year, data = pm25_global)

#Extrahieren von Intercept, Slope für Beschriftung
intercept <- coef(model_global)[1]
slope <- coef(model_global)[2]
r2 <- summary(model_global)$r.squared

#Label erstellen
label_text <- paste0(
  "Intercept = ", round(intercept, 2),
  "\nSlope = ", round(slope, 3), " µg/m³/Jahr",
  "\nR² = ", round(r2, 2))

#Plot mit linearer Regression und Beschriftung
ggplot(pm25_global, aes(x = year, y = global_pm25)) +
  
  geom_line(linewidth = 1.2, color = "#2c7bb6") +
  geom_point(size = 3, color = "#2c7bb6") +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    linetype = "dashed",
    color = "#d7191c"
  ) +
   annotate(
    "text",
    x = min(pm25_global$year) + 1,
    y = max(pm25_global$global_pm25),
    label = label_text,
    hjust = 0,
    size = 4) +
  theme_minimal() +
  labs(
    title = "Globale Entwicklung der PM2.5-Belastung (2010–2020)",
    subtitle = "Lineare Regression der globalen Jahresmittelwerte",
    x = "Jahr",
    y = "PM2.5 (µg/m³)",
    caption = "Quelle: WHO Ambient Air Quality Database 2024 (V6.1); eigene Berechnungen")

#Minimale Anpassunge n am Design des plot  - Lage er Angabe von den Label ändern
p <- ggplot(pm25_global, aes(x = year, y = global_pm25)) +
  geom_line(linewidth = 1.2, color = "#2c7bb6") +
  geom_point(size = 3, color = "#2c7bb6") +
  geom_smooth(method = "lm", se = FALSE,
              linetype = "dashed", color = "#d7191c") +
  theme_minimal() +
  labs(
    title = "Globale Entwicklung der PM2.5-Belastung (2010–2020)",
    x = "Jahr",
    y = "PM2.5 (µg/m³)",
    caption = "Quelle: WHO Ambient Air Quality Database 2024 (V6.1); eigene Berechnungen") +
p +
  labs(
    subtitle = paste0(
      "Lineares Modell: Intercept = ", round(intercept,2),
      " | Slope = ", round(slope,3),
      " µg/m³/Jahr | R² = ", round(r2,2)))+ 
  theme(plot.subtitle = element_text(
    size = 10,
    color = "grey20",
    margin = margin(b = 10)))


#Vorhersagemodelle erstellen
#Darstellung des weltweiten Trend auf Basisaller Regionen - als 
ggplot(pm25_ts, aes(x = year, y = pm25_mean, color = who_region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Globale Entwicklung der PM2.5-Belastung nach WHO-Regionen",
    subtitle = "Jährliche Mittelwerte, 2010–2020",
    x = "Jahr",
    y = "PM2.5-Konzentration (µg/m³)",
    color = "WHO-Region",
    caption = "Quelle: WHO Ambient Air Quality Database (v6.1), eigene Berechnung"
  ) +
  geom_smooth(color = "grey")

#Kontrolle 
count(pm25_ts, who_region)
range(pm25_ts$year)
summary(pm25_ts$pm25_mean)

#diese Vorgehensweise wurde in der Seminarstunde verufen und es soll im weiteren Verlauf ein Vorhersagemodell mit loess()erstllt werden
# Aus Interesse bleibt das Verworfendemodell bestehen, damit ich beim später noch ggf. das Model wiederfinden kann 

#V9. orhersagemodell mit loess() 
#Modelle bauen 
# Lineares Modell
mod_lm <- lm(global_pm25 ~ year, data = pm25_global)

# LOESS Modell
mod_loess <- loess(global_pm25 ~ year,
                   data = pm25_global,
                   span = 0.75)

#Zukunftsdaten erzeugen  (10 Jahres Prognose)
future_years <- data.frame(
  year = seq(
    max(pm25_global$year) + 1,
    max(pm25_global$year) + 10,
    by = 1))
#Prognosse berechnen
future_years$pred_lm <- predict(mod_lm, newdata = future_years)

future_years$pred_loess <- predict(mod_loess,
                                   newdata = future_years)

#Prognose mit  Orginaldaten kombinieren 
pm25_future <- bind_rows(
  
  pm25_global %>%
    mutate(type = "Beobachtung",
           pred_lm = global_pm25,
           pred_loess = global_pm25),
  
  future_years %>%
    mutate(type = "Prognose",
           global_pm25 = NA))

#Visualisieren
ggplot() +
  geom_line(data = pm25_global, #Originaldaten
            aes(x = year, y = global_pm25),
            linewidth = 1.2,
            color = "black") +
  geom_point(data = pm25_global,
             aes(x = year, y = global_pm25),
             size = 2.5,
             color = "black") +
  geom_line(data = future_years, # Lineare Prognose
            aes(x = year, y = pred_lm),
            color = "#d7191c",
            linetype = "dashed",
            linewidth = 1.1) +
  geom_line(data = future_years,  #LOESS Prognose
            aes(x = year, y = pred_loess),
            color = "#2c7bb6",
            linewidth = 1.1) +
  theme_minimal() +
  labs(
    title = "Prognose der globalen PM2.5-Belastung (10 Jahre)",
    subtitle = "Lineares Modell (rot gestrichelt) und LOESS-Modell (blau)",
    x = "Jahr",
    y = "PM2.5 (µg/m³)",
    caption = "Quelle: WHO Ambient Air Quality Database 2024 (V6.1); eigene Berechnungen")

#Visualisierung in 1-Jahres-Schritten
future_years <- data.frame(
  year = seq(
    from = max(pm25_global$year) + 1,
    to   = max(pm25_global$year) + 10,
    by   = 1))
#genauere Berechnung der  Prognosen
future_years$pred_lm <- predict(mod_lm, newdata = future_years)

future_years$pred_loess <- predict(mod_loess,
                                   newdata = future_years)

#Visualisierung mit den einzelnenPrognoseschritten
ggplot() +
  geom_line(data = pm25_global,  # Beobachtete Daten
            aes(x = year, y = global_pm25),
            linewidth = 1.2,
            color = "black") +
  geom_point(data = pm25_global,
             aes(x = year, y = global_pm25),
             size = 2.5,
             color = "black") +
  geom_line(data = future_years,  #Lineare Prognose (Linie + Punkte pro Jahr)
            aes(x = year, y = pred_lm),
            color = "#d7191c",
            linetype = "dashed",
            linewidth = 1.1) +
  geom_point(data = future_years,
             aes(x = year, y = pred_lm),
             color = "#d7191c",
             size = 2) +
  geom_line(data = future_years, # LOESS Prognose (Linie + Punkte pro Jahr)
            aes(x = year, y = pred_loess),
            color = "#2c7bb6",
            linewidth = 1.1) +
  geom_point(data = future_years,
             aes(x = year, y = pred_loess),
             color = "#2c7bb6",
             size = 2) +
  theme_minimal() +
  labs(
    title = "Globale PM2.5-Prognose in 1-Jahres-Schritten",
    subtitle = "Lineares Modell (rot) und LOESS-Modell (blau)",
    x = "Jahr",
    y = "PM2.5 (µg/m³)",
    caption = "Quelle: WHO Ambient Air Quality Database 2024 (V6.1); eigene Berechnungen")

#Loess_Prognose inkl. Confidence Interval
mod_lm <- lm(global_pm25 ~ year, data = pm25_global)

future_years <- data.frame(
  year = seq(2021, 2030, 1))

pred <- predict(
  mod_lm,
  newdata = future_years,
  interval = "confidence")

future_years <- cbind(future_years, pred)

ggplot() +
  geom_point(data = pm25_global,
             aes(year, global_pm25)) +
  geom_smooth(data = pm25_global,
              aes(year, global_pm25),
              method = "loess",
              se = FALSE,
              linewidth = 1.2) +
  geom_line(data = future_years,
            aes(year, fit),
            linetype = "dashed",
            linewidth = 1.2) +
  geom_ribbon(data = future_years,
              aes(year, ymin = lwr, ymax = upr),
              alpha = 0.2) +
   labs(
    title = "Globale PM2.5 Entwicklung und Prognose bis 2030",
    subtitle = "Trend: LOESS | Prognose: Lineare Regression",
    x = "Jahr",
    y = "PM2.5 µg/m³",
    caption = "Quelle: WHO Ambient Air Quality Database 2024") +
  theme_minimal()

#Zweiter Versuch der Prognosenberechnung über ein Schleifenmodell

#Modell erstellen 
mod_loess <- loess(
  global_pm25 ~ year,
  data = pm25_global,
  span = 0.75)

#Zukunftsdaten definieren
future_years <- seq(
  from = max(pm25_global$year) + 1,
  to   = max(pm25_global$year) + 10,
  by   = 1)

#Prognose mit Loess in einer Schleife
#Vector
pred_loess <- numeric(length(future_years))

#Schleife vorbereiten 
for(i in seq_along(future_years)) {
  
  new_data <- data.frame(year = future_years[i])
  
  pred_loess[i] <- predict(
    mod_loess,
    newdata = new_data)}

#Ergebnis prüfen
pred_loess
# Hier ist mir dann eingefallen, dass die Erstellung der Schleifen im Seminar zum Vorhersagemodell über gam() genutzt werden sollte
#von gam()-Modell wurde abgeraten und loess() empfohlen, aus  dokumentativen Gründen , lass ich die codes im skript bestehen 



#10. weltweiter Vergleich mit 2.5-Mittelwerten
#Datenbasis vorbereiten 

library(tidyverse)

pm25_clean <- pm25 %>%
  mutate(
    pm25_concentration = as.numeric(pm25_concentration)
  ) %>%
  filter(
    year >= 2010,
    year <= 2020,
    !is.na(pm25_concentration),
    !is.na(country_name))

library(tidyverse)

pm25_ts <- pm25 %>%
  mutate(pm25_concentration = as.numeric(pm25_concentration)) %>%
  filter(
    year >= 2010,
    year <= 2020,
    !is.na(pm25_concentration),
    who_region %in% c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")
  ) %>%
  group_by(who_region, year) %>%
  summarise(
    pm25_mean = mean(pm25_concentration, na.rm = TRUE),
    .groups = "drop")

count(pm25_ts, who_region)

ggplot(pm25_ts, aes(x = year, y = pm25_mean, group = who_region)) +
  geom_line() +
  geom_point()

region_labels <- c(
  AFR = "Afrika",
  AMR = "Amerika",
  EMR = "Östlicher Mittelmeerraum",
  EUR = "Europa",
  SEAR = "Südostasien",
  WPR = "Westpazifik")

ggplot(pm25_ts, aes(x = year, y = pm25_mean, color = who_region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = scales::hue_pal()(6),
    labels = region_labels,
    name = "WHO-Region"
  ) +
  theme_minimal() +
  labs(
    title = "Globale Entwicklung der PM2.5-Belastung nach WHO-Regionen",
    x = "Jahr",
    y = "PM2.5 (µg/m³)",
    caption = "Quelle: WHO Ambient Air Quality Database (v6.1), eigene Berechnung"
  )

#Mittelwert pro Land (2010-2020) 
pm25_country_mean <- pm25_clean %>%
  group_by(country_name, iso3) %>%
  summarise(
    pm25_mean_2010_2020 = mean(pm25_concentration, na.rm = TRUE),
    .groups = "drop")

summary(pm25_country_mean$pm25_mean_2010_2020)

install.packages("rnaturalearth")
install.packages("rnaturalearthdata")


library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

world <- ne_countries(scale = "medium", returnclass = "sf")

world_pm25 <- world %>%
  left_join(
    pm25_country_mean,
    by = c("iso_a3" = "iso3"))

summary(world_pm25$pm25_mean_2010_2020)

ggplot(world_pm25) +
  geom_sf(aes(fill = pm25_mean_2010_2020))

ggplot(world_pm25) +
  geom_sf(
    aes(fill = pm25_mean_2010_2020),
    color = "gray80",
    size = 0.1
  ) +
  scale_fill_viridis_c(
    name = "PM2.5 (µg/m³)\nMittelwert 2010–2020",
    na.value = "gray90") +
  theme_minimal() +
  labs(
    title = "Globale durchschnittliche PM2.5-Belastung (2010–2020)",
    subtitle = "Länderbasierte Mittelwerte",
    caption = "Quelle: WHO Ambient Air Quality Database (v6.1), eigene Berechnung")

#Erstellung anderer Visualisierung 
#Top 20 Länder mit höchster Belastung 
pm25_top20 <- pm25_country_mean %>%
  arrange(desc(pm25_mean_2010_2020)) %>%
  slice_head(n = 20)

#Balkendiagramm
ggplot(pm25_top20,
       aes(x = reorder(country_name, pm25_mean_2010_2020),
           y = pm25_mean_2010_2020)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Länder mit der höchsten durchschnittlichen PM2.5-Belastung (2010–2020)",
    x = "Land",
    y = "PM2.5 (µg/m³)",
    caption = "Quelle: WHO, eigene Berechnung"
  )

#Dot-Plot
ggplot(pm25_top20,
       aes(x = pm25_mean_2010_2020,
           y = reorder(country_name, pm25_mean_2010_2020))) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Durchschnittliche PM2.5-Konzentration ausgewählter Länder (2010–2020)",
    x = "PM2.5 (µg/m³)",
    y = "Land")

#Tabelle
pm25_top20 %>%
  mutate(pm25_mean_2010_2020 = round(pm25_mean_2010_2020, 2))


#Top 20 Länder mit niedrigster Belastung 
# Niedrigste 20 Länder nach PM2.5
pm25_bottom20 <- pm25_country_mean %>%
  arrange(pm25_mean_2010_2020) %>%  # aufsteigend sortieren
  slice_head(n = 20)

# Balkendiagramm
ggplot(pm25_bottom20,
       aes(x = reorder(country_name, pm25_mean_2010_2020),
           y = pm25_mean_2010_2020)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Länder mit der niedrigsten durchschnittlichen PM2.5-Belastung (2010–2020)",
    x = "Land",
    y = "PM2.5 (µg/m³)",
    caption = "Quelle: WHO, eigene Berechnung")

# Dot-Plot
ggplot(pm25_bottom20,
       aes(x = pm25_mean_2010_2020,
           y = reorder(country_name, pm25_mean_2010_2020))) +
  geom_point(size = 3, color = "steelblue") +
  theme_minimal() +
  labs(
    title = "Durchschnittliche PM2.5-Konzentration ausgewählter Länder (2010–2020)",
    x = "PM2.5 (µg/m³)",
    y = "Land")

# Tabelle
pm25_bottom20 %>%
  mutate(pm25_mean_2010_2020 = round(pm25_mean_2010_2020, 2))

#Visualisierung der Slopes in einer Weltkarte zum besseren Vergleich 
pm25 <- pm25 %>%
  mutate(pm25_concentration = as.numeric(pm25_concentration))

pm25_clean <- pm25 %>%
  filter(!is.na(pm25_concentration))

library(broom)
library(dplyr)

slopes_country <- pm25_clean %>%
  filter(year >= 2010, year <= 2020) %>%
  group_by(country_name) %>%
  do(tidy(lm(pm25_concentration ~ year, data = .))) %>%
  filter(term == "year") %>%
  select(country_name, slope = estimate)

slopes_country <- pm25_clean %>%
  filter(year >= 2010, year <= 2020) %>%
  group_by(country_name) %>%
  filter(n() >= 5) %>%   # mindestens 5 Messpunkte
  do(tidy(lm(pm25_concentration ~ year, data = .))) %>%
  filter(term == "year") %>%
  select(country_name, slope = estimate)

str(pm25)

pm25_country_year <- pm25 %>%
  filter(year >= 2010, year <= 2020) %>%
  filter(!is.na(pm25_concentration)) %>%
  group_by(country_name, year) %>%
  summarise(
    pm25_mean = mean(pm25_concentration, na.rm = TRUE),
    .groups = "drop")

library(broom)

slopes_country <- pm25_country_year %>%
  group_by(country_name) %>%
  filter(n() >= 5) %>%   
  do(tidy(lm(pm25_mean ~ year, data = .))) %>%
  filter(term == "year") %>%
  select(country_name, slope = estimate)

head(slopes_country)
summary(slopes_country$slope)

library(rnaturalearth)
library(sf)

world <- ne_countries(scale = "medium", returnclass = "sf")

world_slopes <- world %>%
  left_join(slopes_country,
            by = c("name" = "country_name"))

ggplot(world_slopes) +
  geom_sf(aes(fill = slope)) +
  theme_minimal() +
  labs(
    title = "Trend der PM2.5-Belastung weltweit (2010–2020)",
    fill = "Slope\n(µg/m³/Jahr)")

#Kleine Änderungen an der Karte - Quellenangabe, Farbänderungen
ggplot(world_slopes) +
  geom_sf(aes(fill = slope), color = "grey40", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "#2c7bb6",     # blau = Verbesserung
    mid = "#ffffbf",     # gelb = stabil
    high = "#d7191c",    # rot = Verschlechterung
    midpoint = 0,
    na.value = "grey90",
    name = "Trend PM2.5\n(µg/m³ pro Jahr)") +
  theme_minimal() +
  labs(
    title = "Globale Trends der PM2.5-Belastung (2010–2020)",
    subtitle = "Lineare Regression basierend auf WHO Ambient Air Quality Database",
    caption = "Quelle: WHO Ambient Air Quality Database 2024 (V6.1); eigene Berechnungen in R")+
  theme(
  legend.position = "right",
  plot.title = element_text(face = "bold", size = 14),
  plot.subtitle = element_text(size = 11),
  plot.caption = element_text(size = 8, color = "grey40"))

+## Weltkarte mit: globalem Trend pro Land, Slope als Farbskala
# Pakete laden
library(tidyverse)
library(broom)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)


# 1. Bereinigung & Vorbereitung
pm25_clean <- pm25 %>%
  mutate(pm25_concentration = as.numeric(pm25_concentration)) %>%
  filter(!is.na(pm25_concentration),
         year >= 2010,
         year <= 2020)

# 2. Slopes pro Land berechnen
slopes_country <- pm25_clean %>%
  group_by(country_name) %>%
  filter(n() >= 5) %>%  # mind. 5 Messpunkte für Stabilität
  do(tidy(lm(pm25_concentration ~ year, data = .))) %>%
  filter(term == "year") %>%
  select(country_name, slope = estimate)

# Intercepts optional für Startpunkt visualisieren
intercepts_country <- pm25_clean %>%
  group_by(country_name) %>%
  filter(n() >= 5) %>%
  do(tidy(lm(pm25_concentration ~ year, data = .))) %>%
  filter(term == "(Intercept)") %>%
  select(country_name, intercept = estimate)


# 3. Weltkarte vorbereiten
world <- ne_countries(scale = "medium", returnclass = "sf")

# Slopes & Intercepts an Länder-Shape anhängen
world_slopes <- world %>%
  left_join(slopes_country, by = c("name" = "country_name")) %>%
  left_join(intercepts_country, by = c("name" = "country_name"))

# 4. Karte erstellen
ggplot(world_slopes) +
  geom_sf(aes(fill = slope), color = "grey40", linewidth = 0.1) + # Länder mit Farbskala nach Slope
  scale_fill_gradient2( # Farbskala: blau = Verbesserung, gelb = stabil, rot = Verschlechterung
    low = "#2c7bb6",
    mid = "#ffffbf",
    high = "#d7191c",
    midpoint = 0,
    na.value = "grey90",
    name = "Trend PM2.5\n(µg/m³/Jahr)") +
  theme_minimal() +
   labs(
    title = "Globale Trends der PM2.5-Belastung (2010–2020)",
    subtitle = "Slope = jährliche Veränderung der PM2.5-Konzentration",
    caption = "Quelle: WHO Ambient Air Quality Database 2024 (V6.1); eigene Berechnung") +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 8, color = "grey40"))
