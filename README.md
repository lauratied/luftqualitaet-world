# luftqualitaet-world

Globale PM2.5-Analyse 2010–2020

Dieses Repository enthält die Analyse der globalen Luftqualität (PM2.5) auf Basis der WHO Ambient Air Quality Database (Version 2024, V6.1). Ziel ist die Visualisierung der Entwicklung und Trends der PM2.5-Konzentrationen nach WHO-Regionen, Ländern und global sowie die Erstellung von Prognosen für die Jahre 2021–2030.

Metadaten: 
- Quelle: WHO Ambient Air Quality Database 2024 (V6.1)
- Zeitraum: 2010–2020
- Inhalte (colenames):
   "who_region"         "iso3"               "country_name"       "city"              
    "year"               "version"            "pm10_concentration" "pm25_concentration"
    "no2_concentration"  "pm10_tempcov"       "pm25_tempcov"       "no2_tempcov"       
   "type_of_stations"   "reference"          "web_link"           "population"        
    "population_source"  "latitude"           "longitude"          "who_ms"    
- Rohdaten befinden sich im Ordner `data/`

Projektstruktur/Ordnerstruktur
- `R/` : R-Skripte zur Analyse
- `data/` : Rohdaten (Excel/CSV)
- `output/` : erzeugte Plots, Tabellen, Reports
- `.gitignore` : ignorierte Dateien (temporäre Dateien, große Rohdaten, etc.)


Analyse
- Berechnung von Jahresmittelwerten pro Land und Region
- Lineare Regression für globale Trends
- LOESS-Glättung zur Trendvisualisierung
- Erstellung von Prognosen für 2021–2030 basierend auf linearem Modell
- Visualisierung:
  - Zeitreihenplots pro Region
  - Globale Durchschnittsentwicklung
  - Weltkarten mit Trends (Slope)


Quellen & Lizenz
- WHO Ambient Air Quality Database 2024 (V6.1)
- Eigene Berechnungen und Visualisierungen in R
