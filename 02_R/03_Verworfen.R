#VErworfendes Skript 
#Korrektur der Legende 
region_labels <- c(
  "AFR" = "Afrika",
  "AMR" = "Amerika",
  "EMR" = "Östlicher Mittelmeerraum",
  "EUR" = "Europa",
  "SEAR" = "Südostasien",
  "WPR" = "Westpazifik"
)

unique(pm25_ts$who_region)

pm25_ts <- pm25_ts %>%
  filter(who_region %in% c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR"))


ls()
nrow(pm25_ts)
count(pm25_ts, who_region)
str(pm25_ts)

ggplot(pm25_ts, aes(x = year, y = pm25_mean)) +
  geom_line()

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
    .groups = "drop"
  ) # nochmal  überarbeiten 