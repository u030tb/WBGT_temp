setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
source("R_code/1_prefectural_data.R")

# 成果物
regional_division
pref_list
metapredictor_table
pref_vec
region_vec_level


# -------------------------------------------------------------------------
# hourly_WBGT -------------------------------------------------------------
# -------------------------------------------------------------------------

# prefectureごとのhourly_data
load("data/WBGT_data/pref_hourly_WBGT.RData")

# from list to table
nationwide_hourly_WBGT <- 
  map(1:nrow(regional_division),function(p){
    pref_hourly_WBGT[[p]] %>% 
      mutate(prefJap = names(pref_hourly_WBGT)[p]) %>% 
      return()
  }) %>% 
  bind_rows() %>% 
  as_tibble() %>% 
  left_join(regional_division %>% select(prefJap,prefname,capEng),
            by="prefJap") %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(month %in% months)
  

# missing_rate 
missing_data <- 
  nationwide_hourly_WBGT %>% 
  group_by(prefname,capEng) %>% 
  summarize(missing_rate = paste0(round(1 - naniar::prop_complete(WBGT),5) * 100,"%")) %>% 
  ungroup() %>% 
  mutate(proxy_spot = "",
         prefname = prefname %>% as.factor %>% 
           fct_relevel(pref_vec)) %>% 
  arrange(prefname)

# missing occured in 4 prefs 
missing_data %>% 
  filter(missing_rate != "0%")


# 気象庁気温の過去データでの近隣の都市から選択するということで。
# May-Novemberならこの8県
# 福島   ~ 福島から梁川(36066)     - Fukushima ~ Yanagawa
# 埼玉   ~ さいたまから越谷(43256) - Saitama ~ Koshigaya
# 神奈川 ~ 横浜から海老名(46091)   - Yokohama ~ Ebina
# 愛知   ~ 名古屋から大府(51216)   - Nagoya ~ Obu
# 三重   ~ 津から四日市(53061)     - Tsu ~ Yokkaichi
# 高知   ~ 高知から後免(74187)     - Kochi ~ Gomen
# 福岡   ~ 福岡から太宰府(82191)   - Fukuoka ~ Dazai
# 佐賀   ~ 佐賀から白石(85166)     - Saga ~ Shiraishi

missing_data[missing_data$prefname == "Fukushima","proxy_spot"] = "Yanagawa"
missing_data[missing_data$prefname == "Saitama",  "proxy_spot"] = "Koshigaya"
missing_data[missing_data$prefname == "Kanagawa", "proxy_spot"] = "Ebina"
missing_data[missing_data$prefname == "Aichi",    "proxy_spot"] = "Obu"
missing_data[missing_data$prefname == "Mie",      "proxy_spot"] = "Yokkaichi"
missing_data[missing_data$prefname == "Kochi",    "proxy_spot"] = "Gomen"
missing_data[missing_data$prefname == "Fukuoka",  "proxy_spot"] = "Dazai"
missing_data[missing_data$prefname == "Saga",     "proxy_spot"] = "Shiraishi"


library(gt)

WBGT_missing_report <- 
  missing_data %>% 
  filter(missing_rate != "0%") %>% 
  rename(prefecture = prefname,
         capitals = capEng) %>% 
  gt() %>% 
  tab_options(
    table.font.names = "Arial"
  ) %>% 
  cols_width(
    everything() ~ px(120)
  ) %>% 
  cols_align(
    align = "center", 
    columns = TRUE
  ) %>% 
  tab_footnote(
    footnote = html("The percentage of missingness in hourly WBGT data"),
    locations = cells_column_labels(
      column = vars("missing_rate")
    )) %>% 
  tab_footnote(
    footnote = html("The names of measurement spots that were used when there were missings in hourly WBGT data at measurement spots in capital cities"),
    locations = cells_column_labels(
      column = vars("proxy_spot")
    )) %>% 
  tab_header(
    title = md("Table S1"),
    subtitle = md("8 prefectures with missingness in hourly WBGT data")
  ) %>% 
  cols_label(
    prefecture = md("Prefectures"),
    capitals = md("Capital cities"),
    missing_rate = md("Missing rate"),
    proxy_spot = md("Proxy spot")
  ) %>% 
  opt_align_table_header(align = "left")

WBGT_missing_report

# save --------------------------------------------------------------------
setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
gtsave(WBGT_missing_report,
       "figure/TableS1_WBGT_missing_report.pdf",
       zoom=0.7)



