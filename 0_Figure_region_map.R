# (1) region_map
# (2) exposure map by temp and WBGT
# setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
source("R_code/1_prefectural_data.R")

regional_division
pref_list
metapredictor_table
pref_vec
region_vec_level

##############################################################################
# 
# nasty pref names
# 
##############################################################################

# prefJap_vec <- 
#   c("北海道",
#     "青森","岩手","宮城","秋田","山形","福島",
#     "茨城","栃木","群馬","埼玉","千葉","東京","神奈川",
#     "新潟","富山","石川","福井",
#     "山梨","長野",
#     "岐阜","静岡","愛知","三重",
#     "滋賀","京都","大阪","兵庫","奈良","和歌山",
#     "鳥取","島根","岡山","広島",
#     "山口",
#     "徳島","香川","愛媛","高知",
#     "福岡","佐賀","長崎","熊本","大分",
#     "宮崎","鹿児島",
#     "沖縄")
# 
# prefJap_tofuken_vec <- 
#   c("北海道",
#     "青森県","岩手県","宮城県","秋田県","山形県","福島県",
#     "茨城県","栃木県","群馬県","埼玉県","千葉県","東京都","神奈川県",
#     "新潟県","富山県","石川県","福井県",
#     "山梨県","長野県",
#     "岐阜県","静岡県","愛知県","三重県",
#     "滋賀県","京都府","大阪府","兵庫県","奈良県","和歌山県",
#     "鳥取県","島根県","岡山県","広島県",
#     "山口県",
#     "徳島県","香川県","愛媛県","高知県",
#     "福岡県","佐賀県","長崎県","熊本県","大分県",
#     "宮崎県","鹿児島県",
#     "沖縄県")
# 
# 
# spot <- 
#   c("札幌",
#     "青森","盛岡","仙台","秋田","山形","福島",
#     "水戸","宇都宮","前橋","さいたま","千葉","東京","横浜",
#     "新潟","富山","金沢","福井",
#     "甲府","長野",
#     "岐阜","静岡","名古屋","津",
#     "大津","京都","大阪","神戸","奈良","和歌山",
#     "鳥取","松江","岡山","広島","山口",
#     "徳島","高松","松山","高知",
#     "福岡","佐賀","長崎","熊本","大分",
#     "宮崎","鹿児島",
#     "那覇")
# 
# summary_table_prefname <- 
#   data.frame(
#     prefname_Jap_short = prefJap_vec,
#     prefname_Jap_long = prefJap_tofuken_vec,
#     prefname = pref_vec,
#     capital_spot = spot
#   )
# 
# write.csv(summary_table_prefname,
#           file='data/summary_table_prefname.csv',
#           row.names = F)

summary_table_prefname <- 
  read.csv("data/summary_table_prefname.csv")


# -------------------------------------------------------------------------
# Japan_map --------------------------------------------------------------
# -------------------------------------------------------------------------

pacman::p_load(sf)


# https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
sf::sf_use_s2(FALSE)

pref_map <-
  read_sf("data/shape_file/japan_ver83/japan_ver83.shp") %>%
  select(-P_NUM, -H_NUM) %>%
  rename(prefJap_tofuken = KEN) %>%
  left_join(data.frame(prefJap = prefJap_vec,
                       prefJap_tofuken = prefJap_tofuken_vec),
            by = "prefJap_tofuken") %>% 
  # 東京のislands除外
  filter(JCODE %ni% c(13361:13364,13381:13382,13401:13402,13421)) %>%
  # 鹿児島のislands除外
  filter(GUN %ni% c("鹿児島郡","熊毛郡","大島郡")) %>%
  filter(SIKUCHOSON %ni% c("西之表市","奄美市","所属未定地")) %>%
  # 沖縄一部除外
  filter(GUN %ni% c("宮古郡","八重山郡")) %>%
  filter(SIKUCHOSON %ni% c("宮古島市","石垣市","所属未定地",
                           # 本島から東
                           "北大東村","南大東村",
                           # 本島から北
                           "久米島町")) %>%
  # 県ごとにまとめ
  group_by(prefJap) %>% 
  summarize(geometry = geometry %>% st_union())

pref_map <- 
  pref_map %>% 
  left_join(regional_division %>% select(prefJap, region),
            by="prefJap") %>%
  mutate(prefJap = prefJap %>% as.factor() %>%
           fct_relevel(prefJap_vec),
         region = region %>% as.factor() %>% 
           fct_relevel(region_vec_level)) %>% 
  arrange(prefJap)

# 沖縄の場所移動のための自作関数
# https://rpubs.com/ktgrstsh/775867
shift_okinawa <- function(data,
                          colname_pref,
                          okinawaname,
                          zoom_rate = 3,
                          pos = c(4, 17)) {
  pref_name_vec <- data %>% pull(colname_pref)
  row_okinawa <- which(pref_name_vec == okinawaname)
  geo <- data[["geometry"]][row_okinawa]
  cent <- sf::st_centroid(geo)
  geo2 <- (geo - cent) * zoom_rate + cent + pos
  data[["geometry"]][row_okinawa] <- geo2
  return(sf::st_as_sf(data))
}

# 沖縄のための区切り線を引くための自作関数
layer_autoline_okinawa <-
  function(i){
    ggplot2::annotate(
      "segment",
      x = c(129, 132.5, 136),
      xend = c(132.5, 136, 136),
      y = c(40, 40, 42),
      yend = c(40, 42, 46),
      lty = "dotted",
      size = .pt / 15,
      color = "black"
    )
  }


###############################################################################
# 
# (1) region_map
# 
###############################################################################

region_map <- function(ftitle){
  pref_map %>%
    shift_okinawa(colname_pref = "prefJap",
                  okinawaname = "沖縄") %>%
    ggplot() +
    layer_autoline_okinawa() +
    geom_sf(aes(fill=region),
            alpha=1,
            # 境界線の太さ、zeroでちょうどいい
            size = 0) +
    
    scale_fill_manual(name='region',
                      values=c("Hokkaido"     = "coral",
                               "Tohoku"       = "orange",
                               "Kanto/Koshin" = "tan",
                               "Hokuriku"     = "mediumseagreen",
                               "Tokai"        = "seagreen",
                               "Kinki"        = "mediumaquamarine",
                               "Chugoku"      = "cornflowerblue",
                               "Shikoku"      = "lightsteelblue",
                               "Kyushu(North)"= "darkviolet",
                               "Kyushu(South)"= "violet",
                               "Okinawa"      = "hotpink")) +
    
    labs(x=NULL,
         y=NULL,
         title = ftitle,
         subtitle = NULL,
         caption = NULL) + 
    
    theme(panel.background=element_rect(fill="transparent"),
          # 全体の枠線
          panel.border = element_rect(colour = "grey", fill=NA, size=1),
          # 目盛線
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          # 目盛ラベル
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          # 凡例タイトル
          legend.title = element_blank(),
          legend.key.size = unit(0.4, 'cm'),
          # フォント
          text=element_text(size=11,family="sans")
    )
  
}

region_map("(A)")



# タイトル（一応設定残します）
# plot.title = element_text(colour="black", 
#                           size=12, 
#                           family="sans")
# キャプション（一応設定残します）
# plot.caption = element_text(hjust = 0, 
#                             colour="black", 
#                             size=9, 
#                             face="italic")



# figure付随の文言たちはfigureのpdfとは別にせんといけんらしい(EHPM)
# title
# Fig. 1 Regional Classification defined by Japan Meteorological Agency.
# caption
# Note that Kyushu_N and Kyushu_S mean North Kyushu and South Kyushu, respectively.


###############################################################################
# 
# (2) exposure map
# 
###############################################################################

pref_exposure_summary_map <- 
  pref_map %>% 
  left_join(
    summary_table_prefname %>% rename(prefJap = prefname_Jap_short),
    by = "prefJap"
  ) %>% 
  left_join(
    metapredictor_table,
    by = "prefname"
  ) %>% 
  select(prefname,temperature = mean_temperature,
         WBGT = mean_WBGT,geometry)


exposure_map <- function(fheat,ftitle){
  
  legend_title <- switch(
    fheat,
    "Temperature" = bquote("Temperature (°C)"),
    "WBGT" = bquote("WBGT (°C)")
  )
  
  pref_exposure_summary_map %>%
    rename(Temperature = temperature) %>% 
    shift_okinawa(colname_pref = "prefname",
                  okinawaname = "Okinawa") %>%
    ggplot() +
    layer_autoline_okinawa() +
    geom_sf(aes_(fill=as.name(fheat)),
            alpha=1,
            size = 0) + 
    scale_fill_gradient(high="#dc143c", low="#ffe4e1") + 
    labs(x=NULL,
         y=NULL,
         title = ftitle,
         fill = legend_title,
         subtitle = NULL,
         caption = NULL) + 
    
    theme(panel.background=element_rect(fill="transparent"),
          # legend.position = c(0.8,0.2),
          # 全体の枠線
          panel.border = element_rect(colour = "grey", fill=NA, size=1),
          # panel.border = element_blank(),
          # 目盛線
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          # 目盛ラベル
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          # フォント
          text=element_text(size=11,family="sans")) %>% 
    return()
}


exposure_map("Temperature","(B)")
exposure_map("WBGT","(C)")



# -------------------------------------------------------------------------
# save
# -------------------------------------------------------------------------

pacman::p_load(patchwork)


above <- 
  plot_spacer() + 
  region_map("(A)") + 
  plot_spacer() + 
  plot_layout(widths = c(1.3,2,0.7))

bottom <- 
  exposure_map("Temperature","(B)") + 
  exposure_map("WBGT","(C)")

# above / bottom



pdf("figure/Fig1_regional_classification.pdf",
    width=10,
    height=6)

above / bottom

dev.off()
