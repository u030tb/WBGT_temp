# prefectural_data

pacman::p_load(
  tidyverse,
  lubridate,
  splines,
  dlnm,
  Epi,
  mixmeta
)
`%ni%` <- Negate("%in%")

# [1] regional_division
# [2] prepare prefecture-level data
# [3] prefecture-level first-stage analysis
# [4] BLUP from nationwide meta-analysis

# -------------------------------------------------------------------------
# month?
# months <- 6:9
# dos_vec <- 1:122

months <- 5:10
dos_vec <- 1:184

###############################################################################
# 
# regional_division
# 
###############################################################################

# prefecture and region ---------------------------------------------------

pref_vec <- 
  c(# Hokkaido(1)
    "Hokkaido",
    # Tohoku(6)
    "Aomori","Iwate","Miyagi","Akita","Yamagata","Fukushima",
    # KantoKoshin(7)
    "Ibaraki","Tochigi","Gunma","Saitama","Chiba","Tokyo","Kanagawa",
    # Hokuriku(4)
    "Niigata","Toyama","Ishikawa","Fukui",
    # KantoKoshin(2)
    "Yamanashi","Nagano",
    # Tokai(4)
    "Gifu", "Shizuoka","Aichi","Mie",
    # Kinki(6)
    "Shiga","Kyoto","Osaka","Hyogo","Nara","Wakayama",
    # Chugoku(4)
    "Tottori","Shimane","Okayama","Hiroshima",
    # Kyushu_N(6)
    "Yamaguchi",
    # Shikoku(4)
    "Tokushima","Kagawa","Ehime","Kochi",
    # Kyushu_N(6)
    "Fukuoka","Saga","Nagasaki","Kumamoto","Oita",
    # Kyushu_S(2)
    "Miyazaki","Kagoshima",
    # Okinawa(1)
    "Okinawa"
  )

# region English name
# https://www.data.jma.go.jp/gmd/cpd/longfcst/en/tourist.html

region_vec <-
  c("Hokkaido",
    rep("Tohoku",6),
    rep("Kanto/Koshin",7),
    rep("Hokuriku",4),
    rep("Kanto/Koshin",2),
    rep("Tokai",4),
    rep("Kinki",6),
    rep("Chugoku",4),
    "Kyushu(North)",
    rep("Shikoku",4),
    rep("Kyushu(North)",5),
    rep("Kyushu(South)",2),
    "Okinawa")


region_vec_level <- 
  c("Hokkaido",
    "Tohoku",
    "Kanto/Koshin",
    "Hokuriku",
    "Tokai",
    "Kinki",
    "Chugoku",
    "Shikoku",
    "Kyushu(North)",
    "Kyushu(South)",
    "Okinawa")

# capital cities
capEng_vec <- 
  c("Sapporo",
    "Aomori","Morioka","Sendai","Akita","Yamagata","Fukushima",
    "Mito","Utsunomiya","Maebashi","Saitama","Chiba","Tokyo","Yokohama",
    "Niigata","Toyama","Kanazawa","Fukui",
    "Kofu","Nagano",
    "Gifu","Shizuoka","Nagoya","Tsu",
    "Otsu","Kyoto","Osaka","Kobe","Nara","Wakayama",
    "Tottori","Matsue","Okayama","Hiroshima",
    "Yamaguchi",
    "Tokushima","Takamatsu","Matsuyama","Kochi",
    "Fukuoka","Saga","Nagasaki","Kumamoto","Oita",
    "Miyazaki","Kagoshima",
    "Naha")

prefJap_vec <- 
  c("北海道",
    "青森","岩手","宮城","秋田","山形","福島",
    "茨城","栃木","群馬","埼玉","千葉","東京","神奈川",
    "新潟","富山","石川","福井",
    "山梨","長野",
    "岐阜","静岡","愛知","三重",
    "滋賀","京都","大阪","兵庫","奈良","和歌山",
    "鳥取","島根","岡山","広島",
    "山口",
    "徳島","香川","愛媛","高知",
    "福岡","佐賀","長崎","熊本","大分",
    "宮崎","鹿児島",
    "沖縄")


# table for prefecture, capitals and region
regional_division = 
  tibble(prefcode = 1:47,
         prefname = pref_vec,
         capEng = capEng_vec,
         prefJap = prefJap_vec,
         region = factor(region_vec,levels=unique(region_vec)))

###############################################################################
# 
# prepare prefecture-level factors
# mean and SD of temperature and WBGT during warm season
# 
###############################################################################


# -------------------------------------------------------------------------
# temperature -------------------------------------------------------------
# -------------------------------------------------------------------------

load("data/temperature_data/all_temp.rda")

pref_temp <- 
  all_temp %>% 
  map_df(~{bind_rows(.x)}) %>% 
  filter(month %in% months) %>% 
  mutate(date = ymd(paste(year,month,day,sep="-"))) %>% 
  select(capEng=place,date,tmax) %>% 
  left_join(regional_division,by="capEng") %>% 
  group_nest(prefcode,prefname,capEng)

pref_temp_metapredictor <- 
  pref_temp %>% 
  mutate(mean_temperature  = map_dbl(data,~{.x$tmax %>% mean}),
         sd_temperature    = map_dbl(data,~{.x$tmax %>% sd})) %>% 
  select(prefname,mean_temperature,sd_temperature)

# -------------------------------------------------------------------------
# WBGT --------------------------------------------------------------------
# -------------------------------------------------------------------------

# prefectural-WBGT --------------------------------------------------------
load("data/WBGT_data/capitals.rda")

pref_WBGT <- 
  capitals %>%
  select(prefJap,date,maxWBGT) %>% 
  # filter(prefJap == levels(capitals$prefJap)[p]) %>% 
  left_join(regional_division,
            by="prefJap") %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% months) %>% 
  select(prefname,date,maxWBGT) %>% 
  group_nest(prefname)

pref_WBGT_metapredictor <- 
  pref_WBGT %>% 
  unnest(data) %>% 
  rename(WBGT = maxWBGT) %>% 
  group_by(prefname) %>% 
  summarize(across(.cols=c(WBGT),
                   .fns=list(mean = mean,
                             sd = sd),
                   .names="{fn}_{col}"))

# -------------------------------------------------------------------------
# mortality ---------------------------------------------------------------
# -------------------------------------------------------------------------

# prefectural-level all-cause mortality -----------------------------------
load("data/mortality_data/JPmort47pref.RData")

death_list <- 
  map_df(1:47,function(p){
    plist[[p]] %>% 
      mutate(prefname = names(plist)[p])
  }) %>% 
  select(prefname,date,all) %>% 
  left_join(regional_division,
            by="prefname") %>% 
  mutate(year = year(date),
         month = month(date),
         dow = wday(date,label=T)) %>% 
  filter(month %in% months) %>% 
  mutate(dos = rep(dos_vec,10*47)) %>% 
  select(prefname,date,all,year,month,dos,dow) %>% 
  group_nest(prefname)



# meta-predictors ---------------------------------------------------------

metapredictor_table <- 
  pref_temp_metapredictor %>% 
  left_join(pref_WBGT_metapredictor,by="prefname") %>% 
  left_join(regional_division %>% select(prefname,region),
            by="prefname")

# temp_WBGT_death ---------------------------------------------------------

# pref_temp
# pref_WBGT
# death_list

pref_list <-
  pref_temp %>% unnest(data) %>% 
  left_join(pref_WBGT %>% unnest(data),
            by=c("prefname","date")) %>% 
  left_join(death_list %>% unnest(data),
            by=c("prefname","date")) %>% 
  group_nest(prefcode,prefname,capEng,region)


pref_list$data[[1]] %>% tail


regional_division
pref_list
metapredictor_table
pref_vec
region_vec_level


# -------------------------------------------------------------------------
# correlation between temperature and WBGT
# -------------------------------------------------------------------------

map_df(1:47,function(fpref){
  fdata = pref_list$data[[fpref]]
  data.frame(
    prefname = pref_list$prefname[fpref],
    corr = cor(fdata$tmax,fdata$maxWBGT)
  ) %>% return()
}) %>% arrange(corr)










