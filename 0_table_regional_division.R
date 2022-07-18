# 0_table_regional_division


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
  c("–kŠC“¹",
    "ÂX","ŠâŽè","‹{é","H“c","ŽRŒ`","•Ÿ“‡",
    "ˆïé","“È–Ø","ŒQ”n","é‹Ê","ç—t","“Œ‹ž","_“Þì",
    "VŠƒ","•xŽR","Îì","•Ÿˆä",
    "ŽR—œ","’·–ì",
    "Šò•Œ","Ã‰ª","ˆ¤’m","ŽOd",
    "Ž ‰ê","‹ž“s","‘åã","•ºŒÉ","“Þ—Ç","˜a‰ÌŽR",
    "’¹Žæ","“‡ª","‰ªŽR","L“‡",
    "ŽRŒû",
    "“¿“‡","ì","ˆ¤•Q","‚’m",
    "•Ÿ‰ª","²‰ê","’·è","ŒF–{","‘å•ª",
    "‹{è","Ž­Ž™“‡",
    "‰«“ê")


# table for prefecture, capitals and region
regional_division = 
  tibble(prefcode = 1:47,
         prefname = pref_vec,
         capEng = capEng_vec,
         prefJap = prefJap_vec,
         region = factor(region_vec,levels=unique(region_vec)))

# -------------------------------------------------------------------------
# save
write.csv(regional_division,
          file="data/0_table_regional_division.csv",
          row.names = FALSE)

