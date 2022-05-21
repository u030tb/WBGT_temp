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
  c("ñkäCìπ",
    "ê¬êX","ä‚éË","ã{èÈ","èHìc","éRå`","ïüìá",
    "àÔèÈ","ì»ñÿ","åQîn","çÈã ","êÁót","ìåãû","ê_ìﬁêÏ",
    "êVäÉ","ïxéR","êŒêÏ","ïüà‰",
    "éRóú","í∑ñÏ",
    "äÚïå","ê√â™","à§ím","éOèd",
    "é†âÍ","ãûìs","ëÂç„","ï∫å…","ìﬁó«","òaâÃéR",
    "íπéÊ","ìáç™","â™éR","çLìá",
    "éRå˚",
    "ìøìá","çÅêÏ","à§ïQ","çÇím",
    "ïüâ™","ç≤âÍ","í∑çË","åFñ{","ëÂï™",
    "ã{çË","é≠éôìá",
    "â´ìÍ")


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

# äeìsìπï{åßÇ≤Ç∆ÇÃãCâ∑list
pref_temp <- 
  all_temp %>% 
  map_df(~{bind_rows(.x)}) %>% 
  filter(month %in% months) %>% 
  mutate(date = ymd(paste(year,month,day,sep="-"))) %>% 
  select(capEng=place,date,tmax) %>% 
  left_join(regional_division,by="capEng") %>% 
  group_nest(prefcode,prefname,capEng)

# äeìsìπï{åßÇ≤Ç∆ÇÃ(âƒèÍÇÃ)ãCâ∑ÇÃmean/SD
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

# äeìsìπï{åßÇ≤Ç∆ÇÃWBGT
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

# äeìsìπï{åßÇ≤Ç∆ÇÃ(âƒèÍÇÃ)WBGTÇÃïΩãœÅESD
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

# -------------------------------------------------------------------------
# remove 2010
# -------------------------------------------------------------------------

# pref_temp
# pref_WBGT
# death_list

pref_list <-
  pref_temp %>% unnest(data) %>% 
  left_join(pref_WBGT %>% unnest(data),
            by=c("prefname","date")) %>% 
  left_join(death_list %>% unnest(data),
            by=c("prefname","date")) %>% 
  filter(year != 2010) %>% 
  group_nest(prefcode,prefname,capEng,region)

###############################################################################
# 
# first-stage analysis
# 
###############################################################################

### set parameters for first-stage analysis ---------------------------------
# heat-response relationship
varfun <- "ns"
# 70-95Ç∆Ç©ÇÕÇ‚ÇÁÇ»Ç¢ï˚Ç™Ç¢Ç¢Ç©Ç»ÅAç≈çÇtempÇ™â∫ÇËÇ™Çø
varper <- c(50, 90)

# lag-response relationship
laglag <- 10
lagnk <- 2

# df of ns for within-summer seasonality
# -------------------------------------------------------------------------
df_ns_dos <- 2
# -------------------------------------------------------------------------

# statistical model -------------------------------------------------------
# reduced coef & vcov -----------------------------------------------------

first_result_list <- 
  pref_list %>% 
  
  ### temp ###
  mutate(first_prepre_temp = map(data, ~{
    
    # data ------------------------------------------------------------
    f_data = .x
    
    # crossbasis ------------------------------------------------------
    f_cb_temp <- 
      crossbasis(f_data$tmax, 
                 
                 lag=laglag,
                 argvar=list(fun=varfun,
                             knots=quantile(f_data$tmax,varper/100,na.rm=T),
                             Bound = range(f_data$tmax, na.rm = T)),
                 arglag=list(fun=varfun,
                             knots=logknots(laglag, lagnk)),
                 group = f_data$year)
    
    # statmodel -------------------------------------------------------
    f_model_temp <- 
      glm(all ~ f_cb_temp + year + dow + 
            ns(dos,knots=quantile(dos_vec,
                                  probs=seq(df_ns_dos)/(df_ns_dos+1))):factor(year),
          data = f_data, 
          family = quasipoisson)
    
    # crossreduce for first_stage -------------------------------------
    # reference := mean() ~ (âﬂãéÇÃêlà◊ìIäàìÆóRóàÇÃéÄé“êîå§ãÜ
    f_crossreduce_temp <- 
      crossreduce(f_cb_temp, f_model_temp, 
                  cen = mean(f_data$tmax,na.rm=T),
                  by = 0.1)
    
    # first-stage curve -----------------------------------------------
    f_crosspred_temp <- 
      crosspred(f_cb_temp,f_model_temp,
                by=0.1,cen = mean(f_data$tmax,na.rm=T))
    f_MMT <- 
      f_crosspred_temp$allRRfit[which.min(f_crosspred_temp$allRRfit)] %>% names %>% as.numeric
    f_crosspred_temp <- 
      crosspred(f_cb_temp,f_model_temp,
                by=0.1,cen = f_MMT)
    
    # -------------------------------------------------------------------------
    list(crossreduce_temp = f_crossreduce_temp,
         crosspred_temp = f_crosspred_temp) %>% 
      return()
    
  })) %>% 
  
  # coef & vcov of reduced parameters
  mutate(first_coef_temp = map(first_prepre_temp,~{.x$crossreduce_temp %>% coef}),
         first_vcov_temp = map(first_prepre_temp,~{.x$crossreduce_temp %>% vcov}),
         crosspred_temp = map(first_prepre_temp,~{.x$crosspred_temp})) %>% 
  select(-first_prepre_temp) %>% 
  
  
  ### WBGT ###
  mutate(first_prepre_WBGT = map(data, ~{
    
    
    # data ------------------------------------------------------------------
    f_data = .x
    
    # crossbasis ------------------------------------------------------------
    f_cb_WBGT <- 
      crossbasis(f_data$maxWBGT, 
                 lag=laglag,
                 argvar=list(fun=varfun,
                             knots=quantile(f_data$maxWBGT,varper/100,na.rm=T),
                             Bound = range(f_data$maxWBGT, na.rm = T)),
                 arglag=list(fun=varfun,
                             knots=logknots(laglag, lagnk)),
                 group = f_data$year)
    
    # statmodel -------------------------------------------------------------
    f_model_WBGT <- 
      glm(all ~ f_cb_WBGT + year + dow + 
            ns(dos,knots=quantile(dos_vec,seq(df_ns_dos)/(df_ns_dos+1))):factor(year),
          data = f_data, 
          family = quasipoisson)
    
    # crossreduce for first_stage ---------------------------------------------
    # reference := mean() ~ (âﬂãéÇÃêlà◊ìIäàìÆóRóàÇÃéÄé“êîå§ãÜ
    f_crossreduce_WBGT <- 
      crossreduce(f_cb_WBGT, f_model_WBGT, 
                  cen = mean(f_data$maxWBGT,na.rm=T),
                  by = 0.1)
    
    # first-stage curve -------------------------------------------------------
    f_crosspred_WBGT <- 
      crosspred(f_cb_WBGT,f_model_WBGT,
                by=0.1,cen = mean(f_data$maxWBGT,na.rm=T))
    f_MMW <- 
      f_crosspred_WBGT$allRRfit[which.min(f_crosspred_WBGT$allRRfit)] %>% names %>% as.numeric
    f_crosspred_WBGT <- 
      crosspred(f_cb_WBGT,f_model_WBGT,
                by=0.1,cen = f_MMW)
    
    # -------------------------------------------------------------------------
    list(crossreduce_WBGT = f_crossreduce_WBGT,
         crosspred_WBGT = f_crosspred_WBGT) %>% 
      return()
    
  })) %>% 
  
  # coef & vcov of reduced parameters
  mutate(first_coef_WBGT = map(first_prepre_WBGT,~{.x$crossreduce_WBGT %>% coef}),
         first_vcov_WBGT = map(first_prepre_WBGT,~{.x$crossreduce_WBGT %>% vcov}),
         crosspred_WBGT = map(first_prepre_WBGT,~{.x$crosspred_WBGT})) %>% 
  select(-first_prepre_WBGT)


###############################################################################
# 
# BLUP from nationwide meta-analysis 
#
############################################################################### 

# -------------------------------------------------------------------------
# meta-regression ---------------------------------------------------------
# -------------------------------------------------------------------------
nationwide_2stage_BLUP <- 
  first_result_list %>% 
  select(prefcode,prefname,data,region) %>% 
  
  mutate(high_pers = map(data,
                         ~{data.frame(
                           high_pers = paste0("per",seq(90,97.5,2.5)),
                           tmax_pers = .x$tmax %>% quantile(probs=seq(90,97.5,2.5)/100) %>% round(1),
                           maxWBGT_pers = .x$maxWBGT %>% quantile(probs=seq(90,97.5,2.5)/100) %>% round(1)
                         )})) %>% 
  
  ### temp
  mutate(nationwide_temp_BLUP = 
           mixmeta(as.matrix(bind_rows(first_result_list$first_coef_temp)) ~ 
                     mean_temperature + 
                     sd_temperature,
                   first_result_list$first_vcov_temp,
                   random = ~1|region/prefname,
                   data = metapredictor_table,
                   control= list(showiter = T, igls.inititer = 10),
                   method = "reml") %>% 
           blup(vcov=T)) %>% 
  
  ### WBGT
  mutate(nationwide_WBGT_BLUP = 
           mixmeta(as.matrix(bind_rows(first_result_list$first_coef_WBGT)) ~ 
                     mean_WBGT +
                     sd_WBGT,
                   first_result_list$first_vcov_WBGT,
                   random = ~1|region/prefname,
                   data = metapredictor_table,
                   control = list(showiter = T, igls.inititer = 10),
                   method = "reml") %>% 
           blup(vcov=T))


# -------------------------------------------------------------------------
# bvar & MMX & MMXP -------------------------------------------------------
# -------------------------------------------------------------------------

nationwide_2stage_BLUP <- 
  
  nationwide_2stage_BLUP %>% 
  
  ### bvar_temp ###
  mutate(bvar_temp = map(data,~{
    
    # percentileÇ≤Ç∆Ç…äÓíÍÇìæÇÈ
    predvar_temp <- quantile(.x$tmax,0:1000/1000,na.rm=T) %>% round(1)
    argvar_temp <- list(x = predvar_temp, 
                        fun = varfun,
                        knots = quantile(.x$tmax, varper/100, na.rm = T),
                        Bound = range(.x$tmax, na.rm = T))
    bvar_temp <- do.call(onebasis, argvar_temp)
    return(bvar_temp)
    
  })) %>% 
  
  ### MMT & MMTP ###
  mutate(MMTP = map2_dbl(bvar_temp,nationwide_temp_BLUP,
                         ~{seq(1,99,0.1)[which.min((.x %*% .y$blup)[which(seq(0,100,0.1) == 1):which(seq(0,100,0.1) == 99)])]})) %>% 
  mutate(MMT = map2_dbl(data,MMTP,
                        ~{quantile(.x$tmax, .y/100, na.rm = T)})) %>% 
  
  ### bvar_WBGT ###
  mutate(bvar_WBGT = map(data,~{
    
    # percentileÇ≤Ç∆Ç…äÓíÍÇìæÇÈ
    predvar_WBGT <- quantile(.x$maxWBGT,0:1000/1000,na.rm=T) %>% round(1)
    argvar_WBGT <- list(x = predvar_WBGT, 
                        fun = varfun,
                        knots = quantile(.x$maxWBGT, varper/100, na.rm = T),
                        Bound = range(.x$maxWBGT, na.rm = T))
    bvar_WBGT <- do.call(onebasis, argvar_WBGT)
    return(bvar_WBGT)
    
  })) %>% 
  
  ### MMW & MMWP ###
  mutate(MMWP = map2_dbl(bvar_WBGT,nationwide_WBGT_BLUP,
                         ~{seq(1,99,0.1)[which.min((.x %*% .y$blup)[which(seq(0,100,0.1) == 1):which(seq(0,100,0.1) == 99)])]})) %>% 
  mutate(MMW = map2_dbl(data,MMWP,
                        ~{quantile(.x$maxWBGT, .y/100, na.rm = T)}))


# -------------------------------------------------------------------------
# crosspredçÏê¨ -------------------------------------------------------------
# -------------------------------------------------------------------------
# crosspredÇçÏê¨Ç∑ÇÈÇ…ÇÕ4Ç¬ïœêîÇ™ïKóvÇ»ÇÃÇ≈ï ìrçÏê¨
nationwide_2stage_BLUP <- 
  nationwide_2stage_BLUP %>% 
  ### temp ###
  mutate(crosspred_temp_BLUP = 
           nationwide_2stage_BLUP %>% 
           select(bvar_temp,nationwide_temp_BLUP,MMT) %>% 
           pmap(function(bvar_temp,nationwide_temp_BLUP,MMT){
             
             crosspred(bvar_temp,
                       coef=nationwide_temp_BLUP$blup,
                       vcov=nationwide_temp_BLUP$vcov,
                       model.link="log",
                       by=0.1,
                       cen=MMT) %>% 
               return()
             
           })) %>% 
  
  ### WBGT ###
  mutate(crosspred_WBGT_BLUP = 
           nationwide_2stage_BLUP %>% 
           select(bvar_WBGT,nationwide_WBGT_BLUP,MMW) %>% 
           pmap(function(bvar_WBGT,nationwide_WBGT_BLUP,MMW){
             
             crosspred(bvar_WBGT,
                       coef=nationwide_WBGT_BLUP$blup,
                       vcov=nationwide_WBGT_BLUP$vcov,
                       model.link="log",
                       by=0.1,
                       cen=MMW) %>% 
               return()
             
           })) %>% 
  
  mutate(region = region %>% 
           as.factor %>% fct_relevel(region_vec_level),
         prefname = prefname %>% 
           as.factor %>% fct_relevel(pref_vec))

##############################################################################
# 
# 1-1. pref analysis
# 
##############################################################################

RR_highper <- 
  
  nationwide_2stage_BLUP %>% 
  select(prefcode,prefname,region,high_pers,contains("crosspred")) %>% 
  mutate(RR_temp_highper = 
           map2(high_pers,crosspred_temp_BLUP,
                ~{data.frame(predvar = round(.y$predvar,1),
                             allRRlow = .y$allRRlow,
                             allRRfit = .y$allRRfit,
                             allRRhigh = .y$allRRhigh) %>% 
                    filter(predvar %in% round(.x$tmax_pers,1)) %>% 
                    mutate(high_per = paste0("per",seq(90,97.5,2.5))) %>% 
                    as_tibble()})) %>% 
  mutate(RR_WBGT_highper = 
           map2(high_pers,crosspred_WBGT_BLUP,
                ~{data.frame(predvar = round(.y$predvar,1),
                             allRRlow = .y$allRRlow,
                             allRRfit = .y$allRRfit,
                             allRRhigh = .y$allRRhigh) %>% 
                    filter(predvar %in% round(.x$maxWBGT_pers,1)) %>% 
                    mutate(high_per = paste0("per",seq(90,97.5,2.5))) %>% 
                    as_tibble()}))

##############################################################################
# 
# [3.regional_results]
# 
###############################################################################
# -------------------------------------------------------------------------
# logRRèÄîı -----------------------------------------------------------------
# -------------------------------------------------------------------------

logRR_highper <- 
  
  nationwide_2stage_BLUP %>% 
  select(prefcode,prefname,region,high_pers,contains("crosspred")) %>% 
  mutate(logRR_temp_highper = 
           map2(high_pers,crosspred_temp_BLUP,
                ~{data.frame(predvar = round(.y$predvar,1),
                             logRR = .y$allfit,
                             logRRvar = .y$allse ^ 2) %>% 
                    filter(predvar %in% round(.x$tmax_pers,1)) %>% 
                    mutate(high_per = paste0("per",seq(90,97.5,2.5))) %>% 
                    as_tibble()})) %>% 
  mutate(logRR_WBGT_highper = 
           map2(high_pers,crosspred_WBGT_BLUP,
                ~{data.frame(predvar = round(.y$predvar,1),
                             logRR = .y$allfit,
                             logRRvar = .y$allse ^ 2) %>% 
                    filter(predvar %in% round(.x$maxWBGT_pers,1)) %>% 
                    mutate(high_per = paste0("per",seq(90,97.5,2.5))) %>% 
                    as_tibble()}))

logRR_highper <- 
  logRR_highper %>% 
  select(prefcode,prefname,region,logRR_temp_highper) %>% 
  unnest(logRR_temp_highper) %>% 
  mutate(heat = "temperature") %>% 
  bind_rows(logRR_highper %>% 
              select(prefcode,prefname,region,logRR_WBGT_highper) %>% 
              unnest(logRR_WBGT_highper) %>% 
              mutate(heat = "WBGT")) %>% 
  mutate(prefname = prefname %>% as.factor %>% 
           fct_relevel(rev(pref_vec))) %>% 
  # WBGT ~ 1 / temp ~ 0
  mutate(heat = heat %>% 
           plyr::mapvalues(c("temperature","WBGT"),
                           c(0,1)) %>% 
           as.numeric)


# -------------------------------------------------------------------------
# pooling -----------------------------------------------------------------
# -------------------------------------------------------------------------

pooled_RRtot <- 
  map_df(0:1,function(fheat){
    map(1:4,function(fper){
      
      # percentile --------------------------------------------------------------
      f_logRR_highper <- 
        logRR_highper %>% 
        filter(high_per == paste0("per",seq(90,97.5,2.5))[fper],
               heat == fheat)
      
      # nationwide --------------------------------------------------------------
      # national resultÇ…ÇÕtwo-level random-effectïKê{
      mixmeta_nationwide <- 
        mixmeta(logRR,
                logRRvar,
                random= ~ 1|region/prefname,
                data = f_logRR_highper,
                method = "reml")
      
      nationwide_result <- 
        mixmeta_nationwide %>% 
        ci.exp() %>% 
        as_tibble() %>% 
        select(Estimate=`exp(Est.)`,L95=`2.5%`,U95=`97.5%`) %>% 
        mutate(region = "Nationwide",
               I2 = mixmeta_nationwide %>% summary() %>% .$i2stat %>% round(1))
      
      
      # region ------------------------------------------------------------------
      region_result <- 
        map_df(1:length(region_vec_level),~{
          
          if (.x %in% c(1,11)) {
            
            # ÉÅÉ^ÉAÉiÇµÇ»Ç¢ñkäCìπâ´ìÍÇÕlogâªÇµÇ»Ç¢allRRfitÇ»Ç«Ç©ÇÁéÊìæ
            bind_rows(
              RR_highper %>% 
                select(prefname,region,RR_temp_highper) %>% 
                unnest(RR_temp_highper) %>% 
                mutate(heat = 0),
              RR_highper %>% 
                select(prefname,region,RR_WBGT_highper) %>% 
                unnest(RR_WBGT_highper) %>% 
                mutate(heat = 1)
            ) %>% 
              filter(heat == fheat,
                     region == region_vec_level[.x],
                     high_per == paste0("per",seq(90,97.5,2.5))[fper]) %>% 
              select(Estimate = allRRfit,
                     L95 = allRRlow,
                     U95 = allRRhigh) %>% 
              mutate(region = region_vec_level[.x],
                     I2 = 0) %>% 
              return()
            
          } else {
            
            # ñkäCìπâ´ìÍà»äOÇÃÇÕlogâªÇµÇƒÉÅÉ^ÉAÉi
            f_mixmeta <- 
              mixmeta(logRR ~ 1,
                      logRRvar,
                      data = f_logRR_highper %>% 
                        filter(region == region_vec_level[.x]),
                      method = "reml")
            
            f_mixmeta %>% 
              ci.exp() %>% 
              as_tibble() %>% 
              select(Estimate=`exp(Est.)`,L95=`2.5%`,U95=`97.5%`) %>% 
              mutate(region = region_vec_level[.x],
                     I2 = f_mixmeta %>% summary() %>% .$i2stat %>% round(1)) %>% 
              return()
          }
        })
      
      bind_rows(nationwide_result,
                region_result) %>% 
        mutate(heat = fheat,
               high_per = paste0("per",seq(90,97.5,2.5))[fper],
               group = ifelse(region == "Nationwide","Nationwide","region")) %>% 
        return()
      
    })
  }) %>% 
  mutate(region = region %>% as.factor %>% 
           fct_relevel(rev(c(region_vec_level,"Nationwide"))))


###############################################################################
# 
# [4.metareg_usingWBGT]
# 
###############################################################################

RR_usingWBGT <- 
  tibble(pers = 1:4) %>% 
  
  # mixmeta -----------------------------------------------------------------
mutate(logRR_mixmeta = map(pers,~{
  mixmeta(logRR ~ heat,
          logRRvar,
          data = logRR_highper %>% 
            filter(high_per == paste0("per",seq(90,97.5,2.5))[.x]),
          random= ~ 1|region/prefname,
          method = "reml")
})) %>% 
  
  # I2 ----------------------------------------------------------------------
mutate(I2 = map_dbl(logRR_mixmeta,~{
  .x %>% summary %>% .$i2stat %>% round(2) %>% 
    return()
})) %>% 
  
  # I2_pre ----------------------------------------------------------------------
mutate(I2_pre = map_dbl(pers,~{
  mixmeta(logRR ~ 1,
          logRRvar,
          data = logRR_highper %>% 
            filter(high_per == paste0("per",seq(90,97.5,2.5))[.x]),
          random= ~ 1|region/prefname,
          method = "reml") %>% 
    summary %>% .$i2stat %>% round(2) %>% 
    return()
})) %>% 
  
  # usingWBGT ---------------------------------------------------------------
mutate(result_mixmeta = map2(pers,logRR_mixmeta,~{
  .y %>% 
    ci.exp %>% 
    as.data.frame() %>% 
    mutate(rowlabel = rownames(.)) %>% 
    filter(rowlabel == "heat") %>% 
    select(Estimate=`exp(Est.)`,L95=`2.5%`,U95=`97.5%`) %>% 
    mutate(group = "main",
           vars = paste0("per",seq(90,97.5,2.5))[.x]) %>% 
    # percent change
    mutate(across(.cols=all_of(c("Estimate","L95","U95")),
                  .fns=function(mimi){(mimi-1)*100})) %>% 
    as_tibble()
})) %>% 
  unnest(result_mixmeta) %>% 
  select(-logRR_mixmeta)


RR_usingWBGT


# -------------------------------------------------------------------------

