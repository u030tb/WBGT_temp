# setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
# source("R_code/1_prefectural_data.R")
# source("R_code/2_prefectural_BLUP.R")

regional_division
pref_list
metapredictor_table
pref_vec
region_vec_level
nationwide_2stage_BLUP


###############################################################################
### outline ###
# 1. analysis
## 1-1. pref analysis
## 1-2. region analysis
## 1-3. usingWBGT
###############################################################################

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# analysis ----------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

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

RR_highper$RR_temp_highper[[1]]
##############################################################################
# 
# [3.regional_results]
# 
###############################################################################
nationwide_2stage_BLUP$crosspred_temp_BLUP[[1]]$allfit
# -------------------------------------------------------------------------
# logRR準備 -----------------------------------------------------------------
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

# 成果物
logRR_highper
# 2heat * 4pers * 47pref = 376rows

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
      # national resultにはtwo-level random-effect必須
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
            
            # メタアナしない北海道沖縄はlog化しないallRRfitなどから取得
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
            
            # 北海道沖縄以外のはlog化してメタアナ
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
                    .fns=function(mimi){mimi-1})) %>% 
      as_tibble()
  })) %>% 
  unnest(result_mixmeta) %>% 
  select(-logRR_mixmeta)
  
  
RR_usingWBGT

# save(RR_usingWBGT,
#      file="R_code/usingWBGT_data/RR_usingWBGT.R")


