# setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
source("R_code/1_prefectural_data.R")

# 本章の成果物
regional_division
pref_list
metapredictor_table
pref_vec
region_vec_level

###############################################################################
# 
# first-stage analysis(first time)
# to obtain heat-association mortality
# 
###############################################################################

### set parameters for first-stage analysis ---------------------------------
# heat-response relationship
varfun <- "ns"
varper <- c(50, 90)

# lag-response relationship
laglag <- 10
lagnk <- 2

# df of ns for within-summer seasonality
df_ns_dos <- 2


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
      glm(all ~ f_cb_temp + dow + year + 
            ns(dos,knots=quantile(dos_vec,probs=c(1/3,2/3))):factor(year),
          data = f_data, 
          family = quasipoisson)
    
    # crossreduce for first_stage -------------------------------------
    # reference := mean() ~ (過去の人為的活動由来の死者数研究
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
      glm(all ~ f_cb_WBGT + dow + year + 
            ns(dos,knots=quantile(dos_vec,probs=c(1/3,2/3))):factor(year),
          data = f_data, 
          family = quasipoisson)
    
    # crossreduce for first_stage ---------------------------------------------
    # reference := mean() ~ (過去の人為的活動由来の死者数研究
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


# 本章の成果物
first_result_list

###############################################################################
# 
# second-stage 
# identification to MMT and MMW
#
############################################################################### 

metapredictor_table %>% colnames

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
                   random= ~ 1|region/prefname,
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
                   random= ~ 1|region/prefname,
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
    
    # percentileごとに基底を得る
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
    
    # percentileごとに基底を得る
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

# MMT and MMPの確認
nationwide_2stage_BLUP %>% select(contains("MM"))

###############################################################################
# 
# first-stage (re-centering to MMT and MMW) 
#
############################################################################### 

first_recentering_list <- 
  pref_list %>% 
  left_join(nationwide_2stage_BLUP %>% 
              select(prefname,MMT,MMTP,MMW,MMWP),
            by="prefname") %>% 
  
  ### temp ###
  mutate(recentered_crosspred_temp = map2(data,MMT, ~{
    
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
      glm(all ~ f_cb_temp + dow + 
            ns(dos,knots=quantile(dos_vec,probs=c(1/3,2/3))):factor(year) + 
            year,
          data = f_data, 
          family = quasipoisson)
    
    # -------------------------------------------------------------------------
    f_crosspred_temp = 
      crosspred(f_cb_temp,
                f_model_temp,
                cen=.y,
                by=0.1)
    # -------------------------------------------------------------------------
    # AFにはfirst-stage reduced coef&vcovが必要
    f_crossreduce_temp = 
      crossreduce(f_cb_temp, 
                  f_model_temp, 
                  cen = .y,
                  by = 0.1)
    
    # -------------------------------------------------------------------------
    list(crosspred_temp = f_crosspred_temp,
         crossreduce_temp = f_crossreduce_temp) %>% 
      return()
    
  })) %>% 
  
  mutate(crosspred_temp = map(recentered_crosspred_temp,~{.x$crosspred_temp}),
         crossreduce_temp = map(recentered_crosspred_temp,~{.x$crossreduce_temp})) %>% 
  select(-recentered_crosspred_temp) %>%
  
  ### WBGT ###
  mutate(recentered_crosspred_WBGT = map2(data,MMW, ~{
    
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
      glm(all ~ f_cb_WBGT + dow + 
            ns(dos,knots=quantile(dos_vec,probs=c(1/3,2/3))):factor(year) + 
            year,
          data = f_data, 
          family = quasipoisson)
    
    # -------------------------------------------------------------------------
    f_crosspred_WBGT = 
      crosspred(f_cb_WBGT,
                f_model_WBGT,
                cen=.y,
                by=0.1)
    
    # -------------------------------------------------------------------------
    # AFにはfirst-stage reduced coef&vcovが必要
    f_crossreduce_WBGT = 
      crossreduce(f_cb_WBGT, 
                  f_model_WBGT, 
                  cen = .y,
                  by = 0.1)
    
    # -------------------------------------------------------------------------
    list(crosspred_WBGT = f_crosspred_WBGT,
         crossreduce_WBGT = f_crossreduce_WBGT) %>% 
      return()
    
  })) %>% 
  
  mutate(crosspred_WBGT = map(recentered_crosspred_WBGT,~{.x$crosspred_WBGT}),
         crossreduce_WBGT = map(recentered_crosspred_WBGT,~{.x$crossreduce_WBGT})) %>% 
  select(-recentered_crosspred_WBGT)

first_recentering_list
first_recentering_list$crosspred_temp[[4]] %>% plot("overall")

# 本章の成果物
first_recentering_list

# -------------------------------------------------------------------------
# visualization
# -------------------------------------------------------------------------

# # visualization for re-centered first_pref47 ----------------------------
# 
# setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
# pdf("figure/Others_first_recentered_pref47.pdf",width=8,height=4)
# par(mfrow=c(1,2))
# 
# for(p in 1:47){
#   
#   first_recentering_list$crosspred_temp[[p]] %>%
#     plot("overall",
#          main=paste0(first_recentering_list$prefname[p],"\n(temperature)"),
#          ylim=c(0.95,1.5),
#          xlab="daily maximum temperature",
#          ylab="cumulative RR")
#   abline(v=first_recentering_list$MMT[p],lty="dotted")
#   text(x=first_recentering_list$MMT[p],y=1.5,
#        labels=paste0("MMT=",first_recentering_list$MMT[p],
#                      "(",first_recentering_list$MMTP[p],"%)"))
#   
#   first_recentering_list$crosspred_WBGT[[p]] %>%
#     plot("overall",
#          main=paste0(first_recentering_list$prefname[p],"\n(WBGT)"),
#          ylim=c(0.95,1.5),
#          xlab="daily maximum WBGT",
#          ylab="cumulative RR")
#   abline(v=first_recentering_list$MMW[p],lty="dotted")
#   text(x=first_recentering_list$MMW[p],y=1.5,
#        labels=paste0("MMT=",first_recentering_list$MMW[p],
#                      "(",first_recentering_list$MMWP[p],"%)"))
#   
# }
# 
# dev.off()

##############################################################################
# 
# RR from first curve
# 
##############################################################################

# -------------------------------------------------------------------------
# high percentile ---------------------------------------------------------
# -------------------------------------------------------------------------

first_recentering_RRlogRR <- 
  first_recentering_list %>% 
  mutate(RR_temp_90th = map2(data,crosspred_temp,~{
    
    tibble(
      predvar = .y$predvar %>% round(1),
      Estimate = .y$allRRfit,
      L95 = .y$allRRlow,
      U95= .y$allRRhigh
    ) %>% 
      filter(predvar == quantile(.x$tmax,0.9) %>% round(1)) %>% 
      return()
    
  })) %>% 
  mutate(RR_temp_95th = map2(data,crosspred_temp,~{
    
    tibble(
      predvar = .y$predvar %>% round(1),
      Estimate = .y$allRRfit,
      L95 = .y$allRRlow,
      U95= .y$allRRhigh
    ) %>% 
      filter(predvar == quantile(.x$tmax,0.95) %>% round(1)) %>% 
      return()
    
  })) %>% 
  mutate(RR_WBGT_90th = map2(data,crosspred_WBGT,~{
    
    tibble(
      predvar = .y$predvar %>% round(1),
      Estimate = .y$allRRfit,
      L95 = .y$allRRlow,
      U95= .y$allRRhigh
    ) %>% 
      filter(predvar == quantile(.x$maxWBGT,0.9) %>% round(1)) %>% 
      return()
    
  })) %>% 
  mutate(RR_WBGT_95th = map2(data,crosspred_WBGT,~{
    
    tibble(
      predvar = .y$predvar %>% round(1),
      Estimate = .y$allRRfit,
      L95 = .y$allRRlow,
      U95= .y$allRRhigh
    ) %>% 
      filter(predvar == quantile(.x$maxWBGT,0.95) %>% round(1)) %>% 
      return()
    
  })) %>% 
  
  # logRR -------------------------------------------------------------------
  mutate(logRR_temp_90th = map2(data,crosspred_temp,~{
    
    tibble(
      predvar =  .y$predvar %>% round(1),
      logRR =    .y$allfit,
      logRRvar = .y$allse ^ 2
    ) %>% 
      filter(predvar == quantile(.x$tmax,0.9) %>% round(1)) %>% 
      return()
    
  })) %>% 
  mutate(logRR_temp_95th = map2(data,crosspred_temp,~{
    
    tibble(
      predvar =  .y$predvar %>% round(1),
      logRR =    .y$allfit,
      logRRvar = .y$allse ^ 2
    ) %>% 
      filter(predvar == quantile(.x$tmax,0.95) %>% round(1)) %>% 
      return()
    
  })) %>% 
  mutate(logRR_WBGT_90th = map2(data,crosspred_WBGT,~{
    
    tibble(
      predvar =  .y$predvar %>% round(1),
      logRR =    .y$allfit,
      logRRvar = .y$allse ^ 2
    ) %>% 
      filter(predvar == quantile(.x$maxWBGT,0.9) %>% round(1)) %>% 
      return()
    
  })) %>% 
  mutate(logRR_WBGT_95th = map2(data,crosspred_WBGT,~{
    
    tibble(
      predvar =  .y$predvar %>% round(1),
      logRR =    .y$allfit,
      logRRvar = .y$allse ^ 2
    ) %>% 
      filter(predvar == quantile(.x$maxWBGT,0.95) %>% round(1)) %>% 
      return()
    
  }))

first_recentering_RRlogRR

# mixmeta -----------------------------------------------------------------

mixmeta_pre_90th <- 
  mixmeta(
    logRR ~ 1,
    logRRvar,
    data = bind_rows(first_recentering_RRlogRR %>% 
                       select(region, prefname, logRR_temp_90th) %>% 
                       unnest(logRR_temp_90th) %>% 
                       mutate(heat = 0),
                     first_recentering_RRlogRR %>% 
                       select(region, prefname, logRR_WBGT_90th) %>% 
                       unnest(logRR_WBGT_90th) %>% 
                       mutate(heat = 1)),
    random= ~ 1|region/prefname
  )

mixmeta_pre_95th <- 
  mixmeta(
    logRR ~ 1,
    logRRvar,
    data = bind_rows(first_recentering_RRlogRR %>% 
                       select(region, prefname, logRR_temp_95th) %>% 
                       unnest(logRR_temp_95th) %>% 
                       mutate(heat = 0),
                     first_recentering_RRlogRR %>% 
                       select(region, prefname, logRR_WBGT_95th) %>% 
                       unnest(logRR_WBGT_95th) %>% 
                       mutate(heat = 1)),
    random= ~ 1|region/prefname
  )

mixmeta_90th <- 
  mixmeta(
    logRR ~ heat,
    logRRvar,
    data = bind_rows(first_recentering_RRlogRR %>% 
                       select(region, prefname, logRR_temp_90th) %>% 
                       unnest(logRR_temp_90th) %>% 
                       mutate(heat = 0),
                     first_recentering_RRlogRR %>% 
                       select(region, prefname, logRR_WBGT_90th) %>% 
                       unnest(logRR_WBGT_90th) %>% 
                       mutate(heat = 1)),
    random= ~ 1|region/prefname
  )

mixmeta_95th <- 
  mixmeta(
    logRR ~ heat,
    logRRvar,
    data = bind_rows(first_recentering_RRlogRR %>% 
                       select(region, prefname, logRR_temp_95th) %>% 
                       unnest(logRR_temp_95th) %>% 
                       mutate(heat = 0),
                     first_recentering_RRlogRR %>% 
                       select(region, prefname, logRR_WBGT_95th) %>% 
                       unnest(logRR_WBGT_95th) %>% 
                       mutate(heat = 1)),
    random= ~ 1|region/prefname
  )


mixmeta_90th %>% ci.exp()
mixmeta_95th %>% ci.exp()


RR_usingWBGT_first <- 
  bind_rows(
    # -------------------------------------------------------------------------
    # RR_90th
    # -------------------------------------------------------------------------
    tibble(group = "first",
           vars = "per90") %>% 
      mutate(I2 = mixmeta_90th %>% summary() %>% .$i2stat %>% round(2),
             I2_pre = mixmeta_pre_90th %>% summary() %>% .$i2stat %>% round(2)) %>% 
      bind_cols(
        mixmeta_90th %>% 
          ci.exp() %>% 
          as.data.frame() %>% 
          mutate(rowlabel = rownames(.)) %>% 
          filter(rowlabel == "heat") %>% 
          as_tibble() %>% 
          select(Estimate = `exp(Est.)`,
                 L95 = `2.5%`,
                 U95 = `97.5%`)
      ) %>% 
      # percent change
      mutate(across(.cols=all_of(c("Estimate","L95","U95")),
                    .fns=~{.x-1})),
    
    # -------------------------------------------------------------------------
    # RR_95th
    # -------------------------------------------------------------------------
    tibble(group = "first",
           vars = "per95") %>% 
      mutate(I2 = mixmeta_95th %>% summary() %>% .$i2stat %>% round(2),
             I2_pre = mixmeta_pre_95th %>% summary() %>% .$i2stat %>% round(2)) %>% 
      bind_cols(
        mixmeta_95th %>% 
          ci.exp() %>% 
          as.data.frame() %>% 
          mutate(rowlabel = rownames(.)) %>% 
          filter(rowlabel == "heat") %>% 
          as_tibble() %>% 
          select(Estimate = `exp(Est.)`,
                 L95 = `2.5%`,
                 U95 = `97.5%`)
      ) %>% 
      # percent change
      mutate(across(.cols=all_of(c("Estimate","L95","U95")),
                    .fns=~{.x-1}))
  )

RR_usingWBGT_first

# save(RR_usingWBGT_first,
#      file="R_code/usingWBGT_data/RR_usingWBGT_first.R")

##############################################################################
# 
# AF with first curve (準備)
# 
##############################################################################

# 準備！
first_recentering_list_forAF <- 
  first_recentering_list %>% 
  
  ### bvar_temp ###
  mutate(bvar_temp = map(data,~{
    
    # percentileごとに基底を得る
    predvar_temp <- quantile(.x$tmax,0:1000/1000,na.rm=T) %>% round(1)
    argvar_temp <- list(x = predvar_temp, 
                        fun = varfun,
                        knots = quantile(.x$tmax, varper/100, na.rm = T),
                        Bound = range(.x$tmax, na.rm = T))
    bvar_temp <- do.call(onebasis, argvar_temp)
    return(bvar_temp)
    
  })) %>% 
  
  ### bvar_WBGT ###
  mutate(bvar_WBGT = map(data,~{
    
    # percentileごとに基底を得る
    predvar_WBGT <- quantile(.x$maxWBGT,0:1000/1000,na.rm=T) %>% round(1)
    argvar_WBGT <- list(x = predvar_WBGT, 
                        fun = varfun,
                        knots = quantile(.x$maxWBGT, varper/100, na.rm = T),
                        Bound = range(.x$maxWBGT, na.rm = T))
    bvar_WBGT <- do.call(onebasis, argvar_WBGT)
    return(bvar_WBGT)
    
  })) %>% 
  mutate(first_temp_coef = map(crossreduce_temp,~{.x %>% coef}),
         first_temp_vcov = map(crossreduce_temp,~{.x %>% vcov}),
         first_WBGT_coef = map(crossreduce_WBGT,~{.x %>% coef}),
         first_WBGT_vcov = map(crossreduce_WBGT,~{.x %>% vcov}))

first_recentering_list_forAF

##############################################################################
# 
# AF with first curve (90th)
# 
##############################################################################
# -------------------------------------------------------------------------
# parametric bootstrap ----------------------------------------------------
# -------------------------------------------------------------------------
cutoff_per <- 0.9

nsim=1000

first_pref_temp_AF_90th <-
  first_recentering_list_forAF %>% 
  select(data,MMT,bvar_temp,first_temp_coef,first_temp_vcov) %>% 
  pmap(function(data,MMT,bvar_temp,first_temp_coef,first_temp_vcov){
    
    # 変数追加 --------------------------------------------------------------------
    p_data <- 
      data %>% 
      mutate(over_90th = data$tmax >= quantile(data$tmax,cutoff_per))
    
    # basis作成 -----------------------------------------------------------------
    # 過去作成crosspredはパーセントごとのbasisしか持ってない。
    # そこで時系列データに対してのbasisを再度構築する必要がある。
    p_argvar_temp <- 
      list(fun = varfun,
           intercept = FALSE,
           knots = attr(bvar_temp,"knots"),
           Boundary.knots = attr(bvar_temp,"Boundary.knots"))
    
    p_bvar_temp <- 
      do.call(onebasis, c(list(x = p_data$tmax),p_argvar_temp))
    p_cen_temp <- 
      do.call(onebasis, c(list(x = MMT), p_argvar_temp))
    p_bvarcen_temp <- 
      scale(p_bvar_temp, center = p_cen_temp, scale = F)
    
    
    # 点推定 ---------------------------------------------------------------------
    # BLUP結果のぱらめた取得
    p_coef = first_temp_coef
    p_vcov = first_temp_vcov
    
    # daily AN vector(f-ANでもb-ANでもない注意)
    p_an_heatcold <- (1 - exp(-p_bvarcen_temp %*% p_coef)) * p_data$all
    
    # MMT以上のみの AN
    point_AN <- sum(p_an_heatcold[p_data$over_90th])
    death_denominator <- sum(p_data$all[p_data$over_90th])
    point_AF <- point_AN / death_denominator
    
    
    # 区間推定 parametric bootstrap -----------------------------------------------
    
    # 乱数発生
    set.seed(19941004)
    p_coefsim <- MASS::mvrnorm(nsim, p_coef, p_vcov)
    
    random_AN <- 
      map_dbl(1:nsim,
              function(y){
                y_an <- (1 - exp(-p_bvarcen_temp %*% p_coefsim[y, ])) * p_data$all
                y_random_AN <- sum(y_an[p_data$over_90th])
                return(y_random_AN)
              })
    
    # summary table --------------------------------------------------
    final_list <- 
      list(point_AN = point_AN,
           death_denominator = death_denominator,
           point_AF = point_AF,
           random_AN = random_AN)
    
    return(final_list)
    
  })

first_pref_WBGT_AF_90th <-
  first_recentering_list_forAF %>% 
  select(data,MMW,bvar_WBGT,first_WBGT_coef,first_WBGT_vcov) %>% 
  pmap(function(data,MMW,bvar_WBGT,first_WBGT_coef,first_WBGT_vcov){
    
    # 変数追加 --------------------------------------------------------------------
    p_data <- 
      data %>% 
      mutate(over_90th = data$maxWBGT >= quantile(data$maxWBGT,cutoff_per))
    
    # basis作成 -----------------------------------------------------------------
    # 過去作成crosspredはパーセントごとのbasisしか持ってない。
    # そこで時系列データに対してのbasisを再度構築する必要がある。
    p_argvar_WBGT <- 
      list(fun = varfun,
           intercept = FALSE,
           knots = attr(bvar_WBGT,"knots"),
           Boundary.knots = attr(bvar_WBGT,"Boundary.knots"))
    
    p_bvar_WBGT <- 
      do.call(onebasis, c(list(x = p_data$maxWBGT),p_argvar_WBGT))
    p_cen_WBGT <- 
      do.call(onebasis, c(list(x = MMW), p_argvar_WBGT))
    p_bvarcen_WBGT <- 
      scale(p_bvar_WBGT, center = p_cen_WBGT, scale = F)
    
    
    # 点推定 ---------------------------------------------------------------------
    # BLUP結果のぱらめた取得
    p_coef = first_WBGT_coef
    p_vcov = first_WBGT_vcov
    
    # daily AN vector(f-ANでもb-ANでもない注意)
    p_an_heatcold <- (1 - exp(-p_bvarcen_WBGT %*% p_coef)) * p_data$all
    
    # MMT以上のみの AN
    point_AN <- sum(p_an_heatcold[p_data$over_90th])
    death_denominator <- sum(p_data$all[p_data$over_90th])
    point_AF <- point_AN / death_denominator
    
    
    # 区間推定 parametric bootstrap -----------------------------------------------
    
    # 乱数発生
    set.seed(19941004)
    p_coefsim <- MASS::mvrnorm(nsim, p_coef, p_vcov)
    
    random_AN <- 
      map_dbl(1:nsim,
              function(y){
                y_an <- (1 - exp(-p_bvarcen_WBGT %*% p_coefsim[y, ])) * p_data$all
                y_random_AN <- sum(y_an[p_data$over_90th])
                return(y_random_AN)
              })
    
    # summary table --------------------------------------------------
    final_list <- 
      list(point_AN = point_AN,
           death_denominator = death_denominator,
           point_AF = point_AF,
           random_AN = random_AN)
    
    return(final_list)
    
  })

first_pref_temp_AF_90th[[30]]
first_pref_WBGT_AF_90th[[1]]

# -------------------------------------------------------------------------
# prefAnalysis ------------------------------------------------------------
# -------------------------------------------------------------------------
# empirical distributionの集計 -----------------------------------------------
first_table_temp_AF_90th <- 
  map_df(1:length(first_pref_temp_AF_90th),
         function(p){
           
           random_AF <- 
             first_pref_temp_AF_90th[[p]]$random_AN / first_pref_temp_AF_90th[[p]]$death_denominator
           
           quantile(random_AF,probs=c(0.025,0.975)) %>% 
             t() %>% 
             as.data.frame() %>% 
             mutate(point = first_pref_temp_AF_90th[[p]]$point_AF,
                    prefname = pref_vec[p],
                    AF_var = random_AF %>% var) %>% 
             mutate(across(.cols=where(is.numeric),
                           .fns=~{round(.x,4)})) %>% 
             select(prefname,
                    AF_point = point,
                    AF_var,
                    AF_L95=`2.5%`,
                    AF_U95=`97.5%`) %>% 
             return()
         }) %>% 
  left_join(regional_division %>% 
              select(prefcode,prefname,region),
            by="prefname")

first_table_WBGT_AF_90th <- 
  map_df(1:length(first_pref_WBGT_AF_90th),
         function(p){
           
           random_AF <- 
             first_pref_WBGT_AF_90th[[p]]$random_AN / first_pref_WBGT_AF_90th[[p]]$death_denominator
           
           quantile(random_AF,probs=c(0.025,0.975)) %>% 
             t() %>% 
             as.data.frame() %>% 
             mutate(point = first_pref_WBGT_AF_90th[[p]]$point_AF,
                    prefname = pref_vec[p],
                    AF_var = random_AF %>% var) %>% 
             mutate(across(.cols=where(is.numeric),
                           .fns=~{round(.x,4)})) %>% 
             select(prefname,
                    AF_point = point,
                    AF_var,
                    AF_L95=`2.5%`,
                    AF_U95=`97.5%`) %>% 
             return()
         }) %>% 
  left_join(regional_division %>% 
              select(prefcode,prefname,region),
            by="prefname")


first_table_temp_AF_90th
first_table_WBGT_AF_90th

# -------------------------------------------------------------------------
# usingWBGT ---------------------------------------------------------------
# -------------------------------------------------------------------------

first_AF_table_90th <- 
  bind_rows(
    first_table_temp_AF_90th %>% mutate(heat = 0),
    first_table_WBGT_AF_90th %>% mutate(heat = 1)
  )

first_mixmeta_pre_90th <- 
  mixmeta(AF_point ~ 1,
          AF_var,
          random= ~ 1|region/prefname,
          data = first_AF_table_90th,
          method = "reml")
first_mixmeta_90th <- 
  mixmeta(AF_point ~ heat,
          AF_var,
          random= ~ 1|region/prefname,
          data = first_AF_table_90th,
          method = "reml")

AF_usingWBGT_first_90th <- 
  first_mixmeta_90th %>% 
  ci.lin() %>% 
  as.data.frame() %>% 
  mutate(rowlabel = rownames(.)) %>% 
  filter(rowlabel == "heat") %>% 
  select(Estimate,L95=`2.5%`,U95=`97.5%`) %>% 
  mutate(across(.cols=all_of(c("Estimate","L95","U95")),
                .fns=~{.x*100})) %>% 
  mutate(group = "first",
         vars = "90th cutoff",
         I2 = first_mixmeta_90th %>% summary() %>% .$i2stat %>% round(2),
         I2_pre = first_mixmeta_pre_90th %>% summary() %>% .$i2stat %>% round(2))

# 成果物
AF_usingWBGT_first_90th

# save(AF_usingWBGT_first_90th,
#      file="R_code/usingWBGT_data/AF_usingWBGT_first_90th.R")


##############################################################################
# 
# AF with first curve (95th)
# 
##############################################################################
# -------------------------------------------------------------------------
# parametric bootstrap ----------------------------------------------------
# -------------------------------------------------------------------------
cutoff_per <- 0.95

nsim=1000

first_pref_temp_AF_95th <-
  first_recentering_list_forAF %>% 
  select(data,MMT,bvar_temp,first_temp_coef,first_temp_vcov) %>% 
  pmap(function(data,MMT,bvar_temp,first_temp_coef,first_temp_vcov){
    
    # 変数追加 --------------------------------------------------------------------
    p_data <- 
      data %>% 
      mutate(over_95th = data$tmax >= quantile(data$tmax,cutoff_per))
    
    # basis作成 -----------------------------------------------------------------
    # 過去作成crosspredはパーセントごとのbasisしか持ってない。
    # そこで時系列データに対してのbasisを再度構築する必要がある。
    p_argvar_temp <- 
      list(fun = varfun,
           intercept = FALSE,
           knots = attr(bvar_temp,"knots"),
           Boundary.knots = attr(bvar_temp,"Boundary.knots"))
    
    p_bvar_temp <- 
      do.call(onebasis, c(list(x = p_data$tmax),p_argvar_temp))
    p_cen_temp <- 
      do.call(onebasis, c(list(x = MMT), p_argvar_temp))
    p_bvarcen_temp <- 
      scale(p_bvar_temp, center = p_cen_temp, scale = F)
    
    
    # 点推定 ---------------------------------------------------------------------
    # BLUP結果のぱらめた取得
    p_coef = first_temp_coef
    p_vcov = first_temp_vcov
    
    # daily AN vector(f-ANでもb-ANでもない注意)
    p_an_heatcold <- (1 - exp(-p_bvarcen_temp %*% p_coef)) * p_data$all
    
    # MMT以上のみの AN
    point_AN <- sum(p_an_heatcold[p_data$over_95th])
    death_denominator <- sum(p_data$all[p_data$over_95th])
    point_AF <- point_AN / death_denominator
    
    
    # 区間推定 parametric bootstrap -----------------------------------------------
    
    # 乱数発生
    set.seed(19941004)
    p_coefsim <- MASS::mvrnorm(nsim, p_coef, p_vcov)
    
    random_AN <- 
      map_dbl(1:nsim,
              function(y){
                y_an <- (1 - exp(-p_bvarcen_temp %*% p_coefsim[y, ])) * p_data$all
                y_random_AN <- sum(y_an[p_data$over_95th])
                return(y_random_AN)
              })
    
    # summary table --------------------------------------------------
    final_list <- 
      list(point_AN = point_AN,
           death_denominator = death_denominator,
           point_AF = point_AF,
           random_AN = random_AN)
    
    return(final_list)
    
  })

first_pref_WBGT_AF_95th <-
  first_recentering_list_forAF %>% 
  select(data,MMW,bvar_WBGT,first_WBGT_coef,first_WBGT_vcov) %>% 
  pmap(function(data,MMW,bvar_WBGT,first_WBGT_coef,first_WBGT_vcov){
    
    # 変数追加 --------------------------------------------------------------------
    p_data <- 
      data %>% 
      mutate(over_95th = data$maxWBGT >= quantile(data$maxWBGT,cutoff_per))
    
    # basis作成 -----------------------------------------------------------------
    # 過去作成crosspredはパーセントごとのbasisしか持ってない。
    # そこで時系列データに対してのbasisを再度構築する必要がある。
    p_argvar_WBGT <- 
      list(fun = varfun,
           intercept = FALSE,
           knots = attr(bvar_WBGT,"knots"),
           Boundary.knots = attr(bvar_WBGT,"Boundary.knots"))
    
    p_bvar_WBGT <- 
      do.call(onebasis, c(list(x = p_data$maxWBGT),p_argvar_WBGT))
    p_cen_WBGT <- 
      do.call(onebasis, c(list(x = MMW), p_argvar_WBGT))
    p_bvarcen_WBGT <- 
      scale(p_bvar_WBGT, center = p_cen_WBGT, scale = F)
    
    
    # 点推定 ---------------------------------------------------------------------
    # BLUP結果のぱらめた取得
    p_coef = first_WBGT_coef
    p_vcov = first_WBGT_vcov
    
    # daily AN vector(f-ANでもb-ANでもない注意)
    p_an_heatcold <- (1 - exp(-p_bvarcen_WBGT %*% p_coef)) * p_data$all
    
    # MMT以上のみの AN
    point_AN <- sum(p_an_heatcold[p_data$over_95th])
    death_denominator <- sum(p_data$all[p_data$over_95th])
    point_AF <- point_AN / death_denominator
    
    
    # 区間推定 parametric bootstrap -----------------------------------------------
    
    # 乱数発生
    set.seed(19941004)
    p_coefsim <- MASS::mvrnorm(nsim, p_coef, p_vcov)
    
    random_AN <- 
      map_dbl(1:nsim,
              function(y){
                y_an <- (1 - exp(-p_bvarcen_WBGT %*% p_coefsim[y, ])) * p_data$all
                y_random_AN <- sum(y_an[p_data$over_95th])
                return(y_random_AN)
              })
    
    # summary table --------------------------------------------------
    final_list <- 
      list(point_AN = point_AN,
           death_denominator = death_denominator,
           point_AF = point_AF,
           random_AN = random_AN)
    
    return(final_list)
    
  })

first_pref_temp_AF_95th[[30]]
first_pref_WBGT_AF_95th[[1]]

# -------------------------------------------------------------------------
# prefWBGT ----------------------------------------------------------------
# -------------------------------------------------------------------------
# empirical distributionの集計 -----------------------------------------------
first_table_temp_AF_95th <- 
  map_df(1:length(first_pref_temp_AF_95th),
         function(p){
           
           random_AF <- 
             first_pref_temp_AF_95th[[p]]$random_AN / first_pref_temp_AF_95th[[p]]$death_denominator
           
           quantile(random_AF,probs=c(0.025,0.975)) %>% 
             t() %>% 
             as.data.frame() %>% 
             mutate(point = first_pref_temp_AF_95th[[p]]$point_AF,
                    prefname = pref_vec[p],
                    AF_var = random_AF %>% var) %>% 
             mutate(across(.cols=where(is.numeric),
                           .fns=~{round(.x,4)})) %>% 
             select(prefname,
                    AF_point = point,
                    AF_var,
                    AF_L95=`2.5%`,
                    AF_U95=`97.5%`) %>% 
             return()
         }) %>% 
  left_join(regional_division %>% 
              select(prefcode,prefname,region),
            by="prefname")

first_table_WBGT_AF_95th <- 
  map_df(1:length(first_pref_WBGT_AF_95th),
         function(p){
           
           random_AF <- 
             first_pref_WBGT_AF_95th[[p]]$random_AN / first_pref_WBGT_AF_95th[[p]]$death_denominator
           
           quantile(random_AF,probs=c(0.025,0.975)) %>% 
             t() %>% 
             as.data.frame() %>% 
             mutate(point = first_pref_WBGT_AF_95th[[p]]$point_AF,
                    prefname = pref_vec[p],
                    AF_var = random_AF %>% var) %>% 
             mutate(across(.cols=where(is.numeric),
                           .fns=~{round(.x,4)})) %>% 
             select(prefname,
                    AF_point = point,
                    AF_var,
                    AF_L95=`2.5%`,
                    AF_U95=`97.5%`) %>% 
             return()
         }) %>% 
  left_join(regional_division %>% 
              select(prefcode,prefname,region),
            by="prefname")


first_table_temp_AF_95th
first_table_WBGT_AF_95th

# -------------------------------------------------------------------------
# usingWBGT ---------------------------------------------------------------
# -------------------------------------------------------------------------

first_AF_table_95th <- 
  bind_rows(
    first_table_temp_AF_95th %>% mutate(heat = 0),
    first_table_WBGT_AF_95th %>% mutate(heat = 1)
  )

first_mixmeta_pre_95th <- 
  mixmeta(AF_point ~ 1,
          AF_var,
          random= ~ 1|region/prefname,
          data = first_AF_table_95th,
          method = "reml")
first_mixmeta_95th <- 
  mixmeta(AF_point ~ heat,
          AF_var,
          random= ~ 1|region/prefname,
          data = first_AF_table_95th,
          method = "reml")

AF_usingWBGT_first_95th <- 
  first_mixmeta_95th %>% 
  ci.lin() %>% 
  as.data.frame() %>% 
  mutate(rowlabel = rownames(.)) %>% 
  filter(rowlabel == "heat") %>% 
  select(Estimate,L95=`2.5%`,U95=`97.5%`) %>% 
  mutate(across(.cols=all_of(c("Estimate","L95","U95")),
                .fns=~{.x*100})) %>% 
  mutate(group = "first",
         vars = "95th cutoff",
         I2 = first_mixmeta_95th %>% summary() %>% .$i2stat %>% round(2),
         I2_pre = first_mixmeta_pre_95th %>% summary() %>% .$i2stat %>% round(2))

# 成果物
AF_usingWBGT_first_95th

# save(AF_usingWBGT_first_95th,
#      file="R_code/usingWBGT_data/AF_usingWBGT_first_95th.R")

