# source("R_code/1_prefectural_data.R")
par(family= "HiraKakuProN-W3")

regional_division
pref_list
metapredictor_table
pref_vec
region_vec_level

###############################################################################
# 
# first-stage analysis
# 
###############################################################################

### set parameters for first-stage analysis ---------------------------------
# heat-response relationship
varfun <- "ns"

# knots for exposure-response association
varper <- c(50, 90)

# lag-response association
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
    
    # data
    f_data = .x
    
    # crossbasis
    f_cb_temp <- 
      crossbasis(f_data$tmax, 
                 
                 lag=laglag,
                 argvar=list(fun=varfun,
                             knots=quantile(f_data$tmax,varper/100,na.rm=T),
                             Bound = range(f_data$tmax, na.rm = T)),
                 arglag=list(fun=varfun,
                             knots=logknots(laglag, lagnk)),
                 group = f_data$year)
    
    # statmodel
    f_model_temp <- 
      glm(all ~ f_cb_temp + year + dow + 
            ns(dos,knots=quantile(dos_vec,
                                  probs=seq(df_ns_dos)/(df_ns_dos+1))):factor(year),
          data = f_data, 
          family = quasipoisson)
    
    # crossreduce for first_stage
    # reference := mean() ~ 
    f_crossreduce_temp <- 
      crossreduce(f_cb_temp, f_model_temp, 
                  cen = mean(f_data$tmax,na.rm=T),
                  by = 0.1)
    
    # first-stage curve
    f_crosspred_temp <- 
      crosspred(f_cb_temp,f_model_temp,
                by=0.1,cen = mean(f_data$tmax,na.rm=T))
    f_MMT <- 
      f_crosspred_temp$allRRfit[which.min(f_crosspred_temp$allRRfit)] %>% names %>% as.numeric
    f_crosspred_temp <- 
      crosspred(f_cb_temp,f_model_temp,
                by=0.1,cen = f_MMT)

    list(crossreduce_temp = f_crossreduce_temp,
         crosspred_temp = f_crosspred_temp,
         # qAIC
         qAIC_temp = fqaic(f_model_temp)) %>% 
      return()
    
  })) %>% 
  
  # coef & vcov of reduced parameters
  mutate(first_coef_temp = map(first_prepre_temp,~{.x$crossreduce_temp %>% coef}),
         first_vcov_temp = map(first_prepre_temp,~{.x$crossreduce_temp %>% vcov}),
         crosspred_temp = map(first_prepre_temp,~{.x$crosspred_temp}),
         qAIC_temp = map_dbl(first_prepre_temp,~{.x$qAIC_temp})) %>% 
  select(-first_prepre_temp) %>% 
  
  
  ### WBGT ###
  mutate(first_prepre_WBGT = map(data, ~{
    
    
    # data
    f_data = .x
    
    # crossbasis
    f_cb_WBGT <- 
      crossbasis(f_data$maxWBGT, 
                 lag=laglag,
                 argvar=list(fun=varfun,
                             knots=quantile(f_data$maxWBGT,varper/100,na.rm=T),
                             Bound = range(f_data$maxWBGT, na.rm = T)),
                 arglag=list(fun=varfun,
                             knots=logknots(laglag, lagnk)),
                 group = f_data$year)
    
    # statmodel
    f_model_WBGT <- 
      glm(all ~ f_cb_WBGT + year + dow + 
            ns(dos,knots=quantile(dos_vec,seq(df_ns_dos)/(df_ns_dos+1))):factor(year),
          data = f_data, 
          family = quasipoisson)
    
    # crossreduce for first_stage
    # reference := mean() ~ 
    f_crossreduce_WBGT <- 
      crossreduce(f_cb_WBGT, f_model_WBGT, 
                  cen = mean(f_data$maxWBGT,na.rm=T),
                  by = 0.1)
    
    # first-stage curve
    f_crosspred_WBGT <- 
      crosspred(f_cb_WBGT,f_model_WBGT,
                by=0.1,cen = mean(f_data$maxWBGT,na.rm=T))
    f_MMW <- 
      f_crosspred_WBGT$allRRfit[which.min(f_crosspred_WBGT$allRRfit)] %>% names %>% as.numeric
    f_crosspred_WBGT <- 
      crosspred(f_cb_WBGT,f_model_WBGT,
                by=0.1,cen = f_MMW)
    

    list(crossreduce_WBGT = f_crossreduce_WBGT,
         crosspred_WBGT = f_crosspred_WBGT,
         # qAIC
         qAIC_WBGT = fqaic(f_model_WBGT)) %>% 
      return()
    
  })) %>% 
  
  # coef & vcov of reduced parameters
  mutate(first_coef_WBGT = map(first_prepre_WBGT,~{.x$crossreduce_WBGT %>% coef}),
         first_vcov_WBGT = map(first_prepre_WBGT,~{.x$crossreduce_WBGT %>% vcov}),
         crosspred_WBGT = map(first_prepre_WBGT,~{.x$crosspred_WBGT}),
         qAIC_WBGT = map_dbl(first_prepre_WBGT,~{.x$qAIC_WBGT})) %>% 
  select(-first_prepre_WBGT)



first_result_list


# # visualization for first_pref47 -------------------------------------------
# 
# pdf("figure/Figure_first_pref47.pdf",width=8,height=4)
# par(mfrow=c(1,2))
# 
# for(p in 1:47){
#   first_result_list$crosspred_temp[[p]] %>%
#     plot("overall",
#          main=paste0(first_result_list$prefname[p],"\n(Temperature)"),
#          ylim=c(0.95,1.5),
#          xlab="Daily maximum temperature",
#          ylab="Cumulative Relative Risk")
#   first_result_list$crosspred_WBGT[[p]] %>%
#     plot("overall",
#          main=paste0(first_result_list$prefname[p],"\n(WBGT)"),
#          ylim=c(0.95,1.5),
#          xlab="Daily maximum WBGT",
#          ylab="Cumulative Relative Risk")
# }
# 
# dev.off()



###############################################################################
# 
# BLUP from nationwide meta-analysis 
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
# Wald test ---------------------------------------------------------------
# -------------------------------------------------------------------------

temp_mixmeta <-
  mixmeta(as.matrix(bind_rows(first_result_list$first_coef_temp)) ~
            mean_temperature + 
            sd_temperature,
          first_result_list$first_vcov_temp,
          random = ~1|region/prefname,
          data = metapredictor_table,
          control= list(showiter = T, igls.inititer = 10),
          method = "reml")
WBGT_mixmeta <-
  mixmeta(as.matrix(bind_rows(first_result_list$first_coef_WBGT)) ~
            mean_WBGT +
            sd_WBGT,
          first_result_list$first_vcov_WBGT,
          random = ~1|region/prefname,
          data = metapredictor_table,
          control= list(showiter = T, igls.inititer = 10),
          method = "reml")

fwald <- function(model,var) {
  ind <- grep(var,names(coef(model)))
  coef <- coef(model)[ind]
  vcov <- vcov(model)[ind,ind]
  waldstat <- coef%*%solve(vcov)%*%coef
  df <- length(coef)
  return(1-pchisq(waldstat,df))
}

wald_result <- 
  data.frame(
    heat = c("Temperature","Temperature","WBGT","WBGT"),
    meta = c("mean","range","mean","range"),
    wald = c(fwald(temp_mixmeta,"mean_temperature")  %>% round(4),
             fwald(temp_mixmeta,"sd_temperature") %>% round(4),
             fwald(WBGT_mixmeta,"mean_WBGT")  %>% round(4),
             fwald(WBGT_mixmeta,"sd_WBGT") %>% round(4))
  )

wald_result
# save(wald_result,
#      file="R_code/wald_result.R")


# -------------------------------------------------------------------------
# bvar & MMX & MMXP -------------------------------------------------------
# -------------------------------------------------------------------------

nationwide_2stage_BLUP <- 
  
  nationwide_2stage_BLUP %>% 
  
  ### bvar_temp ###
  mutate(bvar_temp = map(data,~{
    
    # percentile
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
    
    # percentile
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

nationwide_2stage_BLUP %>% tail

# -------------------------------------------------------------------------
# crosspred
# -------------------------------------------------------------------------

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

nationwide_2stage_BLUP


# -------------------------------------------------------------------------
# visualization
# -------------------------------------------------------------------------

# boldbold <- 1.2
# 
# BLUP_temp_curve <-
#   function(p){
#     p_BLUP_temp <-
#       nationwide_2stage_BLUP %>%
#       filter(prefcode == p) %>%
#       select(prefname,MMT,MMTP,crosspred_temp_BLUP) %>%
#       mutate(across(.cols=contains("MM"),
#                     .fns=~{round(.x,1)}))
#     
#     p_BLUP_temp$crosspred_temp_BLUP[[1]] %>%
#       plot(ylim=c(0.95,1.5),
#            cex.axis = boldbold,
#            cex.lab = boldbold,
#            cex.main = boldbold,
#            xlab="Daily maximum temperature (Åé)",
#            ylab="Relative Risk",
#            family="sans")
#     title(glue::glue("({p}) {p_BLUP_temp$prefname} (Temperature)"),
#           font.main = 1,
#           cex.main = boldbold,
#           adj = 0,
#           family="sans")
#     # MMT
#     segments(x0=p_BLUP_temp$MMT, y0=0.1, x1=p_BLUP_temp$MMT, y1=1.48,
#              lty="dashed")
#     text(x=p_BLUP_temp$MMT,y=1.49,
#          family="sans",
#          cex = boldbold - 0.2,
#          labels=paste0("MMT = ",p_BLUP_temp$MMT," (",p_BLUP_temp$MMTP,"%)"))
#     
#   }
# 
# #
# BLUP_WBGT_curve <-
#   function(p){
#     p_BLUP_WBGT <-
#       nationwide_2stage_BLUP %>%
#       filter(prefcode == p) %>%
#       select(prefname,MMW,MMWP,crosspred_WBGT_BLUP) %>%
#       mutate(across(.cols=contains("MM"),
#                     .fns=~{round(.x,1)}))
#     
#     p_BLUP_WBGT$crosspred_WBGT_BLUP[[1]] %>%
#       plot(ylim=c(0.95,1.5),
#            cex.axis = boldbold,
#            cex.lab = boldbold,
#            cex.main = boldbold,
#            xlab="Daily maximum WBGT (Åé)",
#            ylab="Relative Risk",
#            family="sans")
#     title(glue::glue("({p}) {p_BLUP_WBGT$prefname} (WBGT)"),
#           font.main = 1,
#           cex.main = boldbold,
#           adj = 0,
#           family="sans")
#     
#     # MMW
#     segments(x0=p_BLUP_WBGT$MMW, y0=0.1, x1=p_BLUP_WBGT$MMW, y1=1.48,
#              lty="dashed")
#     text(x=p_BLUP_WBGT$MMW,y=1.49,
#          family="sans",
#          cex = boldbold - 0.2,
#          labels=paste0("MMW = ",p_BLUP_WBGT$MMW," (",p_BLUP_WBGT$MMWP,"%)"))
#     
#   }
# #
# 
# p=34
# BLUP_temp_curve(p)
# BLUP_WBGT_curve(p)


# -------------------------------------------------------------------------
# QAIC 
# -------------------------------------------------------------------------

# bind_rows(
#   
#   # pref-level qAICs were aggregated with weight (pref-level sum of total deaths)
#   first_result_list %>% 
#     select(prefname, qAIC_temp, qAIC_WBGT) %>% 
#     mutate(weight_total_all = map_dbl(1:47,function(x){
#       first_result_list$data[[x]]$all %>% sum
#     })) %>% 
#     mutate(weight_sum = sum(weight_total_all)) %>% 
#     mutate(weight_frac = weight_total_all / weight_sum) %>% 
#     mutate(qAIC_temp_weight = qAIC_temp * weight_frac,
#            qAIC_WBGT_weight = qAIC_WBGT * weight_frac) %>% 
#     summarize(qAIC_temp = sum(qAIC_temp_weight),
#               qAIC_WBGT = sum(qAIC_WBGT_weight)) %>% 
#     mutate(weight = "case_weight"),
#   
#   # simple mean
#   first_result_list %>% 
#     select(prefname, qAIC_temp, qAIC_WBGT) %>% 
#     summarize(qAIC_temp = mean(qAIC_temp),
#               qAIC_WBGT = mean(qAIC_WBGT)) %>% 
#     mutate(weight = "simple")
# ) %>% 
#   
#   write.csv("R_code/qAIC/qAIC_main.csv",
#             row.names = F)

# -------------------------------------------------------------------------
# QAIC by pref
# -------------------------------------------------------------------------

# first_result_list %>% 
#   select(prefname, qAIC_temp, qAIC_WBGT) %>% 
#   mutate(low_WBGT = ifelse(qAIC_temp > qAIC_WBGT, 1, NA)) %>% 
#   as.data.frame()

# -------------------------------------------------------------------------
# visualization for BLUP_pref47
# -------------------------------------------------------------------------

# pdf("figure/FigS1_BLUP_heat_mortality.pdf",width=7,height=10)
# 
# par(mfrow=c(4,2),
#     mar = c(5,5,4,1),
#     family="sans")
# for(p in 1:47){
#   BLUP_temp_curve(p)
#   BLUP_WBGT_curve(p)
# }
# dev.off()

# -------------------------------------------------------------------------
# Tokyo
# -------------------------------------------------------------------------

# pdf("figure/Fig2_BLUP_heat_mortality_in_4prefectures.pdf",
#     width=7,height=10)
# par(mfrow=c(4,2),
#     oma = c(0, 0.5, 0, 0),
#     family="sans")
# 
# p=1
# BLUP_temp_curve(p)
# BLUP_WBGT_curve(p)
# p=13
# BLUP_temp_curve(p)
# BLUP_WBGT_curve(p)
# p=27
# BLUP_temp_curve(p)
# BLUP_WBGT_curve(p)
# p=40
# BLUP_temp_curve(p)
# BLUP_WBGT_curve(p)
# 
# dev.off()