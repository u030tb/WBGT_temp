# Figure_errorbarplot_scatterplot

source("R_code/1_prefectural_data.R")
source("R_code/2_prefectural_BLUP.R")
source("R_code/3_AF_90th.R")
source("R_code/3_AF_95th.R")
source("R_code/3_RR.R")

# 上から順番に
# (left)pref-RR90 ~ (right)region-RR90
# (left)pref-RR95 ~ (right)region-RR95
# (left)pref-AF90 ~ (right)region-AF90
# (left)pref-AF95 ~ (right)region-AF95

# 以下を作成してこい
# AF
table_temp_AF_90th
table_WBGT_AF_90th
table_temp_AF_95th
table_WBGT_AF_95th
pooled_AF_90th
pooled_AF_95th

# RR
RR_highper
pooled_RRtot



###############################################################################
# 
# 具体的なAF and RRの値のテーブル保存
# (nationwideは表示しなくてもいいかな)
# 
###############################################################################

pref_result <- 
  bind_rows(
    # AF ----------------------------------------------------------
    bind_rows(
      table_temp_AF_90th %>% mutate(heat = "Temperature", per = "per90"),
      table_WBGT_AF_90th %>% mutate(heat = "WBGT", per = "per90"),
      table_temp_AF_95th %>% mutate(heat = "Temperature", per = "per95"),
      table_WBGT_AF_95th %>% mutate(heat = "WBGT", per = "per95")
    ) %>% 
      select(prefname,
             Estimate = AF_point,
             L95 = AF_L95,
             U95 = AF_U95,
             heat,
             per) %>% 
      mutate(measure = "AF") %>% 
      mutate(across(.cols = c("Estimate","L95","U95"),
                    .fns = ~{.x*100})),
    
    # RR ----------------------------------------------------------
    bind_rows(
      # Temperature
      RR_highper %>% 
        select(prefcode,prefname,region,RR_temp_highper) %>% 
        unnest(RR_temp_highper) %>% 
        mutate(heat = "Temperature"),
      # WBGT
      RR_highper %>% 
        select(prefcode,prefname,region,RR_WBGT_highper) %>% 
        unnest(RR_WBGT_highper) %>% 
        mutate(heat = "WBGT")
    ) %>% 
      select(prefname,
             Estimate = allRRfit,
             L95 = allRRlow,
             U95 = allRRhigh,
             heat,
             per = high_per) %>% 
      filter(per %in% c("per90","per95")) %>% 
      mutate(measure = "RR")
  ) %>% 
  arrange(measure,per,heat)

pref_result

# write.csv(pref_result,
#           row.names = F,
#           file = "figure/pref_result.csv")

# -------------------------------------------------------------------------
# region
# -------------------------------------------------------------------------
region_vec_level

region_result <- 
  bind_rows(
    
    # AF -----------------------------------------------------
    bind_rows(
      pooled_AF_90th %>% mutate(measure = "AF",
                                per = "per90"),
      pooled_AF_95th %>% mutate(measure = "AF",
                                per = "per95")
    ) %>% 
      mutate(across(.cols = c("Estimate","L95","U95"),
                    .fns = ~{.x*100})),
    
    # RR -----------------------------------------------------
    pooled_RRtot %>% 
      filter(high_per %in% c("per90","per95")) %>% 
      mutate(heat = ifelse(heat == 1, "WBGT","Temperature"),
             measure = "RR") %>% 
      rename(per = high_per)
    
  ) %>% 
  filter(region != "Nationwide") %>% 
  select(-group) %>% 
  as.data.frame() %>% 
  mutate(heat = ifelse(heat == "temperature","Temperature",heat)) %>% 
  arrange(measure,per,heat,region)


region_result

# write.csv(region_result,
#           row.names = F,
#           file = "figure/region_result.csv")

###############################################################################
# 
# errorbarplot 
# 
###############################################################################

### limitation
RR_pref_range <- 
  c(pref_result %>% filter(measure == "RR") %>% pull(L95) %>% min %>% round(2) - 0.02,
    pref_result %>% filter(measure == "RR") %>% pull(U95) %>% max %>% round(2) + 0.02)
AF_pref_range <- 
  c(pref_result %>% filter(measure == "AF") %>% pull(L95) %>% min %>% round(2) - 0.02,
    pref_result %>% filter(measure == "AF") %>% pull(U95) %>% max %>% round(2) + 0.02)
RR_region_range <- 
  c(region_result %>% filter(measure == "RR") %>% pull(L95) %>% min %>% round(2) - 0.02,
    region_result %>% filter(measure == "RR") %>% pull(U95) %>% max %>% round(2) + 0.02)
AF_region_range <- 
  c(0,
    region_result %>% filter(measure == "AF") %>% pull(U95) %>% max %>% round(2) + 0.02)

pref_result
region_result


# -------------------------------------------------------------------------
# function for visualization
# -------------------------------------------------------------------------

# pref-level
function_pref_errorbarplot <- 
  function(fmeasure,fper,ftitle){
    
    pref_result %>% 
      mutate(prefname = prefname %>% as.factor %>% 
               fct_relevel(rev(pref_vec))) %>% 
      filter(per == fper,
             measure == fmeasure) %>% 
      ggplot(aes(x=prefname,
                 y=Estimate,
                 ymin=L95,
                 ymax=U95,
                 color=heat)) + 
      geom_errorbar(position = position_dodge(width = 0.5)) + 
      geom_point(position = position_dodge(width = 0.5)) + 
      geom_hline(yintercept = ifelse(fmeasure == "AF",0,1), linetype="dotted") + 
      scale_y_continuous(limits = if(fmeasure == "AF"){AF_pref_range}else{RR_pref_range}) + 
      coord_flip() +
      scale_color_brewer(palette = "Set1") + 
      labs(x="",
           y=ifelse(fmeasure == "AF","Attributable Fraction(%)","Relative Risk"),
           title=ftitle) + 
      theme_bw() + 
      theme(legend.title=element_blank(),
            text=element_text(size=12,family="sans"))
    
  }

# region-level
function_region_errorbarplot <- 
  function(fmeasure,fper,ftitle){
    
    region_result %>% 
      mutate(region = region %>% as.factor %>% 
               fct_relevel(rev(region_vec_level))) %>% 
      filter(per == fper,
             measure == fmeasure) %>% 
      ggplot(aes(x=region,
                 y=Estimate,
                 ymin=L95,
                 ymax=U95,
                 color=heat)) + 
      geom_errorbar(position = position_dodge(width = 0.5),
                    width = 0,
                    size = 1.25) + 
      geom_point(position = position_dodge(width = 0.5),
                 size = 2) + 
      geom_hline(yintercept = ifelse(fmeasure == "AF",0,1), linetype="dotted") + 
      scale_y_continuous(limits = if(fmeasure == "AF"){AF_region_range}else{RR_region_range}) + 
      coord_flip() +
      scale_color_brewer(palette = "Set1") + 
      labs(x="",
           y=ifelse(fmeasure == "AF","Attributable Fraction(%)","Relative Risk"),
           title=ftitle) + 
      theme_bw() + 
      theme(legend.title=element_blank(),
            text=element_text(size=12,family="sans"))
    
  }

# in total, 8 results
# 2 measures
# 2 percentiles
# pref or region

# -------------------------------------------------------------------------
# (1) [FigS2]   pref-level RR&AF 90th
# (2) [Fig3 ] region-level RR&AF 90th
# (3) [FigS3]   pref-level RR&AF 95th
# (4) [FigS4] region-level RR&AF 95th
# -------------------------------------------------------------------------

library(ggpubr)
# -------------------------------------------------------------------------
# (1) [FigS2]   pref-level RR&AF 90th
# -------------------------------------------------------------------------
gg_FigS2 <-
  ggarrange(
    function_pref_errorbarplot(fmeasure = "RR",
                               fper = "per90",
                               ftitle = "(A)"),
    function_pref_errorbarplot(fmeasure = "AF",
                               fper = "per90",
                               ftitle = "(B)"),
    nrow = 1,
    ncol = 2,
    common.legend = TRUE,
    legend = "right"
  )

# -------------------------------------------------------------------------
# (2) [Fig3 ] region-level RR&AF 90th
# -------------------------------------------------------------------------
gg_Fig3 <- 
  ggarrange(
    function_region_errorbarplot(fmeasure = "RR",
                                 fper = "per90",
                                 ftitle = "(A)"),
    function_region_errorbarplot(fmeasure = "AF",
                                 fper = "per90",
                                 ftitle = "(B)"),
    nrow = 1,
    ncol = 2,
    common.legend = TRUE,
    legend = "right"
  )

# -------------------------------------------------------------------------
# (3) [FigS3]   pref-level RR&AF 95th
# -------------------------------------------------------------------------
gg_FigS3 <- 
  ggarrange(
    function_pref_errorbarplot(fmeasure = "RR",
                               fper = "per95",
                               ftitle = "(A)"),
    function_pref_errorbarplot(fmeasure = "AF",
                               fper = "per95",
                               ftitle = "(B)"),
    nrow = 1,
    ncol = 2,
    common.legend = TRUE,
    legend = "right"
  )

# -------------------------------------------------------------------------
# (4) [FigS4] region-level RR&AF 95th
# -------------------------------------------------------------------------
gg_FigS4 <- 
  ggarrange(
    function_region_errorbarplot(fmeasure = "RR",
                                 fper = "per95",
                                 ftitle = "(A)"),
    function_region_errorbarplot(fmeasure = "AF",
                                 fper = "per95",
                                 ftitle = "(B)"),
    nrow = 1,
    ncol = 2,
    common.legend = TRUE,
    legend = "right"
  )


# -------------------------------------------------------------------------
# save --------------------------------------------------------------------
# -------------------------------------------------------------------------

gg_FigS2
gg_Fig3
gg_FigS3
gg_FigS4
a

pdf("figure/FigS2_errorbarplot_pref_90th.pdf",
    width=10,height=8)
gg_FigS2
dev.off()
pdf("figure/Fig3_errorbarplot_region_90th.pdf",
    width=10,height=4)
gg_Fig3
dev.off()
pdf("figure/FigS3_errorbarplot_pref_95th.pdf",
    width=10,height=8)
gg_FigS3
dev.off()
pdf("figure/FigS4_errorbarplot_region_95th.pdf",
    width=10,height=4)
gg_FigS4
dev.off()



