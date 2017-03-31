source("Functions.R",local=TRUE)
shinyServer(function(input,output){
  #Help
  output$chelp<-renderText({as.character(HelpMetrics(input$cHelp,skip=0,nrows=30))})
  output$mhelp<-renderText({as.character(HelpMetrics(input$mHelp,skip=31,nrows=10))})
  output$shelp<-renderText({as.character(HelpMetrics(input$sHelp,skip=42,nrows=11))})
  output$phelp<-renderText({as.character(HelpMetrics(input$pHelp,skip=54,nrows=19))})
  output$ahelp<-renderText({as.character(HelpMetrics(input$aHelp,skip=74,nrows=22))})
  output$lhelp<-renderText({as.character(HelpMetrics(input$lHelp,skip=97,nrows=34))})
  output$ohelp<-renderText({as.character(HelpMetrics(input$oHelp,skip=132,nrows=14))})
  
  #collation of data into fields
  Cr<-reactive({paste(input$C_g_op,input$C_eu_eu,input$C_zy_zy,input$C_ba_b,input$C_ba_n,input$C_ba_pr,input$C_ecm_ecm,input$C_pr_alv,input$C_au_au,input$C_n_pr,input$C_ft_ft,input$C_fmt_fmt,input$C_n_ns,input$C_al_al,paste(input$C_d_ec_r,input$C_d_ec_l,sep="/"),paste(input$C_OH_r,input$C_OH_l,sep="/"),input$C_ec_ec,input$C_d_d,
                      input$C_n_b,input$C_b_l,input$C_l_o,input$C_ba_o,paste(input$C_ML_r,input$C_ML_l,sep="/"),input$C_po_b,input$C_mfb,input$C_mob,input$C_acb,input$C_nmfs,input$C_nzos,input$C_nas,sep=":")})
  
  M<-reactive({paste(input$M_id_gn,input$M_mbh,input$M_mbb,input$M_go_go,input$M_cdl_cdl,paste(input$M_minrb_r,input$M_minrb_l,sep="/"),paste(input$M_maxrb_r,input$M_maxrb_l,sep="/"),paste(input$M_maxrh_r,input$M_maxrh_l,sep="/"),input$M_ml,input$M_MA,sep=":")})
  
  Sh<-reactive({paste(paste(input$Sh_cl_r,input$Sh_cl_l,sep="/"),paste(input$Sh_csd_r,input$Sh_csd_l,sep="/"),paste(input$Sh_cvd_r,input$Sh_cvd_l,sep="/"),paste(input$Sh_sh_r,input$Sh_sh_l,sep="/"),paste(input$Sh_sb_r,input$Sh_sb_l,sep="/"),paste(input$Sh_gh_r,input$Sh_gh_l,sep="/"),
                                 paste(input$Sh_gb_r,input$Sh_gb_l,sep="/"),paste(input$Sh_ssl_r,input$Sh_ssl_l,sep="/"),paste(input$Sh_ssw_r,input$Sh_ssw_l,sep="/"),paste(input$Sh_al_r,input$Sh_al_l,sep="/"),paste(input$Sh_cpl_r,input$Sh_cpl_l,sep="/"),sep=":")})
  
  P<-reactive({paste(paste(input$P_och_r,input$P_och_l,sep="/"),paste(input$P_ib_r,input$P_ib_l,sep="/"),paste(input$P_pl_r,input$P_pl_l,sep="/"),paste(input$P_pl2_r,input$P_pl2_l,sep="/"),paste(input$P_pl3_r,input$P_pl3_l,sep="/"),
                                paste(input$P_il_r,input$P_il_l,sep="/"),paste(input$P_il2_r,input$P_il2_l,sep="/"),paste(input$P_il3_r,input$P_il3_l,sep="/"),paste(input$P_psh_r,input$P_psh_l,sep="/"),paste(input$P_midpw_r,input$P_midpw_l,sep="/"),
                                paste(input$P_minpw_r,input$P_minpw_l,sep="/"),paste(input$P_iprw_r,input$P_iprw_l,sep="/"),paste(input$P_pa_r,input$P_pa_l,sep="/"),paste(input$P_snw_r,input$P_snw_l,sep="/"),paste(input$P_snd_r,input$P_snd_l,sep="/"),paste(input$P_ad_r,input$P_ad_l,sep="/"),input$P_sl,input$P_sb,input$P_s1b,sep=":")})
  
  UL<-reactive({paste(paste(input$UL_mhl_r,input$UL_mhl_l,sep="/"),paste(input$UL_heb_r,input$UL_heb_l,sep="/"),paste(input$UL_vhhd_r,input$UL_vhhd_l,sep="/"),paste(input$UL_mxhd_r,input$UL_mxhd_l,sep="/"),paste(input$UL_minhd_r,input$UL_minhd_l,sep="/"),paste(input$UL_mrl_r,input$UL_mrl_l,sep="/"),paste(input$UL_srd_r,input$UL_srd_l,sep="/"),
                                 paste(input$UL_trd_r,input$UL_trd_l,sep="/"),paste(input$UL_rhd_r,input$UL_rhd_l,sep="/"),paste(input$UL_mul_r,input$UL_mul_l,sep="/"),paste(input$UL_pul_r,input$UL_pul_l,sep="/"),paste(input$UL_sud_r,input$UL_sud_l,sep="/"),paste(input$UL_tud_r,input$UL_tud_l,sep="/"),paste(input$UL_muc_r,input$UL_muc_l,sep="/"),sep=":")})
  
  LL<-reactive({paste(paste(input$LL_mfl_r,input$LL_mfl_l,sep="/"),paste(input$LL_bfl_r,input$LL_bfl_l,sep="/"),paste(input$LL_efb_r,input$LL_efb_l,sep="/"),paste(input$LL_mfhd_r,input$LL_mfhd_l,sep="/"),paste(input$LL_sfsd_r,input$LL_sfsd_l,sep="/"),paste(input$LL_tfsd_r,input$LL_tfsd_l,sep="/"),paste(input$LL_sfmd_r,input$LL_sfmd_l,sep="/"),paste(input$LL_tfmd_r,input$LL_tfmd_l,sep="/"),
                                 paste(input$LL_mfc_r,input$LL_mfc_l,sep="/"),paste(input$LL_lfcd_r,input$LL_lfcd_l,sep="/"),paste(input$LL_mfcd_r,input$LL_mfcd_l,sep="/"),paste(input$LL_lfcw_r,input$LL_lfcw_l,sep="/"),paste(input$LL_mfcw_r,input$LL_mfcw_l,sep="/"),paste(input$LL_icw_r,input$LL_icw_l,sep="/"),paste(input$LL_icd_r,input$LL_icd_l,sep="/"),paste(input$LL_stl_r,input$LL_stl_l,sep="/"),
                                 paste(input$LL_mtl_r,input$LL_mtl_l,sep="/"),paste(input$LL_ptl_r,input$LL_ptl_l,sep="/"),paste(input$LL_mptb_r,input$LL_mptb_l,sep="/"),paste(input$LL_mdtb_r,input$LL_mdtb_l,sep="/"),paste(input$LL_std_r,input$LL_std_l,sep="/"),paste(input$LL_ttd_r,input$LL_ttd_l,sep="/"),paste(input$LL_tc_r,input$LL_tc_l,sep="/"),paste(input$LL_mfbl_r,input$LL_mfbl_l,sep="/"),paste(input$LL_mfbd_r,input$LL_mfbd_l,sep="/"),sep=":")})
  
  FLB<-reactive({paste(paste(input$FLB_fhhl_r,input$FLB_fhhl_l,sep="/"),paste(input$FLB_fhsl_r,input$FLB_fhsl_l,sep="/"),paste(input$FLB_fhol_r,input$FLB_fhol_l,sep="/"),paste(input$FLB_fhtl_r,input$FLB_fhtl_l,sep="/"),paste(input$FLB_frhl_r,input$FLB_frhl_l,sep="/"),paste(input$FLB_frpl_r,input$FLB_frpl_l,sep="/"),paste(input$FLB_frsl_r,input$FLB_frsl_l,sep="/"),paste(input$FLB_frdl_r,input$FLB_frdl_l,sep="/"),
                                  paste(input$FLB_ffscl_r,input$FLB_ffscl_l,sep="/"),paste(input$FLB_ffdl_r,input$FLB_ffdl_l,sep="/"),paste(input$FLB_ftpl_r,input$FLB_ftpl_l,sep="/"),paste(input$FLB_fttl_r,input$FLB_fttl_l,sep="/"),paste(input$FLB_ftsl_r,input$FLB_ftsl_l,sep="/"),paste(input$FLB_ftdl_r,input$FLB_ftdl_l,sep="/"),paste(input$FLB_ftml_r,input$FLB_ftml_l,sep="/"),sep=":")})
  
  A<-reactive({paste(paste(input$A_ah_r,input$A_ah_l,sep="/"),input$A_bid,input$A_cv,input$A_tpd,input$A_n_gn,sep=":")})
  
  V<-reactive({paste(input$V_c1,input$V_c2,input$V_c3,input$V_c4,input$V_c5,input$V_c6,input$V_c7,input$V_T1,input$V_T2,input$V_T3,input$V_T4,input$V_T5,input$V_T6,input$V_T7,input$V_T8,input$V_T9,input$V_T10,input$V_T11,input$V_T12,input$V_L1,input$V_L2,input$V_L3,input$V_L4,input$V_L5,sep=":")})
  
  MT<-reactive({paste(paste(input$MT_m1_r,input$MT_m1_l,sep="/"),paste(input$MT_m2_r,input$MT_m2_l,sep="/"),paste(input$MT_m3_r,input$MT_m3_l,sep="/"),paste(input$MT_m4_r,input$MT_m4_l,sep="/"),paste(input$MT_m5_r,input$MT_m5_l,sep="/"),sep=":")})
  
  O<-reactive({paste(paste(input$O_mcl_r,input$O_mcl_l,sep="/"),paste(input$O_cb_r,input$O_cb_l,sep="/"),paste(input$O_mtl_r,input$O_mtl_l,sep="/"),input$O_manl,input$O_msl,input$O_s1w,input$O_s3w,sep=":")})
  
  equip<-reactive({na.omit(c(input$Equipment,input$equipment2))})
  
  #create file
  PMessage<-eventReactive(input$Create,{
    CreateMetric(input$POPID,input$POPName,input$Person1,input$dir,equip())
  })
  output$PMessage<-renderPrint({PMessage()})
  
  #Compatability Check
  ComMessage<-eventReactive(input$Compat,{
    CompCheck(input$POPID,input$POPName,input$Person1,input$dir,equip())
  })
  output$ComMessage<-renderPrint({ComMessage()})
  
  #add individual
  IMessage<-eventReactive(input$Append,{
    AppendMetric(input$POPID,input$ID,input$Person2,input$dir,C=Cr(),M=M(),Sh=Sh(),P=P(),UL=UL(),LL=LL(),FLB=FLB(),A=A(),V=V(),MT=MT(),O=O(),AV=input$Average)
  })
  output$IMessage<-renderPrint({IMessage()})
})