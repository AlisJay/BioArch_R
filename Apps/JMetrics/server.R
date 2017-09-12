#Juvenille metric server
source("Functions.R",local=TRUE)
shinyServer(function(input,output){
  #Help####
  output$chelp<-renderText({as.character(HelpMetrics(input$cHelp,skip=0,nrows=28))})
  output$c2help<-renderText({as.character(HelpMetrics(input$c2Help,skip=27,nrows=21))})
  output$shelp<-renderText({as.character(HelpMetrics(input$sHelp,skip=48,nrows=10))})
  output$ulhelp<-renderText({as.character(HelpMetrics(input$ulHelp,skip=58,nrows=10))})
  output$phelp<-renderText({as.character(HelpMetrics(input$pHelp,skip=68,nrows=6))})
  output$llhelp<-renderText({as.character(HelpMetrics(input$llHelp,skip=73,nrows=8))})
  output$ohelp<-renderText({as.character(HelpMetrics(input$oHelp,skip=81,nrows=4))})
  
  #Compiling Fields####
  RL<-function(x){
    paste(input[[paste0(x,"_r")]],input[[paste0(x,"_l")]],sep="/")
  }
  
  equip<-reactive({na.omit(c(input$Equipment,input$equipment2))})
  C<-reactive({c(paste(input$C_O_MWB,input$C_O_SLB,input$C_O_MLB,RL("C_O_MLL"),RL("C_O_MWL"),RL("C_T_SH"),RL("C_T_SW"),RL("C_T_SL"),RL("C_T_PPL"),RL("C_T_PPW"),RL("C_T_TRD"),
                       input$C_S_BL,input$C_S_BW,RL("C_S_LWL"),RL("C_S_LWW"),RL("C_S_GWL"),RL("C_S_GWW"),RL("C_Z_L"),RL("C_Z_OH"),RL("C_Pl_OH"),
                       RL("C_Mx_L"),RL("C_Mx_H"),RL("C_Mx_W"),RL("C_Mx_OL"),RL("C_Mn_BL"),RL("C_Mn_W"),RL("C_Mn_OL"),sep=":"),
                 paste(input$C_O_MWB2,input$C_O_SLB2,input$C_O_MLB2,RL("C_O_MLL2"),RL("C_O_MWL2"),RL("C_T_SH2"),RL("C_T_SW2"),RL("C_T_SL2"),RL("C_T_PPL2"),RL("C_T_PPW2"),RL("C_T_TRD2"),
                       input$C_S_BL2,input$C_S_BW2,RL("C_S_LWL2"),RL("C_S_LWW2"),RL("C_S_GWL2"),RL("C_S_GWW2"),RL("C_Z_L2"),RL("C_Z_OH2"),RL("C_Pl_OH2"),
                       RL("C_Mx_L2"),RL("C_Mx_H2"),RL("C_Mx_W2"),RL("C_Mx_OL2"),RL("C_Mn_BL2"),RL("C_Mn_W2"),RL("C_Mn_OL2"),sep=":"),
                 paste(input$C_O_MWB3,input$C_O_SLB3,input$C_O_MLB3,RL("C_O_MLL3"),RL("C_O_MWL3"),RL("C_T_SH3"),RL("C_T_SW3"),RL("C_T_SL3"),RL("C_T_PPL3"),RL("C_T_PPW3"),RL("C_T_TRD3"),
                       input$C_S_BL3,input$C_S_BW3,RL("C_S_LWL3"),RL("C_S_LWW3"),RL("C_S_GWL3"),RL("C_S_GWW3"),RL("C_Z_L3"),RL("C_Z_OH3"),RL("C_Pl_OH3"),
                       RL("C_Mx_L3"),RL("C_Mx_H3"),RL("C_Mx_W3"),RL("C_Mx_OL3"),RL("C_Mn_BL3"),RL("C_Mn_W3"),RL("C_Mn_OL3"),sep=":"))})
  C2<-reactive({c(paste(RL("C2_P_CH"),RL("C2_P_CW"),RL("C2_P_AH"),RL("C2_P_AW"),RL("C2_P_BLC"),RL("C2_P_BLA"),RL("C2_F_CH"),RL("C2_F_CW"),RL("C2_F_AH"),RL("C2_F_AW"),RL("C2_F_NBC"),RL("C2_F_NBA"),RL("C2_N_L"),RL("C2_N_W"),input$C2_N_AH,input$C2_N_SAW,input$C2_N_MAW,input$C2_N_INSL,input$C2_N_B,RL("C2_INC_L"),RL("C2_V_L"),sep=":"),
                  paste(RL("C2_P_CH2"),RL("C2_P_CW2"),RL("C2_P_AH2"),RL("C2_P_AW2"),RL("C2_P_BLC2"),RL("C2_P_BLA2"),RL("C2_F_CH2"),RL("C2_F_CW2"),RL("C2_F_AH2"),RL("C2_F_AW2"),RL("C2_F_NBC2"),RL("C2_F_NBA2"),RL("C2_N_L2"),RL("C2_N_W2"),input$C2_N_AH2,input$C2_N_SAW2,input$C2_N_MAW2,input$C2_N_INSL2,input$C2_N_B2,RL("C2_INC_L2"),RL("C2_V_L2"),sep=":"),
                  paste(RL("C2_P_CH3"),RL("C2_P_CW3"),RL("C2_P_AH3"),RL("C2_P_AW3"),RL("C2_P_BLC3"),RL("C2_P_BLA3"),RL("C2_F_CH3"),RL("C2_F_CW3"),RL("C2_F_AH3"),RL("C2_F_AW3"),RL("C2_F_NBC3"),RL("C2_F_NBA3"),RL("C2_N_L3"),RL("C2_N_W3"),input$C2_N_AH3,input$C2_N_SAW3,input$C2_N_MAW3,input$C2_N_INSL3,input$C2_N_B3,RL("C2_INC_L3"),RL("C2_V_L3"),sep=":"))})
  Sh<-reactive({c(paste(RL("SH_S_GSL"),RL("SH_S_MGD"),RL("SH_S_GML"),RL("SH_S_SL"),RL("SH_S_W"),RL("SH_S_L"),RL("SH_S_ISH"),RL("SH_S_SSH"),RL("SH_S_AW"),RL("SH_C_L"),sep=":"),
                  paste(RL("SH_S_GSL2"),RL("SH_S_MGD2"),RL("SH_S_GML2"),RL("SH_S_SL2"),RL("SH_S_W2"),RL("SH_S_L2"),RL("SH_S_ISH2"),RL("SH_S_SSH2"),RL("SH_S_AW2"),RL("SH_C_L2"),sep=":"),
                  paste(RL("SH_S_GSL3"),RL("SH_S_MGD3"),RL("SH_S_GML3"),RL("SH_S_SL3"),RL("SH_S_W3"),RL("SH_S_L3"),RL("SH_S_ISH3"),RL("SH_S_SSH3"),RL("SH_S_AW3"),RL("SH_C_L3"),sep=":"))})
  UL<-reactive({c(paste(RL("UL_H_DL"),RL("UL_H_DW"),RL("UL_H_TL"),RL("UL_U_DL"),RL("UL_U_TL"),RL("UL_R_DL"),RL("UL_R_TL"),RL("UL_MC1_L"),RL("UL_MC2_L"),RL("UL_MC2_W"),sep=":"),
                  paste(RL("UL_H_DL2"),RL("UL_H_DW2"),RL("UL_H_TL2"),RL("UL_U_DL2"),RL("UL_U_TL2"),RL("UL_R_DL2"),RL("UL_R_TL2"),RL("UL_MC1_L2"),RL("UL_MC2_L2"),RL("UL_MC2_W2"),sep=":"),
                  paste(RL("UL_H_DL3"),RL("UL_H_DW3"),RL("UL_H_TL3"),RL("UL_U_DL3"),RL("UL_U_TL3"),RL("UL_R_DL3"),RL("UL_R_TL3"),RL("UL_MC1_L3"),RL("UL_MC2_L3"),RL("UL_MC2_W3"),sep=":"))})
  P<-reactive({c(paste(RL("P_Il_L"),RL("P_Il_W"),RL("P_Is_L"),RL("P_Is_W"),RL("P_P_L"),sep=":"),
                 paste(RL("P_Il_L2"),RL("P_Il_W2"),RL("P_Is_L2"),RL("P_Is_W2"),RL("P_P_L2"),sep=":"),
                 paste(RL("P_Il_L3"),RL("P_Il_W3"),RL("P_Is_L3"),RL("P_Is_W3"),RL("P_P_L3"),sep=":"))})
  LL<-reactive({c(paste(RL("LL_F_DL"),RL("LL_F_DW"),RL("LL_F_TL"),RL("LL_T_DL"),RL("LL_T_TL"),RL("LL_Fb_DL"),RL("LL_Fb_TL"),RL("LL_MT1_DL"),RL("LL_MT2_DL"),RL("LL_MT3_DL"),RL("LL_MT4_DL"),RL("LL_MT5_DL"),sep=":"),
                  paste(RL("LL_F_DL2"),RL("LL_F_DW2"),RL("LL_F_TL2"),RL("LL_T_DL2"),RL("LL_T_TL2"),RL("LL_Fb_DL2"),RL("LL_Fb_TL2"),RL("LL_MT1_DL2"),RL("LL_MT2_DL2"),RL("LL_MT3_DL2"),RL("LL_MT4_DL2"),RL("LL_MT5_DL2"),sep=":"),
                  paste(RL("LL_F_DL3"),RL("LL_F_DW3"),RL("LL_F_TL3"),RL("LL_T_DL3"),RL("LL_T_TL3"),RL("LL_Fb_DL3"),RL("LL_Fb_TL3"),RL("LL_MT1_DL3"),RL("LL_MT3_DL3"),RL("LL_MT3_DL3"),RL("LL_MT4_DL3"),RL("LL_MT5_DL3"),sep=":"))})
  O<-reactive({c(paste(input$O_At_NAL,input$O_Ax_NAL,RL("O_R1_L"),sep=":"),
                 paste(input$O_At_NAL2,input$O_Ax_NAL2,RL("O_R1_L2"),sep=":"),
                 paste(input$O_At_NAL3,input$O_Ax_NAL3,RL("O_R1_L3"),sep=":"))})
  
  
  #File####
  #create file
  PMessage<-eventReactive(input$Create,{
    CreateJMetric(input$POPID,input$POPName,input$Person1,input$dir,equip())
  })
  output$PMessage<-renderPrint({PMessage()})
  
  #Compatability Check
  ComMessage<-eventReactive(input$Compat,{
    CompCheck(input$POPID,input$POPName,input$Person1,input$dir,equip())
  })
  output$ComMessage<-renderPrint({ComMessage()})
  
  #add individual
  IMessage<-eventReactive(input$Append,{
    AppendJMetric(input$POPID,input$ID,input$Person2,input$dir,C=C(),C2=C2(),Sh=Sh(),UL=UL(),P=P(),LL=LL(),O=O())
  })
  output$IMessage<-renderPrint({IMessage()})
  
})



