#  ==================================================================================
#   * Program Name       	: 소결보일러 Suction fan speed 결정모델 MAIN RSCRIPT
#   * Source File Name   	: SteamYield_PRED_MAIN.R
#   * Author             	: 
#   * Version           	: 1.0.0
#   * Created Date       	: 2018-00-00
#   * Updated Date      	: 2018-00-00
#   * Last modifier      	: 
#   * Updated content    	: 최초작성
#   * Description       	: 실행주기(1분)
#  ==================================================================================

# -----------------------------------------------------------------------------------
# Package 라이브러리 로딩
# -----------------------------------------------------------------------------------
suppressMessages({
  library(h2o)
  library(methods)
  library(evd)
  library(readxl)
  library(xlsx)
  library(rJava)
  library(RJDBC)
})


# -----------------------------------------------------------------------------------
# Working Directory Setting
# -----------------------------------------------------------------------------------
Working_dir <- "D:/work/Rcode/SinterBoiler_SuctionFanRPM_Control/"
setwd(Working_dir)


# -----------------------------------------------------------------------------------
# DB Connection : Posframe DB(PPAS)
# -----------------------------------------------------------------------------------
# DB접속 inro loading
setwd(file.path(Working_dir,"CONN"))
lapply(list.files(), source)

# working directory setting
setwd(Working_dir)


# -----------------------------------------------------------------------------------
# load Model
# -----------------------------------------------------------------------------------
load(paste0(Working_dir, "./SteamYield.RData"))
Modified_Trainset_NOX <- read_excel("./GY_ElectricPowerStation_TrainData.xlsx", sheet = "train")
target.SteamYield <- 40


# -----------------------------------------------------------------------------------
#  PPAS DB로부터 분단위 실적값 조회
# -----------------------------------------------------------------------------------
inputArgs  <- data.frame(df)
START_TIME <- inputArgs$START_TIME

# Argumnets
print(paste(":::::: 소결보일러 스팀생산 최대화 START_TIME ::::::", START_TIME))

# START_TIME Format 
START_TIME <- gsub("[[:punct:]]|[[:space:]]", "", as.character(as.POSIXct(strptime(START_TIME, "%Y%m%d%H%M%S"))))
START_TIME <- ifelse(nchar(START_TIME) == 8, paste0(START_TIME, "000000"), START_TIME)

# SQL
sql_query <- paste0(" SELECT WORKS_CODE, MIDT_CLT_DT, MI_DT_CLT_SERV_TP,",
                    " ERGCG_BR_FG_O2_C_B_CA_NOXZ_QT,ERGCG_BR_SNCR_EMT_WTR_CA_FL, ERGCG_BR_CA_COG_FG_FL, ERG_GENT_CA_HPN_EFF_EPW, ERGCG_BR_ECO_EN_CA_UWA_TMP, ERGCG_BR_LDG_CA_FG_TMP,",
                    " ERGCG_BR_CA_NG_BRI_TMP, ERGCG_BR_CA_GAH_EX_AR_TMP, ERGCG_BR_CA_GAH_EX_GS_TMP, ERGCG_BR_FEED_UWA_CA_FL, ERGCG_BR_SUPC_HTR_CA_SP_FL, ERGCG_BR_CA_ECO_EX_WGAS_ODEN4,",
                    " ERGCG_BR_CA_ECO_EX_WGAS_ODEN5, ERGCG_BR_CA_ECO_EX_WGAS_ODEN6, ERGCG_BR_FG_O2_C_A_CA_NOXZ_QT ",
                    " FROM TB_E12_BOIL_TAG010 ",
                    " WHERE WORKS_CODE ='P'",
                    " AND MI_DT_CLT_SERV_TP = '9' ",
                    " AND MIDT_CLT_DT = TO_DATE('", START_TIME, "', 'YYYYMMDDHH24MISS') ")

SteamYield_Testset_df <- dbGetQuery(db_connect_PPAS, sql_query)

if(nrow(SteamYield_Testset_df) == 0) {
  # 유효한 실적 데이터가 존재하지 않을 경우
  result <- "0"
  
} else {
  
  # Delete Column : WORKS_CODE, MIDT_CLT_DT, MI_DT_CLT_SERV_TP  
  SteamYield_Testset <- SteamYield_Testset_df[,-c(1:3)]
  
  # Change Column Name 
  item <- c("CoolerHood_ExhaustGasPressure","EmergencyDamper_OpenRate","SteamDrum_Pressure","SteamReceiver_AfterPressure","SuctionFan_RPM",
            "Boiler_Inlet_ExhaustGasTemp","Sinter_WasteHeat_Boiler_SteamYield")
  
  for(i in 1:length(item)){
    names(SteamYield_Testset)[i] <- item[i]
  }
  
  
  # -----------------------------------------------------------------------------------
  # 
  # -----------------------------------------------------------------------------------
  # # Calculating average O2
  # Average.O2_Test               <- (subset(NOx.Emission_Testset, select="O2_D") + subset(NOx.Emission_Testset, select="O2_E") + subset(NOx.Emission_Testset, select="O2_F")) / 3
  # Average.O2_Modified_Train_NOX <- (subset(Modified_Trainset_NOX, select="O2_D") + subset(Modified_Trainset_NOX, select="O2_E") + subset(Modified_Trainset_NOX, select="O2_F")) / 3
  # 
  # # Change Column Name
  # colnames(Average.O2_Test)               <- "Average_O2"
  # colnames(Average.O2_Modified_Train_NOX) <- "Average_O2"
  # 
  # # NOx revising
  # NOx.revising_Test           <- (subset(NOx.Emission_Testset, select="FLUE_GAS_NOX") * (21 - 4) / (21 - subset(NOx.Emission_Testset, select="STACK_O2")))
  # NOx.revising_Modified_Train <- (subset(Modified_Trainset_NOX, select="FLUE_GAS_NOX") * (21 - 4) / (21 - subset(Modified_Trainset_NOX, select="STACK_O2")))
  # 
  # # Change Column Name
  # colnames(NOx.revising_Test)           <- "Revised_NOX"
  # colnames(NOx.revising_Modified_Train) <- "Revised_NOX"
  # 
  # # Column Binding
  # NOx.Emission_Testset  <- cbind(NOx.Emission_Testset[,c(1:11,15)], Average.O2_Test, NOx.revising_Test)
  # Modified_Trainset_NOX <- cbind(Modified_Trainset_NOX[,c(1:11,15)], Average.O2_Modified_Train_NOX, NOx.revising_Modified_Train)
  # 
  # # Input data
  # test.data.frame.for.UREA  <- NOx.Emission_Testset
  # train.data.frame.Modified <- Modified_Trainset_NOX
  
  
  # -----------------------------------------------------------------------------------
  # Perform "SteamYield_DNN_Model"
  # -----------------------------------------------------------------------------------
  # Perform "SteamYield_DNN_Model Regression"
  source("./SteamYield_DNN_Model.R")
  
  # Perform "MultiLayer_Extracted_DNN_ClosedFormFormula"
  source("./SteamYield_MultiLayer_Extracted_ANN_ClosedFormFormula.R")
  
  
  # -----------------------------------------------------------------------------------
  # (PPAS) 결과값 저장
  # -----------------------------------------------------------------------------------
  # 결과 데이터셋
  result_df <- data.frame(WORKS_CODE = SteamYield_Testset_df[1],
                          MIDT_CLT_DT = SteamYield_Testset_df[2],
                          MI_DT_CLT_SERV_TP = SteamYield_Testset_df[3],
                          ERGHG_BR_EMT_WTR_CA_FL = as.numeric(round(Optimal_SuctionFan_RPM,digits = 4)),
                          stringsAsFactors = FALSE)
  
  insert_sql <- paste0("INSERT INTO TB_E12_BOIL_TAG020 VALUES('",result_df$works_code, "','",result_df$mi_dt_clt_serv_tp,"',TO_DATE('",result_df$midt_clt_dt,"', 'YYYY/MM/DD HH24:MI:SS'),",result_df$ERGHG_BR_EMT_WTR_CA_FL, ")")
  dbSendUpdate(db_connect_PPAS, insert_sql);
  
  # PPAS DB Connection close
  dbDisconnect(db_connect_PPAS)
  
  
  # -----------------------------------------------------------------------------------
  #  Return Result
  # -----------------------------------------------------------------------------------
  print(paste(":::::: 소결보일러 스팀생산 최대화 Suction fan speed ::::::", result_df$ERGHG_BR_EMT_WTR_CA_FL))
  
  result <- unlist(result_df$ERGHG_BR_EMT_WTR_CA_FL)
}

