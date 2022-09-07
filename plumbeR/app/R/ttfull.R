# TODO use Enrico's growth algorithm ===========================================
# TODO Convert everything to dtplyr ============================================

#TT2 specific analog temperature correction ====================================
# which corrects temperatures according to battery voltage =====================
source("/app/R/ttbandgapTcorrection.R")
# Complex functions for gap filling and correcting timestamp
source("/app/R/ttimecorrection.R")


# Function to convert V3 TT+ DN analog to Temperature===========================
conv_analog_t_v3 = function(DN){
  T = 127.6 - 0.006045 * DN + 1.26E-7 * DN ^ 2 - 1.15E-12 * DN ^ 3
  return(T)
}

# Growth cleaning===============================================================
growth_data_filter = function(data) {

  data = data %>% group_by(TT_ID, doys) %>% mutate(
    range = max(growt_sens, na.rm=T) - min(growt_sens, na.rm=T),
    month = month(Timestamp)
  ) %>% ungroup()
  data = data %>% group_by(TT_ID, month) %>% mutate(
    range2 = abs(mean(growt_sens, na.rm=T) - growt_sens)
  ) %>% ungroup()
  data$growt_sens[data$range > 5] = NA
  data$growt_sens[data$range2 > 5] = NA
  data = data %>% select(-range, -range2)
  return(data)
}

growth_data_clean = function(data){


  if(length(na.exclude(data)) > 4){
    data = forecast::tsclean(data,replace.missing = F,lambda = "auto")
    data = data %>% as.numeric()
  }
  return(data)
}
# Growth from distance calculation function ====================================
calc_growth = function(data){
  data$growth = 0
  if(which(!is.na(data$growt_sens)) %>% length < 20){

    return(data)
  }else{
    data = data %>% mutate(ts = as.numeric(Timestamp))
    tryCatch(loess(growt_sens ~ ts,data ),finally = return(data))


    lsm = loess(growt_sens ~ ts,data )
    dist = predict(lsm, data)
    incr = data$dist[1:(length(data$dist)-1)]-data$dist[2:length(data$dist)]
    incr[incr>0.005]=0
    incr[incr<0]=0
    data$growth = cumsum(c(0,incr))
    return(data)
  }

}


# Function for applying Savitzky-Golay PLS filter ==============================

savgol_filt_ts = function(ts){
  if (length(ts) < 20) {
    return(ts)
  }

  ts_filt = prospectr::savitzkyGolay(ts, 0, 1, 11)
  ts_filt = c(ts[1:10],ts_filt)
  return(ts_filt)
}

#Function to calc soil moisture for TT+ V3  calibration for chernozem===========
calc_soil_moist = function(data){
  if( nrow(data) < 1 ){
    return(data)
  } else {
    data = data %>% mutate(
      StWC = NA,
      MSHzN = MoistSensHz - 20*Tref_1 - min(MoistSensHz, na.rm=T),
      Wsoil = 792*log10(MSHzN)-604*log10(MSHzN)^2+157*log10(MSHzN)^3-13.7*log10(MSHzN)^4,
      pF = 24.5-26.7*log10(MSHzN)+9.59*log10(MSHzN)^2-0.992*log10(MSHzN)^3
    ) %>% select(-MSHzN)
  }
  data$pF[data$pF < 0.5] = NA
  return(data)
}

#Function to calc wood moisture for TT+ V2 and V3 ==============================
calc_wood_moist = function(data){
  if(nrow(data)<1){return(data)}
  species = unique(data$Species)

  if(unique(data$TTver) == 3){
    m = 0
    b = 0
    if (str_detect(species,"Larix") ||
        str_detect(species,"Picea")) {
      #calibration for spruce
      m <- -5E-6
      b <- 0.2
    }
    if (str_detect(species,"Quercus") ||
        str_detect(species,"Acer") ||
        str_detect(species,"Fagus")){
      #calibration for beech
      m <- -4E-5
      b <- 0.6
    }
    if (str_detect(species,"Pinus")){
      #calibration for pine
      m <- -8E-6
      b <- 0.3
    }
    if (str_detect(species,"Betula") ||
        str_detect(species,"Populus") ||
        str_detect(species,"Sorbus") ||
        str_detect(species,"Tilia") ){
      #calibration for poplar
      m <- -0.0001
      b <- 1.76
    }
    data = data %>% mutate(StWC = m*(MoistSensHz-7.3*(Tref_0-Tref_1))+b)
  }

  if(unique(data$TTver) == 2){

    data = data %>%
      mutate(ECf_Tb = -74.15*Tref_1+min(MoistSensHz, na.rm=T)) %>%
      mutate(ECf_Tc = -70.2*Tref_1+min(MoistSensHz)) %>%
      mutate(dECfb = MoistSensHz - ECf_Tb) %>%
      mutate(dECfc = MoistSensHz - ECf_Tc) %>%
      mutate(dECfdb = (dECfb-min(dECfb, na.rm=T))/(max(dECfb,na.rm=T)-
                                                     min(dECfb,na.rm=T))) %>%
      mutate(dECfdc = (dECfc-min(dECfc, na.rm=T))/(max(dECfc,na.rm=T)-
                                                     min(dECfc,na.rm=T))) %>%
      mutate(VWWCb = -0.36*dECfdb + 0.45) %>%
      mutate( VWWCc = case_when(
        dECfdc > 0.95 ~ -1.3452*dECfdc + 1.422,
        dECfdc <= 0.95 ~ -0.0555*dECfdc + 0.195
      ))
    data = data %>% mutate(StWC = VWWCb)

    if(str_detect(species,"Larix") ||
       str_detect(species,"Picea") ||
       str_detect(species,"Pinus")){
      data = data %>% mutate(StWC = VWWCc)
    }
    data = data %>% select(-ECf_Tb,-ECf_Tc,-dECfb,-dECfc,
                           -dECfdb,-dECfdc,-VWWCb,-VWWCc )
  }

  return(data)
}
# Function for sap flux calculation ============================================
calc_sap_flux_density = function(data){
  data$Theat_1[data$Theat_1 > 50] = NA
  data$Tref_1[data$Tref_1 > 50] = NA
  data$Theat_0[data$Theat_0 > 50] = NA
  data$Tref_0[data$Tref_0 > 50] = NA
  data$Theat_1[data$Theat_1<=-20] = NA
  data$Tref_1[data$Tref_1 <=-20] = NA
  data$Theat_0[data$Theat_0<=-20] = NA
  data$Tref_0[data$Tref_0<=-20] = NA
  data = data %>% ungroup() %>% mutate(doys = yday(Timestamp))
  data = data %>% group_by(TT_ID, doys) %>% mutate(
    dTon = Theat_1 - Tref_1,
    dToff = Theat_0 - Tref_0,
    dTmax = max(dTon - dToff,na.rm = T)) %>% ungroup()
  data = data %>% group_by(TT_ID) %>% mutate(
    Fd = 0.00011899*((dTmax-(dTon - dToff))/(dTon - dToff)) ^ 1.231,  #m3m-2s-1
    Fd = case_when(
      Fd > 0.001 ~ NA_real_,
      TRUE ~ Fd)
  ) %>% ungroup() %>% select(-dTon,-dToff,-dTmax)
  data$Fd[data$Fd > 0.001] = NA
  data$Fd =  savgol_filt_ts(data$Fd)
  if(length(data$Fd)>12){
    data$Fd = baytrends::fillMissing(data$Fd,span = 24,
                                     Dates = data$Timestamp,
                                     max.fill = 12)
  }

  data = data %>% group_by(TT_ID, doys) %>% mutate(
    Fdh = Fd*3600*1000, #dm3m-2h-1 = l m-2 h-1
    Flux = Fdh * ((DBH / pi / 100)^1.8777)*0.755
  ) %>% ungroup()

  return(data)
}

# Function which makes table wise computation of parameteres

ttfull_calc = function(TTdat, step_time=NULL){
  mydata_45 = TTdat$mydata_45
  mydata_4D = TTdat$mydata_4D
  mydata_49 = TTdat$mydata_49
  mydata_60 = TTdat$mydata_60
  mydata_61 = TTdat$mydata_61
  mydata_4B = TTdat$mydata_4B
  mydata_4C = TTdat$mydata_4C
  metadata = TTdat$metadata %>% dplyr::filter(CloudName == TTdat$IDs)
  metadata = metadata %>% rename(TT_ID = id)


  if (!is.null(mydata_45)){
    mydata_45p = mydata_45 %>% mutate(
      Rec_Nr = HexToDec(Rec_Nr),
      Tair = Tair / 10,
      Tref_0 = ttnorm_analog_t(data.frame(Tref_0, adc_Vbat))/10,
      Theat_0 = ttnorm_analog_t(data.frame(Theat_0, adc_Vbat))/10,
      Tref_1 = ttnorm_analog_t(data.frame(Tref_1, adc_Vbat))/10,
      Theat_1 = ttnorm_analog_t(data.frame(Theat_1, adc_Vbat))/10,
      VPD = 0.6108*exp((17.27*Tair) /
                         (Tair+273.15)) *
        (1 - RH/100),
      gx_mean = as.integer(gx_mean),
      gy_mean = as.integer(gy_mean),
      gz_mean = as.integer(gz_mean),
      Accel = (gx_mean^2 + gy_mean^2 + gz_mean^2)^0.5,
      Theta = atan(gx_mean / (gy_mean^2 + gz_mean^2)^0.5) / pi * 180 + 90,
      Psi   = atan(gy_mean / (gx_mean^2 + gz_mean^2)^0.5) / pi * 180 + 90,
      Phi   = atan(((gy_mean^2 + gx_mean^2)^0.5)/gz_mean) / pi * 180 + 90,
      Osc_fct = gx_sd + gy_sd + gz_sd,
      growt_sens = growt_sens / adc_Vbat * 40320,
      growt_sens = case_when(
        growt_sens > 85000 ~ NA_real_,
        growt_sens < 30000 ~ NA_real_,
        #f=y0+a*x+b*x^2+c*x^3, a = 0.000000008,b = -0.0016,c = 89.032
        TRUE ~ 0.000000008*growt_sens^2-0.0016*growt_sens+89.032
      ),
      Bat_mV  =  (1100*131072 / adc_Vbat) + 650
    )
    mydata_45p = mydata_45p  %>%
      mutate(id_rec = paste(TT_ID, as.character(as.integer(Rec_Nr)),sep=""))
  } else {mydata_45p = NULL}

  if (!is.null(mydata_4D)){

    mydata_4Dp = mydata_4D %>% mutate(
      Rec_Nr = HexToDec(Rec_Nr),
      Tair = Tair/10,
      Tref_0 = conv_analog_t_v3(Tref_0),
      Theat_0 = conv_analog_t_v3(Theat_0),
      Tref_1 = conv_analog_t_v3(Tref_1),
      Theat_1 = conv_analog_t_v3(Theat_1),
      VPD = 0.6108*exp((17.27*Tair) /
                         (Tair+273.15)) *
        (1 - RH/100),
      gx_mean = as.integer(gx_mean),
      gy_mean = as.integer(gy_mean),
      gz_mean = as.integer(gz_mean),
      Accel = (gx_mean^2 + gy_mean^2 + gz_mean^2)^0.5,
      Theta = atan(gx_mean/(gy_mean^2+gz_mean^2)^0.5)/ pi * 180+90,
      Psi   = atan(gy_mean/(gx_mean^2+gz_mean^2)^0.5)/ pi * 180+90,
      Phi   = atan(((gy_mean^2+gx_mean^2)^0.5)/gz_mean)/ pi * 180+90,
      Osc_fct = gx_sd + gy_sd + gz_sd,
      growth_dn = growt_sens,
      growth_mV = growth_dn / adc_Vbat * 40320,
      growth_mVb = growth_dn * 1.1 / adc_bandgap,
      growth_cmV = -32.673*growth_mV^3+185.32*growth_mV^2-354.93*growth_mV+262.75,
      growth_cmVb = -32.673*growth_mVb^3+185.32*growth_mVb^2-354.93*growth_mVb+262.75,
      growt_sens = case_when(
        growt_sens > 85000 ~ NA_real_,
        growt_sens < 30000 ~ NA_real_,
        #f=y0+a*x+b*x^2+c*x^3, a = 0.000000008,b = -0.0016,c = 89.032
        TRUE ~ 0.000000008*growt_sens^2-0.0016*growt_sens+89.032
      ),

      Bat_mV  = 2*1100*(adc_Vbat/adc_bandgap)
    )
    mydata_4Dp = mydata_4Dp %>% mutate(
      id_rec = paste(TT_ID,
                    as.character(as.integer(Rec_Nr)),
                    sep=""))
  } else {mydata_4Dp = NULL}



  if (!is.null(mydata_49)){
    mydata_49$gain_factor[mydata_49$gain == 0] <- 1
    mydata_49$gain_factor[mydata_49$gain == 1] <- 3.7
    mydata_49$gain_factor[mydata_49$gain == 2] <- 16
    mydata_49$gain_factor[mydata_49$gain == 3] <- 64

    mydata_49$AS7263_610 <- as.numeric(mydata_49$AS7263_610);
    mydata_49$AS7263_610[mydata_49$AS7263_610 > 65000] <- NA
    mydata_49$AS7263_680 <- as.numeric(mydata_49$AS7263_680);
    mydata_49$AS7263_680[mydata_49$AS7263_680 > 65000] <- NA
    mydata_49$AS7263_730 <- as.numeric(mydata_49$AS7263_730);
    mydata_49$AS7263_730[mydata_49$AS7263_730 > 65000] <- NA
    mydata_49$AS7263_760 <- as.numeric(mydata_49$AS7263_760);
    mydata_49$AS7263_760[mydata_49$AS7263_760 > 65000] <- NA
    mydata_49$AS7263_810 <- as.numeric(mydata_49$AS7263_810);
    mydata_49$AS7263_810[mydata_49$AS7263_810 > 65000] <- NA
    mydata_49$AS7263_860 <- as.numeric(mydata_49$AS7263_860);
    mydata_49$AS7263_860[mydata_49$AS7263_860 > 65000] <- NA
    mydata_49$AS7262_450 <- as.numeric(mydata_49$AS7262_450);
    mydata_49$AS7262_450[mydata_49$AS7262_450 > 65000] <- NA
    mydata_49$AS7262_500 <- as.numeric(mydata_49$AS7262_500);
    mydata_49$AS7262_500[mydata_49$AS7262_500 > 65000] <- NA
    mydata_49$AS7262_550 <- as.numeric(mydata_49$AS7262_550);
    mydata_49$AS7262_550[mydata_49$AS7262_550 > 65000] <- NA
    mydata_49$AS7262_570 <- as.numeric(mydata_49$AS7262_570);
    mydata_49$AS7262_570[mydata_49$AS7262_570 > 65000] <- NA
    mydata_49$AS7262_600 <- as.numeric(mydata_49$AS7262_600);
    mydata_49$AS7262_600[mydata_49$AS7262_600 > 65000] <- NA
    mydata_49$AS7262_650 <- as.numeric(mydata_49$AS7262_650);
    mydata_49$AS7262_650[mydata_49$AS7262_650 > 65000] <- NA

    mydata_49p = mydata_49 %>% mutate(
      Rec_Nr = HexToDec(Rec_Nr),
      integration_factor = as.numeric(integration_T)/50,
      # Infrared spectra
      SUM7262=AS7262_450+AS7262_500+AS7262_550+AS7262_570+AS7262_600+AS7262_650,
      SUM7263=AS7263_610+AS7263_680+AS7263_730+AS7263_760+AS7263_810+AS7263_860,
      SUM72 = SUM7262 + SUM7263,
      AS7263_610_R = AS7263_610/SUM72,
      AS7263_610_A = AS7263_610*integration_factor/gain_factor,
      AS7263_610_Rc = -312.45 + (1.6699 * AS7263_610_R),

      AS7263_680_R = AS7263_680/SUM72,
      AS7263_680_A = AS7263_680*integration_factor/gain_factor,
      AS7263_680_Rc = -561.56 + (1.5199*AS7263_680_R),

      AS7263_730_R = AS7263_730/SUM72,
      AS7263_730_A = AS7263_730/SUM72*integration_factor/gain_factor,
      AS7263_730_Rc = -1511.2 + (1.6209*AS7263_730_R),

      AS7263_760_R = AS7263_760/SUM72,
      AS7263_760_A = AS7263_760*integration_factor/gain_factor,
      AS7263_760_Rc = -1012.5 + (1.4549* AS7263_760_R),

      AS7263_810_R = AS7263_810/SUM72,
      AS7263_810_A = AS7263_810*integration_factor/gain_factor,
      AS7263_810_Rc = 91.58 + (0.8414*AS7263_810_R),

      AS7263_860_R = AS7263_860/SUM72,
      AS7263_860_A = AS7263_860*integration_factor/gain_factor,
      AS7263_860_Rc = 34.88 + (0.531*AS7263_860_R),

      # Visual spectra

      AS7262_450_R = AS7262_450/SUM72,
      AS7262_450_A = AS7262_450*integration_factor/gain_factor,
      AS7262_450_Rc = -212.62 + (0.4562*AS7262_450_R),

      AS7262_500_R = AS7262_500/SUM72,
      AS7262_500_A = AS7262_500*integration_factor/gain_factor,
      AS7262_500_Rc = 232.13 + (0.6257 * AS7262_500_R),

      AS7262_550_R = AS7262_550/SUM72,
      AS7262_550_A = AS7262_550*integration_factor/gain_factor,
      AS7262_550_Rc = -842.1 + (1.0546 * AS7262_450_R),

      AS7262_570_R = AS7262_570/SUM72,
      AS7262_570_A = AS7262_570*integration_factor/gain_factor,
      AS7262_570_Rc = -666.72 + (1.0462 * AS7262_570_R),

      AS7262_600_R = AS7262_600/SUM72,
      AS7262_600_A = AS7262_600*integration_factor/gain_factor,
      AS7262_600_Rc = -328.08 + (0.8654 * AS7262_600_R),

      AS7262_650_R = AS7262_650/SUM72,
      AS7262_650_A = AS7262_650*integration_factor/gain_factor,
      AS7262_650_Rc = 202.77 + (0.7829 * AS7262_450_R)
    )
    mydata_49p = mydata_49p %>%
      mutate(id_rec = paste(TT_ID, as.character(as.integer(Rec_Nr-1)),
                            sep=""))
    mydata_49p = mydata_49p %>% select(-AS7263_610:-integration_factor)
  } else {mydata_49p = NULL}



  if(!is.null(mydata_45) & !is.null(mydata_49)){
    mydata_p = left_join(mydata_45p, mydata_49p, by = "id_rec", copy = F)
  }

  if(!is.null(mydata_4D) & !is.null(mydata_49)){
    mydata_p = left_join(mydata_4Dp, mydata_49p, by = "id_rec", copy = F)
    print("After join table size:")
    print(dim(mydata_p))
  }

  if(is.null(mydata_45) & is.null(mydata_49)){
    return(NULL)
  }

  mydata_p = mydata_p %>% rename(
    Timestamp = Timestamp.x,
    Rec_Nr = Rec_Nr.x,
    Dev_Ty = Dev_Ty.x,
    TT_ID = TT_ID.x,
    SDate = SDate.x,
    STime = STime.x
  ) %>% select(-ends_with(".y"), -gx_mean,-gy_mean,-gz_mean ) %>%
    mutate(Rec_Nr = HexToDec(Rec_Nr))

  # Filtering  ID according to metadata=========================================
  mydata_p = mydata_p %>% dplyr::filter(TT_ID %in% (unique(metadata$TT_ID)))

  #Correct wrong and extrapolate missed dates===================================
  mydata_p = mydata_p %>% group_by(TT_ID) %>%
            do(extrapolate_dates(.,step_time = step_time))

  #Make table with physical values==============================================
  print(metadata)
  print(mydata_p)
  mydata_f = left_join(mydata_p,metadata, by = "TT_ID")

  #Applying Savitsky Golay filter===============================================

  mydata_f = mydata_f %>% group_by(TT_ID, charge) %>% mutate(
    Tref_0 = savgol_filt_ts(Tref_0),
    Theat_0 = savgol_filt_ts(Theat_0),
    Tref_1 = savgol_filt_ts(Tref_1),
    Theat_1 = savgol_filt_ts(Theat_1)
  ) %>% ungroup()

  mydata_f = mydata_f %>% group_by(TT_ID, charge) %>% mutate(
    AS7263_610_R = savgol_filt_ts(AS7263_610_R),
    AS7263_610_Rc = savgol_filt_ts(AS7263_610_Rc),
    AS7263_680_R = savgol_filt_ts(AS7263_680_R),
    AS7263_680_Rc = savgol_filt_ts(AS7263_680_Rc),
    AS7263_730_R = savgol_filt_ts(AS7263_730_R),
    AS7263_730_Rc = savgol_filt_ts(AS7263_730_Rc),
    AS7263_760_R = savgol_filt_ts(AS7263_760_R),
    AS7263_760_Rc = savgol_filt_ts(AS7263_760_Rc),
    AS7263_810_R = savgol_filt_ts(AS7263_810_R),
    AS7263_810_Rc = savgol_filt_ts(AS7263_810_Rc),
    AS7263_860_R = savgol_filt_ts(AS7263_860_R),
    AS7263_860_Rc = savgol_filt_ts(AS7263_860_Rc),
    AS7262_450_R = savgol_filt_ts(AS7262_450_R),
    AS7262_450_Rc = savgol_filt_ts(AS7262_450_Rc),
    AS7262_500_R = savgol_filt_ts(AS7262_500_R),
    AS7262_500_Rc = savgol_filt_ts(AS7262_500_Rc),
    AS7262_550_R = savgol_filt_ts(AS7262_550_R),
    AS7262_550_Rc = savgol_filt_ts(AS7262_550_Rc),
    AS7262_570_R = savgol_filt_ts(AS7262_570_R),
    AS7262_570_Rc = savgol_filt_ts(AS7262_570_Rc),
    AS7262_600_R = savgol_filt_ts(AS7262_600_R),
    AS7262_600_Rc = savgol_filt_ts(AS7262_600_Rc),
    AS7262_650_R = savgol_filt_ts(AS7262_650_R),
    AS7262_650_Rc = savgol_filt_ts(AS7262_650_Rc)
  ) %>% ungroup()
  print("grow?")
# Filter growth=================================================================
  if(any(mydata_f$Species == "Triticum durum")){
      mydata_f = mydata_f} else {
        mydata_f = growth_data_filter(mydata_f)
        # Calculating radial growth
        mydata_f = mydata_f %>% group_by(TT_ID) %>% do(calc_growth(.))
    }

  # Calculating wood moisture===================================================
  # Checking if it is croptalker then calc soil moisture
  if(any(mydata_f$Species == "Triticum durum")){
    mydata_f = mydata_f %>% group_by(TT_ID) %>% do(calc_soil_moist(.))

  } else {
    mydata_f = mydata_f %>% group_by(TT_ID) %>% do(calc_wood_moist(.))
    if(any(mydata_f$TTver)==3){
      mydata_f = mydata_f %>% select(-growth_dn,-growth_mV,-growth_mVb,
                                     -growth_cmV,-growth_cmVb)
    }

  }

  print("moist?")
  # Calculate sap flux density==================================================
  mydata_f = calc_sap_flux_density(mydata_f)

  # Calculate indexes===========================================================
  mydata_f = mydata_f %>% mutate(
    NDVI = (AS7263_810_A - AS7263_680_A )/(AS7263_810_A  + AS7263_680_A),
    NDVI2 = (AS7263_860_R+AS7263_810_R - AS7263_680_R-AS7263_610_R )/
      (AS7263_610_R + AS7263_860_R  + AS7263_680_R + AS7263_810_R),
    NDVIc = (AS7263_810_Rc - AS7263_680_Rc )/(AS7263_810_Rc  + AS7263_680_Rc))

  # Data for exporting writing CSV =============================================
  if(any(mydata_f$Species == "Triticum durum")){
    mydata_exp = mydata_f %>% select(Site,Species,TT_ID,Timestamp,Bat_mV, Tair,
                                     RH, VPD, Tref_0,Accel,Theta,Psi,Phi,Osc_fct,
                                     growth_cmVb,growth_cmV, growth_mVb,
                                     growth_mV,Wsoil,pF,MoistSensHz,
                                     NDVI, AS7263_610_R, AS7263_680_R,
                                     AS7263_760_R, AS7263_810_R,AS7263_860_R,
                                     AS7262_450_R,AS7262_500_R,AS7262_550_R,
                                     AS7262_570_R, AS7262_600_R, AS7262_650_R,
                                     lat,lon,
                                     charge,CloudName,TTver )
  } else {
    mydata_exp = mydata_f %>% select(Site,Species,TT_ID,Timestamp,Bat_mV, Tair,
                                     RH, VPD, Tref_0,Accel,Theta,Psi,Phi,Osc_fct,
                                     growth,StWC,Fd,Fdh,DBH,
                                     Flux, NDVI, AS7263_610_R, AS7263_680_R,
                                     AS7263_760_R, AS7263_810_R,AS7263_860_R,
                                     AS7262_450_R,AS7262_500_R,AS7262_550_R,
                                     AS7262_570_R, AS7262_600_R, AS7262_650_R,
                                     lat,lon,
                                     charge,CloudName,TTver )
  }


  mydata_exp = mydata_exp %>% ungroup() %>% mutate(
    years = year(Timestamp),
    months = month(Timestamp),
    doys = yday(Timestamp),
    hours = hour(Timestamp)
  )

  TTdat = list(mydata_45 = TTdat$mydata_45,
               mydata_4D = TTdat$mydata_4D,
               mydata_49 = TTdat$mydata_49,
               mydata_60 = TTdat$mydata_60,
               mydata_61 = TTdat$mydata_61,
               mydata_4B = TTdat$mydata_4B,
               mydata_4C = TTdat$mydata_4C,
               metadata = metadata,
               mydata_49p = mydata_49p,
               mydata_45p = mydata_45p,
               mydata_4Dp = mydata_4Dp,
               mydata_p = mydata_p,
               mydata_f = mydata_f,
               mydata_exp = mydata_exp)
  return(TTdat)

}




