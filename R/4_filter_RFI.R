#' Plot every image we have to check for RFI
#'
#' 
#' @param data  Output from the 2_data_clean function
#' 
#' @return A dataframe with SAR observations without RFI affected images
#'
#' @export
#' 



filter_RFI = function(SAR_data_clean){
  
  #Images selected for RFI : 
  RFI_images= c("S1A_IW_GRDH_1SDV_20170509T173029_20170509T173054_016508_01B5C7_8298.SAFE",
                "S1A_IW_GRDH_1SDV_20170917T054425_20170917T054450_018411_01EFF6_056A.SAFE",
                "S1A_IW_GRDH_1SDV_20180404T053600_20180404T053625_021313_024ACD_E575.SAFE",
                "S1A_IW_GRDH_1SDV_20180615T053604_20180615T053629_022363_026BD7_7DBC.SAFE",
                "S1A_IW_GRDH_1SDV_20180615T053629_20180615T053654_022363_026BD7_A715.SAFE",
                "S1A_IW_GRDH_1SDV_20190306T053606_20190306T053631_026213_02ED5C_3BCA.SAFE",
                "S1A_IW_GRDH_1SDV_20190307T172205_20190307T172230_026235_02EE25_FF08.SAFE",
                "S1A_IW_GRDH_1SDV_20190422T173844_20190422T173909_026906_0306C1_B9A5.SAFE",
                "S1A_IW_GRDH_1SDV_20190919T054438_20190919T054503_029086_034D16_0395.SAFE",
                "S1A_IW_GRDH_1SDV_20191008T053641_20191008T053706_029363_035695_626B.SAFE",
                "S1A_IW_GRDH_1SDV_20200317T054435_20200317T054500_031711_03A819_4E01.SAFE",
                "S1A_IW_GRDH_1SDV_20200628T053617_20200628T053642_033213_03D905_3716.SAFE",
                "S1A_IW_GRDH_1SDV_20200722T053619_20200722T053644_033563_03E3B0_E331.SAFE",
                "S1A_IW_GRDH_1SDV_20201002T053647_20201002T053712_034613_0407BC_A82B.SAFE",
                "S1A_IW_GRDH_1SDV_20201007T054445_20201007T054510_034686_040A40_240E.SAFE",
                "S1B_IW_GRDH_1SDV_20170503T172937_20170503T173002_005437_00987B_2C96.SAFE",
                "S1B_IW_GRDH_1SDV_20170515T172938_20170515T173003_005612_009D40_C8B4.SAFE",
                "S1B_IW_GRDH_1SDV_20170818T054333_20170818T054358_006990_00C503_BB68.SAFE",
                "S1B_IW_GRDH_1SDV_20170830T054334_20170830T054359_007165_00CA0A_B996.SAFE",
                "S1B_IW_GRDH_1SDV_20170911T054334_20170911T054359_007340_00CF32_BC94.SAFE",
                "S1B_IW_GRDH_1SDV_20171001T172138_20171001T172203_007639_00D7D3_5AE4.SAFE",
                "S1B_IW_GRDH_1SDV_20180415T054333_20180415T054358_010490_0131F2_EF17.SAFE",
                "S1B_IW_GRDH_1SDV_20200411T053521_20200411T053546_021092_02804A_AD47.SAFE",
                "S1B_IW_GRDH_1SDV_20200627T054350_20200627T054415_022215_02A297_24B9.SAFE",
                "S1B_IW_GRDH_1SDV_20200915T172140_20200915T172205_023389_02C6DE_66F3.SAFE",
                "S1B_IW_GRDH_1SDV_20201006T055204_20201006T055229_023688_02D029_512C.SAFE",
                "S1B_IW_GRDH_1SDV_20201008T053549_20201008T053614_023717_02D113_5C90.SAFE",
                "S1A_IW_GRDH_1SDV_20170409T053613_20170409T053638_016063_01A842_5521.SAFE",
                "S1A_IW_GRDH_1SDV_20180403T173837_20180403T173902_021306_024A97_B337.SAFE",
                "S1A_IW_GRDH_1SDV_20180415T173838_20180415T173903_021481_025009_AB37.SAFE",
                "S1A_IW_GRDH_1SDV_20191123T055248_20191123T055313_030034_036DDF_557D.SAFE",
                "S1A_IW_GRDH_1SDV_20201112T054445_20201112T054510_035211_041C76_1163.SAFE",
                "S1B_IW_GRDH_1SDV_20191120T172151_20191120T172216_019014_023E18_A36C.SAFE",
                "S1B_IW_GRDH_1SDV_20201113T053548_20201113T053613_024242_02E16C_A864.SAFE")
  
  length(RFI_images)
  
  SAR_data_clean$image_name = as.character(SAR_data_clean$image_name)
  RFI_images = as.character(RFI_images)
  
  #Out of the dataset ! 
  SAR_data_noRFI = SAR_data_clean %>%
    filter(!image_name %in% RFI_images)
  
  save(SAR_data_noRFI,file="output/SAR_data_noRFI.Rdata")
  
  return(SAR_data_noRFI)
  
}