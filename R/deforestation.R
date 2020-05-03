#' @title CO2 emissions due to deforestation
#' @description This function calculates the CO2 emissions (in Ton CO2) related to deforestation (forestland that changes to other land cover).
#' @param area numeric, the area (in hectares) of deforestation related to an specific land cover
#' @param new_land_cover character, the new land cover of the area
#' @param emission_factor numeric, the emission factor (in ton C/Hectare)
#' @param error_ef numeric, the relative error of the emission factor in percentage (from 0-100)
#' @param points numeric, number of sample points with this specific land cover change
#' @param total_area numeric, the total area of study (in hectares)
#' @param years numeric, number of years between the land cover change analysis
#' @param total_points numeric, number of total sample points
#' @return CO2 deforestation emissions (in Ton CO2) for a certain land cover class
#' @import
#' @export

defor_emiss<-function(area="auto", new_land_cover="the new land cover", emission_factor, error_ef=0, points, total_area, years, total_points){
  if(area=="auto") {
    wi<-points/total_points
    area<- wi*total_area
    Ei_per<-sqrt((wi*(1-wi))/(total_points-1))
    Ei_ha<-Ei_per*total_area
    confidence_interval<-1.96*Ei_ha
    conf_int_per<-confidence_interval/total_area
    ci_inf<-area-Ei_ha
    ci_sup<-area+Ei_ha
    error<-confidence_interval/area
  }

  TonC<-emission_factor*area
  TonC_yr<-TonC/years
  fCO2<-3.666667
  TonCO2<-TonC_yr*fCO2
  RE<-(((emission_factor*(error*area))+(error_ef*emission_factor)*area)/TonCO2)*100

  cat("The emissions of CO2 related to deforestation due to", new_land_cover, "are", TonCO2, "TOnCO2/year with a relative error of ", RE, "%.")
}

