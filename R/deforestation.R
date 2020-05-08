#' @title CO2 emissions due to deforestation
#' @description This function calculates the CO2 emissions (in Ton CO2) related to deforestation (forestland that changes to other land cover).
#' @param area numeric, the area (in hectares) of deforestation related to an specific land cover
#' @param error numeric, error of the area (0-1)
#' @param new_land_cover character, the new land cover of the area
#' @param emission_factor numeric, the emission factor (in ton C/Hectare)
#' @param error_ef numeric, the relative error of the emission factor in percentage (from 0-100)
#' @param points numeric, number of sample points with this specific land cover change
#' @param total_area numeric, the total area of study (in hectares)
#' @param years numeric, number of years between the land cover change analysis
#' @param total_points numeric, number of total sample points
#' @return CO2 deforestation emissions (in Ton CO2) for a certain land cover class
#' @export

defor_emiss<-function(area="auto", error=0, new_land_cover="the new land cover", emission_factor, error_ef=0, points, total_area, years, total_points){

   #If it is not provided, calculates the area using the number of samples for an specific class
  if(area=="auto") {
    wi<-points/total_points
    area<- wi*total_area

    #calculates the standard error
    Ei_per<-sqrt((wi*(1-wi))/(total_points-1))

    #calculates the standard error in hectares
    Ei_ha<-Ei_per*total_area

    #calculates the confidence interval
    confidence_interval<-1.96*Ei_ha

    #calculates the confidence interval in hectares
    conf_int_per<-confidence_interval/total_area

    #calculates the inferior limit of the confidence interval
    ci_inf<-area-Ei_ha

    #calculates the superior limit of the confidence interval
    ci_sup<-area+Ei_ha

    #Calculates the error of the area
    error<-confidence_interval/area
  }

  #calculates the Tons of Carbon emitted
  TonC<-emission_factor*area

  #calculates the Tons of Carbon per year
  TonC_yr<-TonC/years

  #Convert the tons of Carbon into tons of Carbon dioxide
  fCO2<-3.666667
  TonCO2<-TonC_yr*fCO2

  #Calculates the error of the emission by adding the relative errors of area and emission factor
  RE<-(((emission_factor*(error*area))+(error_ef*emission_factor)*area)/TonCO2)*100

  #prints the results
  cat("The emissions of CO2 related to deforestation due to", new_land_cover, "are", TonCO2, "TOnCO2/year with a relative error of ", RE, "%.")
}

