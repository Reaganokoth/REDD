#' @title REDD worldwide 2015
#' @description Shows a map of the situation of forest cover or emissions in the countries of REDD+ projects with data from  CIFOR– CEC – CIRAD – IFRI, 2015 (http://www.reddprojectsdatabase.org/view/countries.php)
#' @param variable character, the information you would like to see: "forestcover" or "emissions", or leave empty to see the countries with REDD+ projects
#' @return A map of countries of REDD+ projects
#' @import httr
#' @import xml2
#' @import magrittr
#' @import pbapply
#' @import magick
#' @import tmap
#' @import dplyr
#' @import stringr
#' @examples
#' REDDworld("forest cover")
#'
#' @export

REDDworld<-function(variable="AUTO", mode = "plot"){

#base url
url_api<- "http://www.reddprojectsdatabase.org/view/countries.php"

#Check what we get back from here
reply <- httr::GET(url_api)

#extract info from body
reply_content<-httr::content(reply)

#crawl for table
xml_prod<-xml2::xml_find_all(reply_content, "//table")

xml2::xml_text(xml2::xml_children(xml2::xml_contents(xml2::xml_children(xml_prod)[[1]])))

#library(magrittr) #I did not understand how to call the function without the library

#Extract the head of the table
prod_head<-xml2::xml_children(xml_prod)[[1]]%>%
  xml2::xml_contents()%>%
  xml2::xml_children()%>%
  xml2::xml_text()

#Extract the body of the table
products_body<-xml2::xml_children(xml_prod)[[2]]%>%
  xml2::xml_children()%>%

  lapply(function(x){
    x<-xml2::xml_contents(x)
    xml2::xml_text(x)[seq(1, length(x), by=1)]
  })

#Build Products data frame

products0<- do.call(rbind.data.frame, products_body)
names(products0) <- c("name","GDP", "Forest_cover", "Deforestation rate", "emissions", "Funds", "View")
products<-dplyr::mutate(products0, member="REDD+ projects")

variable1<-stringr::str_remove(variable, " ")

#changes all the characters to lowercase
variable2<-stringr::str_to_lower(variable1, locale = "en")

#Extract world map


data("World", package = "tmap")

#include REDD data
redd_data0<-merge(World, products, by="name", all=TRUE)
redd_data1<-dplyr::mutate(redd_data0,             emissions.den= as.numeric(emissions)*1000000/(as.numeric(area)))
redd_data<- dplyr::mutate(redd_data1,             forest.den= as.numeric(Forest_cover)*1000/(as.numeric(area)))

#if(mode=="view"){tmap::tmap_mode("view")
#  } else tmap::tmap_mode("plot")

#Choose variable to map

REDDmap<-(if(variable2=="forestcover"){
   tmap::tm_shape(redd_data)+
    tmap::tm_fill("forest.den",title="Forest density",
                  breaks = c(0, 0.10000, 0.20000,0.3, 0.4,0.50000, 0.6000, Inf),
                  textNA = "No REDD+ projects",
                  palette = ("Greens"))+
    tmap::tm_layout(
      title = "Forest cover in countries of REDD+ projects in 2015 (forest ha/km2)")+
    tmap::tm_view(view.legend.position = c("right","bottom"))

} else if (variable2=="emissions"){
  tmap::tm_shape(redd_data)+
  tmap::tm_fill("emissions.den",title="Emissions (Ton of CO2/km2)",
                breaks = c(0, 300, 600,900, 1200,1500, Inf),
                textNA = "No REDD+ projects",
                palette = ("YlOrRd"))+
  tmap::tm_layout(title = "Emissions from Land Use Change and Forestry in countries of REDD+ projects in 2016 (Ton of Co2/km2)")+
  tmap::tm_view(view.legend.position = c("right","bottom"))

} else tmap::tm_shape(redd_data)+
  tmap::tm_polygons("member")+
  tmap::tm_layout(
    title = "Countries of REDD+ projects in 2015"))
show.map<-tmap::tmap_leaflet(
  REDDmap,
  mode = "view",
  show = TRUE,
  add.titles = TRUE,
  in.shiny = FALSE
)

show.map
}
