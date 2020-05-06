#' @title REDD worldwide 2015
#' @description Shows a map of the situation of forest cover or emissions in the countries of REDD+ projects with data from  CIFOR– CEC – CIRAD – IFRI, 2015 (http://www.reddprojectsdatabase.org/view/countries.php)
#' @param variable character, the information you would like to see_ "forestcover" or "emissions"
#' @param style character, style of map it could be "static" or "interactive"
#' @return A map of countries of REDD+ projects
#' @import httr
#' @import xml2
#' @import magrittr
#' @import pbapply
#' @import magick
#' @import tmap
#' @export dplyr

REDDworld<-function(variable="AUTO", style="interactive"){

#base url

url_api<- "http://www.reddprojectsdatabase.org/view/countries.php"

#Check what we get back from here
reply <- httr::GET(url_api)
httr::http_status(reply)

#extract info from body
reply_content<-httr::content(reply)
class(reply_content)

#crawl for table
xml_prod<-xml2::xml_find_all(reply_content, "//table")

xml_text(xml2::xml_children(xml2::xml_contents(xml2::xml_children(xml_prod)[[1]])))

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

products<- do.call(rbind.data.frame, products_body)
names(products) <- c("name","GDP", "Forest_cover", "Deforestation rate", "emissions", "Funds", "View")


data("World")
redd_data<-merge(World, products, by="name", all=TRUE)
dplyr::mutate(redd_data,
              forest.den= Forest_cover/pop_est*pop_est_dens
              )
dplyr::mutate(redd_data,
              emissions.den= emissions/area)
#View(redd_data)

if (style == "static"){
  tmap::tmap_mode("plot")
} else tmap::tmap_mode("view")

tmap::tm_shape(redd_data)+
  tmap::tm_polygons("Forest_cover", convert2density = TRUE)+
  tmap::tmap_options(max.categories= 5)+
  tmap::tm_layout(
    title = "Forest cover in 2015 for countries of REDD+ projects (1000 ha/km2)"
  )
}
