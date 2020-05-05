#' @title REDD worldwide 2015
#' @description Shows a map of the situation of forest cover or emissions in the countries of REDD+ projects
#' @param variable character, the information you would like to see; "forest cover" or "emissions"
#' @param mode character, style of map it could be "static" or "interactive"
#' @return A forest fact or definition of forest concepts
#' @import httr
#' @import xml2
#' @import magrittr
#' @import pbapply
#' @import magick
#' @import tmap
#' @export

REDDworld<-function(variable="AUTO", mode="interactive"){

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
prod_head<-xml_children(xml_prod)[[1]]%>%
  xml_contents()%>%
  xml_children()%>%
  xml_text()

#Extract the body of the table
products_body<-xml_children(xml_prod)[[2]]%>%
  xml_children()%>%

  lapply(function(x){
    x<-xml_contents(x)
    xml_text(x)[seq(1, length(x), by=1)]
  })

products<- do.call(rbind.data.frame, products_body)
names(products) <- c("name","GDP", "Forest cover", "Deforestation rate", "emissions", "Funds", "View")

#install.packages("tmap")

