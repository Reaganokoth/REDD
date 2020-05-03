#' @title Relevant definitions and forest facts
#' @description This function provides some relevant definitions in REDD context and forest facts collected from un-redd.org. These are basic concepts that may change between the countries or projects.
#' @param term character, the term to be explained. It must be one of the words from this list: forest, afforestation, reforestation, bamboo, redwoods, mangroves, importance, floods, droughts, deforestation rate, temperature.
#' @return A forest fact or definition of forest concepts
#' @import stringr
#' @export


definitions<- function(term="auto"){

  term1<-stringr::str_remove(term, " ")
  term.low<-stringr::str_to_lower(term1, locale = "en")

  if(term.low=="auto"){
    def <- "Please, use a term from the list to get the definition or forest fact (forest, afforestation, reforestation), you can find the list in the description of the function"
  } else if (term.low== "forest"){
    def <-"'Forest is a large area of trees whose canopies cover at least 10 per cent of the sky spanning more than 0.5 hectares with trees higher than 5 meters and a canopy cover of more than 10 percent.' -UN-REDD"
  } else if (term.low == "afforestation"){
    def<-"'Afforestation means the planting of trees on land which was never forested' -UN-REDD"
  } else if (term.low == "reforestation"){
    def<-"'Reforestation means planting of trees on land which was forested before.' -UN-REDD"
  } else if (term.low== "bamboo"){
      def <-"'Bamboo is a giant woody grass which is grown chiefly in the tropics. It is one of the fastest growing plants on Earth, with reported growth rates of 100 centimeters (39 inches) in 24 hours.' -UN-REDD"
  } else if (term.low== "redwoods"){
      def<-"'Redwoods are the tallest trees in the world and can easily reach heights of 300 feet (91 meters).' -UN-REDD"
  } else if (term.low == "mangroves"){
      def<-"'Mangroves are trees or shrubs which grows in tidal, chiefly tropical, coastal swamps, having numerous tangled roots that grow above the ground and form dense thickets.' -UN-REDD"
  } else if (term.low == "importance"){
    def <-"'An estimated 1.6 billion people use forests for all or part of their livelihoods. Forests also play a crucial role in fighting catastrophic climate change by absorbing and storing massive amounts of CO2. Forests are home to 80% of the world’s terrestrial biodiversity.' -UN-REDD"
  } else if (term.low == "floods"){
    def<-"'During heavy rains, trees reduce the risk of flooding. Experts say that woodland acts as a barrier to floodwater, while trees also prevent soil erosion, reducing sediment going into rivers and increasing water absorption into the ground.' -UN-REDD"
  } else if (term.low == "droughts"){
    def<-"'Forests can retain excess rainwater, prevent extreme run-offs (...). They can also help mitigate the effects of droughts by releasing water in the dry season, forests can also help provide clean water and mitigate the effects of droughts' -UN-REDD"
  } else if (term.low == "deforestationrate"){
    def <-"'The Earth loses a forest area about the size of 40 football fields every minute.' -UN-REDD"
  } else if (term.low == "temperature"){
    def<-"'Trees could reduce temperatures in cities up to 8°C, lowering use of air conditioning and related emissions by up to 40 per cent.' -UN-REDD"
  } else def<-"Please, use a term from the list to get the definition or forest fact(forest, afforestation, reforestation), you can find the list in the description of the function"

  print(def)
}



