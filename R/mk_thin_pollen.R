#from get_download object
#pull off counts, labdata sample.meta (chronologies)
#make nested tibble
#filter out unwanted stuff

#get taxon list
#get dataset metadata
#calculate percent based on count sum


#' @importFrom dplyr gather rownames_to_column filter left_join %>% as_tibble
#' 

#make thin pollen using correct column of taxon.list
mk_thin_pollen <- function(counts, taxon.list){
  if(any(names(taxon.list) == "alias")){
    name_column <- "alias"
  } else {
    name_column <- "taxon.name"
  }
  
  thin_pollen <- counts %>%
    rownames_to_column(var = "sampleID") %>% 
    gather(key = taxa, value = count, -sampleID) %>% 
    filter(count > 0) %>% 
    left_join(taxon.list, by = c("taxa" = name_column)) %>% 
    
    #filter pollen/spores/ascospores - no charcoal in the counts 
    filter(variable.element %in% c("pollen", "spores", "ascospore")) %>% 
    as_tibble()
  return(thin_pollen)
}