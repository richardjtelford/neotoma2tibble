#from get_download object
#pull off counts, labdata sample.meta (chronologies)
#make nested tibble
#filter out unwanted stuff


#get taxon list
#get dataset metadata
#calculate percent based on count sum

#helper function to extract species counts
#' @importFrom dplyr gather rownames_to_column filter left_join %>% as_tibble
#' @importFrom neotoma counts
#' 

#make thin pollen using correct column of taxon.list
mk_thin_pollen <- function(x){
  counts <- counts(x)
  taxon.list <- x$taxon.list
  
  #some datasets have duplicate taxon.names so use alias if it exists
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


################
#helper functions to extract sample.meta, chronologies or lab.data
#' @importFrom dplyr gather rownames_to_column filter left_join %>% as_tibble

#extract sample.meta 
extract_sample_meta <- function(x){

   out <- x$sample.meta %>% 
     rename(sampleID = sample.id)

   return(out)
}

#extract lab.data - need to deal with inconsistent column names
extract_lab_data <- function(x){
  
  #   out <- x$lab.data
  #   if(ncol(out) == 0){
  #     return(NULL)
  #   }
  # 
  # out <- out %>%
  #   as.data.frame() %>% 
  #   rownames_to_column(var = "sampleID") %>%
  #   as_tibble()
  # return(out)
}