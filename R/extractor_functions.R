#from get_download object
#pull off counts, labdata sample.meta (chronologies)
#make nested tibble
#filter out unwanted stuff


#get taxon list
#get dataset metadata
#calculate percent based on count sum

#helper function to extract species counts
#' @importFrom dplyr rownames_to_column filter left_join %>% as_tibble group_by 
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr nest gather
#' @importFrom neotoma counts
#' 

#make thin pollen using correct column of taxon.list
mk_thin_pollen <- function(x){
  counts <- counts(x)
  taxon.list <- x$taxon.list
  
  #some datasets have duplicate taxon.names so use alias if it exists
  if(any(names(taxon.list) == "alias")){
    name_column <- "alias"
    #avoid character/name clash in join
    taxon.list["alias"] <- as.character(taxon.list["alias"] )
  } else {
    name_column <- "taxon.name"
  }

  #avoid character/name clash in join
  taxon.list["taxon.name"] <- as.character(taxon.list["taxon.name"] )
  

  
  
  thin_pollen <- counts %>%
    rownames_to_column(var = "sample.id") %>% 
    mutate(sample.id = as.integer(sample.id)) %>% 
    gather(key = taxa, value = count, -sample.id) %>% 
    filter(count > 0) %>% 
    left_join(taxon.list, by = c("taxa" = name_column)) %>% 
    # 
    # #filter pollen/spores/ascospores - no charcoal in the counts 
    # filter(variable.element %in% c("pollen", "spores", "ascospore")) %>% 
    group_by(sample.id) %>% 
    nest()
  return(thin_pollen)
}


################
#helper functions to extract sample.meta, chronologies or lab.data
#' @importFrom dplyr gather rownames_to_column filter left_join %>% as_tibble

#extract sample.meta 
extract_sample_meta <- function(x){

   out <- x$sample.meta %>% 
     as_tibble()

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


#' @importFrom dplyr full_join
#put it all together
process_download_item <- function(x){
  
  #extract 
  sample_meta <- extract_sample_meta(x)
  counts <- mk_thin_pollen(x) 
  
  #check matching sampleID
  ##todo
  
  #combine
  out <- sample_meta %>% 
    full_join(counts, by = "sample.id")
  
  return(out)
  
}