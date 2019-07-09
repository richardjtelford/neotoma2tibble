#helper function to extract species counts
#' @importFrom dplyr filter left_join %>% as_tibble group_by mutate_all mutate
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr nest gather
#' @importFrom neotoma counts
#' @importFrom rlang .data
#' 

#make thin pollen using correct column of taxon.list
mk_thin_pollen <- function(x){
  counts <- counts(x)
  taxon.list <- x$taxon.list %>% 
    mutate_all(as.character)#convert columns from factor to avoid problems
    
  #some datasets have duplicate taxon.names so use alias if it exists
  if(any(names(taxon.list) == "alias")){
    name_column <- "alias"
  } else {
    name_column <- "taxon.name"
  }
  
  thin_pollen <- counts %>%
    rownames_to_column(var = "sample.id") %>% 
    mutate(sample.id = as.integer(.data$sample.id)) %>% 
    gather(key = "taxa", value = "count", -.data$sample.id) %>% 
    filter(.data$count > 0) %>% 
    left_join(taxon.list, by = c("taxa" = name_column)) %>% 
    # 
    # #filter pollen/spores/ascospores - no charcoal in the counts 
    # filter(variable.element %in% c("pollen", "spores", "ascospore")) %>% 
    group_by(.data$sample.id) %>% 
    nest()
  return(thin_pollen)
}


################
#helper functions to extract sample.meta, chronologies or lab.data
#' @importFrom dplyr filter left_join %>% as_tibble
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr gather

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
