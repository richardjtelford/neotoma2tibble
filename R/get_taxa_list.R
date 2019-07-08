
#' @importFrom dplyr as_tibble distinct %>% 
#' @importFrom purrr map_df
#' @export

get_taxa_list <- function(x){
  taxon_list <- x %>%
    #extract taxon.lists
    map_df("taxon.list") %>%
    as_tibble() %>% 
    #remove duplicates
    distinct()
  
  return(taxon_list)
}