#' Get unified taxon list from a nested tibble of neotoma data
#' @param x nested tibble from 
#' @examples 
#' require("dplyr") 
#' data(Hordaland)
#' Hordaland_tibble <- as_tibble(Hordaland) 
#' get_taxa_list(Hordaland_tibble)
#' @importFrom dplyr ungroup distinct %>% select
#' @importFrom tidyr unnest
#' @importFrom rlang .data
#' @export

get_taxa_list <- function(x){
  taxon_list <- x %>%
    ungroup() %>% 
    select(.data$data) %>% 
    unnest() %>% 
    select(-.data$count) %>% 
    distinct()

  return(taxon_list)
}
