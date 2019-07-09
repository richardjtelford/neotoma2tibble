#' Convert download_list into a tibble
#' @param x download_list
#' @importFrom dplyr as_tibble %>% everything
#' @importFrom purrr map_df


as_tibble.download_list <- function(x){
  out <- x %>% map_df(process_download_item) %>% 
    select(dataset.id, sample.id, everything())
  return(out)
}