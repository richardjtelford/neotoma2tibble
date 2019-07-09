#' Convert download_list into a tibble
#' @param x download_list
#' @description Converts a download_list produced by \code{\link[neotoma]{get_download}} into a tibble. See \code{\link[neotoma]{get_download}} for more information on how to download data.
#' @examples
#' require("dplyr") 
#' data(Hordaland)
#' as_tibble(Hordaland)
#' @importFrom dplyr as_tibble %>% everything
#' @importFrom purrr map_df
#' @importFrom rlang .data
#' @export


as_tibble.download_list <- function(x){
  out <- x %>% map_df(process_download_item) %>% 
    select(.data$dataset.id, .data$sample.id, everything()) %>% 
    group_by(.data$dataset.id, .data$sample.id)
  
  return(out)
}
