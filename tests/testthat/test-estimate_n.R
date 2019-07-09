context("")


test_that("", {
})

# test_that("Warning when no solution within range n_min:n_max", {
#   df <- tibble::tibble(taxon = letters[1:10], ID = 1, count = 1:10) %>% 
#     mutate(
#       percent = count/sum(count) * 100, 
#       percent = floor(percent * 100)/100 #floor rather than round
#     )
#    expect_output(
#      suppressWarnings(estimate_n(df, ID_cols = "ID")),
#      "Column 'score' violates assertion"
#      )
#    expect_warning(estimate_n(df, ID_cols = "ID"))
#   
#     expect_lt(suppressWarnings(estimate_n(df, ID_cols = "ID")) %>% 
#               unnest(direct_search_est) %>% 
#               dplyr::pull(score), 1)
# })
