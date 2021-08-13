




#' clean table by the group
#'
#' @param meta_table tibble
#' @param group i have no idea
#'
#' @return tibble
#' @export
#'
#' @examples
#' data(combined_table)
#' clean_table_result <- clean_byGroup(combined_table, treatment)
clean_byGroup <- function(meta_table, group) {
  Group <- as.character(rlang::enexpr(group))
  if (!(Group %in% colnames(meta_table))) {
    stop("group not in meta_table colnums")
  }
  Num_group <- length(dplyr::pull(meta_table, {{ group }})) / length(unique(dplyr::pull(meta_table, {{ group }})))
  Data2 <- NULL
  Value <- NULL
  Data <- NULL
  Df <- meta_table %>%
    dplyr::group_nest({{ group }}, .key = "Data") %>%
    dplyr::mutate(Data2 = purrr::map(Data, function(df) {
      df2 <- df %>%
        dplyr::select(where(~ sum(.x == 0) <= Num_group / 2))
      colnames(df2)
    })) %>%
    dplyr::ungroup() %>%
    dplyr::select(Data2) %>%
    tidyr::unnest_longer(Data2) %>%
    dplyr::distinct(Data2) %>%
    unlist %>%
    unname
  meta_table %>%
    dplyr::select(!where(is.numeric),all_of(Df))
}




