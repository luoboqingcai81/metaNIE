


#' Combine table and metadata
#'
#' @param table tibble
#' @param metadata tibble
#' @param convert logical
#'
#' @return tibble
#' @export
#' @importFrom magrittr %>%
NULL
#' @examples
#' data(metadata)
#' data(ko_abundance)
#' combine_table <- combine_metadata(ko_abundance, metadata, convert = T)
combine_metadata <- function(table, metadata, convert = F) {
  if (convert == T) {
    if (!(((ncol(table) - 1) == nrow(metadata)) & all(metadata$sample %in% colnames(table)))) {
      stop("the metadata sample is not equal to the table sample")
    }
    name <- colnames(table)[[1]]
    table_convert <- table %>%
      replace(is.na(.), 0) %>%
      tidyr::pivot_longer(-{{ name }}, names_to = "sample", values_to = "Value") %>%
      tidyr::pivot_wider(names_from = {{ name }}, values_from = Value) %>%
      dplyr::select(!where(~ all(.x == 0))) %>%
      dplyr::left_join(metadata, by = "sample") %>%
      dplyr::relocate(where(is.numeric), .after = where(is.character)) %>%
      dplyr::relocate(sample)
    return(table_convert)
  }
  if (convert == F) {
    if (!((nrow(table) - 1) == nrow(metadata)) & all(metadata$sample %in% rownames(table))) {
      stop("the metadata sample is not equal to the table sample")
    }
    table_convert <- table %>%
      dplyr::left_join(metadata, by = "sample") %>%
      dplyr::relocate(where(is.numeric), .after = where(is.character)) %>%
      dplyr::relocate(sample)
  }
}



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
    dplyr::distinct(Data2)
  meta_table %>%
    dplyr::select(!where(is.numeric), Df$Data2)
}




