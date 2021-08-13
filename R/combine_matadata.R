
#' Combine table and metadata
#'
#' @param table tibble
#' @param metadata tibble
#' @param convert logical
#'
#' @return tibble
#' @export
#' @examples
#' data(metadata)
#' data(ko_abundance)
#' combine_table <- combine_metadata(ko_abundance, metadata, convert = TRUE)
combine_metadata <- function(table, metadata, convert = FALSE) {
  if (convert == TRUE) {
    if (!(((ncol(table) - 1) == nrow(metadata)) & all(metadata$sample %in% colnames(table)))) {
      stop("the metadata sample is not equal to the table sample")
    }
    name <- colnames(table)[[1]]
    Value <- NULL
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
  if (convert == FALSE) {
    if (!((nrow(table) - 1) == nrow(metadata)) & all(metadata$sample %in% rownames(table))) {
      stop("the metadata sample is not equal to the table sample")
    }
    table_convert <- table %>%
      dplyr::left_join(metadata, by = "sample") %>%
      dplyr::relocate(where(is.numeric), .after =where(is.character)) %>%
      dplyr::relocate(sample)
  }
}




