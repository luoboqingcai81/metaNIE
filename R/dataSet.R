#' the group information about the sample.
#'
#' A dataset containing the sample names and group information
#'
#' @format A tibble with 48 rows and 4 variables:
#' \describe{
#'   \item{sample}{sample name}
#'   \item{treatment}{small group}
#'   ...
#' }
#' @source \url{http://www.nowhere1 <- .info/}
"metadata"




#' the KEGG table with counts number.
#'
#' A dataset containing the sample names and KO counts number
#'
#' @format A tibble with 300 rows and 49 variables:
#' \describe{
#'   \item{ko_id}{the KEGG- KO id}
#'   \item{R34}{the sample name}
#'   ...
#' }
#' @source \url{http://www.nowhere2.info/}
"ko_abundance"



#' the ko_abundance table combined with metadata
#'
#' A dataset containing the ko_abundance and metadata
#'
#' @format A tibble with 48 rows and 304 variables:
#' \describe{
#'   \item{sample}{sample name}
#'   \item{K00344}{the KO counts number}
#'   ...
#' }
#' @source \url{http://www.nowhere3.info/}
"combined_table"




