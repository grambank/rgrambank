# TODO: update these functions to handle updated version releases by searching Zenodo via API

#' Loads grambank data (v1.0.3)
#' @param url string. The url to download.
#' @return An rcldf::cldf object
#'
#' @export
load_grambank <- function(url="https://zenodo.org/records/7844558/files/grambank/grambank-v1.0.3.zip?download=1") {
    return(rcldf::cldf(url))
}
#' Loads glottolog data (v4.8)
#' @param url string. The url to download.
#' @return An rcldf::cldf object
#'
#' @export
load_glottolog <- function(url="https://zenodo.org/records/8131091/files/glottolog/glottolog-cldf-v4.8.zip?download=1") {
    return(rcldf::cldf(url))
}
