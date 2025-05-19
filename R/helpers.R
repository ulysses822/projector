#' Convert from degrees to radians
#'
#' @param x (numeric) Angle in degrees.
#'
#' @returns \code{x} in radians.
#' @export
#'
#' @examples \dontrun{
#' projecter::deg2rad(90)
#' }
deg2rad <- function(x) {
  x * pi / 180
}

#' Convert from degrees to radians in scope
#'
#' Converts a named vector of angles from degrees to radians.
#'
#' @param x (list) Named vector of angles in degrees. Names are will be used to
#'   assign objects.
#' @param envir Environment to assign objects into.
#'
#' @returns Nothing, called for side effect.
#' @export
#'
#' @examples \dontrun{
#' }
scope_deg2rad <- function(x, envir = rlang::caller_env()) {
  x |>
    purrr::iwalk(\(x, name) assign(name, projector::deg2rad(x = x), envir = envir))

  invisible(NULL)
}

#' Plot projection
#'
#' Maps a grid of angles using a particular projection.
#'
#' @param projection Projection function.
#' @param ... Additional arguments passed to \code{projection}
#'
#' @returns ggplot object
#' @export
#'
#' @examples \dontrun{
#' projector::plot_projection(projection = projector::aitorff)
#' }
plot_projection <- function(projection, ...) {
  tibble::tibble("lambda" = seq(180, -180, by = -4),
                 "phi" = seq(90, -90, by = -2)) |>
    tidyr::expand(lambda, phi) |>
    purrr::pmap(\(...) projection(...)) |>
    dplyr::bind_rows() |>
    ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::geom_point(mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::xlim(c(pi * 6378, -pi * 6378)) +
    ggplot2::ylim(c(pi * 6378, -pi * 6378))
}
