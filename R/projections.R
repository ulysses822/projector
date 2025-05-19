#' Map projections
#'
#' @param lambda (numeric) Longitude.
#' @param phi (numeric) Latitude.
#' @param R (numeric) Radius of the globe.
#' @param radians (Boolean) Are the angles specified in radians?
#'
#' @returns \code{x} and \code{y} coordinates of equirectangular projection.
#' @name projections
#'
#' @examples \dontrun{
#' }
NULL

#' @rdname projections
#' @export
aitorff <- function(lambda, phi, R = 6378, radians = FALSE) {
  # envir <- environment()

  if (!radians) {
    c("lambda" = lambda, "phi" = phi) |>
      projector::scope_deg2rad()
  }

  sinc <- function(x)
    sin(x) / x

  alpha = acos(cos(phi) * cos(lambda / 2))

  x = R * 2 * cos(phi) * sin (lambda / 2) / sinc(alpha)
  y = R * sin(phi) / sinc(alpha)

  list("x" = x, "y" = y)
}

#' @rdname projections
#'
#' @param lambda_0 (numeric) Central parallel.
#' @param phi_0 (numeric) Central meridian
#' @param standard_parallel (numeric) Standard parallel where the scale of the
#'   projection is true.
#'
#' @export
equirectangular <- function(lambda,
                            phi,
                            R = 6378,
                            lambda_0 = 0,
                            phi_0 = 0,
                            standard_parallel = 0,
                            radians = FALSE) {
  # envir <- environment()

  if (!radians) {
    c(
      "lambda" = lambda,
      "phi" = phi,
      "lambda_0" = lambda_0,
      "phi_0" = phi_0,
      "standard_parallel" = standard_parallel
    ) |>
      projector::scope_deg2rad()
  }

  x = R * (lambda - lambda_0) * cos(standard_parallel)
  y = R * (phi - phi_0)

  list("x" = x, "y" = y)
}

#' @rdname projections
#' @export
winkel_bopc <- function(lambda,
                        phi,
                        R = 6378,
                        lambda_0 = 0,
                        phi_0 = 0,
                        standard_parallel = 0,
                        radians = FALSE) {
  if (!radians) {
    c(
      "lambda" = lambda,
      "phi" = phi,
      "lambda_0" = lambda_0,
      "phi_0" = phi_0,
      "standard_parallel" = standard_parallel
    ) |>
      projector::scope_deg2rad()
  }

  equirectangular <- projector::equirectangular(
    lambda = lambda,
    phi = phi,
    R = R,
    lambda_0 = lambda_0,
    phi_0 = phi_0,
    standard_parallel = standard_parallel,
    radians = TRUE
  )

  aitoff <- projector::aitorff(
    lambda = lambda,
    phi = phi,
    R = R,
    radians = TRUE
  )

  purrr::map2_dbl(equirectangular, aitoff, sum) / 2
}
