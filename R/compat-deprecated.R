#' Deprecated function names
#'
#' These names still work and call their replacements:
#'
#' * `use_go_cache()` is now [use_speedups()];
#' * `reset_go_cache()` is now [reset_speedups()];
#' * `clear_go_cache()` is now [clear_cache()].
#'
#' @return The return value of the replacement.
#' @name paradoxR-deprecated
#' @keywords internal
NULL

#' @rdname paradoxR-deprecated
#' @export
use_go_cache <- function() {
  use_speedups()
}

#' @rdname paradoxR-deprecated
#' @export
reset_go_cache <- function() {
  reset_speedups()
}

#' @rdname paradoxR-deprecated
#' @export
clear_go_cache <- function() {
  clear_cache()
}
