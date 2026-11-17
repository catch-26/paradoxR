# DESeq2's Cook's distances use a trimmed mean per gene and model cell,
# computed with apply(x, 1, mean, trim = ...). The replacement sorts every row
# in one radix order() and averages the middle columns. A row's value is used
# only where it provably equals mean(trim = ) bit for bit; every other row runs
# mean(trim = ) itself:
#
# * one middle value (three samples per cell): the order statistic itself;
# * 64-digit long double (x87 on x86_64 Linux, macOS and Windows): mean() and
#   rowMeans() both return the double nearest to the exact mean S / k up to an
#   error of a few long double units. The exact residual S / k - rowMeans() is
#   computed with error-free double-double sums, and a row is kept only when
#   S / k lies further than that error bound from a rounding boundary between
#   two doubles, so both functions round to the same double
#   (speedup_rounds_like_mean());
# * long double equal to double (53 digits, arm64 macOS) and two or three middle
#   values: mean()'s two-pass algorithm repeated in double; with two or three
#   middle values the order after sort.int(partial = c(lo, hi)) is the sorted
#   order, so the additions run in the same order.
#
# Everything else (NA, Inf, values above 2^1000, integers, other long double
# formats, more middle values on 53-digit platforms) returns NULL and apply()
# runs.

speedup_longdouble_digits <- function() {
  digits <- .Machine$longdouble.digits
  if (is.null(digits)) 53L else as.integer(digits)
}

speedup_row_trimmed_mean <- function(x, trim) {
  if (!is.matrix(x) || !is.double(x) || !is.numeric(trim) || length(trim) != 1L || is.na(trim)) {
    return(NULL)
  }
  n <- ncol(x)
  if (n == 0L || nrow(x) == 0L || !(trim > 0 && trim < 0.5) || !all(is.finite(x)) || max(abs(x)) >= 2^1000) {
    return(NULL)
  }
  lo <- floor(n * trim) + 1
  hi <- n + 1 - lo
  k <- hi - lo + 1
  digits <- speedup_longdouble_digits()
  if (!(k == 1 || digits == 64L || (digits == 53L && k <= 3))) {
    return(NULL)
  }
  o <- order(row(x), x, method = "radix")
  middle <- matrix(x[o], nrow(x), n, byrow = TRUE)[, lo:hi, drop = FALSE]
  value <- if (k == 1) {
    middle[, 1L]
  } else if (digits == 64L) {
    value <- rowMeans(middle)
    slow <- !speedup_rounds_like_mean(middle, value)
    if (any(slow)) {
      value[slow] <- apply(x[slow, , drop = FALSE], 1, mean, trim = trim)
    }
    value
  } else {
    s <- middle[, 1L]
    for (j in seq.int(2L, k)) s <- s + middle[, j]
    s <- s / k
    t <- middle[, 1L] - s
    for (j in seq.int(2L, k)) t <- t + (middle[, j] - s)
    s + t / k
  }
  names(value) <- dimnames(x)[[1L]]
  value
}

# TRUE for rows where mean() of the k middle values returns `value`, the
# rowMeans() result, on a 64-digit long double platform. With u = 2^-64, A the
# sum of |x_i| and D the sum of |x_i - value|:
#
# * rowMeans() and mean()'s first pass add k values in long double, which is
#   within (k - 1) u A of the exact sum S, then divide by k;
# * mean()'s second pass adds the k differences x_i - s, each partial sum at
#   most D, so it adds S / k - s with an error of at most 2 u D;
#
# so both end within 2^-61 (D + |value|) + 2^-62 A of S / k before the final
# rounding to double. The exact sum S is kept as a double-double (hi, lo) by
# TwoSum, `value * k` is split exactly (Veltkamp), and the row is kept when
# |S / k - value| plus that bound stays below half the spacing of doubles at
# `value`: then every number within the bound rounds to `value`.
speedup_rounds_like_mean <- function(middle, value) {
  k <- ncol(middle)
  hi <- middle[, 1L]
  lo <- numeric(length(hi))
  abs_sum <- abs(hi)
  for (j in seq.int(2L, k)) {
    b <- middle[, j]
    s <- hi + b
    bb <- s - hi
    lo <- lo + ((hi - (s - bb)) + (b - bb))
    hi <- s
    abs_sum <- abs_sum + abs(b)
  }
  spread <- abs(middle[, 1L] - value)
  for (j in seq.int(2L, k)) spread <- spread + abs(middle[, j] - value)
  split <- 134217729 * value
  value_hi <- split - (split - value)
  value_lo <- value - value_hi
  residual <- ((hi - k * value_hi) + (lo - k * value_lo)) / k
  size <- abs(value)
  e <- floor(log2(size))
  e <- e - (2^e > size) + (2^(e + 1) <= size)
  half <- ifelse(size == 2^e, 2^(e - 54), 2^(e - 53))
  bound <- 2^-61 * (spread + size) + 2^-62 * abs_sum + 2^-70 * size
  ok <- size >= 2^-1000 & abs(residual) + bound < half
  ok[size == 0] <- abs_sum[size == 0] == 0
  ok
}

speedup_trimmed_rows <- function(x, trim) {
  value <- speedup_row_trimmed_mean(x, trim)
  if (is.null(value)) {
    value <- apply(x, 1, mean, trim = trim)
  }
  value
}

speedup_is_trimmed_apply <- function(expr) {
  is.call(expr) && length(expr) == 5L &&
    identical(expr[[1L]], quote(apply)) &&
    identical(expr[[3L]], 1) &&
    identical(expr[[4L]], quote(mean)) &&
    identical(names(expr)[5L], "trim")
}

speedup_swap_trimmed_apply <- function(expr) {
  if (missing(expr)) {
    return(quote(expr = ))
  }
  if (speedup_is_trimmed_apply(expr)) {
    return(call("speedup_trimmed_rows", speedup_swap_trimmed_apply(expr[[2L]]), speedup_swap_trimmed_apply(expr[[5L]])))
  }
  if (is.call(expr)) {
    return(as.call(lapply(as.list(expr), speedup_swap_trimmed_apply)))
  }
  expr
}

speedup_make_trimmed <- function(original) {
  new_body <- speedup_swap_trimmed_apply(body(original))
  if (identical(new_body, body(original))) {
    return(original)
  }
  fast <- original
  env <- new.env(parent = environment(original))
  env$speedup_trimmed_rows <- speedup_trimmed_rows
  body(fast) <- new_body
  environment(fast) <- env
  fast
}
