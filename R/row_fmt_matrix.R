

#' @title Format \link[base]{matrix} by Rows
#' 
#' @description
#' Format \link[base]{matrix} by rows.
#' 
#' @param x \link[base]{character} \link[base]{matrix}
#' 
#' @param rownm_justify \link[base]{character} scalar, default value is `'left'`, see the function [ws_fmt()]
#' 
#' @param justify \link[base]{character} scalar, default value is `'right'`, see the function [ws_fmt()]
#' 
#' @returns
#' The function [row_fmt_matrix()] returns a \link[base]{character} \link[base]{vector} of the formatted **rows**.
#' 
#' @examples
#' VADeaths |> row_fmt_matrix() |> cat(sep = '\n')
#' volcano[1:5, 1:5] |> row_fmt_matrix() |> cat(sep = '\n')
#' 
#' x = VADeaths
#' colnames(x) = colnames(x) |>
#'  gsub(pattern = ' ', replacement = '\n')
#' x |> row_fmt_matrix() |> cat(sep = '\n')
#' 
#' @seealso \link[base]{print.default} for \link[base]{matrix}
#' 
#' @keywords format matrix row unicode justify
#' @importFrom cli ansi_strip format_inline
#' @export
row_fmt_matrix <- \(x, rownm_justify = 'left', justify = 'right') {
  
  if (!is.matrix(x)) stop('input must be matrix')
  if (!is.character(x)) {
    storage.mode(x) <- 'character'
  }
  
  # names(dimnames(x)) # not considered yet..
  
  dnm <- dimnames(x)
  
  # allow '\n' in colnames `dnm[[2L]]`
  cnm <- if (length(dnm[[2L]]) && any(grepl(pattern = '\\n', x = dnm[[2L]]))) {
    dnm[[2L]] |>
      strsplit(split = '\n') |>
      aug2equilen() |>
      c(list(f = c)) |>
      do.call(what = 'Map', args = _) |>
      unname() |>
      do.call(what = 'rbind', args = _)
  } else dnm[[2L]] # len0 compatible
  
  if (!length(dnm)) { # no row-name, no col-name
    rnm <- NULL
    x1 <- x
  } else if (!length(dnm[[1L]])) { # no row-name
    rnm <- NULL
    x1 <- rbind(cnm, x)
  } else if (!length(cnm)) { # no col-name
    rnm <- dnm[[1L]] # original row-names
    x1 <- x
  } else { # both row-name and col-name
    rnm <- c(
      rep('', times = if (is.matrix(cnm)) nrow(cnm) else 1L),
      dnm[[1L]]
    )
    x1 <- rbind(cnm, x)
  }
  
  rnm_j <- if (length(rnm)) {
    ws_fmt(x = rnm, justify = rownm_justify)
  } # else NULL
  
  x_j <- x1 |>
    apply(MARGIN = 2L, FUN = ws_fmt, justify = justify, simplify = FALSE)
  
  z <- c(list(rnm_j), x_j)
  
  .mapply(
    FUN = paste, 
    dots = z[lengths(z) > 0L], 
    MoreArgs = list(collapse = ' ')
  ) |>
    vapply(FUN = format_inline, keep_whitespace = TRUE, FUN.VALUE = '')
  
}



if (FALSE) {
  (x = list(letters[1L], letters[2:3], letters[4:6]))
  aug2equilen(x)
}
# @param x a \link[base]{list} of \link[base]{character} \link[base]{vector}s
aug2equilen <- \(x) {
  
  ns <- lengths(x)
  
  mapply(FUN = \(x, k) {
    c(x, rep('', times = k))
  }, x = x, k = max(ns) - ns, SIMPLIFY = FALSE)
  
}



