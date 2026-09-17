

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
#' VADeaths |> row_fmt_matrix()
#' 
#' @seealso \link[base]{print.default} for \link[base]{matrix}
#' 
#' @keywords format matrix row unicode justify
#' @importFrom cli ansi_strip format_inline
#' @export
row_fmt_matrix <- function(x, rownm_justify = 'left', justify = 'right') {
  
  if (!is.matrix(x)) stop('input must be matrix')
  if (!is.character(x)) {
    storage.mode(x) <- 'character'
  }
  
  # names(dimnames(x)) # not considered yet..
  
  dnm <- dimnames(x)
  
  if (!length(dnm)) { # no row-name, no col-name
    rnm <- NULL
    x1 <- x
  } else if (!length(dnm[[1L]])) { # no row-name
    rnm <- NULL
    x1 <- rbind(dnm[[2L]], x)
  } else if (!length(dnm[[2L]])) { # no col-name
    rnm <- dnm[[1L]] # original row-names
    x1 <- x
  } else { # both row-name and col-name
    rnm <- c('', dnm[[1L]])
    x1 <- rbind(dnm[[2L]], x)
  }
  
  rnm_j <- if (length(rnm)) {
    ws_fmt(x = rnm, justify = rownm_justify)
  } # else NULL
  
  x_j <- x1 |>
    apply(MARGIN = 2L, FUN = ws_fmt, justify = justify, simplify = FALSE)
  
  .mapply(
    FUN = paste, 
    dots = c(list(rnm_j), x_j), 
    MoreArgs = list(collapse = ' ')
  ) |>
    vapply(FUN = format_inline, keep_whitespace = TRUE, FUN.VALUE = '')
  
}


