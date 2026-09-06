

#' @title An Alternative Print of \link[base]{matrix} by \link[base]{cat}
#' 
#' @description
#' An alternative print of \link[base]{matrix} by \link[base]{cat}.
#' 
#' @param x \link[base]{character} \link[base]{matrix}
#' 
#' @param rownm_justify \link[base]{character} scalar, default value is `'left'`, see the function [ws_fmt()]
#' 
#' @param justify \link[base]{character} scalar, default value is `'right'`, see the function [ws_fmt()]
#' 
#' @returns
#' The function [cat_matrix()] does not have a returned value.
#' 
#' @export
cat_matrix <- function(x, rownm_justify = 'left', justify = 'right') {
  
  if (!is.matrix(x) || !is.character(x)) stop('only dealing with \'character\' \'matrix\', for now')
  
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
    lapply(FUN = cat, sep = '\n')
  
  return(invisible())
  
}


if (FALSE) {
  # ?cli::cli_text does not respect duplicate spaces, as of 2026-04-08
  cli::cli_text('a           b')
}
