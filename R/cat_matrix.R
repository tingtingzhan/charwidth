

#' @title An Alternative Print of \link[base]{matrix} by \link[base]{cat}
#' 
#' @description
#' An alternative print of \link[base]{matrix} by \link[base]{cat}.
#' 
#' @param x \link[base]{character} \link[base]{matrix}
#' 
#' @returns
#' The function [cat_matrix()] does not have a returned value.
#' 
#' @export
cat_matrix <- function(x) {
  
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
  
  rnm_prt <- if (length(rnm)) {
    paste0(rnm, ws_justify(rnm))
  } # else NULL
  
  x_prt <- ncol(x1) |>
    seq_len() |>
    lapply(FUN = \(i) {
      paste0(ws_justify(x1[,i]), x1[,i])
    })
  
  .mapply(
    FUN = paste, 
    dots = c(list(rnm_prt), x_prt), 
    MoreArgs = list(collapse = ' ')
  ) |>
    lapply(FUN = cat, sep = '\n')
  
  return(invisible())
  
}


if (FALSE) {
  # ?cli::cli_text does not respect duplicate spaces, as of 2026-04-08
  cli::cli_text('a           b')
}
