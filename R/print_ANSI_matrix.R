

#' @title Print \link[base]{matrix} with ANSI Escape Sequences
#' 
#' @description
#' Print \link[base]{matrix} with ANSI escape sequences in the body and/or \link[base]{dimnames}.
#' 
#' @param x \link[base]{character} \link[base]{matrix}
#' 
#' @returns
#' The function [print_ANSI_matrix()] does not have a returned value.
#' 
#' @note
#' The function [print_ANSI_matrix()] works only in RStudio, not in R Gui and Positron.
#' 
#' @seealso `colorDF::colorDF`
#' 
#' @importFrom cli ansi_strip
#' @export
print_ANSI_matrix <- function(x) {
  
  if (!is.matrix(x) || !is.character(x)) stop('only dealing with \'character\' \'matrix\', for now')
  
  # names(dimnames(x)) # not considered yet..
  
  d <- dim(x)
  dnm <- dimnames(x)
  dnm0 <- dnm |> lapply(FUN = ansi_strip)
  
  x_ <- array(x, dim = d, dimnames = NULL)
  x0 <- array(ansi_strip(x), dim = d, dimnames = NULL)
  
  if (!length(dnm)) { # no row-name, no col-name
    rnm <- NULL
    x1 <- x0 # ANSI-stripped body
  } else if (!length(dnm[[1L]])) { # no row-name
    rnm <- NULL
    x1 <- rbind(dnm0[[2L]], x0) # ANSI-stripped col-names AND body
  } else if (!length(dnm[[2L]])) { # no col-name
    rnm <- dnm0[[1L]] # ANSI-stripped row-names
    rnm_ANSI <- dnm[[1L]] # original row-names
    x1 <- x0 # ANSI-stripped body
  } else {
    rnm <- c('', dnm0[[1L]]) # ANSI-stripped row-names
    rnm_ANSI <- c('', dnm[[1L]]) # original row-names
    x1 <- rbind(dnm0[[2L]], x0) # ANSI-stripped col-names AND body
    x1_ANSI <- rbind(dnm[[2L]], x_)
  }
  
  rnm_prt <- if (length(rnm)) {
    paste0(rnm_ANSI, ws_justify(rnm))
  } # else NULL
  
  x_prt <- lapply(seq_len(d[2L]), FUN = \(i) {
    paste0(ws_justify(x1[,i]), x1_ANSI[,i])
  })
  
  prt <- .mapply(FUN = paste, dots = c(list(rnm_prt), x_prt), MoreArgs = list(collapse = ' '))
  lapply(prt, FUN = cat, sep = '\n')
  # ?cli::cli_text does not respect duplicate spaces, as of 2026-04-08
  # cli::cli_text('a           b')
  # lapply(prt, FUN = cli_text, sep = '\n')
  return(invisible())
  
}

