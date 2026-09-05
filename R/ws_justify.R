

#' @title Padded Whitespace for Left/Right Justification
#' 
#' @description
#' Padded whitespace for left/right justification.
#' 
#' @param x an R object of \link[base]{mode} \link[base]{character}
#' 
#' @seealso \link[base]{format.default}
#' 
#' @returns
#' The function [ws_justify()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples
#' ws_justify(c('a', 'abc', 'ab'))
#' ws_justify(matrix(c('a', 'abc', 'ab', 'abcd'), nrow = 2L))
#' 
#' @importFrom stringi stri_dup
#' @export
ws_justify <- function(x) {
  n_ <- charwidth(x)
  stri_dup(str = ' ', times = round(max(n_) - n_))
}


if (FALSE) {
  library(microbenchmark)
  library(stringr) # stringr::str_dup
  library(stringi) # stringi::stri_dup
  n = 1e3L
  microbenchmark(
    paste(rep(' ', times = n), collapse = ''), 
    str_dup(' ', times = n), 
    stri_dup(' ', times = n)
  )
}
