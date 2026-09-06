

#' @title Padded Whitespace for Left/Right Justification
#' 
#' @description
#' Padded whitespace for left/right justification.
#' 
#' @param x an R object of \link[base]{mode} \link[base]{character}
#' 
#' @param justify \link[base]{character} scalar, only options are `'left'` and `'right'`, see the function \link[base]{format.default}
#' 
#' @seealso \link[base]{format.default}
#' 
#' @returns
#' The function [ws_fmt()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples
#' ws_fmt(c('a', 'abc', 'ab'))
#' ws_fmt(c('a', 'abc', 'ab'), justify = 'right')
#' 
#' @importFrom stringi stri_dup
#' @export
ws_fmt <- function(x, justify = c('left', 'right')) {
  n_ <- charwidth(x)
  ws <- stri_dup(str = ' ', times = round(max(n_) - n_))
  switch(match.arg(justify), left = {
    paste0(x, ws)
  }, right = {
    paste0(ws, x)
  })
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
