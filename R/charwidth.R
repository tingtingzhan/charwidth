
#' @title Rendered Width of Unicode \link[base]{character}s
#' 
#' @description
#' To find the rendered width of various Unicode \link[base]{character}s in RGui and IDEs.
#' 
#' 
#' @param x \link[base]{character} scalar or \link[base]{vector}
#' 
#' @examples
#' (x = c('tea\u8336\ub2e4\ub3c4\U1f375\u207a'))
#' charwidth(x)
#' x |>
#'  cli::col_red() |>
#'  charwidth()
#'  
#' @keywords unicode utf8 character rendered width
#' @importFrom cli ansi_strip format_inline
#' @importFrom stringr boundary str_split str_detect
#' @export
charwidth <- \(x) {
  x |>
    #format_inline(collapse = FALSE) |> # `collapse` is not working (in the way tzh anticipates) 
    vapply(FUN = format_inline, FUN.VALUE = '') |>
    ansi_strip() |>
    str_split(pattern = boundary(type = 'character')) |>
    vapply(FUN = .charwidth, FUN.VALUE = NA_real_)
}


#' @importFrom stringi stri_enc_mark
#' @importFrom stringr str_detect
.charwidth <- \(x) {
  
  # `x` is after ?stringr::str_split
  
  enc <- stri_enc_mark(x)
  if (!all(enc %in% c('ASCII', 'UTF-8'))) stop('unknown encoding')
  
  utf8 <- (enc == 'UTF-8')
  utf_cj <- x |>
    str_detect(pattern = '\\p{Han}|\\p{Hiragana}|\\p{Katakana}')
  utf_k <- x |>
    str_detect(pattern = '\\p{Hangul}')
  emoji <- x |> 
    str_detect(pattern = '\\p{So}') # Symbols, Other (includes many emojis)
  
  sum(!utf8) + 
    sum(utf8 & !utf_cj & !utf_k & !emoji) +
    sum(utf_cj) * w_cj() +
    sum(utf_k) * w_kr() + 
    sum(emoji) * w_emoji()
  
}









w_cj <- \() {
  if (Sys.getenv('RSTUDIO') == '1') {
    100/60
  } else if (Sys.getenv('POSITRON') == '1') {
    141/85
  } else if (Sys.getenv('TERM_PROGRAM') == 'vscode') {
    1/2
  } else { # Rgui
    140/84
  }
}

w_emoji <- \() {
  if (Sys.getenv('RSTUDIO') == '1') {
    100/60
  } else if (Sys.getenv('POSITRON') == '1') {
    141/85
  } else if (Sys.getenv('TERM_PROGRAM') == 'vscode') {
    1/2
  } else { # Rgui
    136/60
  }
}

w_kr <- \() {
  if (Sys.getenv('RSTUDIO') == '1') {
    101/70
  } else if (Sys.getenv('POSITRON') == '1') {
    141/98
  } else if (Sys.getenv('TERM_PROGRAM') == 'vscode') {
    1/2
  } else { # Rgui
    137/95
  }
}





# @title Check UTF-8 Symbol Width in IDE
# 
# @param x \link[base]{character} scalar
# @param times,total \link[base]{integer} scalars
# 
# @examples
# # not recognized by any rendering
# chk_utf8('\u2009', times = 100L, total = 100L) # thin space
# chk_utf8('\u202f', times = 100L, total = 100L) # narrow no-break space
#' @importFrom stringi stri_dup
chk_utf8 <- \(x, times, total) {
  sprintf('%d/%d\n', total, times) |>
    cat()
  cat(
    stri_dup(str = x, times = times) |>
      sprintf(fmt = '%s\u220e'), 
    stri_dup(str = '-', times = total) |>
      sprintf(fmt = '%s\u220e'), 
    sep = '\n')
}
