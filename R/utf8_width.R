
#' @title UTF-8 Symbol Width in IDE
#' 
#' @param x \link[base]{character} scalar
#' 
#' @param times,total \link[base]{integer} scalars
#' 
#' @examples
#' # RStudio
#' utf8_width('\u8336', times = 60L, total = 100L) # Chinese
#' utf8_width('\u304a', times = 60L, total = 100L) # hiragana
#' utf8_width('\u30aa', times = 60L, total = 100L) # katakana
#' utf8_width('\ub3c4', times = 70L, total = 101L) # hanga
#' utf8_width('\U1f375', times = 60L, total = 100L) # emoji
#' # Positron, mac default
#' utf8_width('\u8336', times = 85L, total = 141L) # Chinese
#' utf8_width('\u304a', times = 85L, total = 141L) # hiragana
#' utf8_width('\u30aa', times = 85L, total = 141) # katakana
#' utf8_width('\U1f375', times = 85L, total = 141) # emoji
#' utf8_width('\ub3c4', times = 98L, total = 141) # hanga
#' # RGui, mac default
#' utf8_width('\u8336', times = 84L, total = 140) # Chinese
#' utf8_width('\u304a', times = 84L, total = 140) # hiragana
#' utf8_width('\u30aa', times = 84L, total = 140) # katakana
#' utf8_width('\U1f375', times = 60L, total = 136) # emoji
#' utf8_width('\ub3c4', times = 95L, total = 137) # hanga
#' 
#' @importFrom cli col_red style_bold
#' @importFrom stringi stri_dup
#' @export
utf8_width <- \(x, times, total) {
  sprintf('%d/%d\n', total, times) |>
    col_red() |>
    style_bold() |>
    cat()
  cat(
    stri_dup(str = x, times = times), 
    stri_dup(str = '-', times = total), 
    sep = '\n')
}



w_cj <- \() {
  if (Sys.getenv('RSTUDIO') == '1') {
    100/60
  } else if (Sys.getenv('POSITRON') == '1') {
    141/85
  } else { # Rgui
    140/84
  }
}

w_emoji <- \() {
  if (Sys.getenv('RSTUDIO') == '1') {
    100/60
  } else if (Sys.getenv('POSITRON') == '1') {
    141/85
  } else { # Rgui
    136/60
  }
}

w_kr <- \() {
  if (Sys.getenv('RSTUDIO') == '1') {
    101/70
  } else if (Sys.getenv('POSITRON') == '1') {
    141/98
  } else { # Rgui
    137/95
  }
}



