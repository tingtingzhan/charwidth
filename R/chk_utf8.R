
#' @title Check UTF-8 Symbol Width in IDE
#' 
#' @param x \link[base]{character} scalar
#' 
#' @param times,total \link[base]{integer} scalars
#' 
#' @examples
#' # RStudio
#' chk_utf8('\u8336', times = 60L, total = 100L) # Chinese
#' chk_utf8('\u304a', times = 60L, total = 100L) # hiragana
#' chk_utf8('\u30aa', times = 60L, total = 100L) # katakana
#' chk_utf8('\ub3c4', times = 70L, total = 101L) # hanga
#' chk_utf8('\U1f375', times = 60L, total = 100L) # emoji
#' # Positron, mac default
#' chk_utf8('\u8336', times = 85L, total = 141L) # Chinese
#' chk_utf8('\u304a', times = 85L, total = 141L) # hiragana
#' chk_utf8('\u30aa', times = 85L, total = 141) # katakana
#' chk_utf8('\U1f375', times = 85L, total = 141) # emoji
#' chk_utf8('\ub3c4', times = 98L, total = 141) # hanga
#' # RGui, mac default
#' chk_utf8('\u8336', times = 84L, total = 140) # Chinese
#' chk_utf8('\u304a', times = 84L, total = 140) # hiragana
#' chk_utf8('\u30aa', times = 84L, total = 140) # katakana
#' chk_utf8('\U1f375', times = 60L, total = 136) # emoji
#' chk_utf8('\ub3c4', times = 95L, total = 137) # hanga
#' 
#' # not recognized by any rendering
#' chk_utf8('\u2009', times = 100L, total = 100L) # thin space
#' chk_utf8('\u202f', times = 100L, total = 100L) # narrow no-break space
#' 
#' @keywords unicode utf8 character rendered width visualize
#' @importFrom cli col_red style_bold
#' @importFrom stringi stri_dup
#' @export
chk_utf8 <- \(x, times, total) {
  sprintf('%d/%d\n', total, times) |>
    col_red() |>
    style_bold() |>
    cat()
  cat(
    stri_dup(str = x, times = times) |>
      sprintf(fmt = '%s\u220e'), 
    stri_dup(str = '-', times = total) |>
      sprintf(fmt = '%s\u220e'), 
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



