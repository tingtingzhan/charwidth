
#' @title An Alternative \link[base]{nchar}
#' 
#' @param x \link[base]{character} \link[base]{vector}
#' 
#' @examples
#' (x = c('tea\u8336\ub2e4\ub3c4\U1f375\u207a'))
#' charwidth(x) 
#' # 10.21905 in RStudio
#' \dontrun{
#' 10.21905 * 9 # approx 92L
#' library(stringi)
#' cat(stri_dup(x, 9L), stri_dup('a', 92L), sep = '\n')
#' }
#' 
#' @importFrom stringr boundary str_split str_detect
#' @export
charwidth <- \(x) {
  x |>
    str_split(pattern = boundary(type = 'character')) |>
    vapply(FUN = .charwidth, FUN.VALUE = NA_real_)
}


# `x` is after ?stringr::str_split
#' @importFrom stringi stri_enc_mark
#' @importFrom stringr str_detect
.charwidth <- \(x) {
  
  enc <- stri_enc_mark(x)
  if (!all(enc %in% c('ASCII', 'UTF-8'))) stop('unknown encoding')
  
  utf8 <- (enc == 'UTF-8')
  utf_cj <- x |>
    str_detect(pattern = '\\p{Han}|\\p{Hiragana}|\\p{Katakana}')
  utf_k <- x |>
    str_detect(pattern = '\\p{Hangul}')
  emoji <- x |>
    str_detect(pattern = '\\p{Emoji}')
  
  if (Sys.getenv('RSTUDIO') == '1') { # RStudio
    sum(!utf8) + 
      sum(utf8 & !utf_cj & !utf_k & !emoji) +
      sum(utf_cj | emoji) * .RStudio_cj_emoji +
      sum(utf_k) * .RStudio_kr
  } else if (Sys.getenv('POSITRON') == '1') {
    sum(!utf8) + 
      sum(utf8 & !utf_cj & !utf_k & !emoji) +
      sum(utf_cj | emoji) * .Positron_cj_emoji +
      sum(utf_k) * .Positron_kr
  } else { # Rgui
    sum(!utf8) + 
      sum(utf8 & !utf_cj & !utf_k & !emoji) +
      sum(utf_cj) * .Rgui_cj +
      sum(utf_k) * .Rgui_kr + 
      sum(emoji) * .Rgui_emoji
  }
}







