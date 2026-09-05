
#' @title An Alternative \link[base]{nchar}
#' 
#' @param x \link[base]{character} \link[base]{vector}
#' 
#' @examples
#' c('tea\U1f375', '\U1f1fa\U1f1f8 and \U1f1e8\U1f1e6') |>
#'   charwidth()
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
  cjk <- x |>
    str_detect(pattern = '\\p{Han}|\\p{Hiragana}|\\p{Katakana}|\\p{Hangul}')
  emoji <- x |>
    str_detect(pattern = '\\p{Emoji}')
  
  if (Sys.getenv('RSTUDIO') == '1') { # RStudio
    sum(!utf8) + 
      sum(utf8 & !cjk & !emoji) +
      sum(cjk | emoji) * .RStudio_cjk_emoji
  } else { # Rgui
    sum(!utf8) + 
      sum(utf8 & !cjk & !emoji) +
      sum(cjk) * .Rgui_CJK + 
      sum(emoji) * .Rgui_emoji
  }
}







