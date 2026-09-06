
#' @title An Alternative \link[base]{nchar}
#' 
#' @param x \link[base]{character} \link[base]{vector}
#' 
#' @examples
#' (x = c('tea\u8336\ub2e4\ub3c4\U1f375\u207a'))
#' charwidth(x)
#' # RGui: 10.81754 * 11 # approx 119
#' # RStudio: 10.21905 * 9 # approx 92L
#' # Positron: 10.1952 * 15 # approx 153
#' \dontrun{
#' library(stringi)
#' cat(stri_dup(x, 11L), stri_dup('a', 119L), sep = '\n') # RGui
#' cat(stri_dup(x, 9L), stri_dup('a', 92L), sep = '\n') # RStudio
#' cat(stri_dup(x, 15L), stri_dup('a', 153L), sep = '\n') # Positron
#' }
#' 
#' x |>
#'  cli::col_red() |>
#'  charwidth()
#' @importFrom cli ansi_strip
#' @importFrom stringr boundary str_split str_detect
#' @export
charwidth <- \(x) {
  x |>
    ansi_strip() |>
    str_split(pattern = boundary(type = 'character')) |>
    vapply(FUN = .charwidth, FUN.VALUE = NA_real_)
}


# `x` is after ?stringr::str_split
#' @importFrom stringi stri_enc_mark
#' @importFrom stringr str_detect
# @importFrom emoji emoji_detect
.charwidth <- \(x) {
  
  enc <- stri_enc_mark(x)
  if (!all(enc %in% c('ASCII', 'UTF-8'))) stop('unknown encoding')
  
  utf8 <- (enc == 'UTF-8')
  utf_cj <- x |>
    str_detect(pattern = '\\p{Han}|\\p{Hiragana}|\\p{Katakana}')
  utf_k <- x |>
    str_detect(pattern = '\\p{Hangul}')
  emoji <- x |> str_detect(pattern = '\\p{So}') # Symbols, Other (includes many emojis)
  #emoji <- x |> emoji_detect()
  
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



#' @importFrom cli col_red style_bold
#' @importFrom stringi stri_dup
.check_width <- \(x, times, total) {
  sprintf('%d/%d\n', total, times) |>
    col_red() |>
    style_bold() |>
    cat()
  cat(
    stri_dup(str = x, times = times), 
    stri_dup(str = 'a', times = total), 
    sep = '\n')
}



