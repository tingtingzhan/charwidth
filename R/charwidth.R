
#' @title Rendered Width of Unicode \link[base]{character}s
#' 
#' @param x \link[base]{character} \link[base]{vector}
#' 
#' @examples
#' (x = c('tea\u8336\ub2e4\ub3c4\U1f375\u207a'))
#' charwidth(x)
#'  
#' # RGui: 10.81754 * 11 # approx 119
#' chk_utf8(x, times = 11L, total = 119L)
#' 
#' # RStudio: 10.21905 * 9 # approx 92L
#' chk_utf8(x, times = 9L, total = 92L)
#' 
#' # Positron: 10.1952 * 15 # approx 153
#' chk_utf8(x, times = 15L, total = 153L)
#' 
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



