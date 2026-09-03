
#' @title An Alternative \link[base]{nchar}
#' 
#' @param x \link[base]{character} \link[base]{vector}
#' 
#' @examples
#' x10 = '\u4e00\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u96f6'
#' x20 = '1234567890'
#' cat(x10, '\n', x20, sep = '')
#' x1 = x10 |> substr(start = 1L, stop = 6L) |> stringi::stri_dup(times = 20L)
#' x2 = x20 |> stringi::stri_dup(times = 20L)
#' cat(x1, '\n', x2, sep = '')
#' 
#' x = c('tea\U1f375', '\U1f1fa\U1f1f8 and \U1f1e8\U1f1e6')
#' charwidth(x)
#' 
#' @importFrom stringi stri_enc_mark
#' @importFrom stringr boundary str_split str_detect
#' @export
charwidth <- \(x) {
  x |>
    str_split(pattern = boundary(type = 'character')) |>
    vapply(FUN = \(i) {
      enc <- stri_enc_mark(i)
      if (!all(enc %in% c('ASCII', 'UTF-8'))) stop('unknown encoding')
      utf8 <- (enc == 'UTF-8')
      if (Sys.getenv('RSTUDIO') == '1') {
        sum(!utf8) + sum(utf8) * .RStudio_utf8
      } else {
        cjk <- str_detect(i, pattern = '\\p{Han}') | 
          str_detect(i, pattern = '\\p{Hiragana}') | 
          str_detect(i, pattern = '\\p{Katakana}') | 
          str_detect(i, pattern = '\\p{Hangul}')
        emoji <- str_detect(i, pattern = '\\p{Emoji}')
        if (!all((cjk | emoji)[utf8])) stop('any other symbol?')
        sum(!utf8) + sum(cjk)*.Rgui_CJK + sum(emoji)*.Rgui_emoji
      }
    }, FUN.VALUE = NA_real_)
}








