
expect_no_error(object = {
  (x = c('tea\u8336\ub2e4\ub3c4\U1f375\u207a'))
  charwidth:::chk_utf8(x, times = 11L, total = 119L) # RGui, mac
  charwidth:::chk_utf8(x, times = 9L, total = 92L) # RStudio, mac
  charwidth:::chk_utf8(x, times = 15L, total = 153L) # Positron, mac
})

