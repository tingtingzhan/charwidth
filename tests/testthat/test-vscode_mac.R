
expect_no_error(object = {
  charwidth:::chk_utf8('\u8336', times = 7L, total = 14L) # Chinese
  charwidth:::chk_utf8('\u304a', times = 7L, total = 14L) # hiragana
  charwidth:::chk_utf8('\u30aa', times = 7L, total = 14L) # katakana
  charwidth:::chk_utf8('\U1f375', times = 7L, total = 14L) # emoji
  charwidth:::chk_utf8('\ub3c4', times = 7L, total = 14L) # hanga
})

