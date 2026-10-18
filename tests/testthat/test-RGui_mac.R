
expect_no_error(object = {
  charwidth:::chk_utf8('\u8336', times = 84L, total = 140) # Chinese
  charwidth:::chk_utf8('\u304a', times = 84L, total = 140) # hiragana
  charwidth:::chk_utf8('\u30aa', times = 84L, total = 140) # katakana
  charwidth:::chk_utf8('\U1f375', times = 60L, total = 136) # emoji
  charwidth:::chk_utf8('\ub3c4', times = 95L, total = 137) # hanga
})

