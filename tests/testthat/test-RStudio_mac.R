

expect_no_error(object = {
  charwidth:::chk_utf8('\u8336', times = 60L, total = 100L) # Chinese
  charwidth:::chk_utf8('\u304a', times = 60L, total = 100L) # hiragana
  charwidth:::chk_utf8('\u30aa', times = 60L, total = 100L) # katakana
  charwidth:::chk_utf8('\ub3c4', times = 70L, total = 101L) # hanga
  charwidth:::chk_utf8('\U1f375', times = 60L, total = 100L) # emoji
})



