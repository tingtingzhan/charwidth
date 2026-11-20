
expect_no_error(object = {
  chk_utf8('\u8336', times = 7L, total = 14L) # Chinese
  chk_utf8('\u304a', times = 7L, total = 14L) # hiragana
  chk_utf8('\u30aa', times = 7L, total = 14L) # katakana
  chk_utf8('\U1f375', times = 7L, total = 14L) # emoji
  chk_utf8('\ub3c4', times = 7L, total = 14L) # hanga
})

