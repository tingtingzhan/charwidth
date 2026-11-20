
expect_no_error(object = {
  chk_utf8('\u8336', times = 84L, total = 140) # Chinese
  chk_utf8('\u304a', times = 84L, total = 140) # hiragana
  chk_utf8('\u30aa', times = 84L, total = 140) # katakana
  chk_utf8('\U1f375', times = 60L, total = 136) # emoji
  chk_utf8('\ub3c4', times = 95L, total = 137) # hanga
})

