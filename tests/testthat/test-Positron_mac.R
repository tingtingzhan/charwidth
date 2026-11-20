
expect_no_error(object = {
  chk_utf8('\u8336', times = 85L, total = 141L) # Chinese
  chk_utf8('\u304a', times = 85L, total = 141L) # hiragana
  chk_utf8('\u30aa', times = 85L, total = 141) # katakana
  chk_utf8('\U1f375', times = 85L, total = 141) # emoji
  chk_utf8('\ub3c4', times = 98L, total = 141) # hanga
})

