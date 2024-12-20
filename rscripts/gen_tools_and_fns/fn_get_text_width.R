fn_get_text_width <- function(txt, font, font_size = 8, units = "inches") {
  
  f <- "mono"
  if (tolower(font) == "arial")
    f <- "sans"
  else if (tolower(font) == "times")
    f <- "serif"
  
  R.devices::devEval("nulldev", {
    par(family = f, ps = font_size)
    ret <- strwidth(txt, units = units) 
  })
  
  return(ret)
}

