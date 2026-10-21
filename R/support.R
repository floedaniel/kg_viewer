# Data support of an occupied KG class = its number of 1 km cells, drawn as
# a lighter tint of the class colour when weaker. Breaks: Fisher natural
# breaks on log10(cells) over all occupied classes, 3 levels, rounded; of
# 3-5 levels tested the most stable under resampling (see
# R/support_bins.R and the vignette). Judgement values.
# Used by app.R, README.qmd and the vignette.

SUPPORT_BREAKS <- c(25, 200)
SUPPORT_NAMES  <- c("high", "medium", "low")
SUPPORT_LABELS <- paste0(SUPPORT_NAMES, "\n",
                         c(paste0(intToUtf8(0x2265), "200"),
                           paste0("25", intToUtf8(0x2013), "199"),
                           paste0("5", intToUtf8(0x2013), "24")))
SUPPORT_TINT   <- c(0, 0.45, 0.72)   # share of white mixed into the class colour

support_level <- function(n) 3L - findInterval(n, SUPPORT_BREAKS)   # 1 high .. 3 low

tint <- function(hex, w) {
  n   <- max(length(hex), length(w))
  hex <- rep_len(hex, n)
  w   <- rep_len(w, n)
  m <- sweep(col2rgb(hex), 2, 1 - w, "*") + outer(c(255, 255, 255), w)
  rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)
}
