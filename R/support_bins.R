# How many data-support levels, and where to break? Compares 3, 4 and 5
# levels of occupied-class cell counts, breaks by quantile (equal count) and
# Fisher natural breaks on log10(cells), each also rounded to readable
# numbers. Scores:
#   GVF          goodness of variance fit on log10(cells) (1 = perfect)
#   stability    share of bootstrap resamples (per species, multinomial over
#                its cells) in which a class keeps its level; falling below
#                the occupancy rule counts as a change
#   stabNorway   same, for the classes of Norway's landscape (Dfc, Dfb, Cfb)
#   weakestLevel stability of the least stable level
library(DBI)
library(dplyr)

compare_support_bins <- function(db, B = 500, seed = 1,
                                 min_cells = 5, min_share = 0.001,
                                 norway = c("Dfc", "Dfb", "Cfb")) {
  con <- dbConnect(RSQLite::SQLite(), db, flags = RSQLite::SQLITE_RO)
  pk  <- dbGetQuery(con, "SELECT eppoCode, kgCode, nCells FROM pestKgClasses")
  dbDisconnect(con)

  is_occ <- function(n) n >= min_cells & n >= min_share * sum(n)
  pk  <- pk %>% group_by(eppoCode) %>% mutate(occ = is_occ(nCells)) %>% ungroup()
  occ <- filter(pk, occ)
  x   <- log10(occ$nCells)

  nice_vals <- c(6, 8, 10, 12, 15, 20, 25, 30, 40, 50, 60, 75, 100, 125, 150, 200,
                 250, 300, 400, 500, 750, 1000, 1500, 2000, 3000, 5000)
  nice <- function(b) sapply(b, function(v) nice_vals[which.min(abs(log(nice_vals) - log(v)))])

  breaks_for <- function(k, method) {
    if (method == "quantile") {
      quantile(occ$nCells, (1:(k - 1)) / k, type = 1, names = FALSE)
    } else {
      ceiling(10^classInt::classIntervals(x, k, style = "fisher")$brks[2:k])
    }
  }
  level <- function(n, b) ifelse(n < min_cells, 0L, findInterval(n, b) + 1L)

  gvf <- function(b) {
    g <- level(occ$nCells, b)
    1 - sum(tapply(x, g, function(v) sum((v - mean(v))^2))) / sum((x - mean(x))^2)
  }

  # Resampled counts per occupied class; 0 = not occupied in that resample
  set.seed(seed)
  boot <- pk %>%
    group_by(eppoCode) %>%
    group_modify(function(d, key) {
      N <- sum(d$nCells)
      m <- rmultinom(B, N, d$nCells / N)               # classes x B
      occ_b <- apply(m, 2, is_occ)
      tibble(kgCode = d$kgCode, orig = d$nCells, occ = d$occ,
             counts = split(m * occ_b, row(m)))
    }) %>%
    ungroup() %>%
    filter(occ)

  score <- function(b) {
    ag  <- mapply(function(n0, cnt) mean(level(cnt, b) == level(n0, b)),
                  boot$orig, boot$counts)
    lev <- level(boot$orig, b)
    tibble(GVF = gvf(b), stability = mean(ag),
           stabNorway = mean(ag[boot$kgCode %in% norway]),
           weakestLevel = min(tapply(ag, lev, mean)),
           perLevel = paste(table(factor(lev, seq_len(length(b) + 1))), collapse = " / "))
  }

  label <- function(b) {
    lo <- c(min_cells, b); hi <- c(b - 1, Inf)
    paste(ifelse(is.infinite(hi), paste0(lo, "+"), paste0(lo, "-", hi)), collapse = ", ")
  }

  run <- function(k, method, b) bind_cols(tibble(levels = k, method = method, breaks = label(b)),
                                          score(b))
  bind_rows(
    lapply(3:5, function(k) bind_rows(lapply(c("quantile", "fisher"), function(m) {
      b <- breaks_for(k, m)
      bind_rows(run(k, m, b), run(k, paste(m, "rounded"), nice(b)))
    })))
  )
}
