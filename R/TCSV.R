readTSVstring <- function(text, ...) {
  dfr <- read.delim(tc <- textConnection(text), ...)
  close(tc)
  dfr
}

readCSVstring <- function(text, ...) {
  dfr <- read.csv(tc <- textConnection(text), ...)
  close(tc)
  dfr
}
