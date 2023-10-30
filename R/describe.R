
library(dplyr)

describe_numeric <- function(x, na.rm=F, integer=F) {
  pattern <- ifelse(
    integer,
    "μ=%.1f σ=%.2f mdn=%.0f, qr=%.1f, %.0f..%.0f%s",
    "μ=%.1f σ=%.2f mdn=%.1f, qr=%.1f, %.1f..%.1f%s"
  )
  has_na <- any(is.na(x))
  sprintf(
    pattern,
    mean(x, na.rm=na.rm),
    sd(x, na.rm=na.rm),
    median(x, na.rm=na.rm),
    quantile(x, 0.75, na.rm=na.rm) - quantile(x, 0.25, na.rm=na.rm),
    min(x, na.rm=na.rm),
    max(x, na.rm=na.rm),
    ifelse(
      na.rm & has_na,
      sprintf(" (%d NA ignored)", sum(is.na(x))),
      ""
    )
  )
}

describe_integer <- function(x, ...)  describe_numeric(x, integer=T, ...)

describe_values <- function(x, na.rm=F) paste(
  data.frame(x=x) %>%
    filter(!na.rm | !is.na(x)) %>%
    group_by(x) %>% summarise(n=length(x), total=nrow(.)) %>%
    mutate(x=paste0(x, ": ", n, " (", sprintf("%.0f%%", n/total*100), ")")) %>%
    pull(x),
  collapse=", "
)

describe_flags <- function(true_value=T, na.rm=F, ...) {
  x <- list(...)
  names <- names(x)
  paste(sapply(names, function(name){
    sums <- sum(x[[name]]==true_value, na.rm=na.rm)
    total <- ifelse(
      na.rm,
      sum(!is.na(x[[name]])),
      length(x[[name]])
    )
    paste0(name, ": ", sums, " (", sprintf("%.0f%%", sums/total*100), ")")
  }), collapse=", ")
}

describe_count <- function(x, matching_values=T, valid_values=NULL){
  has_na <- any(is.na(x))

  is_invalid <- !(x %in% valid_values) & !is.na(x)
  has_invalid <- ifelse(is.null(valid_values), FALSE, any(is_invalid))

  c <- sum(x %in% matching_values, na.rm=T)
  NA_c <- sum(is.na(x))
  i_c <- sum(is_invalid)
  sprintf(
    "%d (%.0f%%)%s%s", c, c/length(x)*100,
    ifelse(
      has_na,
      sprintf(", NA: %d (%.0f%%)", NA_c, NA_c/length(x)*100),
      ""
    ),
    ifelse(
      has_invalid,
      sprintf(", invalid: %d (%.0f%%)", i_c, i_c/length(x)*100),
      ""
    )
  )
}


# Unit Tests
stopifnot(
  describe_integer(0:100) ==
  "μ=50.0 σ=29.30 mdn=50, qr=50.0, 0..100"
)
stopifnot(
  describe_numeric(0:100/100) ==
  "μ=0.5 σ=0.29 mdn=0.5, qr=0.5, 0.0..1.0"
)
stopifnot(
  describe_values(c("m", "m", "w")) ==
  "m: 2 (67%), w: 1 (33%)"
)
stopifnot(
  describe_values(c("m", "m", "w", NA), na.rm=T) ==
    "m: 2 (67%), w: 1 (33%)"
)
stopifnot(
  describe_flags("Ja", Eigenschaft1=c("Ja", "Nein"), Eigenschaft2=c("Ja", "Ja"))
  == "Eigenschaft1: 1 (50%), Eigenschaft2: 2 (100%)"
)
stopifnot(
  describe_flags(na.rm=T, Eigenschaft1=c(T, F, NA), Eigenschaft2=c(NA, T, T))
  == "Eigenschaft1: 1 (50%), Eigenschaft2: 2 (100%)"
)
stopifnot(describe_count(c(T, F, F, F), F) == "3 (75%)")
stopifnot(describe_count(c(T, T, F, NA), T) == "2 (50%), NA: 1 (25%)")
stopifnot(
  describe_count(c(1, 2, 3, NA), 1, valid_values=1:2) ==
  "1 (25%), NA: 1 (25%), invalid: 1 (25%)"
)
