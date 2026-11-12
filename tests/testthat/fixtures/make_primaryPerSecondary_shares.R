# Reference values for test-primaryPerSecondary.R: the shares path (composition = "shares") on the full-data fixture.
# Run it from the package root, with the package version whose shares path is the reference installed, giving the
# path of the fixture's fulldata.gdx (tests/testthat/tmp_fixtures/magpie-default/fulldata.gdx) as the only argument.
gdx <- commandArgs(TRUE)[1]
source("tests/testthat/helper-primaryPerSecondary.R")
out <- NULL
for (allocation in c("value", "mass", "none")) {
  x <- magpie4::primaryPerSecondary(gdx, allocation = allocation)[c("CAZ", "IND"), c("y2010", "y2050"), ]
  x <- magclass::as.data.frame(x, rev = 2)
  names(x) <- c("i", "t", "ksd", "kpr", "value")
  out <- rbind(out, data.frame(allocation = allocation, x[x$value != 0, ], stringsAsFactors = FALSE))
}
out$value <- trimws(formatC(out$value, digits = 15, format = "g"))
file <- "tests/testthat/fixtures/primaryPerSecondary_shares.csv"
writeLines(ppsFingerprint(gdx), file)
suppressWarnings(utils::write.table(out, file, append = TRUE, sep = ",", row.names = FALSE, quote = 1:5))
