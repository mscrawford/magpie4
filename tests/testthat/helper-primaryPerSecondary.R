# fingerprint of the input data the shares path of primaryPerSecondary reads (shares, conversion factors, prices)
ppsFingerprint <- function(gdx) {
  sums <- vapply(c("f20_processing_shares", "f20_processing_conversion_factors", "f15_prices_initial"),
                 function(s) format(sum(gdx2::readGDX(gdx, s)), digits = 12), character(1))
  return(paste("# fingerprint", paste(sums, collapse = " ")))
}
