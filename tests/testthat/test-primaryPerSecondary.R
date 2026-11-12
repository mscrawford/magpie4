test_that("primaryPerSecondary with composition = 'levels' conserves the processed primaries", {
  run_only_if_full_tests_requested()
  gdx <- fullDataGdxPath("magpie-default")
  skip_if_not(file.exists(gdx), "gdx file not available")

  expect_identical(primaryPerSecondary(gdx), primaryPerSecondary(gdx, composition = "shares"))

  lev  <- function(name) unwrap(collapseNames(readGDX(gdx, name, select = list(type = "level"))))
  ksd  <- readGDX(gdx, "ksd")
  prod <- lev("ov_prod_reg")
  proc <- lev("ov_dem_processing")
  dem  <- lev("ov20_dem_processing")
  years <- dimnames(dem)[[2]]
  # processing input without any output (free disposal) is not attributed to a secondary; a disposed secondary
  # input takes its primary content with it
  conv <- unwrap(readGDX(gdx, "i20_processing_conversion_factors")[, years, ])[1, , , , , drop = TRUE]
  noOutput <- apply(conv, c(1, 2, 4), sum) == 0
  processes <- setdiff(readGDX(gdx, "no_milling_ginning20"), "substitutes")
  disposal <- dem[, , 1, ] * 0
  for (p in processes) {
    disposal <- disposal + dem[, , p, ] * rep(noOutput[, p, ], each = dim(dem)[1])
  }

  for (allocation in c("mass", "value")) {
    x <- unwrap(primaryPerSecondary(gdx, allocation = allocation, composition = "levels"))
    k <- dimnames(x)[[4]]
    embodied <- apply(x * as.vector(prod[, years, ksd] - proc[, years, ksd]), c(1, 2, 4), sum)
    disposedSecd <- intersect(dimnames(disposal)[[3]], ksd)
    lost <- disposal[, , k] + apply(x[, , disposedSecd, , drop = FALSE] * as.vector(disposal[, , disposedSecd]),
                                    c(1, 2, 4), sum)
    expect_lt(max(abs(embodied - (proc[, years, k] - lost))), 1e-6)
    expect_gte(min(x), -1e-9)
  }
})
