ppsFingerprint <- function(gdx) {
  paste("# fingerprint", sprintf("%.12g", sum(readGDX(gdx, "f20_processing_shares"))),
        sprintf("%.12g", sum(readGDX(gdx, "f20_processing_conversion_factors"))))
}

test_that("primaryPerSecondary (composition = 'shares') reproduces stored reference values", {
  run_only_if_full_tests_requested()
  gdx <- fullDataGdxPath("magpie-default")
  skip_if_not(file.exists(gdx), "gdx file not available")
  refFile <- test_path("fixtures", "primaryPerSecondary_shares.csv")
  skip_if(readLines(refFile, n = 1) != ppsFingerprint(gdx),
          "reference values were made for another magpie-default fixture; regenerate them with the shares path")
  ref <- utils::read.csv(refFile, comment.char = "#", stringsAsFactors = FALSE)
  for (allocation in unique(ref$allocation)) {
    x <- magclass::as.data.frame(primaryPerSecondary(gdx, allocation = allocation), rev = 2)
    names(x) <- c("i", "t", "ksd", "kpr", "x")
    x$t <- as.integer(as.character(x$t))
    m <- merge(ref[ref$allocation == allocation, ], x, by = c("i", "t", "ksd", "kpr"))
    expect_equal(nrow(m), sum(ref$allocation == allocation))
    expect_equal(m$x, m$value, tolerance = 1e-10)
  }
})

test_that("primaryPerSecondary with composition = 'levels' conserves the processed primaries", {
  run_only_if_full_tests_requested()
  gdx <- fullDataGdxPath("magpie-default")
  skip_if_not(file.exists(gdx), "gdx file not available")

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

test_that("primaryPerSecondary with composition = 'levels' puts the content where the pools put the tonnes", {
  run_only_if_full_tests_requested()
  gdx <- fullDataGdxPath("magpie-default")
  skip_if_not(file.exists(gdx), "gdx file not available")

  lev   <- function(name) unwrap(collapseNames(readGDX(gdx, name, select = list(type = "level"))))
  prod  <- lev("ov_prod_reg")
  dem   <- lev("ov20_dem_processing")
  subst <- lev("ov20_secondary_substitutes")
  years <- dimnames(dem)[[2]]
  kpr   <- dimnames(dem)[[4]]
  x     <- unwrap(primaryPerSecondary(gdx, allocation = "value", composition = "levels"))
  k     <- dimnames(x)[[4]]
  nReg  <- dim(dem)[1]
  # value weights of the outputs j of process p with input m: conv * price / sum over outputs
  conv  <- unwrap(readGDX(gdx, "i20_processing_conversion_factors")[, years, ])[1, , , , , drop = TRUE]
  price <- readGDX(gdx, "f15_prices_initial")[, , dimnames(conv)[[3]]]
  v <- sweep(conv, 3, as.vector(price), "*")
  w <- sweep(v, c(1, 2, 4), apply(v, c(1, 2, 4), sum), "/")
  w[!is.finite(w)] <- 0
  processes <- intersect(readGDX(gdx, "processing20"), readGDX(gdx, "no_milling_ginning20"))
  # input tonnes of m whose content goes into j: sum over processes of dem(p, m) * w(p, j, m); region, year, m
  edge <- function(j, m) {
    e <- 0
    for (p in processes) e <- e + dem[, , p, m] * rep(w[, p, j, m], each = nReg)
    return(e)
  }
  content <- function(j) x[, , j, ] * as.vector(prod[, years, j])   # region, year, primary
  dsub <- function(m) as.vector(dem[, , "substitutes", m])

  # oils: extraction recipe plus the oils used as substitutes, which carry the region's own oils content
  expect_lt(max(abs(content("oils") - edge("oils", k) - dsub("oils") * x[, , "oils", ])), 1e-6)
  # molasses: refining recipe plus the sugar used as substitutes, with the region's sugar content
  expect_lt(max(abs(content("molasses") - edge("molasses", k) - dsub("sugar") * x[, , "sugar", ])), 1e-6)

  # protein pool: whole oilseeds and oilcakes used as substitutes, split between distillers grain and oilcakes in
  # nitrogen of the nominal origin of the relabelled tonnes
  nrAtt <- readGDX(gdx, "fm_attributes")[, , "nr"]
  nr <- as.vector(nrAtt)
  names(nr) <- getItems(nrAtt, dim = 3.2)
  need <- function(j) apply(subst[, , j, , drop = FALSE] * rep(nr[kpr], each = nReg * length(years)), c(1, 2), sum)
  share <- need("distillers_grain") / (need("distillers_grain") + need("oilcakes"))
  oilseeds <- intersect(readGDX(gdx, "oilcake_substitutes20"), k)
  poolIn <- dem[, , "substitutes", k] * 0
  poolIn[, , oilseeds] <- dem[, , "substitutes", oilseeds]
  poolIn <- poolIn + dsub("oilcakes") * x[, , "oilcakes", ]
  ok <- is.finite(share)
  for (j in c("distillers_grain", "oilcakes")) {
    s <- if (j == "oilcakes") 1 - share else share
    viaPool <- content(j) - edge(j, k)
    expect_lt(max(abs((viaPool - as.vector(s) * poolIn)[rep(ok, length(k))])), 1e-6)
  }

  # a region without ethanol production gets the World factor: sugar and molasses at World level, then ethanol
  noEth <- prod[, years, "ethanol"] <= 0
  skip_if_not(any(noEth), "every region produces ethanol in this gdx")
  secd <- intersect(kpr, readGDX(gdx, "ksd"))
  expect_equal(sum(edge("sugar", secd)), 0)
  expect_equal(sum(edge("ethanol", setdiff(secd, c("sugar", "molasses")))), 0)
  wsum <- function(a) apply(a, c(2, 3), sum)                        # year, primary
  wP   <- function(j) colSums(prod[, years, j])
  zSugar <- wsum(edge("sugar", k)) / wP("sugar")
  zMol   <- (wsum(edge("molasses", k)) + colSums(dem[, , "substitutes", "sugar"]) * zSugar) / wP("molasses")
  zEth   <- (wsum(edge("ethanol", k)) + colSums(edge("ethanol", "sugar")) * zSugar +
               colSums(edge("ethanol", "molasses")) * zMol) / wP("ethanol")
  for (i in seq_len(nReg)) {
    for (t in years[noEth[i, ]]) {
      expect_equal(x[i, t, "ethanol", ], zEth[t, ], tolerance = 1e-9)
    }
  }
})
