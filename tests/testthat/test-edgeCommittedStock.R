# Committed-stock accounting of edge carbon (model version L4): the arithmetic identities on a synthetic pool.
# Spec: fragmentation repo, 05-temporal-accounting/documents/COMMITTED_STOCK_ACCOUNTING.md, sections 2 and 6.

.synthPool <- function(seed = 1) {
  set.seed(seed)
  cells <- c("LAM.1", "LAM.2", "SSA.3")
  years <- paste0("y", seq(1995, 2030, 5))
  A   <- new.magpie(cells, years, NULL, fill = 0)
  rho <- new.magpie(cells, years, NULL, fill = 0)
  g   <- new.magpie(cells, years, NULL, fill = 0)
  A[, , ]   <- matrix(runif(length(cells) * length(years), 5, 50), nrow = length(cells))
  rho[, , ] <- matrix(runif(length(cells) * length(years), 80, 160), nrow = length(cells))
  g[, , ]   <- matrix(runif(length(cells) * length(years), 0.02, 0.30), nrow = length(cells))
  A[3, 5, ] <- 0                       # one empty pool-step: no area, no deficit
  list(A = A, rho = rho, g = g, D = g * rho * A, C0 = rho * A)
}

test_that("F + G = E and E + T = D_t - D_t-1 hold exactly on a synthetic pool", {
  s <- .synthPool()
  tm <- magpie4:::.edgeCommittedStockTerms(D = s$D, A = s$A, C0 = s$C0)
  expect_true(all(is.na(tm$E[, 1, ])))
  expect_true(all(is.na(tm$T[, 1, ])))
  expect_equal(as.vector(tm$S), as.vector(s$D))
  expect_equal(as.vector(tm$F + tm$G)[-(1:3)], as.vector(tm$E)[-(1:3)], tolerance = 1e-12)
  dD <- s$D - magpie4:::.edgeLagYears(s$D)
  expect_equal(as.vector(tm$E + tm$T)[-(1:3)], as.vector(dD)[-(1:3)], tolerance = 1e-12)
})

test_that("E reproduces the definition sum_p (dd_t - dd_t-1) A_t and F, G their definitions", {
  s <- .synthPool(2)
  tm <- magpie4:::.edgeCommittedStockTerms(D = s$D, A = s$A, C0 = s$C0)
  lag <- magpie4:::.edgeLagYears
  dd <- s$g * s$rho
  E <- (dd - lag(dd)) * s$A
  F <- (s$g - lag(s$g)) * s$rho * s$A
  G <- lag(s$g) * (s$rho - lag(s$rho)) * s$A
  ok <- !is.na(as.vector(E)) & as.vector(s$A) > 0 & as.vector(lag(s$A)) > 0
  expect_equal(as.vector(tm$E)[ok], as.vector(E)[ok], tolerance = 1e-12)
  expect_equal(as.vector(tm$F)[ok], as.vector(F)[ok], tolerance = 1e-12)
  expect_equal(as.vector(tm$G)[ok], as.vector(G)[ok], tolerance = 1e-12)
})

test_that("an empty pool contributes nothing and does not produce NaN", {
  s <- .synthPool(3)
  s$A[, , ] <- 0; s$D[, , ] <- 0; s$C0[, , ] <- 0
  tm <- magpie4:::.edgeCommittedStockTerms(D = s$D, A = s$A, C0 = s$C0)
  for (k in c("E", "F", "G", "T")) expect_true(all(as.vector(tm[[k]])[-(1:3)] == 0))
  expect_false(any(is.nan(as.vector(tm$E))))
})

test_that("a constant deficit fraction on a constant, growing forest is pure foregone growth", {
  cells <- "LAM.1"; years <- paste0("y", seq(1995, 2020, 5))
  A   <- new.magpie(cells, years, NULL, fill = 10)
  rho <- new.magpie(cells, years, NULL, fill = 0); rho[, , ] <- seq(100, 150, by = 10)
  g   <- new.magpie(cells, years, NULL, fill = 0.2)
  tm <- magpie4:::.edgeCommittedStockTerms(D = g * rho * A, A = A, C0 = rho * A)
  expect_equal(as.vector(tm$F)[-1], rep(0, 5), tolerance = 1e-12)
  expect_equal(as.vector(tm$G)[-1], rep(0.2 * 10 * 10, 5), tolerance = 1e-12)   # g * drho * A = 0.2 * 10 * 10
  expect_equal(as.vector(tm$T)[-1], rep(0, 5), tolerance = 1e-12)
})

test_that("a changing deficit fraction on a static forest is pure fragmentation-driven emission", {
  cells <- "LAM.1"; years <- paste0("y", seq(1995, 2020, 5))
  A   <- new.magpie(cells, years, NULL, fill = 10)
  rho <- new.magpie(cells, years, NULL, fill = 100)
  g   <- new.magpie(cells, years, NULL, fill = 0); g[, , ] <- seq(0.10, 0.35, by = 0.05)
  tm <- magpie4:::.edgeCommittedStockTerms(D = g * rho * A, A = A, C0 = rho * A)
  expect_equal(as.vector(tm$G)[-1], rep(0, 5), tolerance = 1e-12)
  expect_equal(as.vector(tm$F)[-1], rep(0.05 * 100 * 10, 5), tolerance = 1e-12)   # dg * rho * A
})
