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

# ---------------------------------------------------------------------------------------------------------------
# The cohort-consistent ageing / cohort-turnover split of G (added 2026-09-20; spec amendment of that date, build
# record L4_BUILD_2026-09-19.md sections 9.1 (a) and 10 finding A2-1). Before the split, rho being an area-weighted
# mean over age classes meant any within-pool composition change at constant area was booked as a density change and
# landed in G "foregone growth": clear-cut harvest resets cohorts from acx to ac0 without moving area, so G absorbed
# the released deficit as NEGATIVE growth. G is now the cohort-consistent ageing term and H the turnover.

.acSynth <- function(years, acs, Amat, rhoMat, gvec, cell = "LAM.1") {
  Aac   <- new.magpie(cell, years, acs, fill = 0)
  rhoAc <- new.magpie(cell, years, acs, fill = 0)
  for (i in seq_along(years)) {
    Aac[, i, ]   <- Amat[i, ]
    rhoAc[, i, ] <- rhoMat[i, ]
  }
  A  <- magclass::dimSums(Aac, dim = 3)
  C0 <- magclass::dimSums(Aac * rhoAc, dim = 3)
  g  <- new.magpie(cell, years, NULL, fill = 0)
  g[, , ] <- gvec
  list(Aac = Aac, rhoAc = rhoAc, A = A, C0 = C0, D = g * C0)
}

.acTerms <- function(s, years, hvMat = NULL) {
  adv <- magpie4:::.edgeAdvancedC0(s$Aac, s$rhoAc, magpie4:::.edgeAcShift(years))
  hvC0 <- NULL
  if (!is.null(hvMat)) {
    hv <- s$Aac * 0
    for (i in seq_along(years)) hv[, i, ] <- hvMat[i, ]
    hvC0 <- magpie4:::.edgeHarvestC0(hv, s$rhoAc)
  }
  magpie4:::.edgeCommittedStockTerms(D = s$D, A = s$A, C0 = s$C0, C0adv = adv, hvC0 = hvC0)
}

test_that("a clear-cut cohort at constant area is cohort turnover, not foregone growth (known-bug case)", {
  years <- c("y2000", "y2005")
  acs   <- c("ac0", "acx")
  # 4 of 10 Mha clear-cut out of acx into ac0: the pool's AREA does not move, so T is blind to it
  A   <- rbind(c(0, 10), c(4, 6))
  # the standing (unharvested) cohort gains 10 tC/ha along the curve; a freshly cut class carries no carbon
  rho <- rbind(c(0, 100), c(0, 110))
  s  <- .acSynth(years, acs, A, rho, c(0.2, 0.2))
  tm <- .acTerms(s, years)
  released <- 0.2 * 110 * 4     # g_t-1 * rho_acx,t * harvested area: the deficit the cut cohort released
  expect_lt(as.vector(tm$H)[2], 0)
  expect_equal(as.vector(tm$H)[2], -released, tolerance = 1e-12)
  expect_equal(as.vector(tm$G)[2], 0.2 * (110 - 100) * 10, tolerance = 1e-12)  # the surviving cohort's growth only
  # the defect this test pins: the former single term (= G + H) books the whole composition change as NEGATIVE growth
  oldG <- as.vector(tm$G + tm$H)[2]
  expect_equal(oldG, 0.2 * (66 - 100) * 10, tolerance = 1e-12)                 # rho_t = 6 * 110 / 10 = 66 tC/ha
  expect_lt(oldG, 0)
  # (c) the identities
  expect_equal(as.vector(tm$F + tm$G + tm$H)[2], as.vector(tm$E)[2], tolerance = 1e-10)
  expect_equal(as.vector(tm$E + tm$T)[2], as.vector(s$D)[2] - as.vector(s$D)[1], tolerance = 1e-10)
})

test_that("pure ageing (no harvest, no area change) gives H = 0 exactly", {
  years <- c("y2000", "y2005")
  acs   <- c("ac0", "ac5", "ac10", "ac15", "acx")
  A   <- rbind(c(0, 6, 4, 0, 0), c(0, 0, 6, 4, 0))   # every cohort advances exactly one class
  rho <- rbind(c(0, 20, 45, 70, 120), c(0, 22, 48, 74, 125))
  s  <- .acSynth(years, acs, A, rho, c(0.25, 0.25))
  tm <- .acTerms(s, years)
  expect_equal(as.vector(tm$H)[2], 0, tolerance = 1e-12)
  expect_equal(as.vector(tm$G)[2], as.vector(tm$G + tm$H)[2], tolerance = 1e-12)  # identical to the former term
  expect_equal(as.vector(tm$G)[2], 0.25 * (58.4 - 30) * 10, tolerance = 1e-12)    # rho_t 58.4, rho_t-1 30 tC/ha
  # (c) the identities
  expect_equal(as.vector(tm$F + tm$G + tm$H)[2], as.vector(tm$E)[2], tolerance = 1e-10)
  expect_equal(as.vector(tm$E + tm$T)[2], as.vector(s$D)[2] - as.vector(s$D)[1], tolerance = 1e-10)
})

test_that("the cohort shift mirrors GAMS: one class per 5-yr step, two per 10-yr step, acx collects the overflow", {
  expect_equal(magpie4:::.edgeAcShift(c("y2050", "y2055", "y2060", "y2070", "y2080")),
               c(NA, 1L, 1L, 2L, 2L))
  years <- c("y2060", "y2070")                       # a 10-yr step: two classes advance
  acs   <- c("ac0", "ac5", "ac10", "acx")
  A   <- rbind(c(2, 3, 4, 1), c(0, 0, 0, 10))
  rho <- rbind(c(1, 2, 3, 4), c(10, 20, 30, 40))
  s   <- .acSynth(years, acs, A, rho, c(0.1, 0.1))
  adv <- magpie4:::.edgeAdvancedC0(s$Aac, s$rhoAc, magpie4:::.edgeAcShift(years))
  # advanced areas: ac10 = 2 (from ac0), acx = 3 + 4 + 1 = 8 (ac5 shifted in, ac10 and acx collected); total 10
  expect_equal(as.vector(adv)[2], 2 * 30 + 8 * 40, tolerance = 1e-12)
  expect_true(is.na(as.vector(adv)[1]))
})

test_that("age classes are read in set order whatever the order of the third dimension", {
  years <- c("y2000", "y2005")
  acs   <- c("ac0", "ac5", "ac10", "acx")
  A   <- rbind(c(1, 2, 3, 4), c(2, 1, 4, 3))
  rho <- rbind(c(5, 15, 40, 90), c(6, 16, 42, 95))
  s <- .acSynth(years, acs, A, rho, c(0.15, 0.15))
  shift <- magpie4:::.edgeAcShift(years)
  ref  <- magpie4:::.edgeAdvancedC0(s$Aac, s$rhoAc, shift)
  perm <- c("acx", "ac10", "ac0", "ac5")
  scr  <- magpie4:::.edgeAdvancedC0(s$Aac[, , perm], s$rhoAc[, , perm], shift)
  expect_equal(as.vector(scr), as.vector(ref), tolerance = 1e-12)
})

test_that("a pool without age classes has H = 0 and G unchanged", {
  s <- .synthPool(4)
  tm <- magpie4:::.edgeCommittedStockTerms(D = s$D, A = s$A, C0 = s$C0)        # C0adv = NULL
  expect_true(all(as.vector(tm$H)[-(1:3)] == 0))
  expect_equal(as.vector(tm$F + tm$G + tm$H)[-(1:3)], as.vector(tm$E)[-(1:3)], tolerance = 1e-12)
})

# MAJOR-2 of the audit of 2026-09-20: every age-class case above holds the pool's AREA constant, and with
# A_t = A_t-1 the mutant div0(C0adv, A_t) is indistinguishable from the correct div0(C0adv, A_t-1). The two cases
# below have A_t != A_t-1 and hand-computed G and H, which is what kills that mutant. They also settle what H is
# when area changes without harvest: H = 0 only if the area change is class-PROPORTIONAL (it then preserves the
# age-class composition); a SELECTIVE area change leaves a residual in H, which is the conversion-selectivity and
# area-basis content of the term. H is a composition term, not an area term.
#
# Shared construction, classes ac0 / ac5 / ac10 / acx, k = 1, g = 0.25 in both steps:
#   t-1: areas (0, 6, 4, 0) Mha, densities (0, 20, 45, 120) tC/ha  -> A_t-1 = 10, rho_t-1 = 30, C0_t-1 = 300
#   t  : densities (0, 22, 48, 125) tC/ha
#   advanced areas (0, 0, 6, 4) -> C0adv = 6 * 48 + 4 * 125 = 788, rhoAdv = 788 / 10 = 78.8 tC/ha
#   G = 0.25 * (78.8 - 30) * A_t = 12.2 A_t in both cases (G does not see rho_t)

test_that("a proportional area change with pure ageing keeps H = 0 (and kills the A_t denominator)", {
  years <- c("y2000", "y2005")
  acs   <- c("ac0", "ac5", "ac10", "acx")
  A   <- rbind(c(0, 6, 4, 0), c(0, 0, 4.8, 3.2))    # 20 % of the area leaves, taken proportionally from each class
  rho <- rbind(c(0, 20, 45, 120), c(0, 22, 48, 125))
  s  <- .acSynth(years, acs, A, rho, c(0.25, 0.25))
  tm <- .acTerms(s, years)
  expect_equal(as.vector(s$A)[1], 10, tolerance = 1e-12)        # A_t-1
  expect_equal(as.vector(s$A)[2], 8, tolerance = 1e-12)         # A_t, so the two denominators differ
  expect_equal(as.vector(tm$G)[2], 0.25 * (78.8 - 30) * 8, tolerance = 1e-12)   # 97.6
  expect_equal(as.vector(tm$H)[2], 0, tolerance = 1e-12)
  expect_equal(as.vector(tm$T)[2], 0.25 * 30 * (8 - 10), tolerance = 1e-12)     # -15, the area part
  expect_equal(as.vector(tm$F + tm$G + tm$H)[2], as.vector(tm$E)[2], tolerance = 1e-10)
  expect_equal(as.vector(tm$E + tm$T)[2], as.vector(s$D)[2] - as.vector(s$D)[1], tolerance = 1e-10)
  # the mutant rhoAdv = C0adv / A_t would give 788 / 8 = 98.5, hence G = 137 and H = -39.4
  expect_false(isTRUE(all.equal(as.vector(tm$G)[2], 0.25 * (98.5 - 30) * 8)))
})

test_that("a selective area change with pure ageing leaves the conversion residual in H", {
  years <- c("y2000", "y2005")
  acs   <- c("ac0", "ac5", "ac10", "acx")
  A   <- rbind(c(0, 6, 4, 0), c(0, 0, 6, 2))        # the 2 Mha that leave are taken from acx alone
  rho <- rbind(c(0, 20, 45, 120), c(0, 22, 48, 125))
  s  <- .acSynth(years, acs, A, rho, c(0.25, 0.25))
  tm <- .acTerms(s, years)
  expect_equal(as.vector(tm$G)[2], 0.25 * (78.8 - 30) * 8, tolerance = 1e-12)   # 97.6, unchanged: G ignores rho_t
  expect_equal(as.vector(tm$H)[2], 0.25 * (67.25 - 78.8) * 8, tolerance = 1e-12)  # -23.1; rho_t = 538 / 8
  expect_lt(as.vector(tm$H)[2], 0)                              # old classes left, so the mean density fell
  expect_equal(as.vector(tm$F + tm$G + tm$H)[2], as.vector(tm$E)[2], tolerance = 1e-10)
  expect_equal(as.vector(tm$E + tm$T)[2], as.vector(s$D)[2] - as.vector(s$D)[1], tolerance = 1e-10)
  # the mutant denominator A_t would give H = 0.25 * (67.25 - 98.5) * 8 = -62.5
  expect_false(isTRUE(all.equal(as.vector(tm$H)[2], 0.25 * (67.25 - 98.5) * 8)))
})

test_that("the harvest-reset sub-term is -g_t-1 sum_ac rho_ac,t hv_ac,t and carries all of H on a clear cut", {
  years <- c("y2000", "y2005")
  acs   <- c("ac0", "acx")
  A   <- rbind(c(0, 10), c(4, 6))
  rho <- rbind(c(0, 100), c(0, 110))
  hv  <- rbind(c(0, 0), c(0, 4))        # the 4 Mha are harvested OUT OF acx during the step
  s  <- .acSynth(years, acs, A, rho, c(0.2, 0.2))
  tm <- .acTerms(s, years, hvMat = hv)
  expect_equal(as.vector(tm$Hharvest)[2], -0.2 * 110 * 4, tolerance = 1e-12)     # -88
  expect_equal(as.vector(tm$Hharvest)[2], as.vector(tm$H)[2], tolerance = 1e-12) # nothing else moved
  expect_true(is.na(as.vector(tm$Hharvest)[1]))
  # Hharvest is a sub-term of H, never an addend of E
  expect_equal(as.vector(tm$F + tm$G + tm$H)[2], as.vector(tm$E)[2], tolerance = 1e-10)
})

test_that("Hharvest is zero where the model does not clear-cut per age class", {
  s <- .synthPool(5)
  tm <- magpie4:::.edgeCommittedStockTerms(D = s$D, A = s$A, C0 = s$C0)          # C0adv and hvC0 both NULL
  expect_true(all(as.vector(tm$Hharvest)[-(1:3)] == 0))
  expect_true(all(is.na(as.vector(tm$Hharvest)[1:3])))
})
