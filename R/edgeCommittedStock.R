#' @title edgeCommittedStockTerms
#' @description Committed-stock accounting of forest edge carbon (model version L4, 2026-09): the per-pool
#'   arithmetic behind the edge lines of \code{reportEmissions}. Internal.
#'
#' The model carries the edge effect in its own stock: the natural-forest vegc densities are multiplied by a
#' retention factor (1 - g) before the solve, so every carbon flow the model books already contains the edge
#' effect. The committed edge carbon stock (the deficit) on the solved land of a pool is D = g rho A. With the
#' pool's deficit stock D, its solved area A and its unreduced carbon C0 = rho A per cluster and step:
#' \itemize{
#'   \item dd = D / A (deficit density), rho = C0 / A, g = D / C0 (effective applied deficit fraction)
#'   \item S = D: committed stock
#'   \item E = (dd_t - dd_t-1) A_t: edge emission on the forest standing at the end of the step
#'   \item F = (g_t - g_t-1) rho_t A_t: fragmentation-driven part (the deficit fraction changing)
#'   \item G = g_t-1 (rhoAdv_t - rho_t-1) A_t: foregone growth, the COHORT-CONSISTENT ageing term
#'   \item H = g_t-1 (rho_t - rhoAdv_t) A_t: cohort turnover (everything else the mean density does)
#'   \item Hharvest = -g_t-1 sum_ac rho_ac,t hv_ac,t: the harvest-reset part of H, reported informationally
#'   \item T = dd_t-1 (A_t - A_t-1): area transfer, a diagnostic (that carbon sits in land-use change)
#' }
#' Identities: F + G + H = E and E + T = D_t - D_t-1, exactly. Flows are NA in the first step. Where a pool has
#' no area (or no carbon) the densities are 0, so an empty pool contributes nothing to any term.
#'
#' G and H (the split added 2026-09-20, third audit round A2-1). rho is an area-weighted mean over the pool's age
#' classes, so BEFORE the split any change of the within-pool age-class composition at constant area was booked as
#' a density change and landed in G: clear-cut harvest resets cohorts from acx to ac0 without moving area, and G
#' absorbed the released deficit as negative "growth" (measured: -226.6 Mt CO2/yr of the World 2035 secdforest G
#' line of the SSP2 price pilot, -70.5 of the base run's). The split evaluates the previous step's age
#' distribution, advanced by the step's cohort shift, on the CURRENT step's unreduced growth curve:
#' \itemize{
#'   \item k_t = (year_t - year_t-1) / 5 age classes advance in step t (the age-class width is 5 yr): 1 for a
#'         5-yr step, 2 for the 10-yr steps after 2060. This mirrors GAMS exactly
#'         (\code{s35_shift = m_timestep_length_forestry/5} in 35_natveg/pot_forest_may24/presolve.gms,
#'         \code{s32_shift = m_yeardiff_forestry(t)/5} in 32_forestry/dynamic_may24/presolve.gms).
#'   \item Advanced distribution: Atilde_ac,t = A_ac-k,t-1 for every class below acx; the top class acx COLLECTS
#'         everything that would land at or beyond it, Atilde_acx,t = sum over ac >= acx-k of A_ac,t-1; the k
#'         youngest classes receive nothing. Total area is preserved, sum_ac Atilde_ac,t = A_t-1 (the two GAMS
#'         statements, the "usual shift" and the end-of-set correction, in that order).
#'   \item C0adv_t = sum_ac rho_ac,t Atilde_ac,t and rhoAdv_t = C0adv_t / A_t-1: the mean unreduced density the
#'         pool would carry if its cohorts had only aged. The denominator is A_t-1, not A_t, because the advanced
#'         distribution carries the PREVIOUS step's area (sum_ac Atilde = A_t-1); dividing by A_t would not give a
#'         density and would leak the area change into G.
#'   \item The input distribution is the SOLVED state of t-1 (\code{ov35_secdforest}, \code{ov_land_other},
#'         \code{ov32_land} levels), not GAMS's post-disturbance presolve state \code{pc35_*}: the gdx carries no
#'         other end-of-step per-age-class state. GAMS applies the shifting-cultivation disturbance BEFORE the
#'         shift (presolve.gms, the \code{p35_disturbance_loss_secdf} block), so a disturbance reset is NOT in this
#'         counterfactual and therefore shows up in H. The shift RULE mirrors GAMS; the state it is applied to is
#'         the solved one.
#' }
#' G is standing forest gaining density along its growth curve, and nothing else. H collects every other reason
#' the pool's mean unreduced density moves, i.e. every change of the age-class COMPOSITION:
#' \itemize{
#'   \item HARVEST RESETS: clear-cut moves area from its class to ac0 without changing the pool's area, releasing
#'         -g_t-1 sum_ac rho_ac,t hv_ac,t (\code{ov35_hvarea_secdforest}, \code{ov35_hvarea_other}). Reported as
#'         the informational line "Cohort turnover | Harvest resets"; the single largest component, 97 % of the base
#'         run's World 2035 H and 86 % of the price pilot's. Do NOT read it as a share in general: the other
#'         components do not cancel, and Hharvest / H swings between about 0.4 and 6.6 over the base run's century
#'         (H is a difference of terms of either sign, so a small H can sit under a large harvest reset). Read the
#'         two lines together.
#'   \item CONVERSION SELECTIVITY: the classes the solve converts out of the pool
#'         (\code{ov35_secdforest_reduction}) are not a proportional slice of it.
#'   \item DISTURBANCE RESETS: shifting-cultivation loss moves area from the older classes into the establishment
#'         classes in the presolve (see the solved-state note above).
#'   \item MATURATION: youngsecdf area crossing the 20 tC/ha threshold leaves that pool for secdforest.
#'   \item ESTABLISHMENT: area entering the youngest classes.
#'   \item AREA-BASIS REVALUATION: E charges area that left or arrived at the POOL-MEAN deficit density dd_t-1,
#'         while the classes that actually moved differed; this is the mirror image of T and is usually the one
#'         POSITIVE component.
#' }
#' Measured by the audit of 2026-09-20 on the base run (SSP2base_L4gate_ON), secdforest, World 2035, Mt CO2/yr:
#' H -70.52 = harvest -69.00 + conversion selectivity -13.35 + disturbance resets -15.65 + maturation +0.73 +
#' establishment 0 + area basis +26.75 (against T = -25.01). Cumulative 2020-2100, Gt CO2: harvest -3.30,
#' conversion selectivity -2.26, area basis +1.89, disturbance -0.31, maturation +0.51. So H is NOT "dilution by
#' new area entering the young classes": that would be the establishment term, which is the smallest of the six.
#'
#' H = 0 EXACTLY when the pool's age-class composition at t equals the advanced composition of t-1. A
#' class-PROPORTIONAL area change preserves that composition, so pure ageing with a proportional area change gives
#' H = 0; a SELECTIVE area change does not, and that residual is the conversion-selectivity and area-basis content
#' above. H is a composition term, not an area term.
#'
#' CONVENTION: G advances all of the previous step's area, including the cohorts the solve cuts during the same
#' step, so G credits their growth along the curve and H then removes their whole deficit at the current curve.
#' That growth is 2-3.5 % of G (audit measurement). The alternative - excluding cut cohorts from G - was not
#' adopted: G stays the clean counterfactual "what the pool's density would have done had nothing but ageing
#' happened".
#'
#' G + H is the former single "foregone growth" term g_t-1 (rho_t - rho_t-1) A_t, so E, F, T and the two
#' identities are unchanged by the split.
#' For a pool WITHOUT age classes (primforest, whose deficit sits in acx alone) rhoAdv = rho and H = 0 by
#' construction: pass \code{C0adv = NULL}.
#' Spec: fragmentation repo, 05-temporal-accounting/documents/COMMITTED_STOCK_ACCOUNTING.md (section 2 and the
#' amendment of 2026-09-20), L4_BUILD_2026-09-19.md sections 9.1 (a) and 10 (A2-1).
#'
#' @param D committed deficit stock per cluster and step (magpie object, cells x years x 1; Mio tC)
#' @param A solved area of the pool (same shape; Mha)
#' @param C0 unreduced carbon of the pool on the solved area, sum over age classes of rho_ac A_ac (same shape; Mio tC)
#' @param C0adv unreduced carbon of the PREVIOUS step's age distribution advanced by the step's cohort shift and
#'   evaluated on the current step's unreduced densities (same shape, NA in the first step; Mio tC), from
#'   \code{.edgeAdvancedC0}. NULL for a pool without age classes, which gives H = 0.
#' @param hvC0 unreduced carbon on the area harvested in this step, sum over age classes of rho_ac,t hv_ac,t (same
#'   shape; Mio tC), from \code{.edgeHarvestC0}. NULL for a pool the model does not clear-cut per age class, which
#'   gives Hharvest = 0.
#' @return list of magpie objects S, E, F, G, H, Hharvest, T (Mio tC per step; flows are per step, not per year)
#' @author Michael Crawford
#' @keywords internal
#' @noRd
.edgeCommittedStockTerms <- function(D, A, C0, C0adv = NULL, hvC0 = NULL) {
  div0 <- function(a, b) { r <- a / b; r[!is.finite(r)] <- 0; r }
  dd  <- div0(D, A)
  rho <- div0(C0, A)
  g   <- div0(D, C0)
  lagA <- .edgeLagYears(A)
  lagG <- .edgeLagYears(g)
  # rhoAdv: the mean unreduced density of the previous step's cohorts, aged one step. Without age classes it is
  # rho itself, so the cohort-turnover term H vanishes and G is the former single foregone-growth term.
  rhoAdv <- if (is.null(C0adv)) rho else div0(C0adv, lagA)
  list(S = D,
       E = (dd - .edgeLagYears(dd)) * A,
       F = (g - .edgeLagYears(g)) * rho * A,
       G = lagG * (rhoAdv - .edgeLagYears(rho)) * A,
       H = lagG * (rho - rhoAdv) * A,
       # the harvest-reset part of H: a sub-term, NOT an addend of E. lagG * 0 keeps the first step NA.
       Hharvest = if (is.null(hvC0)) lagG * 0 else -lagG * hvC0,
       T = .edgeLagYears(dd) * (A - lagA))
}

#' @title edgeLagYears
#' @description The previous step's value on the current step's year axis (NA in the first step). Internal.
#' @param x magpie object with at least one year
#' @return magpie object of the same shape
#' @author Michael Crawford
#' @keywords internal
#' @noRd
.edgeLagYears <- function(x) {
  y <- x
  y[, 1, ] <- NA
  n <- magclass::nyears(x)
  if (n > 1) y[, -1, ] <- magclass::setYears(x[, -n, ], magclass::getYears(x)[-1])
  y
}

#' @title edgeAcShift
#' @description Number of age classes a cohort advances per time step, k_t = (year_t - year_t-1) / width, the
#'   report-side mirror of GAMS \code{s35_shift} / \code{s32_shift}. NA in the first step. Internal.
#' @param years year labels ("y2000") or integers
#' @param width age-class width in years (5 in MAgPIE's ac set: ac0, ac5, ..., ac300, acx)
#' @return integer vector, one element per year, NA first
#' @author Michael Crawford
#' @keywords internal
#' @noRd
.edgeAcShift <- function(years, width = 5) {
  y <- if (is.character(years)) as.integer(sub("y", "", years)) else as.integer(years)
  dt <- c(NA_integer_, diff(y))
  if (any(dt %% width != 0, na.rm = TRUE)) {
    warning("reportEmissions (edge, L4): time step(s) of ", paste(unique(dt[!is.na(dt) & dt %% width != 0]), collapse = ", "),
            " yr are not multiples of the ", width, "-yr age-class width; the cohort shift is truncated downwards")
  }
  dt %/% width
}

#' @title edgeAcSort
#' @description Age classes of a magpie object in set order (ac0, ac5, ..., ac300, acx). Internal: the committed-stock
#'   cohort shift is positional, so a reordered third dimension would silently age the wrong cohorts.
#' @param x magpie object whose (only) third dimension is the age class
#' @return x with its age classes in ascending order, acx last
#' @author Michael Crawford
#' @keywords internal
#' @noRd
.edgeAcSort <- function(x) {
  nm <- magclass::getNames(x)
  num <- suppressWarnings(as.numeric(sub("^ac", "", nm)))
  num[nm == "acx"] <- Inf
  if (any(is.na(num))) {
    stop("reportEmissions (edge, L4): unrecognised age-class labels: ", paste(nm[is.na(num)], collapse = ", "))
  }
  o <- order(num)
  if (identical(o, seq_along(nm))) x else x[, , nm[o]]
}

#' @title edgeAdvancedC0
#' @description Unreduced carbon of the previous step's age distribution advanced by the step's cohort shift and
#'   evaluated on the current step's unreduced densities: C0adv_t = sum_ac rho_ac,t Atilde_ac,t, with
#'   Atilde the previous step's areas shifted by k_t classes and acx collecting the overflow (the GAMS ageing
#'   rule of 35_natveg / 32_forestry presolve). Internal; see .edgeCommittedStockTerms for the full definition.
#' @param Aac solved area per cluster, step and age class (magpie, cells x years x ac; Mha)
#' @param rhoAc unreduced vegc density per cluster, step and age class (same shape; tC/ha)
#' @param shift cohort shift per step from .edgeAcShift (NA first)
#' @return magpie object (cells x years x 1; Mio tC), NA in the first step
#' @author Michael Crawford
#' @keywords internal
#' @noRd
.edgeAdvancedC0 <- function(Aac, rhoAc, shift) {
  Aac <- .edgeAcSort(Aac)
  rhoAc <- .edgeAcSort(rhoAc)
  if (!identical(magclass::getNames(Aac), magclass::getNames(rhoAc))) {
    stop("reportEmissions (edge, L4): the area and unreduced-density age classes of a pool do not match")
  }
  a <- as.array(Aac)
  r <- as.array(rhoAc)
  nc <- dim(a)[1]; ny <- dim(a)[2]; nac <- dim(a)[3]
  res <- matrix(NA_real_, nrow = nc, ncol = ny)
  for (t in seq_len(ny)[-1]) {
    k <- shift[t]
    ap <- matrix(a[, t - 1, ], nrow = nc)
    if (is.na(k) || k <= 0) {
      adv <- ap                                        # no ageing within the step
    } else if (k >= nac) {
      adv <- matrix(0, nrow = nc, ncol = nac)          # the whole distribution reaches the top class
      adv[, nac] <- rowSums(ap)
    } else {
      adv <- matrix(0, nrow = nc, ncol = nac)          # the k youngest classes receive nothing
      adv[, (k + 1):nac] <- ap[, seq_len(nac - k), drop = FALSE]                     # usual shift
      adv[, nac] <- adv[, nac] + rowSums(ap[, (nac - k + 1):nac, drop = FALSE])      # acx collects the overflow
    }
    res[, t] <- rowSums(adv * matrix(r[, t, ], nrow = nc))
  }
  out <- magclass::dimSums(Aac, dim = 3)
  out[, , ] <- res
  out
}

#' @title edgeHarvestC0
#' @description Unreduced carbon on the area clear-cut in this step, sum_ac rho_ac,t hv_ac,t: the basis of the
#'   harvest-reset part of the cohort-turnover term H. The harvest areas are indexed by the class the area is
#'   harvested FROM in step t (after the cohort shift), so they pair with the current step's unreduced densities.
#'   Internal; see .edgeCommittedStockTerms.
#' @param hvAc harvested area per cluster, step and age class (magpie, cells x years x ac; Mha)
#' @param rhoAc unreduced vegc density per cluster, step and age class (same shape; tC/ha)
#' @return magpie object (cells x years x 1; Mio tC)
#' @author Michael Crawford
#' @keywords internal
#' @noRd
.edgeHarvestC0 <- function(hvAc, rhoAc) {
  hvAc <- .edgeAcSort(hvAc)
  rhoAc <- .edgeAcSort(rhoAc)
  if (!identical(magclass::getNames(hvAc), magclass::getNames(rhoAc))) {
    stop("reportEmissions (edge, L4): the harvested-area and unreduced-density age classes of a pool do not match")
  }
  magclass::dimSums(hvAc * rhoAc, dim = 3)
}
