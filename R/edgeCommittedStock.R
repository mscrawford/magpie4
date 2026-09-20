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
#' PER-COHORT TERMS (E'', F'', G'', T''; spec amendment of 2026-09-20 "per-cohort definition of E and T", DECIDED,
#' record section 11 item 8). The pool-level E above charges area leaving at the POOL-MEAN deficit density and
#' therefore carries both the mirror of T and the selection of which classes were converted; H is the pool-level
#' symptom of the same thing. The per-cohort pair removes it by construction. With dd_ac = g rho_ac and src(ac) the
#' class a cohort in ac came from (acx has k + 1 sources, so the sum below is over all of them):
#' \itemize{
#'   \item E'' = sum_ac Atilde_ac,t (dd_ac,t - dd_src(ac),t-1): the deficit change on the STANDING, AGED cohorts.
#'         Because sum_ac Atilde_ac,t dd_ac,t = g_t C0adv_t and sum_ac Atilde_ac,t dd_src(ac),t-1 = D_t-1 (the
#'         advanced distribution carries the previous cohorts' own areas and deficits), this collapses to
#'         E'' = g_t C0adv_t - D_t-1. No per-class loop is needed.
#'   \item F'' = sum_ac Atilde_ac,t (g_t - g_t-1) rho_ac,t = (g_t - g_t-1) C0adv_t
#'   \item G'' = sum_ac Atilde_ac,t g_t-1 (rho_ac,t - rho_src(ac),t-1) = g_t-1 (C0adv_t - C0_t-1)
#'   \item T'' = sum_ac dd_ac,t (A_ac,t - Atilde_ac,t) = D_t - g_t C0adv_t: every per-class area change at the
#'         class's OWN deficit density (harvest resets, conversions in and out, disturbance resets, establishment).
#' }
#' F'' + G'' = E'' and E'' + T'' = D_t - D_t-1, both by construction. INTERACTION CONVENTION: F'' is evaluated at
#' the CURRENT density rho_ac,t and G'' at the PREVIOUS exposure g_t-1, so the Bennet interaction term
#' Delta g Delta rho sits in F'' - the same convention as the pool-level F and G above, kept deliberately.
#' Note the basis change: E'' sits on the previous step's area (sum_ac Atilde = A_t-1), the pool-level E on A_t.
#' That is the point of the amendment - all area movement is in T'', none of it in E''.
#'
#' Sub-lines of T'', informational and disjoint, each from an exported state:
#' \itemize{
#'   \item T''_harvest = g_t (rhoEst_t hvArea_t - hvC0_t): the clear-cut reset. Area hv_ac,t leaves class ac at
#'         dd_ac,t and re-enters the pool in the ESTABLISHMENT classes. GAMS spreads it equally over ac_est, which
#'         is the first k_t classes (28_ageclass/oct24/presolve.gms:10, ord(ac) <= m_yeardiff_forestry(t)/5, i.e.
#'         exactly the classes Atilde leaves empty) and harvested secondary forest STAYS secondary forest
#'         (35_natveg q35_secdforest_regeneration), so rhoEst is the mean unreduced density over those classes.
#'         For a 5-yr step that is ac0 alone, where rho is 0 by construction; for a 10-yr step it is the mean of
#'         ac0 and ac5. Area-neutral for the pool, which is why the pool-level T is blind to it.
#'   \item T''_conv = -g_t sum_ac rho_ac,t (red_ac,t - hv_ac,t): area genuinely leaving the pool, at each class's
#'         own deficit. The harvest is SUBTRACTED because \code{ov35_secdforest_reduction} bounds
#'         \code{ov35_hvarea_secdforest} from above (q35_hvarea_secdforest is =l=; verified on the gate run, min
#'         red - hv = -7.6e-14), so counting both would double-count the leaving side.
#'   \item T''_other = T'' - T''_harvest - T''_conv: establishment from other sources (primforest harvest
#'         reclassified, restoration), youngsecdf maturation, conversions IN, and the disturbance redistribution -
#'         the latter because \code{red} is measured against the post-disturbance presolve state while Atilde is
#'         the shifted SOLVED state (see the solved-state note above). Defined as the residual, so the three sum
#'         to T'' exactly.
#' }
#' A pool WITHOUT age classes (primforest) is a single class, so C0adv = rho_t A_t-1: E'' = (dd_t - dd_t-1) A_t-1
#' (the previous area, as the definition requires) and T'' = dd_t (A_t - A_t-1) (at the current deficit). H is 0
#' because rhoAdv reduces to rho. This is NOT the pool-level pair, which sits on A_t and dd_t-1 and so keeps the
#' interaction (A_t - A_t-1)(dd_t - dd_t-1) in the headline; the default above removes it (audit F1).
#' Primforest has its own exported harvest and reduction (\code{ov35_hvarea_primforest},
#' \code{ov35_primforest_reduction}), so its area transfer splits like any other pool: harvest leaving at dd_P,t
#' with no return (it becomes secondary forest in the establishment classes, which the secdforest pool counts as an
#' entry under "other transitions"), the non-harvest reduction under conversions, and the shifting-cultivation
#' disturbance loss (\code{p35_disturbance_loss_primf}, which the reduction excludes because the presolve
#' subtracts it from \code{pcm_land} first - verified: -dA = reduction + disturbance to 1e-8 Mha) as the residual
#' under other transitions (audit F2).
#'
#' A note on emptied classes and emptied pools (audit F7, corrected by the re-check). E'' charges the aged cohorts
#' that land in classes the solve then empties within the same step at the pool's g_t, and T'' removes exactly the
#' same area at the same dd_ac,t, so the pair is consistent and the identities hold: an emptied CLASS needs no
#' correction. An emptied POOL did: when the solve takes a cluster's whole pool to zero, D_t and C0_t are 0 and
#' g_t = D_t / C0_t is 0 by the div0 convention, which makes dd_ac,t = 0 for cohorts that were standing at t-1.
#' The whole deficit release then landed in the HEADLINE (E'' = -D_t-1) with T'' = 0, recording no area movement
#' although the pool's entire area left. The applied fraction is not arbitrary there - it is exposure-derived and
#' still defined - so the fix is to take g from \code{p35_degr_applied}, which restores
#' E'' = g_t C0adv - D_t-1 and T'' = -g_t C0adv. Measured on 2026-09-20: 20 emptied youngsecdf cluster-steps and
#' one secdforest step (IND.42, y2020) in the base run, one primforest step (SSA.163, y2045) in the price pilot;
#' World E'' 2020 213.0 -> 215.6 Mt CO2/yr and +0.249 Gt CO2 cumulative 2020-2100.
#'
#' @param D committed deficit stock per cluster and step (magpie object, cells x years x 1; Mio tC)
#' @param A solved area of the pool (same shape; Mha)
#' @param C0 unreduced carbon of the pool on the solved area, sum over age classes of rho_ac A_ac (same shape; Mio tC)
#' @param C0adv unreduced carbon of the PREVIOUS step's age distribution advanced by the step's cohort shift and
#'   evaluated on the current step's unreduced densities (same shape, NA in the first step; Mio tC), from
#'   \code{.edgeAdvancedC0}. For a single-class pool it is rho_ac,t A_t-1 from the EXPORTED class density; if left
#'   NULL it falls back to (C0_t / A_t) A_t-1, which is the same only while the pool has area.
#' @param singleClass TRUE for a pool with no age structure (primforest): rhoAdv is then rho exactly, so the
#'   pool-level cohort turnover H is exactly 0 rather than 0 to rounding.
#' The pool-level continuity terms (E, F, G, H, Hharvest, T) keep D / C0 by design, so they reproduce the interim
#' build on every run; only the per-cohort pair and its sub-lines read the exported applied fraction.
#'
#' @param g the pool's applied deficit fraction per cluster and step (same shape; dimensionless), from
#'   \code{p35_degr_applied} at age class \code{acx} - the value is ac-invariant for secdforest and other and is
#'   carried in \code{acx} alone for primforest, so \code{acx} reads all three. NULL falls back to D / C0, which
#'   is correct only while the pool has carbon: pass it wherever the export exists. Checked against D / C0 to
#'   1e-8 wherever C0 > 0, so the export and the stock identity stay tied. Used for the PER-COHORT pair and its
#'   sub-lines only - the pool-level continuity terms keep D / C0 whatever is passed here.
#' @param poolName pool label used only in that check's error message
#' @param hvC0 unreduced carbon on the area harvested in this step, sum over age classes of rho_ac,t hv_ac,t (same
#'   shape; Mio tC), from \code{.edgeHarvestC0}. NULL for a pool the model does not clear-cut per age class, which
#'   gives Hharvest = 0.
#' @param hvBack unreduced carbon the clear-cut area re-enters THIS pool with (same shape; Mio tC): for secdforest
#'   the establishment-class density times the harvested area (\code{.edgeEstRho} x the area), and NULL or 0
#'   wherever the harvested area leaves the pool - harvested youngsecdf re-enters othernat
#'   (\code{q35_other_regeneration}) and harvested primforest is reclassified as secondary forest
#'   (\code{q35_secdforest_regeneration}), so in both cases nothing comes back
#' @param redC0 unreduced carbon on the FULL per-class area reduction, sum_ac rho_ac,t red_ac,t (same shape;
#'   Mio tC). The harvest is subtracted inside this function, not by the caller
#' @return list of magpie objects S, E, F, G, H, Hharvest, T (pool level) and Epc, Fpc, Gpc, Tpc, TpcHarv,
#'   TpcConv, TpcOther (per cohort), all Mio tC per step (flows are per step, not per year)
#' @author Michael Crawford
#' @keywords internal
#' @noRd
.edgeCommittedStockTerms <- function(D, A, C0, C0adv = NULL, singleClass = FALSE, g = NULL, poolName = NULL,
                                     hvC0 = NULL, hvBack = NULL, redC0 = NULL) {
  div0 <- function(a, b) { r <- a / b; r[!is.finite(r)] <- 0; r }
  dd  <- div0(D, A)
  rho <- div0(C0, A)
  # TWO fractions, deliberately. gRatio = D / C0 is the interim build's pool fraction and the POOL-LEVEL
  # CONTINUITY terms keep it by design, so they reproduce 09d21e4d on every run; only the per-cohort pair reads the
  # export (owner's decision, 2026-09-20). gPc is that export where the caller supplies it: D / C0 collapses to 0
  # by the div0 convention when the solve empties a pool, which would push the whole deficit release of the cohorts
  # standing at t-1 into E'' and leave T'' = 0 (audit re-check, same class as F1), while p35_degr_applied stays
  # defined there. The tie between the two is asserted wherever C0 > 0.
  gRatio <- div0(D, C0)
  gPc <- gRatio
  if (!is.null(g)) {
    dv <- abs(as.array(g) - as.array(gRatio))
    dv[as.array(C0) <= 0] <- 0
    if (any(dv > 1e-8, na.rm = TRUE)) {
      i <- which(dv == max(dv, na.rm = TRUE), arr.ind = TRUE)[1, ]
      stop(sprintf(paste0("reportEmissions (edge, L4): the exported applied deficit fraction and D / C0 disagree ",
                          "by %.3g in pool %s, cluster %s, %s"), max(dv, na.rm = TRUE),
                   if (is.null(poolName)) "(unnamed)" else poolName,
                   dimnames(dv)[[1]][i[1]], dimnames(dv)[[2]][i[2]]))
    }
    gPc <- g
  }
  lagA <- .edgeLagYears(A)
  lagG <- .edgeLagYears(gRatio)     # continuity terms: the interim ratio
  lagGpc <- .edgeLagYears(gPc)      # per-cohort pair: the exported applied fraction
  lagD <- .edgeLagYears(D)
  # A pool without age classes is ONE class, so its advanced distribution is its own previous area at the current
  # density: C0adv = rho_t A_t-1. Defaulting it here (rather than short-circuiting to the pool-level terms) is what
  # puts E'' on Atilde and T'' at dd_t for such a pool too - otherwise the headline keeps its Bennet interaction
  # (A_t - A_t-1)(dd_t - dd_t-1) (audit F1, 2026-09-20: World base 2020 +2.3, 2030 -7.5, 2100 -6.0 Mt CO2/yr).
  # Every pool now runs the same path; H is still 0 here because rhoAdv reduces to rho.
  # A single-class pool should be given C0adv = rho_ac,t A_t-1 built from the EXPORTED class density. The fallback
  # below uses rho = C0_t / A_t, which equals it only while the pool has area: where the solve empties a pool
  # div0 sends rho_t to 0 and the F'' / G'' split collapses, though E'' + T'' still closes. Pass C0adv explicitly.
  # (Primforest has 4 such cluster-steps in the SSP2 price pilot, but 3 carry D_t-1 = 0 - no exposure - so only
  # SSA.163 y2045, 2.6 Mt CO2, is material.)
  if (is.null(C0adv)) { singleClass <- TRUE; C0adv <- rho * lagA }   # a NULL C0adv can only mean one class
  # For one class rhoAdv IS rho, so H is exactly 0; div0(C0adv, lagA) would differ by a ULP and leave a
  # 1e-16-relative H where the invariant is exact.
  rhoAdv <- if (singleClass) rho else div0(C0adv, lagA)
  out <- list(S = D,
              E = (dd - .edgeLagYears(dd)) * A,
              F = (gRatio - lagG) * rho * A,
              G = lagG * (rhoAdv - .edgeLagYears(rho)) * A,
              H = lagG * (rho - rhoAdv) * A,
              # the harvest-reset part of H: a sub-term, NOT an addend of E. lagG * 0 keeps the first step NA.
              # Only a pool WITH age classes has a pool-level composition term, so a single-class pool contributes
              # 0 here as well - its H is identically 0 and this continuity line must stay consistent with it.
              Hharvest = if (singleClass || is.null(hvC0)) lagG * 0 else -lagG * hvC0,
              T = .edgeLagYears(dd) * (A - lagA))
  # Per-cohort pair. Written so that E'' + T'' = dD cancels the g_t C0adv term EXACTLY in floating point; the
  # F'' + G'' = E'' closure carries one rounding of g_t-1 C0_t-1 against D_t-1 (~1e-16 relative).
  Epc <- gPc * C0adv - lagD
  Tpc <- D - gPc * C0adv
  zero <- Tpc * 0                                        # keeps the first step NA
  hvOut <- if (is.null(hvC0)) zero else hvC0
  # The harvest sub-line: area leaves its class at that class's deficit and re-enters the pool with hvBack (0 where
  # the harvested area leaves the pool altogether). The harvest is SUBTRACTED from the reduction here, inside the
  # term, so a caller cannot forget it: the per-class reduction exports bound the harvest from above (audit F3).
  harv <- if (is.null(hvC0) && is.null(hvBack)) zero else gPc * ((if (is.null(hvBack)) zero else hvBack) - hvOut)
  conv <- if (is.null(redC0)) zero else -gPc * (redC0 - hvOut)
  out <- c(out, list(Epc = Epc,
                     Fpc = (gPc - lagGpc) * C0adv,
                     Gpc = lagGpc * (C0adv - .edgeLagYears(C0)),
                     Tpc = Tpc,
                     TpcHarv = harv, TpcConv = conv, TpcOther = Tpc - harv - conv))
  out
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

#' @title edgeEstRho
#' @description Mean unreduced density of the ESTABLISHMENT age classes at t: the classes clear-cut area re-enters.
#'   GAMS spreads establishment equally over \code{ac_est}, which 28_ageclass/oct24/presolve.gms:10 sets to
#'   \code{ord(ac) <= m_yeardiff_forestry(t)/5} - the first k_t classes, exactly the ones the cohort shift leaves
#'   empty. \code{ac_est} is a DYNAMIC set, so a gdx carries only its last step's membership and it must be derived
#'   from k_t here rather than read. Internal; see .edgeCommittedStockTerms.
#' @param rhoAc unreduced vegc density per cluster, step and age class (magpie, cells x years x ac; tC/ha)
#' @param shift cohort shift per step from .edgeAcShift (NA first)
#' @return magpie object (cells x years x 1; tC/ha), NA where the shift is NA
#' @author Michael Crawford
#' @keywords internal
#' @noRd
.edgeEstRho <- function(rhoAc, shift) {
  rhoAc <- .edgeAcSort(rhoAc)
  r <- as.array(rhoAc)
  nc <- dim(r)[1]; ny <- dim(r)[2]; nac <- dim(r)[3]
  res <- matrix(NA_real_, nrow = nc, ncol = ny)
  for (t in seq_len(ny)) {
    k <- shift[t]
    if (is.na(k) || k < 1) next
    res[, t] <- rowMeans(matrix(r[, t, seq_len(min(k, nac))], nrow = nc))
  }
  out <- magclass::dimSums(rhoAc, dim = 3)
  out[, , ] <- res
  out
}
