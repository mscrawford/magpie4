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
#'   \item G = g_t-1 (rho_t - rho_t-1) A_t: foregone growth (the reference density changing)
#'   \item T = dd_t-1 (A_t - A_t-1): area transfer, a diagnostic (that carbon sits in land-use change)
#' }
#' Identities: F + G = E and E + T = D_t - D_t-1, exactly. Flows are NA in the first step. Where a pool has
#' no area (or no carbon) the densities are 0, so an empty pool contributes nothing to any term.
#' Spec: fragmentation repo, 05-temporal-accounting/documents/COMMITTED_STOCK_ACCOUNTING.md.
#'
#' @param D committed deficit stock per cluster and step (magpie object, cells x years x 1; Mio tC)
#' @param A solved area of the pool (same shape; Mha)
#' @param C0 unreduced carbon of the pool on the solved area, sum over age classes of rho_ac A_ac (same shape; Mio tC)
#' @return list of magpie objects S, E, F, G, T (Mio tC per step; flows are per step, not per year)
#' @author Michael Crawford
#' @keywords internal
#' @noRd
.edgeCommittedStockTerms <- function(D, A, C0) {
  div0 <- function(a, b) { r <- a / b; r[!is.finite(r)] <- 0; r }
  dd  <- div0(D, A)
  rho <- div0(C0, A)
  g   <- div0(D, C0)
  list(S = D,
       E = (dd - .edgeLagYears(dd)) * A,
       F = (g - .edgeLagYears(g)) * rho * A,
       G = .edgeLagYears(g) * (rho - .edgeLagYears(rho)) * A,
       T = .edgeLagYears(dd) * (A - .edgeLagYears(A)))
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
