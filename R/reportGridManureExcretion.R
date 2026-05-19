#' @title reportGridManureExcretion
#' @description Reports manure excretion and confinement losses at grid level (0.5 degree).
#'
#' @export
#'
#' @param gdx GDX file
#' @param confinementWeighting Weighting for disaggregating the confinement loss/recycling
#'   outputs to grid: "cropland" (default, by cropland area) or "livestock"
#'   (`ManureExcretion` livestock-location weights, the pre-2026 behaviour).
#'
#' @return MAgPIE object
#' @author Benjamin Leon Bodirsky, Michael Crawford
#' @examples
#' \dontrun{
#' x <- reportGridManureExcretion(gdx)
#' }
#'
#' @section Confinement weighting:
#' Summary outputs (`Manure`, `Manure|+|...`, `Manure|++|...`) use livestock-location
#' weights (`ManureExcretion`). The confinement loss/recycling outputs follow
#' `confinementWeighting`: "cropland" (default) distributes by cropland area, matching the
#' cropland+pasture denominator in `getReportGridNitrogenPollution`; "livestock" uses the
#' pre-2026 `ManureExcretion` weights. Under "cropland", confinement and summary outputs do
#' not balance per grid cell, though cluster totals are conserved.
#' @md

reportGridManureExcretion <- function(gdx, confinementWeighting = "cropland") {

  confinementWeighting <- match.arg(confinementWeighting, c("cropland", "livestock"))

  # Section 1: summary outputs, livestock-location weighting via ManureExcretion.
  manure <- ManureExcretion(gdx, level = "grid")
  awms <- dimSums(manure, dim = "kli")
  kli <- dimSums(manure, dim = "awms")

  # Section 2: confinement disaggregation, gated by confinementWeighting (see @section).
  # Both branches set vm_manure_confinement and confinement_agri for Sections 3-4.
  vm_manure_confinement <- collapseNames(readGDX(gdx, "ov_manure_confinement")[, , "level"][, , "nr"])

  if (confinementWeighting == "cropland") {
    # cropland-area weighting (default)
    confinement_cluster <- collapseNames(
      readGDX(gdx, "ov_manure", select = list(type = "level"))[, , "confinement"][, , "nr"]
    )
    confinement_agri <- dimSums(confinement_cluster, dim = 3)
    confinement_agri <- gdxAggregate(gdx = gdx, x = confinement_agri,
                                     weight = "land", types = "crop",
                                     to = "grid", absolute = TRUE)
    vm_manure_confinement <- gdxAggregate(gdx = gdx, x = vm_manure_confinement,
                                          weight = "land", types = "crop",
                                          to = "grid", absolute = TRUE)
  } else {
    # livestock-location weighting (pre-2026): ManureExcretion confinement output
    confinement <- collapseNames(manure[, , "confinement"])
    confinement_agri <- dimSums(confinement, dim = 3)
    vm_manure_confinement <- gdxAggregate(gdx = gdx, x = vm_manure_confinement,
                                          weight = manure[, , "confinement"],
                                          to = "grid", absolute = TRUE)
  }

  # Section 3: emission fate shares (destiny, dimensionless) applied to vm_manure_confinement.
  pollutants <- c("n2o_n_direct", "nh3_n", "no2_n", "no3_n")
  f55_awms_recycling_share <- readGDX(gdx, "f55_awms_recycling_share")
  f51_ef3_confinement <- readGDX(gdx, "f51_ef3_confinement")
  im_maccs_mitigation <- readGDX(gdx, "im_maccs_mitigation")[, , "awms"][, , pollutants]
  f51_ef3_confinement <- f51_ef3_confinement * collapseNames(1 - im_maccs_mitigation)
  destiny <- add_columns(f51_ef3_confinement, addnm = c("recycling"), dim = 3.3)
  destiny[, , "recycling"] <- f55_awms_recycling_share
  # destiny[,,"n2_n"]<- (1-dimSums(destiny,dim=3.3,na.rm=TRUE))
  if (any(dimSums(destiny, dim = 3.3, na.rm = TRUE) > 1)) {
    stop("error in emission factors")
  }
  destiny <- gdxAggregate(gdx = gdx, x = destiny, to = "grid", absolute = FALSE)

  # memory problems
  emis1 <- vm_manure_confinement[, , c("livst_rum", "livst_milk")] * destiny[, , c("livst_rum", "livst_milk")]
  emis2 <- vm_manure_confinement[, , c("livst_pig", "livst_chick", "livst_egg")] * destiny[, , c("livst_pig", "livst_chick", "livst_egg")]
  emis1 <- dimSums(emis1, dim = "awms_conf")
  emis2 <- dimSums(emis2, dim = "awms_conf")
  destiny <- mbind(emis1, emis2)

  # Section 4: assemble outputs.
  total <- setNames(dimSums(manure), "Manure")
  getNames(awms) <- paste0("Manure|+|", reportingnames(getNames(awms)))
  getNames(kli) <- paste0("Manure|++|", reportingnames(getNames(kli)))

  # confinement loss/recycling outputs (see @section for the grid-level balance caveat)
  losses <- dimSums(destiny[, , pollutants], dim = "kli")
  getNames(losses) <- paste0("Manure|Manure In Confinements|Losses|", reportingnames(getNames(losses)))
  recycling <- dimSums(destiny[, , "recycling"], dim = "kli")
  getNames(recycling) <- paste0("Manure|Manure In Confinements|+|Recycled")
  confinement_loss <- confinement_agri - recycling
  getNames(confinement_loss) <- paste0("Manure|Manure In Confinements|+|Losses")

  out <- mbind(total, awms, kli, recycling, confinement_loss, losses)

  return(out)
}
