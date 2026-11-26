#' @title primaryPerSecondary
#' @description Calculates the amount of primary products needed per unit of secondary
#'              (and tertiary) product, accounting for processing chains and co-product allocation.
#'              This function traces back from secondary/tertiary products through processing
#'              stages to the original primary crop inputs.
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#'              "regglo" (regional and global) or any other aggregation level defined in gdxAggregate
#' @param allocation Method for allocating primary product use when co-products are produced.
#'                   Options:
#'                   "mass" (default): Allocation based on dry matter content of outputs
#'                   "value": Allocation based on economic value of outputs
#'                   "none": No allocation - full primary input attributed to each output
#'                             (sum of allocations > 100 percent when co-products exist)
#' @param composition Where the primary composition of a region's secondary production comes from.
#'                   "shares" (default): from the input data (f20_processing_shares and conversion
#'                             factors), i.e. per tonne of secondary made from the primaries in their
#'                             historical shares.
#'                   "levels": from the model solution (ov20_dem_processing, ov20_secondary_substitutes,
#'                             ov_prod_reg), i.e. per tonne of the secondary the region actually produced.
#'                             With "mass" or "value" allocation, sum over secondaries of factor times
#'                             (production - processing use) then equals the processing use of each primary
#'                             (vm_dem_processing), less free disposal. See details.
#'
#' @details With \code{composition = "levels"} the primary content of a region's production of secondary j is
#'          the processing input that actually made j, divided over the whole production of j:
#'          \itemize{
#'            \item each process p in no_milling_ginning20 other than substitutes contributes
#'                  ov20_dem_processing(p, k) times the allocation weight of j in (p, k); several processes making
#'                  j from k are each counted with their own input (e.g. maize to distillers grain by distilling and
#'                  by fermentation);
#'            \item tonnes relabelled by v20_secondary_substitutes carry the content of the substitute input that
#'                  covers them, pool by pool as in the q20_processing_substitution_* equations (oils by oils,
#'                  molasses by sugar, oilcakes and distillers grain by oilcake_substitutes20, split between the
#'                  two in nitrogen of the nominal origin, brans by kcereals20). The whole substitute input of a
#'                  pool is allocated, including any input above the pool's need;
#'            \item secondary inputs (e.g. sugar to alcohol, oils used as substitutes) carry the region's own
#'                  content of that secondary (a linear system, solved per region and year);
#'            \item the divisor is the whole production ov_prod_reg(j), so tonnes from the processing balance
#'                  flow dilute the content and the content of overproduced (wasted) tonnes stays with the
#'                  produced ones;
#'            \item milling and ginning pass no primary content to their outputs (brans, fibres, maize and rice oils
#'                  and cakes from milling): their input is food use (milling) or the whole production (ginning) of
#'                  the crop, which is not processing use (vm_dem_processing) and stays with the crop's own uses.
#'                  Brans still carry cereal content through the brans substitution pool; fibres carry none;
#'            \item a region that does not produce j gets the factor of the world as a single region.
#'          }
#'          Input to a process without outputs (free disposal), substitute input of a pool without relabelled
#'          tonnes, and substitute input of a product outside the four pools are not attributed to any secondary.
#'          The last counts in vm_dem_processing but no q20_processing_substitution_* equation turns it into a
#'          secondary, so the identity above misses it, without a warning.
#'
#'          Guards, each with a warning that names region and year:
#'          \itemize{
#'            \item processing or substitute input into a secondary the region does not produce
#'                  (ov_prod_reg <= 0) is not attributed (the identity above then misses that content);
#'            \item substitute input to a pool without relabelled tonnes is not attributed (likewise);
#'            \item where a secondary is almost entirely made from itself (in module 20 only oils, used as
#'                  substitutes for oils: production minus that input below 1 percent of production), its content
#'                  per tonne would be the content of the real output amplified more than 100 times; the region
#'                  then gets the World factor for that secondary (the identity then no longer holds for that
#'                  region and year). Where the World itself is in that state, the content is not attributed and
#'                  the factor is 0;
#'            \item NA levels are read as 0.
#'          }
#'          The solve stops if a factor comes out below -1e-9 or not finite, which the pool structure rules out;
#'          rounding can leave factors of the order of -1e-15. Where no input goes into a secondary without
#'          production and none is almost entirely made from itself, it also checks the balance of the solve, per region
#'          and year (content of production net of the secondaries' own processing use = the primary input attributed
#'          to them), and stops if it fails.
#'          The pool structure is that of module 20 realization substitution_may21; for a gdx without its symbols
#'          (ov20_secondary_substitutes, oilcake_substitutes20, kcereals20, no_milling_ginning20) the function stops.
#'
#' @return MAgPIE object containing the amount of primary product needed per unit of secondary product (tDM/tDM)
#'
#' @author Kristine Karstens, David M Chen
#' @seealso \code{\link{production}}, \code{\link{trade}}
#' @importFrom magclass collapseNames mbind dimSums setNames getItems getYears add_dimension getNames
#' @importFrom gdx2 readGDX
#' @examples
#' \dontrun{
#' x <- primaryPerSecondary(gdx)
#' }
#' @export

primaryPerSecondary <- function(gdx,
                                file = NULL,
                                level = "reg",
                                allocation = "mass",
                                composition = "shares") {

  # Validate allocation method
  if (!allocation %in% c("mass", "value", "none")) {
    stop("allocation must be one of: 'mass', 'value', or 'none'")
  }
  if (!composition %in% c("shares", "levels")) {
    stop("composition must be one of: 'shares' or 'levels'")
  }

  # Read required sets from GDX
  ksd       <- readGDX(gdx, "ksd")           # Secondary products
  kcr       <- readGDX(gdx, "kcr")           # Crop products
  kpr       <- readGDX(gdx, "kpr")           # Products that can be processed
  primCrops <- intersect(kcr, kpr)           # Primary crops that can be processed
  simYears  <- readGDX(gdx, "t")             # Simulation years

  # Read processing parameters
  procShares <- readGDX(gdx, "f20_processing_shares")[, simYears, ] # Which primary produces which secondary
  # Conversion factors; "levels" uses the ones the model solved with
  if (composition == "levels" && is.null(readGDX(gdx, "i20_processing_conversion_factors", react = "silent"))) {
    stop("composition = 'levels' needs module 20 realization substitution_may21 ",
         "(not in gdx: i20_processing_conversion_factors)")
  }
  procConvF  <- readGDX(gdx, if (composition == "levels") "i20_processing_conversion_factors"
                        else "f20_processing_conversion_factors")[, simYears, ]

  # Rename kpr dimension to distinguish from ksd
  getNames(procShares, dim = "kpr") <- paste0("kpr_", getNames(procShares, dim = "kpr"))
  getNames(procConvF, dim = "kpr")  <- paste0("kpr_", getNames(procConvF, dim = "kpr"))

  # ================================================================================
  # STEP 1: Calculate allocation weights based on chosen method
  # ================================================================================

  if (allocation == "mass") {
    # Mass-based allocation: Use conversion factors (DM output per DM input)
    # Allocation weight = output_mass / sum(all_output_masses)
    allocWeights <- procConvF / dimSums(procConvF, dim = "ksd")
    # Replace NaN with 0 (occurs when no outputs)
    allocWeights[is.nan(allocWeights)] <- 0
    allocWeights[is.infinite(allocWeights)] <- 0

  } else if (allocation == "value") {
    # Value-based allocation: Use economic value of outputs
    # Allocation weight = (output_mass * price) / sum(output_mass * price for all outputs)
    prices <- readGDX(gdx, "f15_prices_initial")[, , ksd]

    # Value = mass * price for each product
    productValues <- procConvF * prices
    
    # Normalize to get allocation weights
    allocWeights <- productValues / dimSums(productValues, dim = "ksd")
    allocWeights[is.nan(allocWeights)] <- 0
    allocWeights[is.infinite(allocWeights)] <- 0

  } else if (allocation == "none") {
    # No allocation: Each output gets full attribution of all inputs
    # Create dummy with ones where conversion factors are non-zero
    allocWeights <- procConvF
    allocWeights[allocWeights != 0] <- 1
  }

  if (composition == "levels") {
    if (level != "reg") {
      stop("Aggregation levels other than reg are currently not supported.")
    }
    getNames(allocWeights, dim = "kpr") <- sub("^kpr_", "", getNames(allocWeights, dim = "kpr"))
    x <- .primaryPerSecondaryLevels(gdx, allocWeights, simYears, ksd, setdiff(kpr, ksd))
    return(out(x[, , primCrops], file))
  }

  # ================================================================================
  # STEP 2: Calculate primary per secondary (direct processing)
  # ================================================================================
  # For each secondary product, how much of each primary product is needed?
  # Formula: (1 / conversion_factor) * processing_share * allocation_weight

  # Inverse of conversion factor = primary input per unit secondary output
  primPerSecd <- procShares * allocWeights / procConvF

  # Handle division by zero
  primPerSecd[is.nan(primPerSecd)] <- 0
  primPerSecd[is.infinite(primPerSecd)] <- 0

  # Sum over processing types to get total primary per secondary
  primPerSecd <- dimSums(primPerSecd, dim = "processing20")

  # ================================================================================
  # STEP 3: Handle tertiary processing (secondary products that are further processed)
  # ================================================================================
  # Some secondary products (ksd) can also be processed further (are in kpr)
  # e.g., sugar, molasses, oils can be inputs to further processing

  secdPrims <- intersect(kpr, ksd)  # Secondary products that can be processed further
  tertTest  <- dimSums(procConvF[,,paste0("kpr_", secdPrims)], dim = c("processing20", "kpr"))
  tertSecds <- where(tertTest > 0)$true$data

  if (length(secdPrims) > 0) {
    # Extract relationships where secondary products are inputs to tertiary processing
    secdPerTert <- primPerSecd[, , paste0("kpr_", secdPrims)][, , tertSecds]

    # Rename dimensions to track the processing chain
    getNames(secdPerTert, dim = "ksd") <- paste0("ktr_", getNames(secdPerTert, dim = "ksd"))
    getNames(secdPerTert, dim = "kpr") <- gsub("kpr_", "", getNames(secdPerTert, dim = "kpr"))
    getSets(secdPerTert, fulldim = FALSE)[3] <- "ktr.ksd"

    # Calculate primary needed for tertiary via secondary
    # tertiary output needs secondary input, which needs primary input
    primPerTert <- secdPerTert * primPerSecd[, simYears, secdPrims]

    # Handle division by zero
    primPerTert[is.nan(primPerTert)] <- 0
    primPerTert[is.infinite(primPerTert)] <- 0

    # Sum over the intermediate secondary processing step
    primPerTert <- dimSums(primPerTert, dim = "ksd")

    # Clean up dimension names
    getNames(primPerTert, dim = "kpr") <- gsub("kpr_", "", getNames(primPerTert, dim = "kpr"))
    getNames(primPerTert, dim = "ktr") <- gsub("ktr_", "", getNames(primPerTert, dim = "ktr"))

    # Combine direct secondary and tertiary calculations
    # For products that are both secondary and tertiary, we need the total
    getNames(primPerSecd, dim = "kpr") <- gsub("kpr_", "", getNames(primPerSecd, dim = "kpr"))

    # Add tertiary contributions to the direct secondary and reduce to real primary products
    primPerSecd[, , tertSecds] <- primPerSecd[, , tertSecds] + primPerTert[, , tertSecds]
    realPrim    <- setdiff(kpr, ksd)
    primPerSecd <- primPerSecd[, , realPrim]

  } else {
    # No tertiary processing, just clean up names and reduce to real primary products
    getNames(primPerSecd, dim = "kpr") <- gsub("kpr_", "", getNames(primPerSecd, dim = "kpr"))
    realPrim    <- setdiff(kpr, ksd)
    primPerSecd <- primPerSecd[, , realPrim]
  }

  # ================================================================================
  # STEP 4: Filter to only primary crops
  # ================================================================================
  x <- primPerSecd[, , primCrops]

  # ================================================================================
  # STEP 5: Apply regional aggregation if requested
  # ================================================================================
  if (level != "reg") {
    stop("Aggregation levels other than reg are currently not supported.")
    x <- gdxAggregate(gdx = gdx, x = x, weight = NULL, to = level, absolute = FALSE)
  }

  out(x, file)
}

# Primary content per tonne of each region's secondary production, built from the model levels
# (composition = "levels"; see the details of primaryPerSecondary). allocWeights: GLO, t, processing20.ksd.kpr.
.primaryPerSecondaryLevels <- function(gdx, allocWeights, simYears, ksd, realPrim) {
  oilcakeSubst <- readGDX(gdx, "oilcake_substitutes20", react = "silent")
  cereals      <- readGDX(gdx, "kcereals20", react = "silent")
  noMillGin    <- readGDX(gdx, "no_milling_ginning20", react = "silent")
  substLevel   <- readGDX(gdx, "ov20_secondary_substitutes", react = "silent", select = list(type = "level"))
  absent <- c("ov20_secondary_substitutes", "oilcake_substitutes20", "kcereals20",
              "no_milling_ginning20")[c(is.null(substLevel), is.null(oilcakeSubst), is.null(cereals),
                                        is.null(noMillGin))]
  if (length(absent) > 0) {
    stop("composition = 'levels' needs module 20 realization substitution_may21 (not in gdx: ",
         paste(absent, collapse = ", "), ")")
  }
  if (dim(allocWeights)[1] != 1) {
    stop("composition = 'levels' expects global processing conversion factors")
  }
  lev <- function(name, raw = readGDX(gdx, name, select = list(type = "level"))) {
    x <- collapseNames(raw[, simYears, ])
    if (anyNA(x)) {
      warning("primaryPerSecondary: NA level(s) of ", name, " read as 0 in ", .ppsWhere(is.na(x)))
      x[is.na(x)] <- 0
    }
    return(x)
  }
  # the World as one more region, first: its factors are computed from the World totals
  withWorld <- function(x) {
    if ("World" %in% getItems(x, dim = 1)) {
      stop("composition = 'levels' adds the World as a region named 'World'; the gdx already has one")
    }
    world <- dimSums(x, dim = 1)
    getItems(world, dim = 1) <- "World"
    return(mbind(world, x))
  }
  dem   <- withWorld(lev("ov20_dem_processing"))                               # i, t, processing_subst20.kpr
  subst <- withWorld(lev("ov20_secondary_substitutes", substLevel))            # i, t, ksd.kpr
  prod  <- withWorld(lev("ov_prod_reg")[, , ksd])                              # i, t, ksd
  getSets(prod)["d3.1"] <- "ksd"
  nr <- collapseNames(readGDX(gdx, "fm_attributes")[, , "nr"])[, , getItems(subst, dim = "kpr")]
  getSets(nr)["d3.1"] <- "kpr"
  processes <- intersect(readGDX(gdx, "processing20"), noMillGin)
  kprIn     <- getItems(dem, dim = "kpr")
  sec       <- intersect(kprIn, ksd)

  # substitution pools of q20_processing_substitution_*; the protein pool splits between its outputs in N of the
  # nominal origin (the unit of its equation), the other pools have one output
  pools <- list(oils = list(input = "oils", output = "oils", nWeighted = FALSE),
                molasses = list(input = "sugar", output = "molasses", nWeighted = FALSE),
                protein = list(input = oilcakeSubst, output = c("distillers_grain", "oilcakes"), nWeighted = TRUE),
                brans = list(input = cereals, output = "brans", nWeighted = FALSE))

  # tonnes of each input (kpr) whose content goes into the production of each secondary (ksd)
  demProc <- dem[, , processes]
  getSets(demProc)["d3.1"] <- "processing20"
  e <- dimSums(allocWeights[, , processes][, , list(kpr = kprIn)] * demProc, dim = "processing20")
  substInput <- collapseNames(dem[, , "substitutes"])                          # i, t, kpr
  unpooled <- list()
  # full-size 0/1 masks, not subsets: magclass matches subdimensions by their items, so an operand holding only some
  # ksd (or kpr) items is taken as a new subdimension and silently expanded into a cartesian product
  for (name in names(pools)) {
    pool  <- pools[[name]]
    isOut <- prod * 0
    isOut[, , pool$output] <- 1
    isIn  <- substInput * 0
    isIn[, , pool$input] <- 1
    relabelled <- subst * isOut
    if (pool$nWeighted) {
      relabelled <- relabelled * nr
    }
    weight <- dimSums(relabelled, dim = "kpr")                                 # i, t, ksd
    total  <- dimSums(weight, dim = 3)
    pooled <- total > 0
    total[!pooled] <- 1
    e <- e + weight / total * pooled * (substInput * isIn)
    unpooled[[name]] <- !pooled & dimSums(substInput * isIn, dim = 3) > 1e-8
  }

  # guards: input into a secondary without production; a secondary almost entirely made from itself
  isSelf <- e * 0
  isSelf[, , paste(sec, sec, sep = ".")] <- 1
  selfUse <- dimSums(e * isSelf, dim = "kpr")                                 # i, t, ksd
  inputs  <- dimSums(e, dim = "kpr")
  lost    <- prod <= 0 & inputs > 1e-8
  weak    <- prod > 0 & prod - selfUse <= 0.01 * prod
  fired   <- dimSums((prod <= 0 & inputs > 0) | weak, dim = 3) > 0
  ownUse  <- prod * 0                                                          # secondaries' own processing use
  ownUse[, , sec] <- dimSums(e[, , list(kpr = sec)], dim = "ksd")              # [<- aligns by item name

  # content per tonne of production: p_j z_j = e_jk + sum_m e_jm z_m, where p > 0 and j is not almost entirely made
  # from itself; elsewhere the World factor
  # (for the World: 0). One small linear system per region and year.
  z <- new.magpie(getItems(dem, dim = 1), simYears, as.vector(outer(ksd, realPrim, paste, sep = ".")), fill = 0,
                  sets = c("i", "t_all", "ksd", "kpr"))
  zero <- matrix(0, length(ksd), length(realPrim), dimnames = list(ksd, realPrim))
  for (t in simYears) {
    for (r in c("World", setdiff(getItems(z, dim = 1), "World"))) {           # the World first: the regions' fallback
      .ppsWarnGuards(paste(r, t), lapply(unpooled, function(u) u[r, t, ]), lost[r, t, ksd], weak[r, t, ksd],
                     if (r == "World") "content not attributed (factor 0)" else "World factor used")
      fallback <- if (r == "World") zero else matrix(z["World", t, ], length(ksd), dimnames = dimnames(zero))
      pos <- as.vector(prod[r, t, ksd] > 0 & !weak[r, t, ksd])
      z[r, t, ] <- .ppsSolve(unwrap(e[r, t, ])[1, 1, ksd, kprIn], as.vector(prod[r, t, ksd]), pos, fallback,
                             sec, realPrim)
    }
  }
  if (!all(is.finite(z)) || min(z) < -1e-9) {
    stop("primaryPerSecondary: negative or non-finite content per tonne in ",
         .ppsWhere(!is.finite(z) | z < -1e-9))
  }
  .ppsCheckConserved(z, prod - ownUse, e[, , list(kpr = realPrim)], fired)
  return(z[setdiff(getItems(z, dim = 1), "World"), , ])
}

# One warning per guard that applies in a region and year.
.ppsWarnGuards <- function(where, unpooled, lost, weak, fallbackNote) {
  for (name in names(unpooled)) {
    if (any(unpooled[[name]])) {
      warning("primaryPerSecondary: ", where, ": substitute input to the ", name,
              " pool without relabelled tonnes; its primary content is not attributed")
    }
  }
  if (any(lost)) {
    secondaries <- paste(getItems(lost, dim = 3)[as.vector(lost)], collapse = ", ")
    warning("primaryPerSecondary: ", where, ": input into ", secondaries,
            " without production; its primary content is not attributed")
  }
  if (any(weak)) {
    secondaries <- paste(getItems(weak, dim = 3)[as.vector(weak)], collapse = ", ")
    warning("primaryPerSecondary: ", where, ": ", secondaries, " almost entirely made from itself; ", fallbackNote)
  }
}

# Solve p_j z_j - sum_{m in pos} e_jm z_m = e_jk + sum_{m not in pos} e_jm fallback_mk for the secondaries j in pos;
# the others keep the fallback. e: secondaries x inputs; returns secondaries x realPrim, as a vector (ksd fastest).
.ppsSolve <- function(e, p, pos, fallback, sec, realPrim) {
  es <- matrix(0, nrow(e), nrow(e), dimnames = list(rownames(e), rownames(e)))
  es[, sec] <- e[, sec]
  z <- fallback
  if (any(pos)) {
    rhs <- e[pos, realPrim, drop = FALSE] + es[pos, !pos, drop = FALSE] %*% fallback[!pos, , drop = FALSE]
    z[pos, ] <- solve(diag(p[pos], sum(pos)) - es[pos, pos, drop = FALSE], rhs)
  }
  return(as.vector(z))
}

# Region-year pairs where a logical magpie object is TRUE, by year, then region, for messages.
.ppsWhere <- function(flag) {
  at <- which(as.array(dimSums(flag, dim = 3) > 0), arr.ind = TRUE)
  return(paste(getItems(flag, dim = 1)[at[, 1]], getItems(flag, dim = 2)[at[, 2]], collapse = ", "))
}

# Summed over the secondaries, the balance solved in .primaryPerSecondaryLevels gives, per region (and the World),
# year and primary: content of production net of the secondaries' own processing use = the primary input attributed
# to them. It holds by construction where no secondary without production receives input and none is almost
# entirely made from itself; a failure there is an error in the solve.
.ppsCheckConserved <- function(contentPerTonne, netProduction, directInput, guardsFired) {
  net <- dimSums(contentPerTonne * netProduction, dim = "ksd") - dimSums(directInput, dim = "ksd")
  tolerance <- dimSums(directInput, dim = 3)
  tolerance[tolerance < 1] <- 1
  failed <- dimSums(abs(net) > 1e-8 * tolerance, dim = 3) > 0 & !guardsFired
  if (any(failed)) {
    stop("primaryPerSecondary: primary content not conserved in ", .ppsWhere(failed),
         " (largest residual ", signif(max(abs(net) * failed), 3), ")")
  }
  return(invisible(NULL))
}
