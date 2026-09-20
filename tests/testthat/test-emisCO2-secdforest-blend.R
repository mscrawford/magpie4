# emisCO2: the secondary-forest age-class carbon density must be the blend GAMS solved with.
#
# Defect fixed 2026-09-20 (see the CONSTANTS note in R/emisCO2.R). emisCO2 used to REBUILD that density from
# pm_carbon_density_secdforest_ac (FRA-calibrated; edge-haircut IN PLACE under model version L4),
# pm_carbon_density_secdforest_ac_uncalib (never haircut) and the POSTSOLVE export p35_secdforest_natural over
# the solved area. GAMS blends the two RAW curves with the PRESOLVE natural-origin share
# (35_natveg/pot_forest_may24/presolve.gms:253-257), haircuts the blend afterwards (presolve.gms:487-488) and
# exports the result as p35_carbon_density_secdforest(t,j,ac,ag_pools). Measured on the L4 gate runs: the
# rebuild overstated the secdforest vegc stock by +490 MtC World in 2100 with the edge switch ON and by
# -9.3 / -2.8 / +19.9 / -1.5 MtC in 2070-2100 with it OFF (share mismatch alone).
#
# These tests need the two 600 MB L4 gate gdx and cost one ~30 s emisCO2 call each, so every block skips when
# the files are absent and the four runs are cached across blocks. Override the paths with the environment
# variables MAGPIE4_L4GATE_ON_GDX / MAGPIE4_L4GATE_OFF_GDX.

.l4GateGdx <- function(which) {
  fromEnv <- Sys.getenv(paste0("MAGPIE4_L4GATE_", which, "_GDX"), "")
  if (nzchar(fromEnv)) {
    return(fromEnv)
  }
  runDir <- switch(which,
                   ON  = "SSP2base_L4gate_ON_2026-09-19_14.25.45",
                   OFF = "SSP2base_L4gate_OFF_2026-09-19_14.30.30")
  # nolint start: absolute_path_linter. The L4 gate runs are machine-local; override with the env vars above.
  file.path("/Users/turnip/Documents/Work/Projects/Fragmentation/libraries/magpie/output", runDir, "fulldata.gdx")
  # nolint end
}

.skipUnlessGateGdx <- function(which) {
  gdx <- .l4GateGdx(which)
  testthat::skip_if_not(file.exists(gdx), paste0("L4 gate gdx not available: ", gdx))
  gdx
}

# --- instrument 1: capture emisCO2's internal stock objects -----------------------------------------------
# totalStock is magpie4::carbonstock (read straight from ov_carbon_stock), totalStockCheck is emisCO2's own
# area x density reconstruction. Both are local to emisCO2; the capture is spliced in as an extra statement
# immediately before the .validateCalculation() call, which leaves every calculation untouched.
.injectStockCapture <- function(fn, env) {
  parts      <- as.list(body(fn))
  statements <- parts[-1]
  isValidate <- vapply(statements,
                       function(e) is.call(e) && identical(deparse(e[[1]]), ".validateCalculation"),
                       logical(1))
  stopifnot(sum(isValidate) == 1)
  j   <- which(isValidate)
  cap <- bquote(assign("captured",
                       list(totalStock = totalStock, totalStockCheck = totalStockCheck),
                       envir = .(env)))
  body(fn) <- as.call(c(list(as.name("{")), statements[seq_len(j - 1)], list(cap),
                        statements[j:length(statements)]))
  fn
}

# --- instrument 2: hide one gdx symbol from emisCO2's own body ---------------------------------------------
# Masks readGDX in a child of the package namespace, so only the calls written inside emisCO2 are affected.
# Hiding p35_carbon_density_secdforest reproduces the pre-fix (rebuild) code path exactly -- asserted against
# the committed pre-fix source in the "fallback branch" block below.
.hideGdxSymbol <- function(fn, hidden) {
  realReadGDX <- get("readGDX", envir = environment(fn))
  e <- new.env(parent = environment(fn))
  e$readGDX <- function(gdx, ...) {
    dots     <- list(...)
    named    <- names(dots)
    if (is.null(named)) named <- rep("", length(dots))
    symbols  <- unlist(dots[named == ""])
    if (length(symbols) == 1L && identical(as.character(symbols), hidden)) {
      return(NULL)
    }
    realReadGDX(gdx, ...)
  }
  environment(fn) <- e
  fn
}

.emisCO2Cache <- new.env(parent = emptyenv())

.runEmisCO2 <- function(key, fn, gdx) {
  if (!is.null(.emisCO2Cache[[key]])) {
    return(.emisCO2Cache[[key]])
  }
  captureEnv <- new.env()
  instrumented <- .injectStockCapture(fn, captureEnv)
  warnEnv <- new.env(parent = emptyenv())
  warnEnv$warns <- character(0)
  collect <- function(w) {
    assign("warns", c(warnEnv$warns, conditionMessage(w)), envir = warnEnv)
    invokeRestart("muffleWarning")
  }
  out <- withCallingHandlers(instrumented(gdx, level = "cell", sum_cpool = FALSE, sum_land = FALSE),
                             warning = collect)
  res <- list(out = out, warnings = warnEnv$warns,
              totalStock = captureEnv$captured$totalStock,
              totalStockCheck = captureEnv$captured$totalStockCheck)
  assign(key, res, envir = .emisCO2Cache)
  res
}

# ov_carbon_stock(secdforest, <pool>, actual) in MtC -- the model's own stock, per cluster and year.
.refSecdforestStock <- function(gdx, pool) {
  cs <- gdx2::readGDX(gdx, "ov_carbon_stock", select = list(type = "level"))
  magclass::collapseNames(cs[, , "secdforest"][, , pool][, , "actual"])
}

.recSecdforestStock <- function(run, pool) {
  magclass::collapseNames(run$totalStockCheck[, , "secdforest"][, , pool])
}

.worldByYear <- function(x) {
  stats::setNames(vapply(magclass::getYears(x), function(y) sum(x[, y, ]), numeric(1)), magclass::getYears(x))
}


test_that("edge-ON: reconstructed secdforest vegc and litc stocks equal ov_carbon_stock", {
  gdx <- .skipUnlessGateGdx("ON")
  run <- .runEmisCO2("fixON", magpie4::emisCO2, gdx)

  for (pool in c("vegc", "litc")) {
    rec <- .recSecdforestStock(run, pool)
    ref <- .refSecdforestStock(gdx, pool)[, magclass::getYears(rec), ]
    expect_lt(max(abs(rec - ref)), 1e-6) # MtC, every cluster and year
  }
  # the strengthened stock-consistency check must be silent on the fixed code
  expect_equal(run$warnings, character(0))
})


test_that("edge-OFF: reconstructed secdforest vegc and litc stocks equal ov_carbon_stock", {
  # This is the presolve-vs-postsolve natural-origin share leg: it is nonzero even with the edge switch off.
  gdx <- .skipUnlessGateGdx("OFF")
  run <- .runEmisCO2("fixOFF", magpie4::emisCO2, gdx)

  for (pool in c("vegc", "litc")) {
    rec <- .recSecdforestStock(run, pool)
    ref <- .refSecdforestStock(gdx, pool)[, magclass::getYears(rec), ]
    expect_lt(max(abs(rec - ref)), 1e-6)
  }
  expect_equal(run$warnings, character(0))
})


test_that("failing-first: the pre-fix rebuild misses the stock by 490 MtC and now warns", {
  gdx <- .skipUnlessGateGdx("ON")
  prefix <- .runEmisCO2("prefixON", .hideGdxSymbol(magpie4::emisCO2, "p35_carbon_density_secdforest"), gdx)
  fixed  <- .runEmisCO2("fixON", magpie4::emisCO2, gdx)

  rec <- .recSecdforestStock(prefix, "vegc")
  ref <- .refSecdforestStock(gdx, "vegc")[, magclass::getYears(rec), ]
  gap <- rec - ref
  world <- .worldByYear(gap)

  # the defect, with the numbers it was measured at
  expect_equal(world[["y2100"]], 489.99, tolerance = 1e-3)  # MtC World, secdforest vegc stock
  expect_equal(max(abs(gap)), 128.27, tolerance = 1e-3)     # MtC, worst cluster-year
  expect_true(all(abs(world[paste0("y", c(1995, 2000, 2005, 2010, 2015, 2020))]) < 1e-6))
  expect_gt(world[["y2050"]], 90)

  # the same run, with the fix: gone
  expect_lt(max(abs(.recSecdforestStock(fixed, "vegc") - ref)), 1e-6)

  # the stock-consistency check fires on the rebuild and is silent on the fix (it used to be blind to both:
  # under dynSom it collapsed to a soilc-only comparison)
  expect_true(any(grepl("differ from magpie4::carbonstock", prefix$warnings)))
  expect_equal(fixed$warnings, character(0))

  # net land CO2 consequence, World, 2100 (Mt CO2/yr): emisCO2 returns unit = "gas" by default
  netPrefix <- sum(prefix$out[, "y2100", "total"])
  netFixed  <- sum(fixed$out[, "y2100", "total"])
  expect_equal(netFixed - netPrefix, 44.44, tolerance = 1e-2)
})


test_that("edge-OFF failing-first: the postsolve share alone moves the stock in 2070-2100", {
  # Algebraic restatement of the pre-fix rebuild, so this leg costs no emisCO2 call:
  #   stock = sum_ac area * cal - sum_ac min(natural, area) * (cal - uncal)
  gdx <- .skipUnlessGateGdx("OFF")
  blend <- gdx2::readGDX(gdx, "p35_carbon_density_secdforest")
  years <- magclass::getYears(blend)
  area  <- gdx2::readGDX(gdx, "ov35_secdforest", select = list(type = "level"))[, years, ]
  cal   <- magclass::collapseNames(gdx2::readGDX(gdx, "pm_carbon_density_secdforest_ac")[, years, "vegc"])
  uncalRaw <- gdx2::readGDX(gdx, "pm_carbon_density_secdforest_ac_uncalib")[, years, "vegc"]
  uncal <- magclass::collapseNames(uncalRaw)
  nat   <- gdx2::readGDX(gdx, "p35_secdforest_natural")[, years, ]
  nat[nat > area] <- area[nat > area]

  prefixStock <- magclass::dimSums(area * cal, dim = 3) - magclass::dimSums(nat * (cal - uncal), dim = 3)
  blendStock  <- magclass::dimSums(area * magclass::collapseNames(blend[, , "vegc"]), dim = 3)
  ref         <- .refSecdforestStock(gdx, "vegc")[, years, ]

  expect_lt(max(abs(blendStock - ref)), 1e-6)  # the exported blend is exact
  world <- .worldByYear(prefixStock - ref)
  # Nil through 2060 (the natural-origin area is still zero everywhere): the largest World residual in
  # 1995-2060 measured 6.6e-05 MtC (2030, gdx float noise), hence the 1e-3 MtC tolerance the code's own
  # stock check uses rather than a bit-exact zero.
  expect_true(all(abs(world[paste0("y", seq(1995, 2060, 5))]) < 1e-3))
  expect_equal(unname(world[paste0("y", c(2070, 2080, 2090, 2100))]),
               c(-9.325, -2.850, 19.855, -1.532), tolerance = 1e-3)
})


test_that("fallback branch: a gdx without p35_carbon_density_secdforest reproduces the pre-fix result", {
  gdx <- .skipUnlessGateGdx("ON")
  repo <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  testthat::skip_if_not(dir.exists(file.path(repo, ".git")), "not a git working copy")
  testthat::skip_if(unname(Sys.which("git")) == "", "git not on PATH")

  # The revision to compare against: the last one WITHOUT the fix. Before this change is committed that is
  # HEAD; afterwards, point MAGPIE4_EMISCO2_PREFIX_REV at the parent of the fix commit.
  rev <- Sys.getenv("MAGPIE4_EMISCO2_PREFIX_REV", "HEAD")
  src <- suppressWarnings(system2("git", c("-C", shQuote(repo), "show", shQuote(paste0(rev, ":R/emisCO2.R"))),
                                  stdout = TRUE, stderr = FALSE))
  testthat::skip_if(!is.null(attr(src, "status")) || length(src) == 0,
                    paste0("cannot read R/emisCO2.R at ", rev))
  testthat::skip_if(any(grepl("p35_carbon_density_secdforest", src, fixed = TRUE)),
                    paste0(rev, " already contains the fix; ",
                           "set MAGPIE4_EMISCO2_PREFIX_REV to a pre-fix revision"))

  srcFile <- withr::local_tempfile(fileext = ".R")
  writeLines(src, srcFile)
  prefixEnv <- new.env(parent = asNamespace("magpie4"))
  sys.source(srcFile, envir = prefixEnv, keep.source = FALSE)

  prefixRun <- .runEmisCO2("prefixSourceON", prefixEnv$emisCO2, gdx)
  hiddenRun <- .runEmisCO2("prefixON", .hideGdxSymbol(magpie4::emisCO2, "p35_carbon_density_secdforest"), gdx)

  # The fallback path is behaviour-preserving: identical numbers, variable by variable.
  expect_identical(magclass::getNames(hiddenRun$out), magclass::getNames(prefixRun$out))
  expect_identical(magclass::getYears(hiddenRun$out), magclass::getYears(prefixRun$out))
  expect_identical(as.vector(as.array(hiddenRun$out)), as.vector(as.array(prefixRun$out)))

  # The one intended behaviour difference is the strengthened stock-consistency check, which the pre-fix
  # source does not have (under dynSom it compared soilc only, so the 490 MtC vegc gap never warned).
  expect_equal(prefixRun$warnings, character(0))
  expect_true(any(grepl("differ from magpie4::carbonstock", hiddenRun$warnings)))
})


test_that("the exported blend differs from the calibrated curve in vegc only", {
  # Why using the blend for every ag_pool it carries is safe for litc: the M52 calibration and the edge
  # haircut are both vegc-only (presolve.gms:487-490), so the litc blend is the calibrated litc curve.
  gdx <- .skipUnlessGateGdx("ON")
  blend <- gdx2::readGDX(gdx, "p35_carbon_density_secdforest")
  cal   <- gdx2::readGDX(gdx, "pm_carbon_density_secdforest_ac")[, magclass::getYears(blend), ]
  expect_identical(magclass::getItems(blend, dim = "ag_pools"), c("vegc", "litc"))
  expect_identical(magclass::getItems(blend, dim = 3), magclass::getItems(cal, dim = 3))
  expect_identical(as.vector(blend[, , "litc"]), as.vector(cal[, , "litc"]))
  expect_gt(max(abs(blend[, , "vegc"] - cal[, , "vegc"])), 1)
})
