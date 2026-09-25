test_that("every annotation column renders", {
  for (col in annotation_columns) {
    p <- ggRootCellAtlas_annotation(col)
    expect_s3_class(p, "patchwork")
    for (i in 1:7) expect_no_error(ggplot2::ggplot_build(p[[i]]))
  }
})

test_that("a group is drawn in the same colour on every panel", {
  p <- ggRootCellAtlas_annotation("TissueTypes")
  colour_of <- list()
  for (i in 1:7) {
    d <- p[[i]]$data
    fill <- panel_data(p, i)$fill
    for (g in unique(stats::na.omit(d$TissueTypes))) {
      f <- unique(fill[d$TissueTypes %in% g])
      expect_length(f, 1)
      if (!is.null(colour_of[[g]])) expect_equal(f, colour_of[[g]], info = g)
      colour_of[[g]] <- f
    }
  }
})

annotation_fills <- function(p, column) {
  maps <- load_maps()
  out <- character()
  for (i in 1:7) {
    d <- p[[i]]$data
    fill <- panel_data(p, i)$fill
    keep <- !duplicated(d[[column]]) & !is.na(d[[column]])
    out[d[[column]][keep]] <- hex(fill[keep])
  }
  out
}

test_that("default palette is Okabe-Ito for up to 11 groups", {
  fills <- annotation_fills(ggRootCellAtlas_annotation("TissueSubTypes"), "TissueSubTypes")
  expect_length(fills, 10)
  expect_true(all(fills %in% hex(okabe_ito_pal()(11))))
  expect_false(anyDuplicated(fills) > 0)
})

test_that("default palette falls back to distinct hues for many groups", {
  fills <- annotation_fills(ggRootCellAtlas_annotation("Atlas"), "Atlas")
  expect_false(anyDuplicated(fills) > 0)
})

test_that("palette accepts a named vector, an unnamed vector or a function", {
  named <- c(Epidermis = "red", "Ground tissue" = "green", Stele = "blue",
             "Root cap" = "orange", SCN = "purple")
  fills <- annotation_fills(ggRootCellAtlas_annotation("TissueTypes", palette = named),
                            "TissueTypes")
  expect_equal(fills[names(named)], stats::setNames(hex(named), names(named)))

  fills <- annotation_fills(ggRootCellAtlas_annotation("TissueTypes", palette = unname(named)),
                            "TissueTypes")
  expect_setequal(fills, hex(named))

  fills <- annotation_fills(ggRootCellAtlas_annotation("TissueTypes",
                                                       palette = function(n) rep("black", n)),
                            "TissueTypes")
  expect_true(all(fills == "#000000"))
})

test_that("palette that does not cover all groups gives an informative error", {
  expect_error(ggRootCellAtlas_annotation("TissueTypes", palette = c("red", "blue")),
               "2 colours")
  expect_error(ggRootCellAtlas_annotation("TissueTypes", palette = c(Stele = "red")),
               "Epidermis")
})

test_that("cells without annotation use na.colour, dark grey by default", {
  p <- ggRootCellAtlas_annotation("Zones")
  for (i in 1:7) {
    na_cells <- is.na(p[[i]]$data$Zones)
    if (!any(na_cells)) next
    expect_true(all(hex(panel_data(p, i)$fill[na_cells]) == hex("grey30")))
  }
  p <- ggRootCellAtlas_annotation("Zones", na.colour = "pink")
  for (i in 1:7) {
    na_cells <- is.na(p[[i]]$data$Zones)
    if (!any(na_cells)) next
    expect_true(all(hex(panel_data(p, i)$fill[na_cells]) == hex("pink")))
  }
})

test_that("custom maps and layouts can be used", {
  maps <- load_maps()[c("ggPm.At.root.crosssection.m1", "ggPm.At.root.crosssection.e2")]
  p <- ggRootCellAtlas_annotation("Sections", maps = maps)
  expect_equal(unique(stats::na.omit(p[[2]]$data$Sections)), "e2")
  expect_s3_class(ggRootCellAtlas_annotation("Sections", maps = maps, layout = "12"), "patchwork")
  expect_s3_class(ggRootCellAtlas_annotation("Sections", maps = maps[[1]]), "patchwork")
  expect_error(ggRootCellAtlas_annotation("Sections", maps = list(data.frame(x = 1))),
               "missing the column")
  expect_error(ggRootCellAtlas_annotation("Sections", maps = "m1"), "map data frame")
})

test_that("unknown group name gives an informative error", {
  expect_error(ggRootCellAtlas_annotation("NotAColumn"), "NotAColumn")
})

test_that("annotation does not leave objects in the global environment", {
  clear_leaked_maps()
  before <- globals_snapshot()
  ggRootCellAtlas_annotation("Zones")
  expect_identical(globals_snapshot(), before)
})

test_that("works when the working directory is not the package source", {
  old <- setwd(tempdir())
  on.exit(setwd(old))
  expect_s3_class(ggRootCellAtlas_annotation("Zones"), "patchwork")
})
