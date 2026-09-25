# Write an SVG body to a temporary file and import it
import_svg <- function(body, ..., header = "") {
  file <- tempfile(fileext = ".svg")
  writeLines(c(
    paste0('<svg xmlns="http://www.w3.org/2000/svg" ',
           'xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape">'),
    header, body, "</svg>"
  ), file)
  svg_to_map(file, ...)
}

# Points of one cell as a matrix, y flipped back to SVG orientation
cell_xy <- function(map, id = 1) {
  m <- map[map$ROI.id == id, ]
  cbind(m$x, -m$y)
}

test_that("absolute and relative lines give the same square", {
  abs <- import_svg('<path d="M0 0 L10 0 L10 10 L0 10 Z"/>')
  rel <- import_svg('<path d="m0 0 l10 0 0 10 -10 0 z"/>')
  hv  <- import_svg('<path d="M0 0 H10 V10 h-10 z"/>')
  square <- cbind(c(0, 10, 10, 0), c(0, 0, 10, 10))
  expect_equal(cell_xy(abs), square)
  expect_equal(cell_xy(rel), square)
  expect_equal(cell_xy(hv), square)
  expect_named(abs, c("ROI.name", "ROI.id", "point", "x", "y", "svg.id", "fill", "layer"))
})

test_that("y is flipped by default and kept with flip_y = FALSE", {
  body <- '<path d="M0 0 L10 0 L10 5 Z"/>'
  expect_equal(import_svg(body)$y, c(0, 0, -5))
  expect_equal(import_svg(body, flip_y = FALSE)$y, c(0, 0, 5))
})

test_that("compact number syntax is read correctly", {
  map <- import_svg('<path d="M0,0L1e1,0l-.5-5.5.5.5z"/>')
  expect_equal(cell_xy(map), cbind(c(0, 10, 9.5, 10), c(0, 0, -5.5, -5)))
})

test_that("each closed subpath becomes its own cell, relative to the right start", {
  # the second subpath starts relative to the first subpath's start (0,0)
  map <- import_svg('<path id="two" d="m0 0 h5 v5 h-5 z m10 0 h5 v5 h-5 z"/>')
  expect_equal(length(unique(map$ROI.id)), 2)
  expect_equal(cell_xy(map, 2)[1, ], c(10, 0))
  expect_true(all(map$svg.id == "two"))
})

test_that("cubic curves and their smooth continuation follow the curve", {
  map <- import_svg('<path d="M0 0 C0 10 10 10 10 0 S20 -10 20 0 Z"/>', curve_points = 2)
  xy <- cell_xy(map)
  # t = 0.5 of the first segment, and of the reflected second segment
  expect_true(any(abs(xy[, 1] - 5) < 1e-9 & abs(xy[, 2] - 7.5) < 1e-9))
  expect_true(any(abs(xy[, 1] - 15) < 1e-9 & abs(xy[, 2] + 7.5) < 1e-9))
  expect_true(any(abs(xy[, 1] - 20) < 1e-9 & abs(xy[, 2]) < 1e-9))
})

test_that("quadratic curves and their smooth continuation follow the curve", {
  map <- import_svg('<path d="M0 0 Q5 10 10 0 T20 0 Z"/>', curve_points = 2)
  xy <- cell_xy(map)
  expect_true(any(abs(xy[, 1] - 5) < 1e-9 & abs(xy[, 2] - 5) < 1e-9))
  expect_true(any(abs(xy[, 1] - 15) < 1e-9 & abs(xy[, 2] + 5) < 1e-9))
})

test_that("arcs lie on their circle, also with compact flags", {
  for (d in c("M0 0 A5 5 0 0 1 10 0 A5 5 0 0 1 0 0 Z",
              "M0 0a5 5 0 0110 0a5 5 0 01-10 0z")) {
    xy <- cell_xy(import_svg(sprintf('<path d="%s"/>', d)))
    expect_gt(nrow(xy), 10)
    expect_equal(sqrt((xy[, 1] - 5)^2 + xy[, 2]^2), rep(5, nrow(xy)), tolerance = 1e-9)
  }
})

test_that("polygon, rect, circle and ellipse elements are read", {
  map <- import_svg(c(
    '<polygon id="tri" points="0,0 10,0 5,8"/>',
    '<rect id="box" x="20" y="0" width="10" height="5"/>',
    '<circle id="dot" cx="50" cy="0" r="3"/>',
    '<ellipse id="egg" cx="70" cy="0" rx="4" ry="2"/>'
  ))
  expect_equal(unique(map$svg.id), c("tri", "box", "dot", "egg"))
  expect_equal(cell_xy(map, 2), cbind(c(20, 30, 30, 20), c(0, 0, 5, 5)))
  dot <- cell_xy(map, 3)
  expect_equal(sqrt((dot[, 1] - 50)^2 + dot[, 2]^2), rep(3, nrow(dot)))
})

test_that("open outlines are dropped unless closed_only = FALSE", {
  body <- c('<path id="closed" d="M0 0 H5 V5 Z"/>',
            '<path id="open" d="M10 0 H15 V5"/>',
            '<polyline id="line" points="20,0 25,0 25,5"/>')
  expect_equal(unique(import_svg(body)$svg.id), "closed")
  expect_equal(unique(import_svg(body, closed_only = FALSE)$svg.id), c("closed", "open", "line"))
})

test_that("transforms of shapes and their groups are applied", {
  map <- import_svg(c(
    '<g transform="translate(100,50)">',
    '  <rect id="r" width="2" height="1" transform="scale(2)"/>',
    '  <rect id="s" width="2" height="1" transform="rotate(90)"/>',
    '</g>',
    '<rect id="m" width="1" height="1" transform="matrix(1 0 0 1 -5 -5)"/>'
  ))
  expect_equal(cell_xy(map, 1), cbind(c(100, 104, 104, 100), c(50, 50, 52, 52)))
  expect_equal(cell_xy(map, 2), cbind(c(100, 100, 99, 99), c(50, 52, 52, 50)))
  expect_equal(cell_xy(map, 3), cbind(c(-5, -4, -4, -5), c(-5, -5, -4, -4)))
})

test_that("fill comes from inline style, CSS classes, attributes or the parent group", {
  map <- import_svg(
    c('<path id="a" style="fill:#abc" d="M0 0 H1 V1 Z"/>',
      '<path id="b" class="st0" d="M0 0 H1 V1 Z"/>',
      '<path id="c" fill="white" d="M0 0 H1 V1 Z"/>',
      '<g fill="rgb(255,0,0)"><path id="d" d="M0 0 H1 V1 Z"/></g>'),
    header = '<style type="text/css">.st0{fill:#D1E9D1;stroke:#010101;}</style>'
  )
  fills <- map$fill[!duplicated(map$svg.id)]
  names(fills) <- unique(map$svg.id)
  expect_equal(unname(fills[c("a", "b", "c", "d")]),
               c("#AABBCC", "#D1E9D1", "#FFFFFF", "#FF0000"))
})

test_that("cells can be named by fill with a label table", {
  body <- c('<path fill="#D1E9D1" d="M0 0 H1 V1 Z"/>',
            '<path fill="#ADBCAF" d="M2 0 H3 V1 Z"/>',
            '<path fill="#231F20" d="M4 0 H5 V1 Z"/>')
  labels <- c("#d1e9d1" = "Atrichoblast_e1", "#ADBCAF" = "Atrichoblast_e2")
  map <- import_svg(body, label_by = "fill", labels = labels, skip_fills = "#231f20")
  expect_equal(unique(map$ROI.name), c("Atrichoblast_e1", "Atrichoblast_e2"))
  expect_warning(map <- import_svg(body, label_by = "fill", labels = labels), "#231F20")
  expect_true(anyNA(map$ROI.name))
})

test_that("cells can be named by id, layer or not at all", {
  body <- c('<g inkscape:label="Cortex"><path id="p1" d="M0 0 H1 V1 Z"/></g>',
            '<g id="Layer_2"><path id="p2" d="M2 0 H3 V1 Z"/></g>')
  expect_equal(unique(import_svg(body)$ROI.name), c("p1", "p2"))
  expect_equal(unique(import_svg(body, label_by = "layer")$ROI.name), c("Cortex", "Layer_2"))
  expect_true(all(is.na(import_svg(body, label_by = "none")$ROI.name)))
})

test_that("shapes inside defs and clip paths are ignored", {
  map <- import_svg(c('<defs><rect id="hidden" width="5" height="5"/></defs>',
                      '<clipPath><rect id="clip" width="5" height="5"/></clipPath>',
                      '<rect id="shown" width="5" height="5"/>'))
  expect_equal(unique(map$svg.id), "shown")
})

test_that("bad input gives informative errors", {
  expect_error(import_svg('<text>no shapes</text>'), "No shapes")
  expect_error(import_svg('<path d="M0 0 H1"/>'), "No closed outlines")
  expect_error(import_svg('<path d="M0 0 L1"/>'), "missing numbers")
})

test_that("the cross-section drawing reproduces the bundled m1 map", {
  # The bundled cross-sections were built from this Inkscape drawing with the
  # original preparation script; only the y offset and cell numbering differ.
  imp <- svg_to_map(test_path("fixtures", "root_cross.svg"))
  ref <- root_maps()[["ggPm.At.root.crosssection.m1"]]
  expect_equal(length(unique(imp$ROI.id)), length(unique(ref$ROI.id)))

  shift <- function(m) cbind(m$x - min(m$x), m$y - min(m$y))
  a <- shift(ref); b <- shift(imp)
  centre <- function(xy, id) t(sapply(split(seq_len(nrow(xy)), id), function(r) colMeans(xy[r, , drop = FALSE])))
  ca <- centre(a, ref$ROI.id); cb <- centre(b, imp$ROI.id)
  nearest <- apply(ca, 1, function(p) which.min(colSums((t(cb) - p)^2)))
  expect_false(anyDuplicated(nearest) > 0)
  for (k in seq_len(nrow(ca))) {
    pa <- a[ref$ROI.id == as.numeric(rownames(ca)[k]), , drop = FALSE]
    pb <- b[imp$ROI.id == as.numeric(rownames(cb)[nearest[k]]), , drop = FALSE]
    expect_equal(nrow(pa), nrow(pb))
    gap <- max(apply(pa, 1, function(p) min(sqrt(colSums((t(pb) - p)^2)))))
    expect_lt(gap, 1e-6)
  }
})

test_that("imported maps plot with the package functions", {
  map <- import_svg(c('<path fill="#D1E9D1" d="M0 0 H10 V10 H0 Z"/>',
                      '<path fill="#ADBCAF" d="M10 0 H20 V10 H10 Z"/>'),
                    label_by = "fill",
                    labels = c("#D1E9D1" = "Cortex_m1", "#ADBCAF" = "Endodermis_m1"))
  p <- ggRootCellAtlas_annotation("ROI.name", maps = map)
  expect_s3_class(p, "patchwork")
  expect_no_error(ggplot2::ggplot_build(p[[1]]))

  m <- matrix(c(1, 5), nrow = 1, dimnames = list("GENE1", c("Cortex_m1", "Endodermis_m1")))
  p <- ggRootCellAtlas_expression(m, "GENE1", Annotation = "ROI.name", maps = list(a = map, b = map))
  for (i in 1:2) expect_equal(sort(unique(p[[i]]$data$Gene)), c(1, 5))
})
