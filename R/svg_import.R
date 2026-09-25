#' Build a map from an SVG drawing
#'
#' @description Reads the cell outlines of an SVG file (for example drawn in
#' Inkscape or Adobe Illustrator) and returns them as a map in ggPlantmap
#' format, ready for [ggRootCellAtlas_annotation()],
#' [ggRootCellAtlas_expression()] or `ggPlantmap::ggPlantmap.plot()`.
#' ggPlantmap itself can only build maps from Icy ROI files
#' (`ggPlantmap::XML.to.ggPlantmap()`) and cannot read SVG drawings.
#'
#' Every closed outline becomes one cell (`ROI.id`). Supported are `<path>`
#' (all SVG path commands, including curves and arcs), `<polygon>`,
#' `<polyline>`, `<rect>`, `<circle>` and `<ellipse>`, with `transform`
#' attributes on the shapes and their groups. Shapes inside `<defs>`,
#' `<clipPath>`, `<mask>`, `<symbol>`, `<pattern>` and `<marker>` are ignored.
#'
#' @param file Path to the SVG file.
#' @param label_by What names the cells in `ROI.name`: `"id"` (the shape's
#'   `id`), `"fill"` (its fill colour, as `"#RRGGBB"`), `"layer"` (the
#'   Inkscape layer name or the `id` of the enclosing group) or `"none"`.
#' @param labels Optional named vector translating those names into cell
#'   labels, e.g. `c("#D1E9D1" = "Atrichoblast_e1")`. Names not in `labels`
#'   are reported in a warning and get `NA` as `ROI.name`.
#' @param skip_fills Fill colours of shapes to leave out, such as outline or
#'   background shapes, e.g. `"#231F20"`.
#' @param closed_only Keep only closed outlines (default). Open lines, such as
#'   a `<polyline>` or a path without `z`, cannot be drawn as cells.
#' @param curve_points Number of points used to draw each curve segment.
#' @param flip_y Flip the y axis, as SVG counts y downwards (default, same as
#'   `ggPlantmap::XML.to.ggPlantmap()`).
#'
#' @return A data frame with one row per outline point: `ROI.name`, `ROI.id`,
#'   `point`, `x`, `y`, and the source of each cell in `svg.id`, `fill` and
#'   `layer`.
#'
#' @examples
#' svg <- tempfile(fileext = ".svg")
#' writeLines(c(
#'   '<svg xmlns="http://www.w3.org/2000/svg">',
#'   '  <path id="cellA" fill="#E69F00" d="M0 0 H10 V10 H0 Z"/>',
#'   '  <path id="cellB" fill="#56B4E9" d="M10 0 h10 v10 c-5 3 -5 3 -10 0 z"/>',
#'   '</svg>'
#' ), svg)
#' map <- svg_to_map(svg, label_by = "fill",
#'                   labels = c("#E69F00" = "Cortex", "#56B4E9" = "Endodermis"))
#' ggRootCellAtlas_annotation("ROI.name", maps = map)
#'
#' @export

svg_to_map <- function(file, label_by = c("id", "fill", "layer", "none"), labels = NULL,
                       skip_fills = NULL, closed_only = TRUE, curve_points = 10,
                       flip_y = TRUE) {
  label_by <- match.arg(label_by)
  doc <- xml2::read_xml(file)
  css <- svg_css_classes(doc)

  shape_tags <- c("path", "polygon", "polyline", "rect", "circle", "ellipse")
  hidden <- c("defs", "clipPath", "mask", "symbol", "pattern", "marker")
  xpath <- sprintf("//*[%s][not(ancestor::*[%s])]",
                   paste0("local-name()='", shape_tags, "'", collapse = " or "),
                   paste0("local-name()='", hidden, "'", collapse = " or "))
  nodes <- xml2::xml_find_all(doc, xpath)
  if (length(nodes) == 0) {
    stop("No shapes found in the SVG file.", call. = FALSE)
  }
  skip_fills <- normalise_colour(skip_fills)

  cells <- list()
  for (k in seq_along(nodes)) {
    node <- nodes[[k]]
    tag <- xml2::xml_name(node)
    fill <- normalise_colour(svg_property(node, "fill", css))
    if (length(skip_fills) && !is.na(fill) && fill %in% skip_fills) next

    parts <- svg_shape_outlines(node, tag, curve_points)
    if (closed_only) parts <- Filter(function(p) p$closed, parts)
    if (length(parts) == 0) next

    m <- svg_node_transform(node)
    id <- xml2::xml_attr(node, "id")
    if (is.na(id)) id <- paste0(tag, k)
    for (p in parts) {
      xy <- m %*% rbind(p$x, p$y, 1)
      cells[[length(cells) + 1]] <- list(
        x = xy[1, ], y = if (flip_y) -xy[2, ] else xy[2, ],
        svg.id = id, fill = fill, layer = svg_layer(node)
      )
    }
  }
  cells <- lapply(cells, clean_outline)
  cells <- Filter(function(cell) length(cell$x) >= 3, cells)
  if (length(cells) == 0) {
    stop("No closed outlines with at least three points found in the SVG file.", call. = FALSE)
  }

  map <- do.call(rbind, lapply(seq_along(cells), function(i) {
    cell <- cells[[i]]
    data.frame(ROI.id = i, point = seq_along(cell$x), x = cell$x, y = cell$y,
               svg.id = cell$svg.id, fill = cell$fill, layer = cell$layer,
               stringsAsFactors = FALSE)
  }))

  key <- switch(label_by, id = map$svg.id, fill = map$fill, layer = map$layer,
                none = rep(NA_character_, nrow(map)))
  if (!is.null(labels) && label_by != "none") {
    if (label_by == "fill") names(labels) <- normalise_colour(names(labels))
    unmatched <- setdiff(unique(key[!is.na(key)]), names(labels))
    if (length(unmatched) > 0) {
      warning(sprintf("No label for %d %s value(s): %s.", length(unmatched), label_by,
                      paste(unmatched, collapse = ", ")), call. = FALSE)
    }
    key <- unname(labels[key])
  }
  map$ROI.name <- key
  map[, c("ROI.name", "ROI.id", "point", "x", "y", "svg.id", "fill", "layer")]
}

# Drop repeated points and the closing point that duplicates the first one
clean_outline <- function(cell) {
  n <- length(cell$x)
  if (n > 1) {
    keep <- c(TRUE, abs(diff(cell$x)) > 1e-9 | abs(diff(cell$y)) > 1e-9)
    cell$x <- cell$x[keep]
    cell$y <- cell$y[keep]
    n <- length(cell$x)
  }
  if (n > 1 && abs(cell$x[n] - cell$x[1]) < 1e-9 && abs(cell$y[n] - cell$y[1]) < 1e-9) {
    cell$x <- cell$x[-n]
    cell$y <- cell$y[-n]
  }
  cell
}

# --- Styles -----------------------------------------------------------------

# Declarations of simple class selectors (".st0{fill:#FFF}") in <style> blocks
svg_css_classes <- function(doc) {
  css <- paste(xml2::xml_text(xml2::xml_find_all(doc, "//*[local-name()='style']")),
               collapse = "\n")
  css <- gsub("/\\*.*?\\*/", "", css, perl = TRUE)
  rules <- regmatches(css, gregexpr("[^{}]+\\{[^}]*\\}", css))[[1]]
  out <- list()
  for (rule in rules) {
    selectors <- trimws(strsplit(sub("\\{.*", "", rule), ",")[[1]])
    decl <- parse_declarations(sub("^[^{]*\\{([^}]*)\\}$", "\\1", rule))
    for (s in selectors[grepl("^\\.[A-Za-z0-9_-]+$", selectors)]) {
      name <- substring(s, 2)
      out[[name]] <- c(decl, out[[name]][setdiff(names(out[[name]]), names(decl))])
    }
  }
  out
}

parse_declarations <- function(text) {
  if (is.na(text) || !nzchar(text)) return(character())
  parts <- strsplit(strsplit(text, ";")[[1]], ":")
  parts <- Filter(function(p) length(p) >= 2, parts)
  stats::setNames(vapply(parts, function(p) trimws(paste(p[-1], collapse = ":")), ""),
                  vapply(parts, function(p) trimws(p[1]), ""))
}

# A style property of a node: inline style, then CSS class, then attribute,
# then the same on its ancestors (fill is inherited in SVG)
svg_property <- function(node, name, css) {
  for (n in c(list(node), as.list(xml2::xml_parents(node)))) {   # nearest first
    style <- parse_declarations(xml2::xml_attr(n, "style"))
    if (!is.na(style[name])) return(unname(style[name]))
    classes <- strsplit(trimws(xml2::xml_attr(n, "class")), "\\s+")[[1]]
    for (cl in rev(classes[!is.na(classes)])) {
      value <- css[[cl]][name]
      if (!is.null(value) && !is.na(value)) return(unname(value))
    }
    value <- xml2::xml_attr(n, name)
    if (!is.na(value)) return(value)
  }
  NA_character_
}

# "#abc", "white", "rgb(255,255,255)" -> "#AABBCC"; "none" and urls unchanged
normalise_colour <- function(x) {
  if (length(x) == 0) return(character())
  vapply(x, function(col) {
    if (is.na(col) || col %in% c("none", "transparent") || startsWith(col, "url(")) return(col)
    rgb <- regmatches(col, regexec("^rgb\\(\\s*(\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)\\s*\\)$", col))[[1]]
    if (length(rgb) == 4) return(grDevices::rgb(as.numeric(rgb[2]), as.numeric(rgb[3]),
                                                as.numeric(rgb[4]), maxColorValue = 255))
    if (grepl("^#[0-9A-Fa-f]{3}$", col)) {
      col <- paste0("#", paste(rep(strsplit(substring(col, 2), "")[[1]], each = 2), collapse = ""))
    }
    tryCatch(toupper(grDevices::rgb(t(grDevices::col2rgb(col)), maxColorValue = 255)),
             error = function(e) col)
  }, "", USE.NAMES = FALSE)
}

# Inkscape layer label, or the id of the nearest enclosing group
svg_layer <- function(node) {
  for (g in as.list(xml2::xml_parents(node))) {
    if (xml2::xml_name(g) != "g") next
    attrs <- xml2::xml_attrs(g)
    label <- attrs[names(attrs) == "label"]   # inkscape:label, namespace prefix dropped
    if (length(label)) return(unname(label[1]))
    if (!is.na(attrs["id"])) return(unname(attrs["id"]))
  }
  NA_character_
}

# --- Transforms -------------------------------------------------------------

# Combined transform of a node and all its ancestors, as a 3x3 matrix
svg_node_transform <- function(node) {
  m <- diag(3)
  for (n in c(rev(as.list(xml2::xml_parents(node))), list(node))) {   # root first
    m <- m %*% parse_transform(xml2::xml_attr(n, "transform"))
  }
  m
}

parse_transform <- function(text) {
  m <- diag(3)
  if (is.na(text) || !nzchar(trimws(text))) return(m)
  ops <- regmatches(text, gregexpr("[A-Za-z]+\\s*\\([^)]*\\)", text))[[1]]
  for (op in ops) {
    name <- trimws(sub("\\(.*", "", op))
    a <- as.numeric(regmatches(op, gregexpr(svg_number_pattern, op))[[1]])
    t <- switch(
      name,
      matrix = rbind(c(a[1], a[3], a[5]), c(a[2], a[4], a[6]), c(0, 0, 1)),
      translate = rbind(c(1, 0, a[1]), c(0, 1, if (length(a) > 1) a[2] else 0), c(0, 0, 1)),
      scale = diag(c(a[1], if (length(a) > 1) a[2] else a[1], 1)),
      rotate = {
        r <- a[1] * pi / 180
        rot <- rbind(c(cos(r), -sin(r), 0), c(sin(r), cos(r), 0), c(0, 0, 1))
        if (length(a) == 3) {
          rbind(c(1, 0, a[2]), c(0, 1, a[3]), c(0, 0, 1)) %*% rot %*%
            rbind(c(1, 0, -a[2]), c(0, 1, -a[3]), c(0, 0, 1))
        } else {
          rot
        }
      },
      skewX = rbind(c(1, tan(a[1] * pi / 180), 0), c(0, 1, 0), c(0, 0, 1)),
      skewY = rbind(c(1, 0, 0), c(tan(a[1] * pi / 180), 1, 0), c(0, 0, 1)),
      stop(sprintf("Unknown SVG transform \"%s\".", name), call. = FALSE)
    )
    m <- m %*% t
  }
  m
}

# --- Shapes -----------------------------------------------------------------

svg_number_pattern <- "[-+]?(?:\\d*\\.\\d+|\\d+\\.?)(?:[eE][-+]?\\d+)?"

# Outlines of one shape: a list of list(x, y, closed)
svg_shape_outlines <- function(node, tag, curve_points) {
  num <- function(name, default = 0) {
    v <- suppressWarnings(as.numeric(sub("px$", "", xml2::xml_attr(node, name))))
    if (is.na(v)) default else v
  }
  ellipse <- function(cx, cy, rx, ry) {
    t <- seq(0, 2 * pi, length.out = max(24, 4 * curve_points) + 1)[-1]
    list(list(x = cx + rx * cos(t), y = cy + ry * sin(t), closed = TRUE))
  }
  switch(
    tag,
    path = parse_path_data(xml2::xml_attr(node, "d"), curve_points),
    polygon = , polyline = {
      pts <- as.numeric(regmatches(xml2::xml_attr(node, "points"),
                                   gregexpr(svg_number_pattern, xml2::xml_attr(node, "points")))[[1]])
      if (length(pts) < 2) return(list())
      list(list(x = pts[c(TRUE, FALSE)], y = pts[c(FALSE, TRUE)], closed = tag == "polygon"))
    },
    rect = {
      x <- num("x"); y <- num("y"); w <- num("width"); h <- num("height")
      list(list(x = c(x, x + w, x + w, x), y = c(y, y, y + h, y + h), closed = TRUE))
    },
    circle = ellipse(num("cx"), num("cy"), num("r"), num("r")),
    ellipse = ellipse(num("cx"), num("cy"), num("rx"), num("ry"))
  )
}

# SVG path data ("d" attribute) to a list of outlines, one per subpath.
# Curves and arcs are sampled with `curve_points` points per segment.
parse_path_data <- function(d, curve_points = 10) {
  if (is.na(d) || !nzchar(trimws(d))) return(list())
  tokens <- regmatches(d, gregexpr(paste0("[MmZzLlHhVvCcSsQqTtAa]|", svg_number_pattern), d))[[1]]
  i <- 1
  steps <- seq(0, 1, length.out = curve_points + 1)[-1]

  outlines <- list()
  xs <- numeric(); ys <- numeric()
  cur <- c(0, 0); start <- c(0, 0)
  last_cubic <- NULL; last_quad <- NULL
  cmd <- NULL

  flush <- function(closed) {
    if (length(xs) > 0) outlines[[length(outlines) + 1]] <<- list(x = xs, y = ys, closed = closed)
    xs <<- numeric(); ys <<- numeric()
  }
  add <- function(x, y) {
    if (length(xs) == 0) { xs <<- cur[1]; ys <<- cur[2] }
    xs <<- c(xs, x); ys <<- c(ys, y)
    cur <<- c(x[length(x)], y[length(y)])
  }
  number <- function() {
    if (i > length(tokens) || grepl("^[A-Za-z]$", tokens[i])) {
      stop("Malformed SVG path data: a command is missing numbers.", call. = FALSE)
    }
    v <- as.numeric(tokens[i]); i <<- i + 1; v
  }
  # Arc flags may be written without separators: "a5 5 0 011 1"
  flag <- function() {
    tok <- tokens[i]
    if (nchar(tok) > 1 && substr(tok, 1, 1) %in% c("0", "1")) {
      tokens[i] <<- substring(tok, 2)
      return(as.numeric(substr(tok, 1, 1)))
    }
    number()
  }

  while (i <= length(tokens)) {
    if (grepl("^[A-Za-z]$", tokens[i])) {
      cmd <- tokens[i]; i <- i + 1
      if (cmd %in% c("Z", "z")) {
        flush(closed = TRUE)
        cur <- start
        last_cubic <- NULL; last_quad <- NULL
        next
      }
    } else if (is.null(cmd) || cmd %in% c("Z", "z")) {
      stop("Malformed SVG path data: numbers without a command.", call. = FALSE)
    }
    rel <- cmd == tolower(cmd)
    base <- if (rel) cur else c(0, 0)
    cubic <- NULL; quad <- NULL

    switch(
      toupper(cmd),
      M = {
        flush(closed = FALSE)
        cur <- base + c(number(), number())
        start <- cur
        xs <- cur[1]; ys <- cur[2]
        cmd <- if (rel) "l" else "L"   # further pairs are line segments
      },
      L = { p <- base + c(number(), number()); add(p[1], p[2]) },
      H = { x <- number() + if (rel) cur[1] else 0; add(x, cur[2]) },
      V = { y <- number() + if (rel) cur[2] else 0; add(cur[1], y) },
      C = , S = {
        c1 <- if (toupper(cmd) == "C") base + c(number(), number())
              else if (!is.null(last_cubic)) 2 * cur - last_cubic else cur
        c2 <- base + c(number(), number())
        p <- base + c(number(), number())
        t <- steps
        add((1 - t)^3 * cur[1] + 3 * (1 - t)^2 * t * c1[1] + 3 * (1 - t) * t^2 * c2[1] + t^3 * p[1],
            (1 - t)^3 * cur[2] + 3 * (1 - t)^2 * t * c1[2] + 3 * (1 - t) * t^2 * c2[2] + t^3 * p[2])
        cubic <- c2
      },
      Q = , T = {
        c1 <- if (toupper(cmd) == "Q") base + c(number(), number())
              else if (!is.null(last_quad)) 2 * cur - last_quad else cur
        p <- base + c(number(), number())
        t <- steps
        add((1 - t)^2 * cur[1] + 2 * (1 - t) * t * c1[1] + t^2 * p[1],
            (1 - t)^2 * cur[2] + 2 * (1 - t) * t * c1[2] + t^2 * p[2])
        quad <- c1
      },
      A = {
        rx <- number(); ry <- number(); phi <- number()
        large <- flag(); sweep <- flag()
        p <- base + c(number(), number())
        a <- arc_points(cur, p, rx, ry, phi, large, sweep, curve_points)
        add(a$x, a$y)
      },
      stop(sprintf("Unknown SVG path command \"%s\".", cmd), call. = FALSE)
    )
    last_cubic <- cubic
    last_quad <- quad
  }
  flush(closed = FALSE)
  outlines
}

# Points along an elliptical arc (SVG spec, appendix F.6.5)
arc_points <- function(p0, p1, rx, ry, phi_deg, large, sweep, curve_points) {
  rx <- abs(rx); ry <- abs(ry)
  if (rx == 0 || ry == 0 || all(p0 == p1)) return(list(x = p1[1], y = p1[2]))
  phi <- phi_deg * pi / 180
  cp <- cos(phi); sp <- sin(phi)
  h <- (p0 - p1) / 2
  x1 <- cp * h[1] + sp * h[2]
  y1 <- -sp * h[1] + cp * h[2]
  scale <- x1^2 / rx^2 + y1^2 / ry^2
  if (scale > 1) { rx <- sqrt(scale) * rx; ry <- sqrt(scale) * ry }
  num <- rx^2 * ry^2 - rx^2 * y1^2 - ry^2 * x1^2
  coef <- sqrt(max(0, num / (rx^2 * y1^2 + ry^2 * x1^2)))
  if (large == sweep) coef <- -coef
  cx1 <- coef * rx * y1 / ry
  cy1 <- -coef * ry * x1 / rx
  centre <- c(cp * cx1 - sp * cy1, sp * cx1 + cp * cy1) + (p0 + p1) / 2
  angle <- function(u, v) atan2(u[1] * v[2] - u[2] * v[1], sum(u * v))
  u <- c((x1 - cx1) / rx, (y1 - cy1) / ry)
  v <- c((-x1 - cx1) / rx, (-y1 - cy1) / ry)
  theta <- angle(c(1, 0), u)
  delta <- angle(u, v)
  if (sweep == 0 && delta > 0) delta <- delta - 2 * pi
  if (sweep == 1 && delta < 0) delta <- delta + 2 * pi
  n <- max(2, ceiling(curve_points * abs(delta) / (pi / 2)))
  t <- theta + delta * seq(0, 1, length.out = n + 1)[-1]
  x <- cp * rx * cos(t) - sp * ry * sin(t) + centre[1]
  y <- sp * rx * cos(t) + cp * ry * sin(t) + centre[2]
  x[n] <- p1[1]; y[n] <- p1[2]   # end exactly on the target point
  list(x = x, y = y)
}
