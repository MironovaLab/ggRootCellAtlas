#' Generate Root Cell Atlas Annotation Visualizations
#'
#' @description This function creates annotated visualizations for various root sections.
#' It uses a shared color palette to represent group annotations and combines multiple
#' plots into a single composite visualization.
#'
#' @param Group_name Character. Name of the column in the root maps that holds
#'   the group annotations to be visualized: one of `"SubCellTypes"`,
#'   `"CellTypes"`, `"TissueSubTypes"`, `"TissueTypes"`, `"Zones"`,
#'   `"Sections"`, `"Atlas"` or `"Atlas_reduced"`.
#' @param palette Colours for the groups: a palette function taking the number
#'   of colours (default [okabe_ito_pal()], colour-blind safe for up to 11
#'   groups), an unnamed vector of colours assigned in order, or a vector named
#'   by group.
#' @param na.colour Colour of cells without annotation. Defaults to dark grey.
#' @param maps Maps to draw: a list of map data frames, or a single one.
#'   Defaults to the bundled root maps, [root_maps()]. Use [svg_to_map()] to
#'   build your own from an SVG drawing.
#' @param layout Arrangement of the maps as a patchwork `design` string (see
#'   [patchwork::wrap_plots()]). By default the bundled maps use the root
#'   layout and other maps a simple grid.
#'
#' @return A patchwork object combining the annotated plots, one per
#'   Arabidopsis thaliana root section, with a shared legend.
#'
#' @examples
#' ggRootCellAtlas_annotation("Zones")
#' ggRootCellAtlas_annotation("TissueTypes")
#' ggRootCellAtlas_annotation("Atlas_reduced")
#'
#' # Own colours, named by group
#' ggRootCellAtlas_annotation("TissueTypes", palette = c(
#'   Epidermis = "#E69F00", "Ground tissue" = "#009E73", Stele = "#CC79A7",
#'   "Root cap" = "#56B4E9", SCN = "#D55E00"
#' ))
#'
#' @import ggplot2
#' @import patchwork
#' @import ggPlantmap
#'
#' @export

ggRootCellAtlas_annotation <- function(Group_name, palette = okabe_ito_pal(), na.colour = "grey30",
                                       maps = root_maps(), layout = NULL) {

  maps <- check_maps(maps)
  check_annotation_column(Group_name, maps, "Group_name")

  # Generate color palette
  palette <- do.call(generate_common_palette,
                     c(list(Group_name), unname(maps), list(color_palette = palette)))

  # Generate plots; identical scales let patchwork merge them into one legend
  plots <- lapply(maps, function(map) {
    ggPlantmap.plot(data = map, .data[[Group_name]], show.legend = TRUE) +
      scale_fill_manual(values = palette, limits = names(palette),
                        na.value = na.colour, name = Group_name)
  })

  combine_maps(plots, maps, layout)
}
