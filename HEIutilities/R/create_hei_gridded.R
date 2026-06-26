#' Create a State of Global Air-style gridded map
#'
#' Builds a global gridded map from a `terra` raster and `sf` polygon
#' boundaries, applies HEI/State of Global Air styling, and optionally saves the
#' resulting plot in one or more file formats. When `save_plot = FALSE`, the
#' function returns the unwatermarked `ggplot` object so it can be inspected,
#' modified, or saved by the caller.
#'
#' @param gridded_data A `terra::SpatRaster` containing the gridded values to
#'   plot. For `cut_data = TRUE`, pass a classified raster whose values map to
#'   the requested discrete legend labels. For `cut_data = FALSE`, pass a
#'   continuous raster.
#' @param max_global_cells Maximum number of raster cells to draw in the main
#'   map. Passed to `tidyterra::geom_spatraster(maxcell = ...)`.
#' @param max_inset_cells Maximum number of raster cells intended for inset
#'   maps. This argument is currently reserved and is not used by the function.
#' @param poly_data An `sf` object containing boundary polygons. The object must
#'   include a `loc_name` column because the function uses it to fill selected
#'   disputed areas.
#' @param watermark Logical. If `TRUE`, also creates watermarked saved output
#'   when `save_plot = TRUE`. The current implementation expects this to remain
#'   `TRUE`.
#' @param watermark_text Character string used as the watermark label.
#' @param watermark_x,watermark_y Numeric watermark coordinates. These arguments
#'   are currently reserved; the watermark is drawn in the lower-left corner.
#' @param palette Character vector of colors. For `cut_data = TRUE`, colors are
#'   used as a discrete scale. For `cut_data = FALSE`, colors are used in a
#'   continuous gradient.
#' @param trans Character string or transformation object passed to
#'   `ggplot2::scale_fill_gradientn()` when `cut_data = FALSE`.
#' @param breaks Numeric vector of legend breaks. These are also named with
#'   `legend_labels` internally.
#' @param cut_data Logical. If `TRUE`, draw the raster with a discrete fill
#'   scale. If `FALSE`, draw it with a continuous gradient fill scale.
#' @param legend_title Character string used as the fill legend title. Markdown
#'   is supported because legend titles are rendered with
#'   `ggtext::element_markdown()`.
#' @param legend_labels Character vector of labels shown in the legend.
#' @param legend_labels_format Character string controlling legend label
#'   rendering. Use `"markdown"` for markdown labels; any other value uses
#'   plain text labels.
#' @param legend_blank_col Color used to fill the final legend key when the last
#'   entry is intended to be text-only.
#' @param legend_pos_x,legend_pos_y Numeric legend position coordinates in the
#'   range used by `ggplot2::theme(legend.position = c(x, y))`.
#' @param disputed Logical. If `TRUE`, draw disputed boundaries from
#'   `disputed_boundaries`.
#' @param disputed_boundaries An `sf` object containing disputed boundary line
#'   or polygon features. Required when `disputed = TRUE`.
#' @param disp_fil Fill color used for selected disputed or special-status
#'   polygon areas, currently French Guiana and Western Sahara when present in
#'   `poly_data$loc_name`.
#' @param disp_col Line color used when drawing `disputed_boundaries`.
#' @param bord_col Boundary line color for `poly_data`.
#' @param insets Logical flag intended for inset maps. This argument is
#'   currently reserved and is not used by the function.
#' @param plot_units Units passed to `ggplot2::ggsave()`.
#' @param plot_width,plot_height Plot dimensions passed to `ggplot2::ggsave()`.
#' @param plot_dps Plot resolution, in dots per inch, passed to
#'   `ggplot2::ggsave(dpi = ...)`.
#' @param plot_font Font family used in plot text.
#' @param plot_legend_title_size,plot_legend_text_size,plot_text_size Numeric
#'   font sizes for legend titles, legend labels, and general plot text.
#' @param plot_key_size,plot_key_spacing Numeric legend key size and vertical
#'   spacing, in centimeters.
#' @param plot_legend_background Fill color for the legend background.
#' @param save_plot Logical. If `TRUE`, save plot files and a metadata text file
#'   to `out_path`. If `FALSE`, return the unwatermarked `ggplot` object.
#' @param out_path Character path prefix where output files are written when
#'   `save_plot = TRUE`. Include a trailing path separator if needed, because
#'   filenames are built with `paste0(out_path, out_file_name, ...)`.
#' @param out_file_name Character file stem used for saved plot and metadata
#'   outputs.
#' @param out_types Character vector of output formats to save. Supported values
#'   are `"png"`, `"jpg"`, `"pdf"`, and `"eps"`.
#' @param info_source,info_year,info_poll,info_data Metadata values written to
#'   the companion text file when `save_plot = TRUE`.
#'
#' @return If `save_plot = FALSE`, a `ggplot` object. If `save_plot = TRUE`, the
#'   function is called for its side effects: plot files and a metadata text file
#'   are written to disk and no value is returned.
#'
#' @examples
#' if (requireNamespace("sf", quietly = TRUE) &&
#'     requireNamespace("terra", quietly = TRUE)) {
#'   r <- terra::rast(
#'     nrows = 2,
#'     ncols = 2,
#'     xmin = 0,
#'     xmax = 2,
#'     ymin = 0,
#'     ymax = 2,
#'     vals = c(0.2, 0.4, 0.6, 0.8)
#'   )
#'
#'   boundary <- sf::st_as_sf(
#'     data.frame(
#'       loc_name = "Example",
#'       wkt = "POLYGON ((0 0, 2 0, 2 2, 0 2, 0 0))"
#'     ),
#'     wkt = "wkt",
#'     crs = 4326
#'   )
#'
#'   p <- create_hei_gridded(
#'     gridded_data = r,
#'     poly_data = boundary,
#'     palette = c("#216999", "#d96354"),
#'     breaks = c(0, 1),
#'     cut_data = FALSE,
#'     legend_title = "Example value",
#'     legend_labels = c("Low", "High"),
#'     legend_blank_col = "white",
#'     bord_col = "gray40",
#'     disp_fil = "gray90",
#'     disp_col = "gray40",
#'     save_plot = FALSE
#'   )
#'   p
#' }
#'
#' @export
create_hei_gridded <- function(
    gridded_data,
    max_global_cells = 5e6,
    max_inset_cells = 5e6,
    poly_data,
    watermark =  TRUE,
    watermark_text = "State of Global Air",
    watermark_x = -Inf,
    watermark_y = Inf,
    palette,
    trans = "identity",
    breaks,
    cut_data = TRUE,
    legend_title,
    legend_labels,
    legend_labels_format = "standard",
    legend_blank_col, # color used to fill in the last legend entry when you want it to be just text
    legend_pos_x = 0,
    legend_pos_y = 0,
    disputed = FALSE, # disputed boundaries toggle
    disputed_boundaries = NULL, # disputed boundaries data parameter
    disp_fil,
    disp_col, # color for disputed polygon regions like Western Sahara or French Guiana
    bord_col,
    insets = FALSE,
    plot_units = "px",
    plot_width = 2600,
    plot_height = 1450,
    plot_dps = 300,
    plot_font = "PT Sans",
    plot_legend_title_size = 9,
    plot_legend_text_size = 9,
    plot_text_size = 9,
    plot_key_size = .6,
    plot_key_spacing = .2,
    plot_legend_background = "#FFFFFF",
    save_plot = TRUE,
    out_path,
    out_file_name,
    out_types = c("png", "jpg", "pdf", "eps"),
    info_source,
    info_year,
    info_poll,
    info_data
    ) {

  # ggplot2 themeing reference :)
  # https://ggplot2.tidyverse.org/reference/theme.html
  if(legend_labels_format == "markdown") {
    main_theme <-   ggplot2::theme(
      text = ggplot2::element_text(size = plot_text_size, lineheight = 0.1, family = plot_font),
      legend.position = c(legend_pos_x, legend_pos_y),
      legend.title = ggtext::element_markdown(size = plot_legend_title_size, family = plot_font),
      legend.text = ggtext::element_markdown(size = plot_legend_text_size, family = plot_font),
      legend.background = ggplot2::element_rect(colour = NA, fill = plot_legend_background),
      legend.key.size = ggplot2::unit(plot_key_size, 'cm'),
      legend.key.spacing.y = ggplot2::unit(plot_key_spacing, 'cm'),
      plot.margin = ggplot2::margin(t = 0, r = 0, b = 100, l = 0)
    )
  } else {
    main_theme <-   ggplot2::theme(
      text = ggplot2::element_text(size = plot_text_size, lineheight = 0.1, family = plot_font),
      legend.position = c(legend_pos_x, legend_pos_y),
      legend.title = ggtext::element_markdown(size = plot_legend_title_size, family = plot_font),
      legend.text = ggplot2::element_text(size = plot_legend_text_size, family = plot_font),
      legend.background = ggplot2::element_rect(colour = NA, fill = plot_legend_background),
      legend.key.size = ggplot2::unit(plot_key_size, 'cm'),
      legend.key.spacing.y = ggplot2::unit(plot_key_spacing, 'cm'),
      plot.margin = ggplot2::margin(t = 0, r = 0, b = 100, l = 0)
    )
  }

  names(breaks) <- legend_labels

  p1 <- ggplot2::ggplot() +
    ggplot2::labs(fill = legend_title) +
    ggplot2::coord_sf(crs = "+proj=longlat +datum=WGS84 +no_defs", expand = FALSE) +
    ggplot2::geom_sf(data = poly_data, fill = NA, size = .2, color = bord_col, show.legend = FALSE) +
    ggplot2::geom_sf(data = poly_data |> dplyr::filter(loc_name %in% c("French Guiana", "Western Sahara")), size = .2, fill = disp_fil, color = bord_col, show.legend = FALSE) +
    ggthemes::theme_map() +
    main_theme

  if(disputed) {
    p1 <- p1 +
      geom_sf(data = disputed_boundaries, color = disp_col, fill = NA, linetype = "dotted", size = .2, show.legend = FALSE)
  }

  # browser()

  if(!cut_data) {
    p1 <- p1 +
      ggplot2::scale_fill_gradientn(colours = palette,
                           limits=c(breaks[1], breaks[length(breaks)]),
                           breaks = breaks,
                           trans = trans,
                           na.value = NA
      )
  } else {
    p1 <- p1 +
      ggplot2::scale_fill_discrete(type = palette,
                          labels = legend_labels,
                          na.value = NA,
                          drop = FALSE
      )  +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          override.aes = list(fill = c(palette, legend_blank_col)),
          byrow = TRUE
        )
      )
  }

  if(watermark){
    p1.w <- p1 +
      ggplot2::annotate("text",
                        x = -Inf, y = -Inf,
                        hjust = -0.02, vjust = -0.5,
                        label = watermark_text,
                        color = "gray50",
                        size = 3,
                        family = "PT Sans",
                        fontface = "italic")
  }

  gridded <- tidyterra::geom_spatraster(data = gridded_data, maxcell = max_global_cells)

  if(is.list(gridded)) {
    gridded <- gridded[1:2]
  }

  final.plot <- p1 +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 0, r = 1, b = 0, l = 1))

  final.plot$layers <- c(
    gridded,
    final.plot$layers
    )

  final.plot.watermark <- p1.w +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 0, r = 1, b = 0, l = 1))

  final.plot.watermark$layers <- c(
    gridded,
    final.plot.watermark$layers
  )

  if(save_plot) {

    if ("png" %in% out_types) {
      ggplot2::ggsave(plot = final.plot,
             filename = paste0(out_path, out_file_name, ".png"),
             device = "png",
             units = plot_units,
             width = plot_width,
             height = plot_height,
             dpi = plot_dps,
             bg = 'white'
             )
      ggplot2::ggsave(plot = final.plot.watermark,
             filename = paste0(out_path, out_file_name, "_watermark.png"),
             device = "png",
             units = plot_units,
             width = plot_width,
             height = plot_height,
             dpi = plot_dps,
             bg = 'white'
      )
    }

    if ("jpg" %in% out_types) {
      ggplot2::ggsave(plot = final.plot,
             filename = paste0(out_path, out_file_name, ".jpg"),
             device = "jpg",
             units = plot_units,
             width = plot_width,
             height = plot_height,
             dpi = plot_dps,
             bg = 'white'
             )
      ggplot2::ggsave(plot = final.plot.watermark,
             filename = paste0(out_path, out_file_name, "_watermark.jpg"),
             device = "jpg",
             units = plot_units,
             width = plot_width,
             height = plot_height,
             dpi = plot_dps,
             bg = 'white'
      )
    }

    if ("pdf" %in% out_types) {
      ggplot2::ggsave(plot = final.plot,
             filename = paste0(out_path, out_file_name, ".pdf"),
             device = cairo_pdf,
             units = plot_units,
             width = plot_width,
             height = plot_height,
             dpi = plot_dps,
             bg = 'white'
             )
      ggplot2::ggsave(plot = final.plot.watermark,
             filename = paste0(out_path, out_file_name, "_watermark.pdf"),
             device = cairo_pdf,
             units = plot_units,
             width = plot_width,
             height = plot_height,
             dpi = plot_dps,
             bg = 'white'
      )
    }

    if ("eps" %in% out_types) {
      ggplot2::ggsave(plot = final.plot,
             filename = paste0(out_path, out_file_name, ".eps"),
             device = cairo_ps,
             units = plot_units,
             width = plot_width,
             height = plot_height,
             dpi = plot_dps,
             bg = 'white')
      ggplot2::ggsave(plot = final.plot.watermark,
             filename = paste0(out_path, out_file_name, "_watermark.eps"),
             device = cairo_ps,
             units = plot_units,
             width = plot_width,
             height = plot_height,
             dpi = plot_dps,
             bg = 'white')
    }

  } else {
    return(final.plot)
  }

  # write out readme containing all info about the plot
  info_list <- list(
    Source = info_source,
    Year = info_year,
    Pollutant = info_poll,
    Data = info_data,
    Data_transformation = trans
  )

  sink(paste0(out_path, out_file_name, ".txt"))
  print(info_list)
  sink()

}
