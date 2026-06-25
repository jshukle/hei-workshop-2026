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