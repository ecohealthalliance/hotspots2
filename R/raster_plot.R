spectral <- function(numcolors = 256) {
  require(RColorBrewer)
  rev(colorRampPalette(brewer.pal(11, "Spectral"))(numcolors))
}

raster_plot <- function(x, png_file = NULL, maxpixels = ncell(x), cuts = 255, pal_fun = viridis,
                        bg_color = "white", ...) {
  library(viridis)
  plot_theme <- list(layout.heights = list(top.padding = 0,
                                           main.key.padding = 0,
                                           key.axis.padding = 0,
                                           axis.xlab.padding = 0,
                                           xlab.key.padding = 0,
                                           key.sub.padding = 0,
                                           bottom.padding = 0),
                     layout.widths = list(left.padding = 0,
                                          key.ylab.padding = 0,
                                          ylab.axis.padding = 0,
                                          axis.key.padding = 0,
                                          right.padding = 0),
                     axis.line = list(col = 0),
                     panel.background = list(col = bg_color))

  if (is.null(png_file)) {
    spplot(x, maxpixels = maxpixels, col.regions = pal_fun(cuts + 1), cuts = cuts, colorkey = FALSE, par.settings = plot_theme, ...)
  } else {
    png(png_file, width = ncol(x), height = nrow(x))
    print(spplot(x, maxpixels = maxpixels, col.regions = pal_fun(cuts + 1), cuts = cuts, colorkey = FALSE, par.settings = plot_theme, ...))
    dev.off()
  }
}
