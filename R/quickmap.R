# Functions related to plotting.

quickmap <- function(data, fill, geom = "raster", limits = NULL, plot_factor = FALSE, pal_fun = "viridis", ...) {
  require(ggplot2)
  require(RColorBrewer)
  require(viridis)

  arglist <- as.list(match.call())

  arglist[[1]] <- substitute(qplot)

  data <- eval(arglist$data)
  arglist$data <- data

  if ("lon" %in% names(data)) {
    arglist$x <- substitute(lon)
    arglist$y <- substitute(lat)
  } else if ("x" %in% names(data)) {
    arglist$x <- substitute(x)
    arglist$y <- substitute(y)
  } else if ("gridid" %in% names(data)) {
    arglist$data <- substitute(left_join(data, template_df))
    arglist$x <- substitute(lon)
    arglist$y <- substitute(lat)
  }

  if (is.character(arglist$fill)) {
    arglist$fill <- parse(text = arglist$fill)
  }

  arglist$geom <- geom
  arglist$pal_fun <- NULL

  qplot_call <- as.call(arglist)

  # Looks at the variable we're gonna use to fill to see how manu values there are.
  if (is.name(arglist$fill)) {
    numcolors <- length(unique(data[[as.character(arglist$fill)]]))
  } else {
    numcolors <- 100
  }

  pal <- get(pal_fun)(numcolors)

  title <- arglist$fill

  if (plot_factor == TRUE) {
    quickmap <- eval(qplot_call) + coord_fixed() + ggtitle(title)
  } else {
    quickmap <- eval(qplot_call) + coord_fixed() + scale_fill_gradientn(colours = pal, limits = limits) + ggtitle(title)
  }

  return(quickmap)
}

theme_nothing <- function(base_size = 12, base_family = "Helvetica")
{
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      rect = element_blank(),
      line = element_blank(),
      text = element_blank(),
      legend.position = "none"
      # axis.ticks = element_text
    )
}

nolines <- function(base_size = 12, base_family = "Helvetica")
{
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      rect = element_blank(),
      line = element_blank()
      # text = element_blank()
      # legend.position = "none"
      # axis.ticks = element_text
    )
}

notext <- function() {
  require(ggplot2)
  labs(x=NULL, y=NULL)
}

notext <- function() {
  require(ggplot2)
  theme(text = element_blank())
}

notitle <- function() {
  require(ggplot2)
  labs(title=NULL)
}

nolegend <- function() {
  require(ggplot2)
  theme(legend.position = "none")
}

simple_legend <- function() {
  guides(fill = guide_colorbar(title = element_blank(), label = FALSE))
}

# spectral <- rev(colorRampPalette(brewer.pal(11, "Spectral"))(100))
spectral <- function(numcolors) {
  rev(colorRampPalette(brewer.pal(11, "Spectral"))(numcolors))
}

quickglobe <- function(fill, data, geom = "tile", limits = NULL, plot_factor = FALSE, orientation = c(-90, 0, 0), ...) {
  require(ggplot2)
  require(RColorBrewer)

  arglist <- as.list(match.call())

  arglist[[1]] <- substitute(qplot)

  if ("lon" %in% names(eval(arglist$data))) {
    arglist$x <- substitute(lon)
    arglist$y <- substitute(lat)
  } else if ("x" %in% names(eval(arglist$data))) {
    arglist$x <- substitute(x)
    arglist$y <- substitute(y)
  }

  if (is.character(arglist$fill)) {
    arglist$fill <- parse(text = arglist$fill)
  }

  arglist$geom <- geom

  qplot_call <- as.call(arglist)

  # Looks at the variable we're gonna use to fill to see how manu values there are.
  if (is.name(arglist$fill)) {
    numcolors <- length(unique(eval(arglist$data)[[as.character(arglist$fill)]]))
  } else {
    numcolors <- 100
  }

  pal <- spectral(numcolors)

  title <- arglist$fill

  if (plot_factor == TRUE) {
    quickmap <- eval(qplot_call) + coord_map(projection = "perspective", orientation = orientation, dist = 10) + ggtitle(title)
  } else {
    quickmap <- eval(qplot_call) + coord_map(projection = "perspective", orientation = orientation, dist = 10) + scale_fill_gradientn(colours = pal, limits = limits) + ggtitle(title)
  }

  return(quickmap)
}

centered_limits <- function(x) {
  x <- na.omit(x)
  x <- x[is.finite(x)]
  lim <- max(abs(c(max(x), min(x))))
  lims <- c(-lim, lim)
  return(lims)
}

quantvar <- function(x) {
  breaks <- quantile(x, seq(0, 1, length.out=256), na.rm = TRUE)
  breaks <- unique(breaks)
  if (inherits(x, "RasterLayer")) {
    quantvar <- raster::cut(x, breaks = breaks, include.lowest = TRUE)
  } else {
    quantvar <- cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  }
  return(quantvar)
}