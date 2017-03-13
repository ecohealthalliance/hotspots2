# Functions related to plotting.

quickmap <- function(data, fill, geom = "raster", limits = NULL, plot_factor = FALSE, trans_log = FALSE, pal_fun = "viridis", fill_name = NULL, ...) {
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
  arglist$fill_name <- NULL
  arglist$trans_log <- NULL

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
  } else if(trans_log == TRUE) {
    quickmap <- eval(qplot_call) + coord_fixed() + scale_fill_gradientn(colours = pal, limits = limits, trans = "log", name = fill_name, labels = function(x) format(x, digits = 3)) + ggtitle(title)
  } else {
    quickmap <- eval(qplot_call) + coord_fixed() + scale_fill_gradientn(colours = pal, limits = limits, name = fill_name) + ggtitle(title)
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

theme_black_nothing <- function(base_size = 12, base_family = "Helvetica")
{
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      line = element_blank(),
      text = element_blank(),
      panel.background = element_rect(fill = "black", color  =  NA),
      legend.position = "none"
      # axis.ticks = element_text
    )
}

theme_black_legend <- function(base_size = 10, base_family = "Helvetica") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      line = element_blank(),
      # text = element_blank(),
      axis.text = element_blank(),
      panel.background = element_rect(fill = "black", color  =  NA),
      legend.position = "right",
      legend.text = element_text(size = 8, color = "white")
      # axis.ticks = element_text
    )
}

theme_black <- function(base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Specify axis options
      axis.line = element_blank(),
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.ticks = element_line(color = "white", size  =  0.2),
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),
      axis.ticks.length = unit(0.3, "lines"),
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),
      legend.key = element_rect(color = "white",  fill = "black"),
      legend.key.size = unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = base_size*0.8, color = "white"),
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),
      legend.position = "right",
      legend.text.align = NULL,
      legend.title.align = NULL,
      legend.direction = "vertical",
      legend.box = NULL,
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),
      panel.border = element_rect(fill = NA, color = "white"),
      panel.grid.major = element_line(color = "grey35"),
      panel.grid.minor = element_line(color = "grey20"),
      panel.margin = unit(0.5, "lines"),
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = element_text(size = base_size*0.8, color = "white"),
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),
      plot.title = element_text(size = base_size*1.2, color = "white"),
      plot.margin = unit(rep(1, 4), "lines")
    )
}
