#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(viridis)
})

input_file <- "data/processed/pink_sheet_combined.csv"
output_dir <- "docs/assets/plots"
output_indices <- file.path(output_dir, "pink_sheet_indices.png")
output_energy <- file.path(output_dir, "pink_sheet_energy.png")
output_fertilizers <- file.path(output_dir, "pink_sheet_fertilizers.png")
output_grains <- file.path(output_dir, "pink_sheet_grains.png")
output_grainsurea <- file.path(output_dir, "pink_sheet_grainsurea.png")

required_indices <- c("Energy", "Grains", "Fertilizers")
prices_energy <- c("Crude_oil_Brent", "Natural_gas_Europe", "Coal_Australian")
prices_fertilizers <- c("Urea", "DAP", "Potassium_chloride")
prices_grains <- c("Wheat_US_SRW", "Maize", "Rice_Thai_5")
label_gap_frac <- 0.07
annotation_top_frac <- 0.12
annotation_left_frac <- 0.04
single_y_expand <- c(-annotation_left_frac+.02, 0.15)
double_y_expand <- c(-0.15, 0.15)
plot_width <- 16
plot_height <- 9
plot_dpi <- 300

pal <- viridis(9, option = "H", begin = .1, end = .9)
pal

series_labels <- c(
  Energy = "Energy",
  Grains = "Grains",
  Fertilizers = "Fertilizers",
  Crude_oil_Brent = "Crude Oil (Brent)",
  Natural_gas_Europe = "Natural Gas (Europe)",
  Coal_Australian = "Coal (Australia)",
  Urea = "Urea",
  DAP = "DAP",
  Potassium_chloride = "Potash",
  Wheat_US_SRW = "Wheat (US SRW)",
  Maize = "Maize (US No2)",
  Rice_Thai_5 = "Rice (Thai 5%)"
)

fail_gracefully <- function(message_text, status = 0L) {
  message(sprintf("Plot generation skipped: %s", message_text))
  quit(save = "no", status = status)
}

if (!file.exists(input_file)) {
  fail_gracefully(sprintf("processed dataset not found at %s", input_file))
}

adjust_end_labels <- function(
    dt,
    x_col = "Date",
    y_col = "Value",
    group_col = "Series",
    x_nudge_days = 180,
    min_gap_frac = 0.04,
    lower_pad_frac = 0.02,
    upper_pad_frac = 0.02
) {
  dt <- data.table::as.data.table(data.table::copy(dt))

  label_dt <- dt[, .SD[which.max(get(x_col))], by = group_col]
  data.table::setorderv(label_dt, y_col, order = -1L)

  y_rng <- range(dt[[y_col]], na.rm = TRUE)
  y_span <- diff(y_rng)
  if (!is.finite(y_span) || y_span == 0) {
    y_span <- max(abs(y_rng[1]), 1)
  }

  min_gap <- y_span * min_gap_frac
  lower_bound <- y_rng[1] + lower_pad_frac * y_span
  upper_bound <- y_rng[2] - upper_pad_frac * y_span

  label_dt[, x_lab := max(dt[[x_col]], na.rm = TRUE) + x_nudge_days]
  label_dt[, y_lab := get(y_col)]

  if (nrow(label_dt) >= 2) {
    for (i in 2:nrow(label_dt)) {
      if ((label_dt$y_lab[i - 1] - label_dt$y_lab[i]) < min_gap) {
        label_dt$y_lab[i] <- label_dt$y_lab[i - 1] - min_gap
      }
    }
  }

  if (min(label_dt$y_lab, na.rm = TRUE) < lower_bound) {
    shift_up <- lower_bound - min(label_dt$y_lab, na.rm = TRUE)
    label_dt[, y_lab := y_lab + shift_up]
  }

  if (max(label_dt$y_lab, na.rm = TRUE) > upper_bound) {
    shift_down <- max(label_dt$y_lab, na.rm = TRUE) - upper_bound
    label_dt[, y_lab := y_lab - shift_down]
  }

  label_dt[, y_lab := pmin(pmax(y_lab, lower_bound), upper_bound)]
  label_dt[]
}

apply_series_labels <- function(dt) {
  dt <- data.table::copy(dt)
  series_keys <- as.character(dt$Series)
  dt[, Series_Label := fifelse(
    series_keys %in% names(series_labels),
    unname(series_labels[series_keys]),
    series_keys
  )]
  dt[]
}

load_plot_data <- function(input_path) {
  plot_dt <- tryCatch(
    fread(input_path),
    error = function(e) {
      fail_gracefully(sprintf("unable to read %s (%s)", input_path, conditionMessage(e)))
    }
  )

  required_columns <- c("Date", "Series", "Value")
  missing_columns <- setdiff(required_columns, names(plot_dt))
  if (length(missing_columns)) {
    fail_gracefully(sprintf(
      "processed dataset is missing required columns: %s",
      paste(missing_columns, collapse = ", ")
    ))
  }

  plot_dt[, Date := as.Date(Date)]
  plot_dt[, Value := suppressWarnings(as.numeric(Value))]
  plot_dt <- plot_dt[!is.na(Date) & !is.na(Value)]

  if (!nrow(plot_dt)) {
    fail_gracefully("all candidate rows were missing Date or Value after parsing")
  }

  file_update <- if ("Update" %in% names(plot_dt)) unique(plot_dt$Update) else character()
  if (length(file_update) > 1L) {
    file_update <- sort(file_update)
    warning(sprintf(
      "Multiple Update values found; using the latest one: %s",
      tail(file_update, 1L)
    ))
  }

  list(
    data = plot_dt[Date >= as.Date("2000-01-01")],
    file_update = tail(file_update, 1L)
  )
}

prepare_series_data <- function(plot_dt, series_levels) {
  filtered_dt <- data.table::copy(plot_dt[Series %in% series_levels])
  if (!nrow(filtered_dt)) {
    fail_gracefully("no rows found for the requested series")
  }

  filtered_dt[, Series := factor(Series, levels = series_levels)]
  filtered_dt <- apply_series_labels(filtered_dt)
  filtered_dt[]
}

build_caption <- function(file_update) {
  if (!length(file_update) || is.na(file_update) || !nzchar(file_update)) {
    update_text <- "an unknown date"
  } else {
    update_date <- as.Date(file_update)
    if (is.na(update_date)) {
      update_text <- file_update
    } else {
      update_text <- format(update_date, "%d %B %Y")
    }
  }

  paste0(
    "Source: World Bank 'Pink Sheet' updated on ",
    update_text,
    "\navailable at https://www.worldbank.org/en/research/commodity-markets"
  )
}

base_plot_theme <- theme(
  plot.title.position = "plot",
  legend.position = "none",
  panel.grid.major.y = element_line(linewidth = 0.6, linetype = 3, color = "gray"),
  plot.caption = element_text(hjust = 0, color = "dimgray", family = "mono"),
  plot.margin = margin(30, 90, 20, 90)
)

compute_y_scale <- function(
    values,
    n_breaks = 5,
    top_padding_frac = annotation_top_frac,
    lower_limit = 0
) {
  y_rng <- range(values, na.rm = TRUE)
  y_bottom <- min(lower_limit, y_rng[1])
  y_top <- y_rng[2]
  breaks <- pretty(c(y_bottom, y_top), n = n_breaks)
  breaks <- breaks[breaks >= y_bottom & breaks <= y_top]

  if (!length(breaks)) {
    breaks <- unique(c(y_bottom, y_top))
  }

  top_tick <- max(breaks, na.rm = TRUE)
  y_span <- max(y_top - y_bottom, 1e-6)
  y_upper <- max(y_top, top_tick) + y_span * top_padding_frac

  list(
    breaks = breaks,
    y_lower = y_bottom,
    y_upper = y_upper
  )
}

create_single_axis_plot <- function(
    plot_dt,
    label_dt,
    caption_text,
    annotation_text,
    y_lower = 0,
    y_labels = function(x) sprintf("%4.0f", x)
) {
  x_left <- min(plot_dt$Date, na.rm = TRUE)
  y_scale <- compute_y_scale(plot_dt$Value, lower_limit = y_lower)
  y_upper <- y_scale$y_upper
  x_range <- as.numeric(diff(range(plot_dt$Date, na.rm = TRUE)))
  y_annotation <- y_upper
  x_annotation <- x_left - round(annotation_left_frac * x_range)
  
  ggplot(
    plot_dt,
    aes(x = Date, y = Value, color = Series)
  ) +
    geom_line(linewidth = 1) +
    geom_text(
      data = label_dt,
      aes(x = x_lab, y = y_lab, label = Series_Label),
      hjust = 0,
      size = 8,
      show.legend = FALSE
    ) +
    scale_color_manual(values = pal[c(2, 7, 9)]) +
    annotate(
      "text",
      x = x_annotation,
      y = y_annotation,
      label = annotation_text,
      hjust = 1,
      vjust = 1,
      size = 6
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = " ",
      caption = caption_text
    ) +
    scale_y_continuous(
      labels = y_labels,
      breaks = y_scale$breaks,
      limits = c(y_scale$y_lower, y_upper),
      expand = c(0, 0)
    ) +
    scale_x_date(
      expand = expansion(mult = single_y_expand)
    ) +
    coord_cartesian(clip = "off") +
    theme_classic(base_size = 28) +
    base_plot_theme +
    theme(
      axis.text = element_text(family = "mono")
    )
}

save_plot <- function(plot_obj, filename) {
  ggsave(
    filename = filename,
    plot = plot_obj,
    width = plot_width,
    height = plot_height,
    dpi = plot_dpi,
    bg = "white"
  )
  message(sprintf("Saved plot to %s", filename))
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

plot_inputs <- load_plot_data(input_file)
all_plot_dt <- plot_inputs$data
plot_caption <- build_caption(plot_inputs$file_update)

# Indices

indices_dt <- prepare_series_data(all_plot_dt, required_indices)
indices_labels <- adjust_end_labels(
  dt = indices_dt,
  x_col = "Date",
  y_col = "Value",
  group_col = "Series",
  x_nudge_days = 180,
  min_gap_frac = label_gap_frac
)

indices_plot <- create_single_axis_plot(
  plot_dt = indices_dt,
  label_dt = indices_labels,
  caption_text = plot_caption,
  annotation_text = "Index"
)

save_plot(indices_plot, output_indices)

# Fertilizer prices

fertilizers_dt <- prepare_series_data(all_plot_dt, prices_fertilizers)
fertilizers_labels <- adjust_end_labels(
  dt = fertilizers_dt,
  x_col = "Date",
  y_col = "Value",
  group_col = "Series",
  x_nudge_days = 180,
  min_gap_frac = label_gap_frac
)

fertilizers_plot <- create_single_axis_plot(
  plot_dt = fertilizers_dt,
  label_dt = fertilizers_labels,
  caption_text = plot_caption,
  annotation_text = "($/mt)"
)

save_plot(fertilizers_plot, output_fertilizers)

# Grain prices

grains_dt <- prepare_series_data(all_plot_dt, prices_grains)
grains_labels <- adjust_end_labels(
  dt = grains_dt,
  x_col = "Date",
  y_col = "Value",
  group_col = "Series",
  x_nudge_days = 180,
  min_gap_frac = label_gap_frac
)

grains_plot <- create_single_axis_plot(
  plot_dt = grains_dt,
  label_dt = grains_labels,
  caption_text = plot_caption,
  annotation_text = "($/mt)"
)

save_plot(grains_plot, output_grains)


# grains to urea ratios

grainsurea_dt <- merge(grains_dt,fertilizers_dt[Series=="Urea"][,.(Date,Urea=Value)],by="Date")

grainsurea_dt[,Value:=Value/Urea]

grainsurea_labels <- adjust_end_labels(
  dt = grainsurea_dt,
  x_col = "Date",
  y_col = "Value",
  group_col = "Series",
  x_nudge_days = 180,
  min_gap_frac = label_gap_frac
)

grainsurea_plot <- create_single_axis_plot(
  plot_dt = grainsurea_dt,
  label_dt = grainsurea_labels,
  caption_text = plot_caption,
  annotation_text = "Price Ratio",
  y_labels = function(x) sprintf("%.2f", x)
)

save_plot(grainsurea_plot, output_grainsurea)


# Energy prices

energy_dt <- prepare_series_data(all_plot_dt, prices_energy)
energy_dt[, Value_plot := fifelse(
  Series == "Natural_gas_Europe",
  Value * 5,
  Value
)]

energy_labels <- adjust_end_labels(
  dt = energy_dt,
  x_col = "Date",
  y_col = "Value_plot",
  group_col = "Series",
  x_nudge_days = 180,
  min_gap_frac = label_gap_frac
)

gas_breaks <- pretty(
  c(0, energy_dt[Series == "Natural_gas_Europe", max(Value, na.rm = TRUE)]),
  n = 5
)

gas_axis_dt <- data.table(
  y = gas_breaks * 5,
  lab = gas_breaks
)

x_left <- min(energy_dt$Date, na.rm = TRUE)
y_scale <- compute_y_scale(energy_dt$Value_plot)
x_range <- as.numeric(diff(range(energy_dt$Date, na.rm = TRUE)))
y_upper <- y_scale$y_upper
y_annotation <- y_upper

x_main_lab <- x_left - round(0.2 * x_range)   # $/bbl and $/mt
x_gas_tick1 <- x_left - round(0.012 * x_range)  # gas tick start
x_gas_tick2 <- x_left - round(0.02 * x_range)  # gas tick end
x_gas_lab   <- x_left - round(0.00 * x_range)  # gas tick labels and $/mmbtu

gas_axis_dt[, `:=`(
  x_tick_start = x_gas_tick1,
  x_tick_end   = x_gas_tick2,
  x_lab        = x_gas_lab
)]

gas_axis_dt <- gas_axis_dt[y!=0]

energy_plot <- ggplot(
  energy_dt,
  aes(x = Date, y = Value_plot, color = Series)
) +
  geom_line(linewidth = 1) +
  geom_text(
    data = energy_labels,
    aes(x = x_lab, y = y_lab, label = Series_Label),
    hjust = 0,
    size = 8,
    show.legend = FALSE
  ) +
  geom_segment(
    data = gas_axis_dt,
    aes(x = x_tick_start, xend = x_tick_end, y = y, yend = y),
    inherit.aes = FALSE,
    linewidth = 1.2,
    color = "black"
  ) +
  geom_text(
    data = gas_axis_dt,
    aes(x = x_lab, y = y, label = lab),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = .5,
    size = 8,
    color = "black",
    family = "mono"
  ) +
  annotate(
    "text",
    x = x_main_lab,
    y = y_annotation,
    label = "Crude Oil ($/bbl)\nCoal ($/mt)",
    hjust = 0,
    vjust = 1,
    size = 6
  ) +
  annotate(
    "text",
    x = x_gas_lab,
    y = y_annotation,
    label = "Natural Gas ($/mmbtu)",
    hjust = 0,
    vjust = 1,
    size = 6
  ) +
  scale_color_manual(values = pal[c(2, 7, 9)]) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = " ",
    caption = plot_caption
  ) +
  scale_y_continuous(
    labels = function(x) sprintf("%4.0f", x),
    breaks = y_scale$breaks,
    limits = c(0, y_upper),
    expand = c(0, 0)
  ) +
  scale_x_date(
    expand = expansion(mult = double_y_expand)
  ) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = 28) +
  base_plot_theme +
  theme(
    axis.text = element_text(family = "mono")
  )



energy_plot

save_plot(energy_plot, output_energy)
