#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

input_file <- "data/processed/pink_sheet_combined.csv"
output_dir <- "docs/assets/plots"
output_indices <- file.path(output_dir, "pink_sheet_indices.png")
output_prices <- file.path(output_dir, "pink_sheet_prices.png")

required_indices <- c("Energy", "Grains", "Oils_Meals", "Fertilizers")
required_prices <- c("Urea", "Rice_Thai_5", "Maize", "Wheat_US_SRW")

fail_gracefully <- function(message_text, status = 0L) {
  message(sprintf("Plot generation skipped: %s", message_text))
  quit(save = "no", status = status)
}

if (!file.exists(input_file)) {
  fail_gracefully(sprintf("processed dataset not found at %s", input_file))
}


# Indices

plot_dt <- tryCatch(
  fread(input_file),
  error = function(e) {
    fail_gracefully(sprintf("unable to read %s (%s)", input_file, conditionMessage(e)))
  }
)

file_update <- unique(plot_dt$Update)

required_columns <- c("Date", "Series", "Value")
missing_columns <- setdiff(required_columns, names(plot_dt))
if (length(missing_columns)) {
  fail_gracefully(sprintf(
    "processed dataset is missing required columns: %s",
    paste(missing_columns, collapse = ", ")
  ))
}

plot_dt <- plot_dt[Series %in% required_indices]
if (!nrow(plot_dt)) {
  fail_gracefully("no rows found for the requested series")
}

plot_dt[, Date := as.Date(Date)]
plot_dt[, Value := suppressWarnings(as.numeric(Value))]
plot_dt <- plot_dt[!is.na(Date) & !is.na(Value)]

if (!nrow(plot_dt)) {
  fail_gracefully("all candidate rows were missing Date or Value after parsing")
}

plot_dt[, Series := factor(Series, levels = required_indices)]

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

plot_object <- ggplot(plot_dt[Date>="2000-01-01"], aes(x = Date, y = Value, color = Series)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(option = "H", begin = 0.1, end = 0.9) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Index",
    color = "Series",
    caption = paste0(
      "Source: World Bank 'Pink Sheet' updated on ",
      format(file_update, "%d %B %Y"),
      "\navailable at https://www.worldbank.org/en/research/commodity-markets"
    )
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = c(0, 0)
  ) +
  theme_classic(base_size = 28) +
  theme(
    plot.title.position = "plot",
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.width = grid::unit(1.5, "lines"),
    legend.box.spacing = unit(0, "pt"),
    legend.margin = margin(t = 0),
    panel.grid.major.y = element_line(linewidth = .6, linetype = 3, color = "gray"),
    plot.caption = element_text(hjust = 0, color="dimgray")
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave(
  filename = output_indices,
  plot = plot_object,
  width = 16,
  height = 9,
  dpi = 300,
  bg = "white"
)

message(sprintf("Saved plot to %s", output_indices))

# Prices

plot_dt <- tryCatch(
  fread(input_file),
  error = function(e) {
    fail_gracefully(sprintf("unable to read %s (%s)", input_file, conditionMessage(e)))
  }
)

file_update <- unique(plot_dt$Update)

required_columns <- c("Date", "Series", "Value")
missing_columns <- setdiff(required_columns, names(plot_dt))
if (length(missing_columns)) {
  fail_gracefully(sprintf(
    "processed dataset is missing required columns: %s",
    paste(missing_columns, collapse = ", ")
  ))
}

plot_dt <- plot_dt[Series %in% required_prices]
if (!nrow(plot_dt)) {
  fail_gracefully("no rows found for the requested series")
}

plot_dt[, Date := as.Date(Date)]
plot_dt[, Value := suppressWarnings(as.numeric(Value))]
plot_dt <- plot_dt[!is.na(Date) & !is.na(Value)]

if (!nrow(plot_dt)) {
  fail_gracefully("all candidate rows were missing Date or Value after parsing")
}

plot_dt[, Series := factor(Series, levels = required_prices)]

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

plot_object <- ggplot(plot_dt[Date>="2000-01-01"], aes(x = Date, y = Value, color = Series)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(option = "H", begin = 0.1, end = 0.9) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "$/mt",
    color = "Series",
    caption = paste0(
      "Source: World Bank 'Pink Sheet' updated on ",
      format(file_update, "%d %B %Y"),
      "\navailable at https://www.worldbank.org/en/research/commodity-markets"
    )
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = c(0, 0)
  ) +
  theme_classic(base_size = 28) +
  theme(
    plot.title.position = "plot",
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.width = grid::unit(1.5, "lines"),
    legend.box.spacing = unit(0, "pt"),
    legend.margin = margin(t = 0),
    panel.grid.major.y = element_line(linewidth = .6, linetype = 3, color = "gray"),
    plot.caption = element_text(hjust = 0, color="dimgray")
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave(
  filename = output_prices,
  plot = plot_object,
  width = 16,
  height = 9,
  dpi = 300,
  bg = "white"
)

message(sprintf("Saved plot to %s", output_prices))
