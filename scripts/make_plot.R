#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

input_file <- "data/processed/pink_sheet_indices.csv"
output_dir <- "docs/assets/plots"
output_file <- file.path(output_dir, "pink_sheet_indices.png")

required_series <- c("Energy", "Grains", "Oils_Meals", "Fertilizers")

fail_gracefully <- function(message_text, status = 0L) {
  message(sprintf("Plot generation skipped: %s", message_text))
  quit(save = "no", status = status)
}

if (!file.exists(input_file)) {
  fail_gracefully(sprintf("processed dataset not found at %s", input_file))
}

plot_dt <- tryCatch(
  fread(input_file),
  error = function(e) {
    fail_gracefully(sprintf("unable to read %s (%s)", input_file, conditionMessage(e)))
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

plot_dt <- plot_dt[Series %in% required_series]
if (!nrow(plot_dt)) {
  fail_gracefully("no rows found for the requested series")
}

plot_dt[, Date := as.Date(Date)]
plot_dt[, Value := suppressWarnings(as.numeric(Value))]
plot_dt <- plot_dt[!is.na(Date) & !is.na(Value)]

if (!nrow(plot_dt)) {
  fail_gracefully("all candidate rows were missing Date or Value after parsing")
}

plot_dt[, Series := factor(Series, levels = required_series)]

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

plot_object <- ggplot(plot_dt[Date>="2000-01-01"], aes(x = Date, y = Value, color = Series)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(option = "H", begin = 0.1, end = 0.9) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Index value",
    color = "Series",
    caption = "Source: World Bank 'Pink Sheet' available at\nhttps://www.worldbank.org/en/research/commodity-markets"
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.title.position = "plot",
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = grid::unit(1.2, "lines"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.caption = element_text(hjust = 0, color="dimgray")
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave(
  filename = output_file,
  plot = plot_object,
  width = 16,
  height = 9,
  dpi = 300,
  bg = "white"
)

message(sprintf("Saved plot to %s", output_file))
