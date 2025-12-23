# install.packages(c("ggplot2", "dplyr", "tidyr"))  # if needed
library(ggplot2)
library(dplyr)
library(tidyr)

## ---- Data ----
categories <- c(
  "Other Races, College, Young",
  "Black, College, Young",
  "Other Races, College, Middle-Age",
  "Black, College, Middle-Age",
  "White, College, Middle-Age"
)

values_2010 <- c(909.2, 769.6, 75.7, 133.4, 48.6)
values_2019 <- c(NA,    NA,    293.1, 226.8, 116.9)

df <- tibble(
  category   = categories,
  value2010  = values_2010,
  value2019  = values_2019
) %>%
  # y_base is row position (top to bottom)
  mutate(y_base = rev(seq_along(category)))

## ---- Long format for year-specific arrows ----
df_long <- df %>%
  pivot_longer(
    cols = starts_with("value"),
    names_to = "year",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  mutate(
    year = ifelse(year == "value2010", "2010", "2019")
  ) %>%
  group_by(category) %>%
  mutate(has_both = n() == 2) %>%
  ungroup()

# Vertical offsets so 2010 & 2019 arrows and labels don't overlap
arrow_offset  <- 0.15
label_offset  <- 0.1

df_long <- df_long %>%
  mutate(
    y = y_base + case_when(
      has_both & year == "2019" ~  arrow_offset,
      has_both & year == "2010" ~ -arrow_offset,
      TRUE                      ~  0
    ),
    x_start = 0.25,
    x_end   = 0.75,
    x_mid   = (x_start + x_end) / 2
  )

## ---- Line width scaling (only line thickness varies, not arrowhead size) ----
all_vals <- df_long$value
vmin <- min(all_vals)
vmax <- max(all_vals)

scale_lw <- function(v) {
  1.5 + 4.5 * (v - vmin) / (vmax - vmin)   # adjust range if you like
}

df_long <- df_long %>%
  mutate(linewidth = scale_lw(value))

## ---- Left/right labels (Male / Female) ----
male_labels <- df %>%
  transmute(
    category,
    x = 0.25,
    y = y_base,
    label = paste0("Male:\n", category)
  )

female_labels <- df %>%
  transmute(
    category,
    x = 0.75,
    y = y_base,
    label = paste0("Female:\n", category)
  )

## ---- Plot ----
p <- ggplot() +
  # Arrows (double-headed), color by year, thickness by value
  geom_segment(
    data = df_long,
    aes(x = x_start, xend = x_end,
        y = y,       yend = y,
        color = year,
        size  = linewidth),
    arrow = arrow(ends = "both",
                  length = unit(0.05, "cm"), # arrowhead size (fixed)
                  type   = "closed")
  ) +
  # Numeric labels above each arrow
  geom_text(
    data = df_long,
    aes(x = x_mid, y = y + label_offset,
        label = sprintf("%s: %.1f", year, value),
        color = year),
    vjust = 0.1
  ) +
  # Male labels on the left
  geom_text(
    data = male_labels,
    aes(x = x, y = y, label = label),
    hjust = 1, vjust = 0.5
  ) +
  # Female labels on the right
  geom_text(
    data = female_labels,
    aes(x = x, y = y, label = label),
    hjust = 0, vjust = 0.5
  ) +
  scale_color_manual(values = c("2010" = "orange", "2019" = "blue")) +
  scale_size_identity() +                        # use linewidth directly
  coord_cartesian(xlim = c(0, 1)) +
  theme_void() +
  theme(legend.position = "none")

p
