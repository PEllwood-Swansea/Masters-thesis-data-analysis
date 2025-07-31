# Load libraries (install packages if not previously installed)

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(randomcoloR)

# Load the data
raw <- read.csv("combined_reshape_finished.csv")
raw <- raw %>% filter(Condition == "Control")


# Clean coculture column- there were issues with the data so i need to clean the column
raw$coculture <- iconv(raw$coculture, from = "", to = "ASCII", sub = "")
raw$coculture <- gsub("–|—", "-", raw$coculture)

# Filter valid coculture format - gets rid of invalid formats
raw <- raw %>%
  filter(!is.na(coculture)) %>%
  filter(grepl("^S\\d+-S\\d+$", coculture)) %>%
  filter(Time %% 6 == 0)  # Only keep 6-hour intervals

# Define the starter strain - to identify monocultures
raw <- raw %>%
  mutate(starter = str_extract(coculture, "^S\\d+")) %>%
  mutate(is_mono = coculture == paste0(starter, "-", starter))

# Summarise mean and SE
summary_data <- raw %>%
  group_by(starter, coculture, Time, is_mono) %>%
  summarise(
    mean_area = mean(area, na.rm = TRUE),
    se_area = sd(area, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Gets the order right - s11 and s12 were between s1 &3
starter_order <- c(paste0("S", 1:10), "S11", "S12")
summary_data$starter <- factor(summary_data$starter, levels = starter_order)

# Create a custom color vector- we want monocultures to be black and cocultures as random colours
all_cocultures <- unique(summary_data$coculture)
mono_cultures <- summary_data %>% filter(is_mono) %>% pull(coculture) %>% unique()

color_vector <- setNames(
  rep(scales::hue_pal()(length(all_cocultures)), length.out = length(all_cocultures)),
  all_cocultures
)
color_vector[mono_cultures] <- "black"  # Set monocultures to black



# Get all cocultures
all_cocultures <- unique(summary_data$coculture)
mono_cultures <- summary_data %>% filter(is_mono) %>% pull(coculture) %>% unique()
co_cultures <- setdiff(all_cocultures, mono_cultures)

# Generate distinct colors for co-cultures - randomising colours
set.seed(420)  # for reproducibility
co_colors <- distinctColorPalette(length(co_cultures))


# Assign colors: black for monocultures, distinct for cocultures
color_vector <- setNames(rep("black", length(all_cocultures)), all_cocultures)
color_vector[co_cultures] <- co_colors


# Plot
ggplot(summary_data, aes(x = Time, y = mean_area, group = coculture, color = coculture)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_area - se_area, ymax = mean_area + se_area), width = 0.5) +
  facet_wrap(~ starter, scales = "free_y") +
  scale_color_manual(values = color_vector) +
  labs(
    title = "Coculture growth in Control media",
    x = "Time",
    y = "Colony area (mm²)",
    color = "Co-culture"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(1.2, "cm")
  )
