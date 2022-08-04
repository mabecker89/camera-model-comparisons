#-----------------------------------------------------------------------------------------------------------------------

# Step 3: Visualization!

#-----------------------------------------------------------------------------------------------------------------------

library(readr)
library(ggtext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)

df <- read_csv("data/processed/bootstrap_sp.csv")
df.sea <- read_csv("data/processed/bootstrap_spseas.csv")

# Full comp - not including season.

df1 <- df %>%
  tibble::remove_rownames() %>%
  select(common_name = sp_season, npairs, 10:12) %>%
  filter(npairs > 4) %>%
  mutate(common_name = paste0(common_name, "\n(n cam pairs = ", npairs, ")")) %>%
  mutate(common_name = fct_reorder(common_name, hf2_dur_as_pct_of_pc900_median, .desc = TRUE))

df1 %>%
  ggplot(aes(x = common_name,
             y = hf2_dur_as_pct_of_pc900_median,
             ymin = hf2_dur_as_pct_of_pc900_lci,
             ymax = hf2_dur_as_pct_of_pc900_uci)) +
  geom_hline(yintercept = 100, size = 0.5, linetype = 2) +
  geom_errorbar(width = 0.2, color = "steelblue") +
  geom_point(size = 4, color = "steelblue") +
  geom_text(aes(x = common_name,
                 y = hf2_dur_as_pct_of_pc900_median,
                 label = paste0(round(hf2_dur_as_pct_of_pc900_median, digits = 1), "%")),
            size = 2,
            nudge_y = 3, nudge_x = 0.25,
            color = "grey20") +
  coord_flip() +
  labs(y = "HF2 FOV Duration (as % of PC900 FOV Duration)",
       title = "Full Comparison (not including season)") +
  #theme_abmi() +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(7, 0, 0, 0), size = 9))

ggsave(filename = "plot1_fullcomp.png", dpi = 300, height = 5, width = 7)

# Seasonal comp
df2 <- df.sea %>%
  tibble::remove_rownames() %>%
  separate(sp_season, into = c("common_name", "season"), sep = "_") %>%
  select(common_name, season, npairs, 11:13) %>%
  filter(npairs > 3,
         !common_name == "Red Squirrel") %>%
  mutate(common_name = paste0(common_name, " - ", season, "\n(n cam pairs = ", npairs, ")"))

title <- 'Seasonal Comparison: <span style="color:#006400;">Summer</span> and <span style="color:#A52A2A;">Winter</span>'

df2 %>%
  ggplot(aes(x = common_name,
             y = hf2_dur_as_pct_of_pc900_median,
             ymin = hf2_dur_as_pct_of_pc900_lci,
             ymax = hf2_dur_as_pct_of_pc900_uci)) +
  geom_hline(yintercept = 100, size = 0.5, linetype = 2) +
  geom_errorbar(width = 0.2, aes(color = season)) +
  geom_point(size = 3, aes(color = season)) +
  geom_text(aes(x = common_name,
                y = hf2_dur_as_pct_of_pc900_median,
                label = paste0(round(hf2_dur_as_pct_of_pc900_median, digits = 1), "%")),
            size = 2,
            nudge_y = 4, nudge_x = 0.3,
            color = "grey20") +
  scale_color_manual(values = c("darkgreen", "brown")) +
  scale_y_continuous(breaks = seq(75, 250, 25)) +
  coord_flip(ylim = c(75, 200)) +
  labs(y = "HF2 FOV Duration (as % of PC900 FOV Duration)",
       title = title) +
  #theme_abmi() +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none",
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(margin = margin(7, 0, 0, 0), size = 9),
        plot.title = element_markdown(size = 12, margin = margin(0, 0, 7, 0)))

ggsave(filename = "plot2_seasonal.png", dpi = 300, height = 5, width = 7)

#-----------------------------------------------------------------------------------------------------------------------

# Make table.

df_new <- df %>%
  mutate(across(2:12, round, digits = 1)) %>%
  mutate(season = "all") %>%
  rename(species = sp_season)

df_everything <- df.sea %>%
  separate(sp_season, into = c("species", "season"), sep = "_") %>%
  mutate(across(3:13, round, digits = 1)) %>%
  bind_rows(df_new) %>%
  arrange(species, season)

images <- df_everything %>%
  select(1:3, contains("images"))

duration <- df_everything %>%
  select(1:3, contains("dur"))

write_csv(images, "data/results/summary_images.csv")
write_csv(duration, "data/results/summary_duration.csv")



