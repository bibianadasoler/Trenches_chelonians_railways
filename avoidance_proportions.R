library(dplyr)
library(ggplot2)
library(magick)
library(cowplot)

general_data <- read.csv(here::here("SM2_trench_trials.csv"), sep = ";")
tortoise <- image_read(here::here("tortoise.png"))
trachemys <- image_read(here::here("trachemys.png"))

data <- general_data %>%
  filter(type != "no_trench") %>%
  dplyr::select(species, individual, reach_tie, avoidance, exit_way) %>%
  mutate(species = factor(species, levels = c("Tortoise_type1", "Tortoise", "Trachemys")),
         reach_tie = case_when(reach_tie %in% c("yes") ~ "Reach", 
                               reach_tie %in% c("no") ~ "No",   
                               suppressWarnings(as.numeric(reach_tie)) > 0 ~ "Reach",
                               suppressWarnings(as.numeric(reach_tie)) == 0 ~ "No",
                               TRUE ~ as.character(reach_tie)),
         avoidance = case_when(avoidance %in% c("yes") ~ "Avoidance",
                               suppressWarnings(as.numeric(avoidance)) > 0~ "Avoidance",
                               avoidance %in% c("no") ~ "No-avoidance",
                               suppressWarnings(as.numeric(avoidance))  == 0 ~ "No-avoidance",
                               TRUE ~ as.character(avoidance)),
         exit_way = case_when(exit_way == "trench" ~ "Trench",
                              exit_way == "rail" ~ "Rails",
                              exit_way == "barrier" ~ "Barrier",
                              exit_way == "unable" ~ "No exit",
                              TRUE ~ as.character(exit_way)),
         interaction_label = case_when(reach_tie == "Reach" & avoidance == "Avoidance" ~ "Reached and avoided",
                                       reach_tie == "Reach" & avoidance == "No-avoidance" ~ "Reached but did not avoid",
                                       reach_tie == "No" ~ "Did not reach"),
         interaction_label = factor(interaction_label, levels = c("Did not reach", "Reached but did not avoid", "Reached and avoided"))) %>%
  group_by(species, reach_tie, avoidance, exit_way, interaction_label) %>%
  summarise(n = n(), .groups = "drop")


proportion_data <- data %>%
  group_by(species, reach_tie, avoidance, interaction_label) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  group_by(species) %>%
  mutate(proportion = n / sum(n) * 100,
         species = factor(species, levels = c("Trachemys", "Tortoise", "Tortoise_type1")))



(prop_plot <- ggplot(proportion_data, aes(x = species, y = proportion, fill = interaction_label)) +
    geom_bar(stat = "identity", width = 0.25) +
    labs(x = " ", y = "Percentage of the trials (%)", fill = " ") +
    scale_x_discrete(breaks = c("Tortoise_type1", "Tortoise", "Trachemys"),
                     labels = c("Type 1", "Type 2", "Type 2")
    ) +
    scale_fill_manual(values = c("antiquewhite4", "antiquewhite3", "grey85"), 
                      breaks = c("Reached and avoided", "Reached but did not avoid", "Did not reach") ) +
    theme_minimal() +
    theme(axis.title.x = element_text(size = 20, color = "black"),
          axis.text.x = element_text(size = 20,  vjust = 3.5, color = "black"), 
          axis.text.y = element_text(size = 20, color = "black"),
          legend.text = element_text(size = 18),
          legend.position = "bottom",
          panel.grid = element_blank()) +
    coord_flip()
)
# Create i# Create i# Create image annotations with cowplot
(image_annotated_plot <- ggdraw(prop_plot) +
    draw_image(here::here("tortoise.png"), x = 0.015, y = 0.72, width = 0.07, height = 0.07) +
    draw_image(here::here("tortoise.png"), x = 0.015, y = 0.47, width = 0.07, height = 0.07) +
    draw_image(here::here("trachemys.png"), x = 0.009, y = 0.21, width = 0.09, height = 0.09) )

ggsave(image_annotated_plot, filename = here::here("figures", "avoidance_panel.jpg"), 
       dpi = 300, width = 4000, height = 1800, unit = "px") 


