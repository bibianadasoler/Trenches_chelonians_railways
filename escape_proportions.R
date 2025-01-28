### dados tartarugas
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(magick)
library(cowplot)

general_data <- read.csv(here::here("SM2_trench_trials.csv"), sep = ";")
tortoise <- image_read(here::here("tortoise.png"))
trachemys <- image_read(here::here("trachemys.png"))


type_1_data <- general_data %>%
  filter(species == "Tortoise_type1")

type_2_data <- general_data %>%
  filter(species != "Tortoise_type1",
         type == "2")

no_trench_data <- general_data %>%
  filter(species != "Tortoise_type1",
         type == "no_trench")

T1_Tortoise_proportion <- type_1_data %>%
  group_by(exit_way) %>%
  summarise(frequency = n()) %>%
  mutate(proportion = frequency / sum(frequency) * 100) %>%
  ungroup() %>%
  mutate(exit_way = factor(exit_way, levels = c("trench", "rail", "barrier", "unable"),
                        labels = c("Trench", "Rails", "Barrier", "No exit")))

T2_Slider_proportion <- type_2_data %>%
  filter(species == "Trachemys") %>%
  group_by(exit_way) %>%
  summarise(frequency = n(), .groups = 'drop') %>%
  mutate(proportion = frequency / sum(frequency) * 100) %>%
  ungroup() %>%
  mutate(exit_way = factor(exit_way, levels = c("trench", "rail", "barrier", "unable"),
                        labels = c("Trench", "Tracks", "Barrier", "No exit"))) %>%
  tidyr::complete(exit_way, fill = list(frequency = 0, proportion = 0))

T2_Tortoise_proportion <- type_2_data %>%
  filter(species == "Tortoise") %>%
  group_by(exit_way) %>%
  summarise(frequency = n(), .groups = 'drop') %>%
  mutate(proportion = frequency / sum(frequency) * 100) %>%
  ungroup() %>%
  mutate(exit_way = factor(exit_way, levels = c("trench", "rail", "barrier", "unable"),
                        labels = c("Trench", "Tracks", "Barrier", "No exit")))

NoTrench_Slider_proportion <- no_trench_data %>%
  filter(species == "Trachemys") %>%
  group_by(exit_way) %>%
  summarise(frequency = n(), .groups = 'drop') %>%
  mutate(proportion = frequency / sum(frequency) * 100) %>%
  ungroup() %>%
  mutate(exit_way = factor(exit_way, levels = c("rail", "unable"),
                        labels = c("Tracks", "No exit")))

NoTrench_Tortoise_proportion <- no_trench_data %>%
  filter(species == "Tortoise") %>%
  group_by(exit_way) %>%
  summarise(frequency = n(), .groups = 'drop') %>%
  mutate(proportion = frequency / sum(frequency) * 100) %>%
  ungroup() %>%
  mutate(exit_way = factor(exit_way, levels = c("rail", "unable"),
                        labels = c("Tracks", "No exit"))) %>%
  tidyr::complete(exit_way, fill = list(frequency = 0, proportion = 0))


# plots

(T1_T <- ggplot(T1_Tortoise_proportion, aes(x = exit_way, y = proportion, fill = exit_way)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.4) +
    labs(x = "Exit way",  y = "Exit proportion in the trials") +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,100)) +
    scale_fill_manual(values = c("#08589e", "#2b8cbe", "#4eb3d3", "#FFCC99")) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 18.5),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 16.5, vjust = 4),
          plot.title = element_text(size = 19),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank()) )
T1_T_image <- ggdraw() +
  draw_image(tortoise, x = 0.27, y = 0, width = 0.5, height = 0.5) + 
  draw_label("Type 1 (n = 57)", x = 0.08, y = 0.25, hjust = 0, size = 17)
(T1_T_final <- plot_grid(T1_T_image, T1_T, ncol = 1,  rel_heights = c(0.2, 1)) )

ggsave(filename = here::here("figures", "T1_escape.jpg"),  plot = T1_T_final,  
       width = 5, height = 6, units = "in", dpi = 300)



(T2_S <- ggplot(T2_Slider_proportion, aes(x = exit_way, y = proportion, fill = exit_way)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.4) +
    labs(x = "Exit way",  y = "Exit proportion in the trials") +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,100)) +
    scale_fill_manual(values = c("#08589e", "#2b8cbe", "#4eb3d3", "#FFCC99")) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 18.5),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 16.5, vjust = 4),
          plot.title = element_text(size = 19),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank()) )
T2_S_image <- ggdraw() +
  draw_image(trachemys, x = 0.29, y = 0, width = 0.55, height = 0.55) + 
  draw_label("A) Type 2 (n = 60)", x = 0.08, y = 0.25, hjust = 0, size = 17)
(T2_S_final <- plot_grid(T2_S_image, T2_S, ncol = 1,  rel_heights = c(0.2, 1)) )

(T2_T <- ggplot(T2_Tortoise_proportion, aes(x = exit_way, y = proportion, fill = exit_way)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.4) +
    labs(x = "Exit way",  y = " ") +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,100)) +
    scale_fill_manual(values = c("#08589e", "#2b8cbe", "#4eb3d3", "#FFCC99")) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 16.5, vjust = 4),
          axis.text.y = element_blank(),
          plot.title = element_text(size = 19),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank()) )
T2_T_image <- ggdraw() +
  draw_image(tortoise, x = 0.32, y = 0, width = 0.5, height = 0.5) + 
  draw_label("B) Type 2 (n = 33)", x = 0.08, y = 0.25, hjust = 0, size = 17)
(T2_T_final <- plot_grid(T2_T_image, T2_T, ncol = 1,  rel_heights = c(0.2, 1)) )

panel_P2 <- (T2_S_final | T2_T_final)
panel_P2
ggsave(filename = here::here("figures", "T2_escape.png"),  plot = panel_P2,  
       width = 10, height = 6, units = "in", dpi = 300)

(NT_S <- ggplot(NoTrench_Slider_proportion, aes(x = saida, y = proportion, fill = saida)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.4) +
    labs(x = "Exit way",  y = "Exit proportion in the trials") +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,100)) +
    scale_fill_manual(values = c("#2b8cbe", "#FFCC99")) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text = element_text(size = 15.5),
          axis.title = element_text(size = 17),
          axis.text.x = element_text(angle = 45, v = 1.7, h = 1.5),
          plot.title = element_text(size = 17),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) )
NT_S_image <- ggdraw() +
  draw_image(trachemys, x = 0.35, y = -0.1, width = 0.55, height = 0.55) + 
  draw_label("A) No trenches - Slider (n = 15)", x = 0.08, y = 0.25, hjust = 0, size = 17)
NT_S_final <- plot_grid(NT_S_image, NT_S, ncol = 1,  rel_heights = c(0.2, 1))

(NT_T <- ggplot(NoTrench_Tortoise_proportion, aes(x = saida, y = proportion, fill = saida)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.4) +
    labs(x = "Exit way",  y = " ") +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,100)) +
    scale_fill_manual(values = c("#2b8cbe", "#FFCC99")) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text = element_text(size = 15.5),
          axis.title = element_text(size = 17),
          axis.text.x = element_text(angle = 45, v = 1.7, h = 1.5),
          axis.text.y = element_blank(),
          plot.title = element_text(size = 17),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) )
NT_T_image <- ggdraw() +
  draw_image(tortoise, x = 0.41, y = -0.1, width = 0.5, height = 0.5) + 
  draw_label("B) No trenches - Tortoise (n = 15)", x = 0.08, y = 0.25, hjust = 0, size = 17)
NT_T_final <- plot_grid(NT_T_image, NT_T, ncol = 1,  rel_heights = c(0.2, 1))

panel_NT <- (NT_S_final | NT_T_final)
panel_NT
ggsave(filename = "C:/Users/bibia/OneDrive/Doutorado/Tartarugas/Manuscrito/graphs/NT_exits.png",  plot = panel_NT,  
       width = 14, height = 7, units = "in", dpi = 300)


