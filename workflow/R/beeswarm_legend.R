library(ggplot2)
library(cowplot)

# Create the plot with the legend
p <- ggplot() +
  geom_point(aes(x = 1, y = 1, color = "Bootstrapped model predictions"), size = 5) + 
  geom_line(aes(x = c(1, 2), y = c(1, 1), linetype = "Impact trend"), size = 1) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        legend.margin = margin(6, 6, 6, 6),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.box.spacing = unit(0, "pt")) +
  scale_color_manual(values = c("Bootstrapped model predictions" = "lightgrey")) +
  scale_linetype_manual(values = c("Impact trend" = "dashed")) +
  guides(
    color = guide_legend(order = 1, override.aes = list(linetype = 0)), 
    linetype = guide_legend(order = 2, override.aes = list(color = "black"))
  )

# Extract just the legend
legend <- get_legend(p)

# Create a new plot with just the legend
legend_plot <- plot_grid(legend)

# Save the legend as a PDF
ggsave("final_results/shap_legend.pdf", legend_plot, width = 6, height = 1)