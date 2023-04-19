library(dplyr) # Import necessary library

# Define a function to plot all nodes and connections, between all phases, in the Monash dataset
make.ona.plot <- function(set, 
                          plot_name
                          ) {
  monash_plot <- plot(set, title = plot_name) %>%
    units(
      points = set$points,
      points_color = "red",
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) %>%
    edges(
      weights = set$line.weights,
      edge_color = c("red")) %>%
    nodes(
      self_connection_color = c("red"))
  print(monash_plot)
}