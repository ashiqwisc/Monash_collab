library(dplyr) # Import necessary library

# Define a function to plot all nodes and connections, between all phases, in the Monash dataset
make.ona.plot <- function(set, 
                          plot_name, 
                          flip_x = FALSE
                          ) {
  if(flip_x == TRUE){
    set$points$MR1 <- (-1)*set$points$MR1
    set$rotation$nodes$MR1 <- (-1)*set$rotation$nodes$MR1
  }
  color <- c("red", "blue")
  low_perf <- plot(set, title = "ONA plot for low performance") %>%
    edges(
      weights = set$line.weights$comparison$low,
      edge_size_multiplier = 0.5,
      edge_color = c("red")) %>%
    nodes(
      node_size_multiplier = 0.5,
      self_connection_color = c("red")) %>%
    units(
      points=set$points[set$points$comparison == "low",],
      points_color = c("red"),
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) 
  print(low_perf)
  
  high_perf <- plot(set, title = "ONA plot for high performance") %>%
    edges(
      weights = set$line.weights$comparison$high,
      edge_size_multiplier = 0.5,
      edge_color = c("blue")) %>%
    nodes(
      node_size_multiplier = 0.5,
      self_connection_color = c("blue")) %>%
    units(
      points=set$points[set$points$comparison == "high",],
      points_color = c("blue"),
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE)
  print(high_perf)
  
  # FirstGame and SecondGame subtracted plot
  sub <- plot(set, title = "Difference: low performance (red) vs high performance (blue)") %>%
    units(
      points = set$points$comparison$low, 
      points_color = "red",
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) %>%
    units(
      points = set$points$comparison$high, 
      points_color = "blue",
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) %>%
    edges(
      weights = (colMeans(set$line.weights$comparison$low) - colMeans(set$line.weights$comparison$high)),
      edge_size_multiplier = 2,
      edge_arrow_saturation_multiplier = 2,
      # node_position_multiplier = node_position_multiplier,
      edge_color = c("red","blue")) %>%
    nodes(
      node_size_multiplier = 2,
      # node_position_multiplier = node_position_multiplier,
      self_connection_color = c("red","blue"))
  print(sub)
  print(t.test(set$points$comparison$low$MR1, set$points$comparison$high$MR1))
}