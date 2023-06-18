library(dplyr) # Import necessary library

# Define a function to plot all nodes and connections, between all phases, in the Monash dataset
# Plot for both phase 2 and phase 3
make.ona.plot <- function(set, 
                          plot_name, 
                          flip_x = FALSE
                          ) {
  if(flip_x == TRUE){
    set$points$MR1 <- (-1)*set$points$MR1
    set$rotation$nodes$MR1 <- (-1)*set$rotation$nodes$MR1
  }
  
  # Period 2
  color <- c("red", "blue")
  low_perf_p2 <- plot(set, title = "Phase 2: ONA plot for low performance") %>%
    edges(
      weights = set$line.weights[set$line.weights$comparison == "low" & set$points$phase == "phase 2",],
      edge_size_multiplier = 0.5,
      edge_color = c("red")) %>%
    nodes(
      node_size_multiplier = 0.5,
      self_connection_color = c("red")) %>%
    units(
      points=set$points[set$points$comparison == "low" & set$points$phase == "phase 2",],
      points_color = c("red"),
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) 
  print(low_perf_p2)
  
  high_perf_p2 <- plot(set, title = "Phase 2: ONA plot for high performance") %>%
    edges(
      weights = set$line.weights[set$line.weights$comparison == "high" & set$points$phase == "phase 2",],
      edge_size_multiplier = 0.5,
      edge_color = c("blue")) %>%
    nodes(
      node_size_multiplier = 0.5,
      self_connection_color = c("blue")) %>%
    units(
      points=set$points[set$points$comparison == "high" & set$points$phase == "phase 2",],
      points_color = c("blue"),
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE)
  print(high_perf_p2)
  
  sub_p2 <- plot(set, title = "Phase 2 Difference: low performance (red) vs high performance (blue)") %>%
    units(
      points = set$points[set$points$comparison == "low" & set$points$phase == "phase 2",], 
      points_color = "red",
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) %>%
    units(
      points = set$points[set$points$comparison == "high" & set$points$phase == "phase 2",], 
      points_color = "blue",
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) %>%
    edges(
      weights = (colMeans(set$line.weights[set$line.weights$comparison == "low" & set$points$phase == "phase 2",]) - colMeans(set$line.weights[set$line.weights$comparison == "high" & set$points$phase == "phase 2",])),
      edge_size_multiplier = 2,
      edge_arrow_saturation_multiplier = 2,
      # node_position_multiplier = node_position_multiplier,
      edge_color = c("red","blue")) %>%
    nodes(
      node_size_multiplier = 1,
      # node_position_multiplier = node_position_multiplier,
      self_connection_color = c("red","blue"))
  print(sub_p2)
  
  # Period 3
  low_perf_p3 <- plot(set, title = "Phase 3: ONA plot for low performance") %>%
    edges(
      weights = set$line.weights[set$line.weights$comparison == "low" & set$points$phase == "phase 3",],
      edge_size_multiplier = 0.5,
      edge_color = c("red")) %>%
    nodes(
      node_size_multiplier = 0.5,
      self_connection_color = c("red")) %>%
    units(
      points=set$points[set$points$comparison == "low" & set$points$phase == "phase 3",],
      points_color = c("red"),
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) 
  print(low_perf_p3)
  
  high_perf_p3 <- plot(set, title = "Phase 3: ONA plot for high performance") %>%
    edges(
      weights = set$line.weights[set$line.weights$comparison == "high" & set$points$phase == "phase 3",],
      edge_size_multiplier = 0.5,
      edge_color = c("blue")) %>%
    nodes(
      node_size_multiplier = 0.5,
      self_connection_color = c("blue")) %>%
    units(
      points=set$points[set$points$comparison == "high" & set$points$phase == "phase 3",],
      points_color = c("blue"),
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE)
  print(high_perf_p3)
  
  sub_p3 <- plot(set, title = "Phase 3 Difference: low performance (red) vs high performance (blue)") %>%
    units(
      points = set$points[set$points$comparison == "low" & set$points$phase == "phase 3",], 
      points_color = "red",
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) %>%
    units(
      points = set$points[set$points$comparison == "high" & set$points$phase == "phase 3",], 
      points_color = "blue",
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) %>%
    edges(
      weights = (colMeans(set$line.weights[set$line.weights$comparison == "low" & set$points$phase == "phase 3",]) - colMeans(set$line.weights[set$line.weights$comparison == "high" & set$points$phase == "phase 3",])),
      edge_size_multiplier = 2,
      edge_arrow_saturation_multiplier = 2,
      # node_position_multiplier = node_position_multiplier,
      edge_color = c("red","blue")) %>%
    nodes(
      node_size_multiplier = 1,
      # node_position_multiplier = node_position_multiplier,
      self_connection_color = c("red","blue"))
  print(sub_p3)
  
  
}
