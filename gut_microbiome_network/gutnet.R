library(igraph)
library(ggraph)
library(tidyverse)

# Define three sets of terms
set1 <- c("Fiber", "Fermented Foods(?)", "polyphenols (berries, tea, cocoa)", "Prebiotics (inulin, fructo-oligosaccharides)", "Complex carbohydrates (tubers, bamboo shoots, fruits, mushrooms)", 
          "Animal Protein and Fat", "Added Sugars and Sweeteners", "Processing additives and refined ingredients",
          "Food additives (emulsifiers, sweeteners)")
set2 <- c("Bacteroides", "Prevotella", "Faecalibacterium", "Ruminococcus", "Blautia", "Clostridium","Viruses and Phages", 
          "Archaea-methanogens","Saccharomyces", "Malassezia", "Candida")
set3 <- c("Digestion","Vitamin Synthesis (vitamin K)", "Immune System Development",
           "Protection against Pathogens (competition)", "Obesity and Type 2 Diabetes",
            "Metabolic Syndrome", "high blood pressure", "high blood sugar","abnormal cholesterol levels",
             "Inflammatory Bowel Disease", "Crohn's", "ulcerative colitis", "Irritable Bowel Syndrome", 
              "Celiac Disease", "Depression and Anxiety (?)", "Autism Spectrum Disorder (?)",
                "Parkinson's and Alzheimer's Diseases (?)", "Colorectal Cancer", "atherosclerosis", "hypertension",
                "heart failure", "Allergies", "Asthma", "chronic kidney disease")



# Create directed edges from Set1 to Set2
edges1 <- expand.grid(set1, set2) %>% rename(from = Var1, to = Var2)
# Create directed edges from Set2 to Set3
edges2 <- expand.grid(set2, set3) %>% rename(from = Var1, to = Var2)

# Combine edges
edges <- rbind(edges1, edges2)

# Create a directed graph object
graph <- graph_from_data_frame(edges, directed = TRUE)

# Assign levels to nodes
node_levels <- data.frame(
  name = c(set1, set2, set3),
  level = c(rep(1, length(set1)), rep(2, length(set2)), rep(3, length(set3)))
)

# Plot the directed network using a tree layout
ggraph(graph, layout = "tree", circular = FALSE) + coord_flip() +
  geom_edge_link(arrow = arrow(type = "closed", length = unit(4, "mm")), alpha = 0.5) +
  geom_node_point(size = 5, color = "steelblue") +
  geom_node_text(aes(label = name), vjust = 1.5, size = 3) +
  theme_void()