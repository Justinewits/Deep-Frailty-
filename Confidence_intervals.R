

library(dplyr)
## DeeFrailty Model
#IBS
files <- list(
  Zimbabwe     = "fold_ibsFULLdataZimbabwesresultsNEW.csv",
  Zambia       = "fold_ibsFULLdataZambiasresultsNEW.csv",
  `South Africa` = "fold_ibsFULLdataSouthAfricasresultsNEW.csv",
  Malawi       = "fold_ibsFULLdataMalawisresultsNEW.csv"
)

results <- data.frame(
  Country   = character(),
  n         = integer(),
  Mean      = numeric(),
  SD        = numeric(),
  CI_Lower  = numeric(),
  CI_Upper  = numeric(),
  stringsAsFactors = FALSE
)


for(country in names(files)){ 
  df <- read.csv(files[[country]])
  
  x <- df$fold_ibs
  tt <- t.test(x, conf.level = 0.95)
  results <- bind_rows(results,
                       data.frame(
                         Country  = country,
                         n        = length(x),
                         Mean     = mean(x),
                         SD       = sd(x),
                         CI_Lower = tt$conf.int[1],
                         CI_Upper = tt$conf.int[2],
                         stringsAsFactors = FALSE
                       )
  )
}
print(results)


#CI
files <- list(
  Zimbabwe     = "fold_c_indicesFULLdataZimbabwesresultsNEW2.csv",
  Zambia       = "fold_c_indicesFULLdataZambiasresultsNEW2.csv",
  `South Africa` = "fold_c_indicesFULLdataSouthAfricasresultsNEW2.csv",
  Malawi       = "fold_c_indicesFULLdataMalawisresultsNEW2.csv"
)

results <- data.frame(
  Country   = character(),
  n         = integer(),
  Mean      = numeric(),
  SD        = numeric(),
  CI_Lower  = numeric(),
  CI_Upper  = numeric(),
  stringsAsFactors = FALSE
)


for(country in names(files)){ 
  df <- read.csv(files[[country]])
  
  x <- df$fold_c_indices
  tt <- t.test(x, conf.level = 0.95)
  results <- bind_rows(results,
                       data.frame(
                         Country  = country,
                         n        = length(x),
                         Mean     = mean(x),
                         SD       = sd(x),
                         CI_Lower = tt$conf.int[1],
                         CI_Upper = tt$conf.int[2],
                         stringsAsFactors = FALSE
                       )
  )
}
print(results)


## DeepHit Model

# C-index
files <- list(
  Zimbabwe     = "Zimbabwe_Cindex_BrierScore_DeepHit.csv",
  Zambia       = "Zambia_Cindex_BrierScore_DeepHit.csv",
  `South Africa` = "South_Africa_Cindex_BrierScore_DeepHit.csv",
  Malawi       = "Malawi_Cindex_BrierScore_DeepHit.csv"
)

results <- data.frame(
  Country   = character(),
  n         = integer(),
  Mean      = numeric(),
  SD        = numeric(),
  CI_Lower  = numeric(),
  CI_Upper  = numeric(),
  stringsAsFactors = FALSE
)


for(country in names(files)){ 
  df <- read.csv(files[[country]])
  
  x <- df$C.index
  tt <- t.test(x, conf.level = 0.95)
  results <- bind_rows(results,
                       data.frame(
                         Country  = country,
                         n        = length(x),
                         Mean     = mean(x),
                         SD       = sd(x),
                         CI_Lower = tt$conf.int[1],
                         CI_Upper = tt$conf.int[2],
                         stringsAsFactors = FALSE
                       )
  )
}
print(results)

## IBS
files <- list(
  Zimbabwe     = "Zimbabwe_Cindex_BrierScore_DeepHit.csv",
  Zambia       = "Zambia_Cindex_BrierScore_DeepHit.csv",
  `South Africa` = "South_Africa_Cindex_BrierScore_DeepHit.csv",
  Malawi       = "Malawi_Cindex_BrierScore_DeepHit.csv"
)

results <- data.frame(
  Country   = character(),
  n         = integer(),
  Mean      = numeric(),
  SD        = numeric(),
  CI_Lower  = numeric(),
  CI_Upper  = numeric(),
  stringsAsFactors = FALSE
)


for(country in names(files)){ 
  df <- read.csv(files[[country]])
  
  x <- df$Brier.score
  tt <- t.test(x, conf.level = 0.95)
  results <- bind_rows(results,
                       data.frame(
                         Country  = country,
                         n        = length(x),
                         Mean     = mean(x),
                         SD       = sd(x),
                         CI_Lower = tt$conf.int[1],
                         CI_Upper = tt$conf.int[2],
                         stringsAsFactors = FALSE
                       )
  )
}
print(results)




## RSF Model

## C-index
files <- list(
  Zimbabwe     = "Zimbabwe_Cindex_BrierScoreRSF.csv",
  Zambia       = "Zambia_Cindex_BrierScoreRSF.csv",
  `South Africa` = "South_Africa_Cindex_BrierScoreRSF.csv",
  Malawi       = "Malawi_Cindex_BrierScoreRSF.csv"
)

results <- data.frame(
  Country   = character(),
  n         = integer(),
  Mean      = numeric(),
  SD        = numeric(),
  CI_Lower  = numeric(),
  CI_Upper  = numeric(),
  stringsAsFactors = FALSE
)


for(country in names(files)){ 
  df <- read.csv(files[[country]])
  
  x <- df$C.index
  tt <- t.test(x, conf.level = 0.95)
  results <- bind_rows(results,
                       data.frame(
                         Country  = country,
                         n        = length(x),
                         Mean     = mean(x),
                         SD       = sd(x),
                         CI_Lower = tt$conf.int[1],
                         CI_Upper = tt$conf.int[2],
                         stringsAsFactors = FALSE
                       )
  )
}
print(results)

## IBS

files <- list(
  Zimbabwe     = "Zimbabwe_Cindex_BrierScoreRSF.csv",
  Zambia       = "Zambia_Cindex_BrierScoreRSF.csv",
  `South Africa` = "South_Africa_Cindex_BrierScoreRSF.csv",
  Malawi       = "Malawi_Cindex_BrierScoreRSF.csv"
)

results <- data.frame(
  Country   = character(),
  n         = integer(),
  Mean      = numeric(),
  SD        = numeric(),
  CI_Lower  = numeric(),
  CI_Upper  = numeric(),
  stringsAsFactors = FALSE
)


for(country in names(files)){ 
  df <- read.csv(files[[country]])
  
  x <- df$Brier.score
  tt <- t.test(x, conf.level = 0.95)
  results <- bind_rows(results,
                       data.frame(
                         Country  = country,
                         n        = length(x),
                         Mean     = mean(x),
                         SD       = sd(x),
                         CI_Lower = tt$conf.int[1],
                         CI_Upper = tt$conf.int[2],
                         stringsAsFactors = FALSE
                       )
  )
}
print(results)



## Plot 
read.csv("Zimbabwe_Cindex_BrierScore_DeepHit.csv")
read.csv("Zambia_Cindex_BrierScore_DeepHit.csv")
read.csv("South_Africa_Cindex_BrierScore_DeepHit.csv")
read.csv("Malawi_Cindex_BrierScore_DeepHit.csv")
files <- list(
  "Zimbabwe_Cindex_BrierScore_DeepHit.csv" = "Brier.score.at.time.30.00",
  "Zambia_Cindex_BrierScore_DeepHit.csv" = "Brier.score.at.time.30.00",
  "South_Africa_Cindex_BrierScore_DeepHit.csv" = "Brier.score.at.time.29.98",
  "Malawi_Cindex_BrierScore_DeepHit.csv" = "Brier.score.at.time.30.00"
)

for (file in names(files)) {
  df <- read.csv(file)
  colnames(df)[colnames(df) == files[[file]]] <- "Brier.score"
  write.csv(df, file, row.names = FALSE)
}

library(tidyverse)

files <- list(
  RSF = list(
    Zimbabwe = "Zimbabwe_Cindex_BrierScoreRSF.csv",
    Zambia = "Zambia_Cindex_BrierScoreRSF.csv",
    `South Africa` = "South_Africa_Cindex_BrierScoreRSF.csv",
    Malawi = "Malawi_Cindex_BrierScoreRSF.csv"
  ),
  DeepHit = list(
    Zimbabwe = "Zimbabwe_Cindex_BrierScore_DeepHit.csv",
    Zambia = "Zambia_Cindex_BrierScore_DeepHit.csv",
    `South Africa` = "South_Africa_Cindex_BrierScore_DeepHit.csv",
    Malawi = "Malawi_Cindex_BrierScore_DeepHit.csv"
  ),
  DeepFrailty_IBS = list(
    Zimbabwe = "fold_ibsFULLdataZimbabwesresultsNEW.csv",
    Zambia = "fold_ibsFULLdataZambiasresultsNEW.csv",
    `South Africa` = "fold_ibsFULLdataSouthAfricasresultsNEW.csv",
    Malawi = "fold_ibsFULLdataMalawisresultsNEW.csv"
  ),
  DeepFrailty_Cindex = list(
    Zimbabwe = "fold_c_indicesFULLdataZimbabwesresultsNEW2.csv",
    Zambia = "fold_c_indicesFULLdataZambiasresultsNEW2.csv",
    `South Africa` = "fold_c_indicesFULLdataSouthAfricasresultsNEW2.csv",
    Malawi = "fold_c_indicesFULLdataMalawisresultsNEW2.csv"
  )
)

get_metric_df <- function(file_list, metric_col, metric_name, model_name) {
  map_dfr(names(file_list), function(country) {
    df <- tryCatch(read.csv(file_list[[country]]), error = function(e) return(NULL))
    if (!is.null(df) && metric_col %in% colnames(df)) {
      data.frame(
        Country = country,
        Model = model_name,
        Metric = df[[metric_col]],
        MetricType = metric_name
      )
    } else {
      NULL
    }
  })
}


ibs_rsf       <- get_metric_df(files$RSF, "Brier.score", "IBS", "RSF")
ibs_deephit   <- get_metric_df(files$DeepHit, "Brier.score", "IBS", "DeepHit")
ibs_deepfrail <- get_metric_df(files$DeepFrailty_IBS, "fold_ibs", "IBS", "DeepFrailty")

cidx_rsf       <- get_metric_df(files$RSF, "C.index", "C-index", "RSF")
cidx_deephit   <- get_metric_df(files$DeepHit, "C.index", "C-index", "DeepHit")
cidx_deepfrail <- get_metric_df(files$DeepFrailty_Cindex, "fold_c_indices", "C-index", "DeepFrailty")

plot_data <- bind_rows(
  ibs_rsf, ibs_deephit, ibs_deepfrail,
  cidx_rsf, cidx_deephit, cidx_deepfrail
)


plot_data <- plot_data %>%
  mutate(
    Country = factor(Country, levels = c("Zimbabwe", "Zambia", "South Africa", "Malawi")),
    Model = factor(Model, levels = c("RSF", "DeepHit", "DeepFrailty"))
  )

ggplot(plot_data$MetricType["IBS"], aes(x = Model, y = Metric, fill = Model)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.9) +
  facet_grid(MetricType ~ Country, scales = "free_y") +
  labs(
    title = "Model Comparison Across Countries",
    y = "Score",
    x = "Model"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )




### Split
p_ibs <- ggplot(
  filter(plot_data, MetricType == "IBS"),
  aes(x = Model, y = Metric, fill = Model)
) +
  geom_boxplot(outlier.shape = 21, alpha = 0.9) +
  facet_wrap(~Country, scales = "free_y") +
  labs(
    title = "Integrated Brier Scores (IBS) Comparison Across Models and Countries",
    y = "Integrated Brier Score",
    x = "Model"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    plot.margin = margin(t = 15, r = 10, b = 10, l = 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = "Fig7.pdf",
  plot = p_ibs,
  width = 10,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"  # Ensures white background
)

## C -index 


p <- ggplot(
  filter(plot_data, MetricType == "C-index"),
  aes(x = Model, y = Metric, fill = Model)
) +
  geom_boxplot(outlier.shape = 21, alpha = 0.9) +
  facet_wrap(~Country, scales = "free_y") +
  labs(
    title = "Concordance Index (C-index) Comparison Across Models and Countries",
    y = "C-index",
    x = "Model"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    plot.margin = margin(t = 15, r = 10, b = 10, l = 10)  # extra top space
  )

ggsave(
  filename = "Fig6.pdf",
  plot = p,
  width = 10,
  height = 7,
  units = "in",
  dpi = 300,
  limitsize = FALSE,
  bg = "white"
)

### new review plot

library(ggplot2)
library(dplyr)
library(patchwork)

plot_data <- plot_data %>%
  mutate(
    Country = factor(
      Country,
      levels = c("Zimbabwe", "Zambia", "South Africa", "Malawi")
    ),
    Model = factor(
      Model,
      levels = c("RSF", "DeepHit", "DeepFrailty")
    ),
    MetricType = factor(
      MetricType,
      levels = c("C-index", "IBS")
    )
  )


p_combined <- ggplot(
  plot_data,
  aes(x = Country, y = Metric, fill = Model)
) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.65,
    outlier.shape = 21,
    alpha = 0.9
  ) +
  facet_wrap(
    ~ MetricType,
    scales = "free_y",
    ncol = 1
  ) +
  labs(
    title = "Performance Comparison Across Models and Countries",
    x = "Country",
    y = "Performance metric value",
    fill = "Model"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      size = 17,
      face = "bold",
      hjust = 0.5
    ),
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    strip.text = element_text(
      face = "bold",
      size = 14
    ),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

print(p_combined)

ggsave(
  filename = "Fig_Performance_Grouped_Boxplots_Combined.pdf",
  plot = p_combined,
  width = 11,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)


p_combined <- ggplot(
  plot_data,
  aes(x = Country, y = Metric, fill = Model)
) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.65,
    outlier.shape = 21,
    alpha = 0.9
  ) +
  facet_wrap(
    ~ MetricType,
    scales = "free_y",
    ncol = 1
  ) +
  labs(
    title = "Performance Comparison Across Models and Countries",
    x = "Country",
    y = NULL,
    fill = "Model"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

print(p_combined)

ggsave(
  filename = "Fig_Performance_Grouped_Boxplots_Combined.pdf",
  plot = p_combined,
  width = 11,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)


## Flow


library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(xml2)

consort_diagram <- grViz("
digraph consort_flow {
  graph [layout = dot, rankdir = TB, fontsize = 12]

  node [
    shape = box,
    style = filled,
    fontname = Helvetica,
    fontcolor = black,
    color = black,
    penwidth = 1.2
  ]

  A  [label = 'Assessed for eligibility\\nN = 35,353', fillcolor = '#AED6F1', style = rounded]
  B  [label = 'Excluded (n = 8,756)\\n• Missing covariate information', fillcolor = '#F5B7B1', style = rounded]
  C  [label = 'Included in final analysis\\nN = 26,597', fillcolor = '#ABEBC6', style = rounded]

  M  [label = 'Malawi\\nn = 12,671', fillcolor = '#FDFEFE', style = rounded]
  SA [label = 'South Africa\\nn = 2,760', fillcolor = '#FDFEFE', style = rounded]
  Z  [label = 'Zambia\\nn = 6,730', fillcolor = '#FDFEFE', style = rounded]
  ZW [label = 'Zimbabwe\\nn = 4,436', fillcolor = '#FDFEFE', style = rounded]

  edge [color = gray30, penwidth = 1.2]

  A -> B
  A -> C
  C -> M
  C -> SA
  C -> Z
  C -> ZW
}
")


svg_raw <- export_svg(consort_diagram)
svg_doc <- read_xml(svg_raw)


dims <- xml_attrs(xml_find_first(svg_doc, "//svg"))
bg_rect <- read_xml(sprintf('<rect width="%s" height="%s" fill="white"/>', dims["width"], dims["height"]))
xml_add_child(xml_find_first(svg_doc, "//svg"), bg_rect, .where = 0)

rsvg_png(
  charToRaw(as.character(svg_doc)),
  file = "consort_flow_diagram.png",
  width = 2480,
  height = 3508
)


