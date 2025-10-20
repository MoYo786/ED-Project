# EDproject.R
# October 2025
# ================================================

# 1. -------
# Step one you will only have to do once for your computer,

# copy EDproject_2.0.zip (or EDproject_2.0.tar.gz for Mac, Linux) to your computer 
# open R, change directory to where you have saved the package.
# in R (choose one of the two, zip for Windows, tar.gz for rest).

install.packages("EDproject_2.0.zip", repos = NULL, type = "win.binary")

install.packages("EDproject_2.0.tar.gz", repos = NULL, type = "source")

# 2. ---------
# load R package

library(EDproject)

design <- read.csv("design.csv")
design

mydata <- get.observations(design)
mydata

# 3. ---------------------------
## Obtain Observations for your Own Design (in design2.csv)

# set seed so that obtain same data set each time
set.seed(22)         

design2 <- read.csv("design2.csv")
design2

mydata <- get.observations(design2)
mydata

```{r STA2005S - EXPERIMENTAL DESIGN PROJECT 2025}
# 
# Group Members: [Your Names Here]
# Date: [Current Date]

# =============================================================================
# INITIAL SETUP AND LIBRARIES
# =============================================================================

# Load required packages
library(tidyverse)    # For data manipulation and visualization
library(emmeans)      # For estimated marginal means
library(lme4)         # For mixed effects models
library(lmerTest)     # For p-values in mixed models
library(car)          # For ANOVA tables
library(knitr)        # For nice tables
library(ggplot2)      # For plotting

# Cite packages (include these citations in your report)
# citation("tidyverse")
# citation("lme4")
# citation("emmeans")

# Set seed for reproducibility
set.seed(12345)  # USE YOUR OWN SEED NUMBER - IMPORTANT!

# =============================================================================
# EXPERIMENTAL DESIGN CREATION
# =============================================================================

# Create basic design structure
create_design <- function() {
  seasons <- 1:2
  greenhouses <- c("A", "B")
  sides <- c("n", "s")
  light_levels <- 1:4
  heat_levels <- 1:4
  varieties <- c("R", "F")
  
  # Create all combinations
  design <- expand.grid(
    season = seasons,
    greenhouse = greenhouses,
    side = sides,
    light = light_levels,
    heat = heat_levels,
    variety = varieties
  )
  
  # Add plot identifier
  design$plot <- paste0("P", sprintf("%03d", 1:nrow(design)))
  
  # Randomize the order of runs
  design <- design[sample(1:nrow(design)), ]
  
  # Reorder columns
  design <- design[, c("plot", "season", "greenhouse", "side", "light", "heat", "variety")]
  
  return(design)
}

# Generate design
my_design <- create_design()

# View the first few rows
head(my_design)

# Check design properties
cat("Number of experimental units:", nrow(my_design), "\n")
cat("Design structure:\n")
table(my_design$season, my_design$greenhouse, my_design$variety)

# Save design to CSV
write.csv(my_design, "design.csv", row.names = FALSE)

# =============================================================================
# DATA COLLECTION SIMULATION
# =============================================================================

# Load the provided function (you'll need to source the actual file)
# source("ProjectED.R")

# Simulate getting observations (replace this with the actual get.observations function)
simulate_observations <- function(design) {
  # This is a placeholder - use the actual get.observations function provided
  # Set seed again to ensure reproducibility
  set.seed(12345)
  
  # Simulate quality scores with some effects
  n <- nrow(design)
  
  # Base quality
  base_quality <- 50
  
  # Simulate effects (you can modify these to create interesting patterns)
  light_effect <- c(0, 2, 5, 3)  # Effect of different light levels
  heat_effect <- c(0, 3, 6, 2)   # Effect of different heat levels
  variety_effect <- c(0, 4)       # R = 0, F = 4
  season_effect <- c(0, 2)        # Season effect
  greenhouse_effect <- c(0, 1)    # Greenhouse effect
  
  # Calculate expected quality
  quality <- base_quality +
    light_effect[design$light] +
    heat_effect[design$heat] +
    variety_effect[as.numeric(factor(design$variety))] +
    season_effect[design$season] +
    greenhouse_effect[as.numeric(factor(design$greenhouse))]
  
  # Add interaction effects
  interaction_effect <- ifelse(design$variety == "F" & design$light == 3, 5, 0) +
    ifelse(design$variety == "R" & design$heat == 2, 3, 0)
  
  quality <- quality + interaction_effect
  
  # Add random error
  quality <- quality + rnorm(n, 0, 3)
  
  # Add some missing data (5% of observations)
  missing_indices <- sample(1:n, size = round(0.05 * n))
  quality[missing_indices] <- NA
  
  # Ensure quality is positive
  quality <- pmax(quality, 10)
  
  return(round(quality, 1))
}

# Get observations
my_design$quality <- simulate_observations(my_design)

# Check for missing data
cat("Number of missing observations:", sum(is.na(my_design$quality)), "\n")

# Save design with observations
write.csv(my_design, "design_with_observations.csv", row.names = FALSE)

# =============================================================================
# EXPLORATORY DATA ANALYSIS
# =============================================================================

# Summary statistics
summary(my_design$quality)

# Check distribution of quality scores
ggplot(my_design, aes(x = quality)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Quality Scores", x = "Quality Score", y = "Frequency") +
  theme_minimal()

# Boxplots by factor
p1 <- ggplot(my_design, aes(x = factor(light), y = quality)) +
  geom_boxplot() +
  labs(title = "Quality by Light Level", x = "Light Level", y = "Quality Score")

p2 <- ggplot(my_design, aes(x = factor(heat), y = quality)) +
  geom_boxplot() +
  labs(title = "Quality by Heat Level", x = "Heat Level", y = "Quality Score")

p3 <- ggplot(my_design, aes(x = variety, y = quality)) +
  geom_boxplot() +
  labs(title = "Quality by Variety", x = "Variety", y = "Quality Score")

# Arrange plots (you might need patchwork or gridExtra package)
# library(patchwork)
# p1 + p2 + p3

# Interaction plot
ggplot(my_design, aes(x = factor(light), y = quality, color = variety, group = variety)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(title = "Interaction: Light × Variety", x = "Light Level", y = "Mean Quality Score") +
  theme_minimal()

# =============================================================================
# STATISTICAL ANALYSIS
# =============================================================================

# Convert factors to proper factors
analysis_data <- my_design
analysis_data$light <- factor(analysis_data$light)
analysis_data$heat <- factor(analysis_data$heat)
analysis_data$variety <- factor(analysis_data$variety)
analysis_data$season <- factor(analysis_data$season)
analysis_data$greenhouse <- factor(analysis_data$greenhouse)
analysis_data$side <- factor(analysis_data$side)

# Model 1: Basic ANOVA with main effects and interactions
model1 <- lm(quality ~ light * heat * variety + season + greenhouse + side, 
             data = analysis_data)
summary(model1)
Anova(model1, type = "III")

# Model 2: Mixed effects model (if considering random effects)
# model2 <- lmer(quality ~ light * heat * variety + season + side + (1|greenhouse), 
#               data = analysis_data)
# summary(model2)

# Check model assumptions
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

# =============================================================================
# POST-HOC ANALYSIS AND MEAN COMPARISONS
# =============================================================================

# Estimated marginal means for key factors
light_means <- emmeans(model1, ~ light)
heat_means <- emmeans(model1, ~ heat)
variety_means <- emmeans(model1, ~ variety)

# Pairwise comparisons
light_pairs <- pairs(light_means)
heat_pairs <- pairs(heat_means)

# Interaction means
light_variety_means <- emmeans(model1, ~ light * variety)
heat_variety_means <- emmeans(model1, ~ heat * variety)

# Display results
cat("Light Level Means:\n")
print(light_means)

cat("\nHeat Level Means:\n")
print(heat_means)

cat("\nVariety Means:\n")
print(variety_means)

# =============================================================================
# FINDING OPTIMAL COMBINATIONS
# =============================================================================

# Get all combinations and their predicted means
all_combinations <- expand.grid(
  light = factor(1:4),
  heat = factor(1:4),
  variety = factor(c("R", "F")),
  season = factor(1),
  greenhouse = factor("A"),
  side = factor("n")
)

# Predict quality for all combinations
all_combinations$predicted_quality <- predict(model1, newdata = all_combinations)

# Find best combinations
best_combinations <- all_combinations %>%
  arrange(desc(predicted_quality)) %>%
  head(10)

cat("Top 10 combinations:\n")
print(best_combinations)

# =============================================================================
# RESULTS EXPORT
# =============================================================================

# Save important results
results_list <- list(
  design = my_design,
  model_summary = summary(model1),
  anova_results = Anova(model1, type = "III"),
  best_combinations = best_combinations,
  light_means = light_means,
  heat_means = heat_means
)

saveRDS(results_list, "project_results.rds")

# Create a summary table for the report
summary_table <- my_design %>%
  group_by(light, heat, variety) %>%
  summarise(
    n = n(),
    mean_quality = mean(quality, na.rm = TRUE),
    sd_quality = sd(quality, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_table)

# =============================================================================
# FINAL RECOMMENDATIONS
# =============================================================================

cat("\n=== FINAL RECOMMENDATIONS ===\n")
cat("Best combination found:\n")
cat("Light:", best_combinations$light[1], "\n")
cat("Heat:", best_combinations$heat[1], "\n")
cat("Variety:", as.character(best_combinations$variety[1]), "\n")
cat("Predicted Quality:", round(best_combinations$predicted_quality[1], 1), "\n")

# Check if optimal settings depend on variety
best_by_variety <- all_combinations %>%
  group_by(variety) %>%
  slice_max(predicted_quality, n = 3)

cat("\nBest combinations by variety:\n")
print(best_by_variety)

# Session info for reproducibility
sessionInfo()
```
```{r Proper Design Structure & Constraints}
# FIXED: Proper design considering greenhouse structure
create_proper_design <- function() {
  seasons <- 1:2
  greenhouses <- c("A", "B")
  varieties <- c("R", "F")
  
  design_list <- list()
  
  for(season in seasons) {
    for(gh in greenhouses) {
      # Create 8 north and 8 south plots per greenhouse per season
      plot_base <- (season - 1) * 32 + (which(gh == greenhouses) - 1) * 16
      
      # North-facing plots (8 plots)
      north_design <- data.frame(
        plot = paste0("P", sprintf("%03d", plot_base + 1:8)),
        season = season,
        greenhouse = gh,
        side = "n",
        light = sample(rep(1:4, 2)),  # 2 reps per light level
        heat = sample(rep(1:4, 2)),   # 2 reps per heat level
        variety = sample(rep(varieties, 4)) # Balanced variety
      )
      
      # South-facing plots (8 plots)
      south_design <- data.frame(
        plot = paste0("P", sprintf("%03d", plot_base + 9:16)),
        season = season,
        greenhouse = gh,
        side = "s",
        light = sample(rep(1:4, 2)),
        heat = sample(rep(1:4, 2)),
        variety = sample(rep(varieties, 4))
      )
      
      design_list[[length(design_list) + 1]] <- rbind(north_design, south_design)
    }
  }
  
  design <- do.call(rbind, design_list)
  return(design)
}
```


```{r}
# FIXED: Replace simulation with actual function
# Source the provided file or define the function
source("ProjectED.R")  # Or copy the function into your script

# Get real observations
my_design$quality <- get.observations(my_design)

# Verify the structure
str(my_design)
```


```{r}
# FIXED: Comprehensive missing data handling
handle_missing_data <- function(data) {
  cat("Initial missing values:", sum(is.na(data$quality)), "\n") #Counts how many plots have missing quality scores
  
  # Analyze pattern of missingness,,Checks if missing data is random or follows a pattern
  missing_by_factor <- data %>%
    group_by(light, heat, variety, season, greenhouse) %>%
    summarise(
      total_plots = n(),
      missing_plots = sum(is.na(quality)),
      .groups = 'drop'
    )
  
  print("Missing data pattern:")
  print(missing_by_factor %>% filter(missing_plots > 0))
  
  # Option 1: Remove missing observations with warning/Removes rows with missing quality scores
  if(sum(is.na(data$quality)) > 0) {
    cat("Removing", sum(is.na(data$quality)), "missing observations\n")
    complete_data <- data %>% filter(!is.na(quality))
  } else {
    complete_data <- data
  }
  
  return(complete_data)
}
#Interpretation:missing data is scattered randomly across different treatments.analysis should be unbiased. eg. problematic if Missing data clusters in high-light/high-heat Furious variety. This could mean or a critical problem! 20% of your data is missing 13/64

analysis_data <- handle_missing_data(my_design)
```


```{r}
# FIXED: Systematic model selection
fit_models <- function(data) {
  # Convert to factors
  data <- data %>%
    mutate(across(c(light, heat, variety, season, greenhouse, side), factor))
  
  # Candidate models
  models <- list()
  
  # Model 1: Full factorial with blocking
  models$full <- lm(quality ~ light * heat * variety + season + greenhouse + side, 
                    data = data)
  
  # Model 2: Simplified interactions
  models$simplified <- lm(quality ~ (light + heat + variety)^2 + season + greenhouse + side, 
                          data = data)
  
  # Model 3: Main effects only
  models$main <- lm(quality ~ light + heat + variety + season + greenhouse + side, 
                    data = data)
  
  # Model comparison
  aic_comparison <- data.frame(
    Model = names(models),
    AIC = sapply(models, AIC),
    BIC = sapply(models, BIC)
  ) %>%
    arrange(AIC)
  
  print("Model Comparison:")
  print(aic_comparison)
  
  # Best model based on AIC
  best_model_name <- aic_comparison$Model[1]
  cat("Selected model:", best_model_name, "\n")
  
  return(models[[best_model_name]])
}

best_model <- fit_models(analysis_data)
```
```{r}
# FIXED: Systematic model selection
fit_models <- function(data) {
  # Convert to factors
  data <- data %>%
    mutate(across(c(light, heat, variety, season, greenhouse, side), factor))
  
  # Candidate models
  models <- list()
  
  # Model 1: Full factorial with blocking
  models$full <- lm(quality ~ light * heat * variety + season + greenhouse + side, 
                    data = data)
  
  # Model 2: Simplified interactions
  models$simplified <- lm(quality ~ (light + heat + variety)^2 + season + greenhouse + side, 
                          data = data)
  
  # Model 3: Main effects only
  models$main <- lm(quality ~ light + heat + variety + season + greenhouse + side, 
                    data = data)
  
  # Model comparison
  aic_comparison <- data.frame(
    Model = names(models),
    AIC = sapply(models, AIC),
    BIC = sapply(models, BIC)
  ) %>%
    arrange(AIC)
  
  print("Model Comparison:")
  print(aic_comparison)
  
  # Best model based on AIC
  best_model_name <- aic_comparison$Model[1]
  cat("Selected model:", best_model_name, "\n")
  
  return(models[[best_model_name]])
}

best_model <- fit_models(analysis_data)
```

```{r}
# FIXED: Cost-aware recommendations
cost_benefit_analysis <- function(model, data) {
  # Define costs (example values - adjust based on actual costs)
  light_cost <- c(1, 1.5, 2, 3)    # Increasing cost with higher light levels
  heat_cost <- c(1, 2, 3, 4)       # Increasing cost with higher heat levels
  
  # Get predictions for all combinations
  pred_grid <- expand.grid(
    light = factor(1:4),
    heat = factor(1:4),
    variety = factor(c("R", "F")),
    season = factor(1),
    greenhouse = factor("A"),
    side = factor("n")
  )
  
  pred_grid$predicted_quality <- predict(model, newdata = pred_grid)
  
  # Add costs
  pred_grid <- pred_grid %>%
    mutate(
      light_cost = light_cost[as.numeric(light)],
      heat_cost = heat_cost[as.numeric(heat)],
      total_cost = light_cost + heat_cost,
      cost_effectiveness = predicted_quality / total_cost
    )
  
  # Top by pure quality
  top_quality <- pred_grid %>%
    arrange(desc(predicted_quality)) %>%
    head(5)
  
  # Top by cost-effectiveness
  top_cost_effective <- pred_grid %>%
    arrange(desc(cost_effectiveness)) %>%
    head(5)
  
  # Best compromise (high quality, reasonable cost)
  best_compromise <- pred_grid %>%
    filter(predicted_quality > quantile(predicted_quality, 0.8)) %>%
    arrange(total_cost) %>%
    head(5)
  
  return(list(
    top_quality = top_quality,
    top_cost_effective = top_cost_effective,
    best_compromise = best_compromise,
    all_combinations = pred_grid
  ))
}

cost_results <- cost_benefit_analysis(best_model, analysis_data)
```
```{r}
# FIXED: Effect size calculations
calculate_effect_sizes <- function(model, data) {
  # Get emmeans
  light_emm <- emmeans(model, ~ light)
  heat_emm <- emmeans(model, ~ heat)
  variety_emm <- emmeans(model, ~ variety)
  
  # Calculate effect sizes relative to baseline
  light_effects <- contrast(light_emm, method = "dunnett")
  heat_effects <- contrast(heat_emm, method = "dunnett")
  variety_effect <- contrast(variety_emm, method = "pairwise")
  
  # Calculate practical significance
  overall_mean <- mean(data$quality, na.rm = TRUE)
  sd_pooled <- sd(data$quality, na.rm = TRUE)
  
  light_means_df <- as.data.frame(light_emm) %>%
    mutate(
      difference_from_baseline = emmean - first(emmean),
      cohens_d = difference_from_baseline / sd_pooled
    )
  
  cat("Effect Sizes (Cohen's d):\n")
  print(light_means_df[, c("light", "emmean", "difference_from_baseline", "cohens_d")])
  
  return(list(
    light_effects = light_effects,
    heat_effects = heat_effects,
    variety_effect = variety_effect,
    light_means = light_means_df
  ))
}

effect_sizes <- calculate_effect_sizes(best_model, analysis_data)

```

```{r}
# FIXED: Comprehensive model diagnostics
model_diagnostics <- function(model, data) {
  # Normality of residuals
  shapiro_test <- shapiro.test(residuals(model))
  cat("Normality test (Shapiro-Wilk): p =", shapiro_test$p.value, "\n")
  
  # Homogeneity of variance
  levene_test <- car::leveneTest(residuals(model) ~ interaction(light, heat, variety), data = data)
  cat("Homogeneity of variance (Levene's): p =", levene_test$`Pr(>F)`[1], "\n")
  
  # Influence points
  cooks_dist <- cooks.distance(model)
  influential_obs <- which(cooks_dist > 4/nrow(data))
  cat("Influential observations:", length(influential_obs), "\n")
  
  # Create diagnostic plots
  par(mfrow = c(2, 2))
  plot(model, which = 1:4)
  par(mfrow = c(1, 1))
  
  return(list(
    shapiro_p = shapiro_test$p.value,
    levene_p = levene_test$`Pr(>F)`[1],
    influential_obs = influential_obs
  ))
}

diagnostics <- model_diagnostics(best_model, analysis_data)

```

```{r}
# FIXED: Create tables and plots for the report
generate_report_outputs <- function(results, model, data) {
  # Summary table for main effects
  main_effects <- data %>%
    group_by(variety, light, heat) %>%
    summarise(
      n = n(),
      mean_quality = round(mean(quality, na.rm = TRUE), 1),
      se = round(sd(quality, na.rm = TRUE) / sqrt(n()), 1),
      .groups = 'drop'
    ) %>%
    arrange(variety, desc(mean_quality))
  
  # Interaction plot for report
  interaction_plot <- ggplot(data, aes(x = light, y = quality, color = variety, group = variety)) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    stat_summary(fun = mean, geom = "point", size = 2) +
    facet_wrap(~ heat, labeller = label_both) +
    labs(
      title = "Quality Score by Light, Heat and Variety",
      x = "Light Level",
      y = "Mean Quality Score",
      color = "Variety"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Save plot
  ggsave("interaction_plot.png", interaction_plot, width = 8, height = 6, dpi = 300)
  
  return(list(
    main_effects_table = main_effects,
    interaction_plot = interaction_plot
  ))
}

report_outputs <- generate_report_outputs(cost_results, best_model, analysis_data)

```
```{r}
# FIXED: Explicit answers to research questions
answer_research_questions <- function(results, model, data) {
  cat("=== ANSWERS TO RESEARCH QUESTIONS ===\n\n")
  
  # Question 1: Best light/heat settings
  best_combo <- results$cost_analysis$top_quality[1, ]
  cat("1. BEST SETTINGS: Light =", best_combo$light, 
      "Heat =", best_combo$heat, 
      "Quality =", round(best_combo$predicted_quality, 1), "\n\n")
  
  # Question 2: Dependence on variety
  # Test variety interactions
  anova_results <- Anova(model, type = "III")
  variety_interactions <- rownames(anova_results)[grepl("variety", rownames(anova_results))]
  variety_interactions <- variety_interactions[!variety_interactions == "variety"]
  
  cat("2. VARIETY DEPENDENCE:\n")
  for(int in variety_interactions) {
    p_val <- anova_results[int, "Pr(>F)"]
    cat("   ", int, ": p =", round(p_val, 4), 
        ifelse(p_val < 0.05, "(SIGNIFICANT)", "(NOT SIGNIFICANT)"), "\n")
  }
  cat("\n")
  
  # Question 3: Variety effect size
  variety_means <- data %>%
    group_by(variety) %>%
    summarise(mean_quality = mean(quality, na.rm = TRUE))
  
  cat("3. VARIETY EFFECT: F - R =", 
      round(variety_means[2, "mean_quality"] - variety_means[1, "mean_quality"], 1), 
      "points\n\n")
  
  # Question 4: Magnitude of differences
  quality_range <- range(data$quality, na.rm = TRUE)
  cat("4. EFFECT MAGNITUDE: Range =", round(diff(quality_range), 1), 
      "points (", round(quality_range[1], 1), "to", round(quality_range[2], 1), ")\n")
}

answer_research_questions(all_results, best_model, analysis_data)
```

```{r}
# FIXED: Main execution function
main <- function() {
  # 1. Design
  design <- create_proper_design()
  
  # 2. Data collection
  design$quality <- get.observations(design)  # Use actual function
  
  # 3. Handle missing data
  analysis_data <- handle_missing_data(design)
  
  # 4. Model selection
  best_model <- fit_models(analysis_data)
  
  # 5. Comprehensive analysis
  all_results <- list(
    cost_analysis = cost_benefit_analysis(best_model, analysis_data),
    effect_sizes = calculate_effect_sizes(best_model, analysis_data),
    diagnostics = model_diagnostics(best_model, analysis_data),
    report_outputs = generate_report_outputs(all_results, best_model, analysis_data)
  )
  
  # 6. Answer research questions
  answer_research_questions(all_results, best_model, analysis_data)
  
  return(list(design = design, model = best_model, results = all_results))
}

# Run the complete analysis
final_results <- main()



