# 15.9.2025
# Analysis of behavior data

require(dplyr)
require(tidyr)
require(ggplot2)

get_col_names <- function(prefix, plist, sep = '.'){


  colnames <- prefix
  for (factor_name in names(plist)){
    level_names <- plist[[factor_name]]
    colnames2 <- NULL
    nxt <- 1
    for (k in 1:length(colnames)){
      for (j in 1:length(level_names)){
        colnames2[nxt] <- paste(colnames[k], level_names[j], sep = sep)
        nxt <- nxt + 1
      }
    }
  colnames <- colnames2
  }
  return(colnames)
}

# We saw that results are not that good using Brunner-Munzel with permutations for the mean.
# We want to use all fingers but we do not care about the finger factor, only if flexion and extension are different in respect for a given lesion.

#Lets look first at the distribution of the observations:
#setwd("c:/scripts/Brain-Lab/Repo/brainlab/code/")
x <- read.csv('../data/individuation/kinetics_flip_31.5.24.csv')
fingers <- c('T','I','M','R','L')
directions <- c('Flex','Ext')
vars <- get_col_names(prefix = 'nimp_med.II', plist = list(Direction = directions, Finger = fingers))

x.filt <- x %>% filter(Side == 'L')
x.filt$X <- 1:dim(x.filt)[1]
x.long <- x.filt %>% 
  select(all_of(c('pcode','X',vars))) %>%
  pivot_longer(cols = all_of(vars),
               names_to = c('Measure','Direction','Finger'),
               names_pattern = "^nimp_med\\.([^\\.]+)\\.([^\\.]+)\\.([^\\.]+)$",
               values_to = 'Value') 

g <- ggplot(x.long, aes(x = Value, fill = Direction)) +
  geom_density(alpha = 0.5) + facet_grid(cols = vars(Finger))

print(g)
# It seems that the data is largely symetric.

# Now we want to add to this data the lesion status
lesion <- read.csv('../data/individuation/L_lesmat.csv')

lesion_behav <- left_join(x = x.long, y = lesion, by = 'pcode') 

voxel_names <- colnames(lesion)[2:(ncol(lesion)-1)]

lesion_matrix <- as.matrix(lesion[,2:(ncol(lesion)-1)])

# select a voxel with relatively high involvement:
involvement <- colMeans(lesion_matrix)
involvement[involvement>0.4][1]
# X201 
0.4347826 

lesion_name <- 'X201'

# subset the data to this voxel:
data <- left_join(x = x.long, 
                  y = lesion %>% 
                    select(all_of(c('pcode',lesion_name))), by = 'pcode') %>%
                    rename(Lesion = all_of(lesion_name))  

# just for the sport, let plot each finger and direction for lesioned and non-lesioned patients::
g2 <- ggplot(data, aes(x = Value, fill = as.factor(Lesion))) +
  geom_density(alpha = 0.5) + facet_grid(rows = vars(Finger), cols = vars(Direction)) +
  labs(fill = 'Lesion')
print(g2)

d <- data %>% rename(score = Value, direction = Direction, finger = Finger, lesion = Lesion, patient = pcode) %>% select(-Measure)

# Plot a DAG:
require(dagitty)
dag <- dagitty("
dag {
  Lesion -> FlexionScore
  Lesion -> ExtensionScore
  Patient -> FlexionScore
  Patient -> ExtensionScore
  Finger -> FlexionScore
  Finger -> ExtensionScore
}
")

coordinates(dag) <- list(
  x = c(Lesion = 0, Patient = -1, Finger = 1, FlexionScore = -0.5, ExtensionScore = 0.5),
  y = c(Lesion = 1, Patient = 0, Finger = 0, FlexionScore = -1, ExtensionScore = -1)
)
plot(dag)

adjustmentSets(dag, exposure = "Lesion", outcome = "FlexionScore")


# Using rethinking package to build a mixed model:
library(rethinking)

# Convert direction to index
d$dir_id <- ifelse(d$direction == "flexion", 1, 2)

# Standardize score for better convergence
d$score_std <- standardize(d$score)

m <- ulam(
  alist(
    score_std ~ normal(mu, sigma),
    mu <- a[patient] + f[finger] + b[dir_id] * lesion,
    
    # Varying intercepts
    a[patient] ~ normal(0, 0.5),
    f[finger] ~ normal(0, 0.5),
    
    # Direction-specific lesion effects
    b[dir_id] ~ normal(0, 1),
    
    sigma ~ exponential(1)
  ),
  data = d,
  chains = 4,
  cores = 4
)

post <- extract.samples(m)

# Compute probabilities for each causal class
p_both <- mean(post$b[,1] < 0 & post$b[,2] < 0)
p_flex_only <- mean(post$b[,1] < 0 & post$b[,2] >= 0)
p_ext_only <- mean(post$b[,1] >= 0 & post$b[,2] < 0)
p_neither <- mean(post$b[,1] >= 0 & post$b[,2] >= 0)

# Display results
round(c(
  "Critical for both" = p_both,
  "Flexion only" = p_flex_only,
  "Extension only" = p_ext_only,
  "Not critical" = p_neither
), 3)