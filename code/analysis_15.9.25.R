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

x <- read.csv('data/individuation/kinetics_flip_31.5.24.csv')
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
lesion <- read.csv('data/individuation/L_lesmat.csv')

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

# Looks nice!
# Now lets fit a linear mixed-effect model:

model <- lmerTest::lmer(Value ~ Direction * Lesion + (1|pcode) + (1|Finger), data = data)
summary(model)

# Widen the data to include different fingers as separate columns:
data_wide <- data %>%
  pivot_wider(names_from = Finger, values_from = Value)

# perform a MANOVA with Wild Bootstrap:
require(MANOVA.RM)
manova_model <- MANOVA.wide(cbind(T, I, M, R, L) ~ Direction * Lesion,
                            data = data_wide,
                            subject = 'pcode',
                            iter = 1000, 
                            resampling = 'WildBS')


summary(manova_model)

#Perform PERMANOVA:
require(vegan)

# prepare data:
finger_data<- data.wide[,c(fingers)]

# create a distance matrix:
dist_matrix <- vegdist(finger_data, method = 'euclidean')

# Run Permanova:
adonis2(dist_matrix ~ Direction * lesion, data = data.wide, permutations = 999)


