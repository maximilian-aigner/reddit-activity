# Data preparation

library(vroom)
library(Rcpp)
sourceCpp('src/fast-factor.cpp')

df <- vroom('data/cleaned.csv',
             col_names = c("created", "author", "subreddit", "score"),
             col_types = "icci")

# Perform the analysis on a fraction of the full dataset (OPTIONAL)
# ratio <- 1
# asample <- sample(1:nrow(df), size = floor(ratio * nrow(df)), replace = FALSE)
# df <- df[asample,]

# Adjust for timezone (should be EST-based)
df$created <- as.POSIXct(as.numeric(df$created) - 5 * 60 * 60, origin = "1970-01-01")

# Use C++ factor generation, otherwise it's too slow
df$author <- fast_factor(df$author)
df$subreddit <- fast_factor(df$subreddit)

# Additional preparation (if needed) -- 
# drop out low- and high-frequency users/subs

# library(dplyr)
# df <- df %>% group_by(author) %>% filter(n() > 10) %>% ungroup() %>%
#    group_by(subreddit) %>% filter(n() > 100) %>% droplevels()
