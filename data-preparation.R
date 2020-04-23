# Data preparation

library(vroom)

df <- vroom('data/cleaned.csv',
             col_names = c("created", "author", "subreddit", "score"),
             col_types = "icci")

# Perform the analysis on a fraction of the full dataset
ratio <- 1
asample <- sample(1:nrow(df), size = floor(ratio * nrow(df)), replace = FALSE)
df <- df[asample,]

# Adjust for timezone (should be EST-based)
df$created <- as.POSIXct(as.numeric(df$created) - 5 * 60 * 60, origin = "1970-01-01")

# TODO: This is slow, speed up factor generation or bypass altogether
df$author <- factor(df$author)
df$subreddit <- factor(df$subreddit)

# Additional preparation (if needed) -- 
# drop out low- and high-frequency users/subs

# df <- df %>% group_by(author) %>% filter(n() > 10) %>% ungroup() %>%
#   group_by(subreddit) %>% filter(n() > 100) %>% droplevels()