# Some methods for community detection

library(Matrix)
library(lda.svi)
library(tibble)

posts_crosstab <- xtabs(~ author + subreddit, df, sparse = TRUE)

# For other methods, it is convienent to have (i, j)-indexing
# rather than the default (i, p) index in dgCmatrix
posts_crosstab <- as(posts_crosstab, "TsparseMatrix")

#############################
# Latent Dirichlet Allocation
#############################
lda <- lda_svi(t(posts_crosstab), passes = 10, batchsize = 80, K = 9,
               maxiter = 5e3)

# The tidy parameter in lda_svi doesn't seem to work, so do this manually
lda_topics <- dcast(lda$theta, document ~ topic) %>% column_to_rownames('document')

###################################
# Non-Negative Matrix Factorisation
###################################

# Scale by user, since we don't care about reproducing high numbers
# in the high-traffic subreddits; leaving the post counts
# will overweight high-traffic users / subreddits
posts_crosstab <- posts_crosstab / rowSums(posts_crosstab)

# Need this since nnmf requires `matrix` type
class(posts_crosstab) <- "matrix"

pnnmf <- nnmf(posts_crosstab, 10)
nnmf_topics <- t(pnnmf$H)
