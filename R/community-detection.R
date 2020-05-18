# Some methods for community detection

library(Matrix)
library(lda.svi)
library(tibble)
library(rsparse)
library(irlba)

posts_crosstab <- xtabs(~ author + subreddit, df, sparse = TRUE)

# For other methods, it is convienent to have (i, j)-indexing
# rather than the default (i, p) index in dgCmatrix
posts_crosstab <- as(posts_crosstab, "TsparseMatrix")
crosstab_tfidf <- log1p(posts_crosstab)
idfs <- log( ncol(posts_crosstab) / rowSums(posts_crosstab > 0))
crosstab_tfidf <- crosstab_tfidf * idfs

##############################
# Singular Value Decomposition
##############################

# Classic SVD decomposition of the TF-IDF weighted matrix
# Compute the TF-IDF weighted matrix

ssvd <- irlba(crosstab_tfidf, nv = 100, verbose = TRUE)
rownames(ssvd$u) <- dimnames(posts_crosstab)[["author"]]
rownames(ssvd$v) <- dimnames(posts_crosstab)[["subreddit"]]

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

# Use tf-idf scaling, since:
# - we don't care about reproducing high numbers in the high-traffic
#   subreddits; leaving the post counts will overweight
#   high-traffic users / subreddits
# - higher information content per matrix entry


wrmf <- WRMF$new(rank = 50, non_negative = TRUE)
W <- wrmf$fit_transform(crosstab_tfidf, 25, 0.001)
H <- wrmf$components

# Recommendations
recommend_sub <- function(name, k = 20) {
  myq <- H[, name, drop = FALSE]
  sims <- text2vec::sim2(t(myq), t(H), method = "cosine")[1,]
  return(head(sort(sims, decreasing = TRUE), k))
}

recommend_user <- function(uname, k = 20) {
  myq <- t(W[uname,])
  sims <- text2vec::sim2(as.matrix(myq), W, method = "cosine")[1,]
  return(head(sort(sims, decreasing = TRUE), k))
}
