dt_root <- read.csv("data/data_t0.tsv", sep = "\t", header = TRUE, encoding = "UTF-8", na.strings = "")

pMiss <- function(x){sum(is.na(x))/length(x)*100}
aMiss <- function(x){sum(is.na(x))}

p_rows <- apply(dt_root, 1, pMiss)

# Exclusao de linhas com 100% de missing
dt <- dt_root[p_rows != 100, ]

p_cols <- apply(dt, 2, pMiss)
names(p_cols) <- colnames(dt)

View(as.data.frame(p_cols))
