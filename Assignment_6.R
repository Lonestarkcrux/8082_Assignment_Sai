library(data.table)
library(ggplot2)
library(foreach)

# --- load data ---
inv <- fread("https://raw.githubusercontent.com/BIOL8082/Stats_Spring2026/refs/heads/main/Class06_Multivariate_pt1/inversion.csv")
markers <-  fread("https://raw.githubusercontent.com/BIOL8082/Stats_Spring2026/refs/heads/main/Class06_Multivariate_pt1/markers.csv")




markers[, ral_id_num := as.numeric(gsub("line_", "", ral_id))]
inv[, ral_id := as.numeric(gsub("DGRP_", "", `DGRP Line`))]


stopifnot(!any(is.na(markers$ral_id_num)))
stopifnot("ral_id" %in% names(inv))  


geno <- dcast(markers, ral_id_num ~ snp_id, value.var = "dosage")

mat <- as.matrix(geno[, -"ral_id_num"])
rownames(mat) <- geno$ral_id_num



col_means <- colMeans(mat, na.rm = TRUE)
na_idx <- which(is.na(mat), arr.ind = TRUE)
mat[na_idx] <- col_means[na_idx[, 2]]


pca <- prcomp(mat, center = TRUE, scale. = TRUE)


proj <- as.data.table(pca$x[, 1:10])
proj[, ral_id := geno$ral_id_num]  



p_base <- ggplot(proj, aes(PC1, PC2)) +
  geom_point(alpha = 0.8) +
  theme_bw()

print(p_base)


proj <- merge(proj, inv, by = "ral_id", all.x = TRUE)


inv_cols <- setdiff(names(inv), c("ral_id", "DGRP Line"))
pcs <- paste0("PC", 1:10)



inv_cols <- grep("^In\\(", names(proj), value = TRUE)
inv_cols <- inv_cols[grepl("\\.y$", inv_cols)]

tests <- foreach(invn = inv_cols, .combine = rbind) %do% {
  foreach(pc = pcs, .combine = rbind) %do% {
    
    d <- proj[!is.na(get(invn)), .(y = get(pc), g = factor(get(invn)))]
    
  
    if (nlevels(d$g) < 2) {
      data.table(inversion = invn, pc = pc, p = NA_real_)
    } else {
      fit <- lm(y ~ g, data = d)
      data.table(inversion = invn, pc = pc, p = anova(fit)$`Pr(>F)`[1])
    }
  }
}

setorder(tests, p)
best <- tests[!is.na(p)][1]
print(best)

best_inv <- best$inversion
best_pc <- best$pc


 most meaningful ANOVA table

d_best <- proj[!is.na(get(best_inv)), .(y = get(best_pc), g = factor(get(best_inv)))]
fit_best <- aov(y ~ g, data = d_best)

cat("\nMost meaningful ANOVA:\n")
print(summary(fit_best))


p_final <- ggplot(proj, aes(PC1, PC2, color = factor(get(best_inv)))) +
  geom_point(alpha = 0.85) +
  theme_bw() +
  labs(color = best_inv,
       title = paste0("PC1 vs PC2 colored by ", best_inv, " (best effect on ", best_pc, ")"))

print(p_final)

pdf("PC1_PC2_inversion.pdf", width = 7, height = 6)
print(p_final)
dev.off()

cat("\nSaved figure: PC1_PC2_inversion.pdf\n")

