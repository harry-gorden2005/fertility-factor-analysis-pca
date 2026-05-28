library(ggplot2)
library(dplyr)
library(tidyr)
library(lmtest)
library(pheatmap)
library(scatterplot3d)
library(psych)

# 1.读取数据
df <- read.csv("data/fertility_data.csv") 
head(df)

# 2.描述性图表
p1 <- ggplot(df, aes(x = Year, y = TFR)) +
  geom_line() + geom_point() +
  labs(title = "2010-2021 中国总和生育率变化", y = "总和生育率") +
  theme_minimal()
print(p1)

p2 <- ggplot(df, aes(x = Year, y = PCDIR)) +
  geom_col(fill = "#69b3a2") +
  labs(title = "全国居民人均可支配收入", x = "年份", y = "元") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2)

# 收入vs消费
df_long <- df %>%
  select(Year, PCDIR, PCCER) %>%
  pivot_longer(-Year, names_to = "指标", values_to = "金额")
p3 <- ggplot(df_long, aes(x = Year, y = 金额, color = 指标)) +
  geom_line(size = 1) +
  labs(title = "人均可支配收入 vs 人均消费支出")
print(p3)

# 3.主成分分析
X <- df[, -c(1,2)]   # 去掉 Year 和 TFR，保留所有自变量
pca_result <- prcomp(X, scale. = TRUE)
summary(pca_result)

# 碎石图
screeplot(pca_result, type = "line", main = "碎石图")

# 提取前三个主成分得分
pc_scores <- predict(pca_result)[, 1:3]
colnames(pc_scores) <- c("PC1", "PC2", "PC3")
df_pca <- cbind(df, pc_scores)

# 4.主成分回归
Y_scaled <- scale(df$TFR)
model <- lm(Y_scaled ~ PC1 + PC2 + PC3, data = df_pca)
summary(model)

# 5 模型诊断
res <- residuals(model)

#标准化残差图
plot(1:12, scale(res), ylab = "标准化残差", xlab = "观测序号",
     main = "标准化残差分布图")
abline(h = 0, lty = 2)

#Q-Q 图
qqnorm(res, main = "残差 Q-Q 图")
qqline(res)

#Durbin-Watson检验（自相关）
dwtest(model)

#Breusch-Pagan检验（异方差）
bptest(model)

# Bartlett 球形检验（是否适合 PCA）
cortest.bartlett(cor(X), n = nrow(X))

# 6. 载荷热力图
loadings <- pca_result$rotation[, 1:3]
pheatmap(loadings, cluster_rows = FALSE, cluster_cols = FALSE,
         main = "主成分载荷热力图", fontsize_row = 10, fontsize_col = 12)

# 7. 三维主成分得分图
scatterplot3d(pc_scores[,1], pc_scores[,2], pc_scores[,3],
              xlab = "第一主成分", ylab = "第二主成分", zlab = "第三主成分",
              main = "主成分得分图", color = "blue", pch = 16)

# 8. 输出关键结果
cat("\n===== 关键结果汇总 =====\n")
cat("PCA 方差解释比例:\n")
print(summary(pca_result)$importance[,1:3])
cat("\n回归模型 R² =", summary(model)$r.squared, "\n")
cat("调整后 R² =", summary(model)$adj.r.squared, "\n")
cat("DW 检验 p 值 =", dwtest(model)$p.value, "\n")
cat("BP 检验 p 值 =", bptest(model)$p.value, "\n")