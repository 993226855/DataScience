norm.test <- function(x, breaks = 20, alpha = 0.05,
plot = TRUE){
if(plot == TRUE)
{#设置图形界面（多图合为一张图）
opar <- par(no.readonly = TRUE)
layout(matrix(c(1,1,2,3),2,2,byrow = TRUE),
width = c(2,2),heights = c(2,2))
#绘制直方图
hist(x, freq = FALSE, breaks = seq(min(x),
max(x), length = breaks), main = 'x的直方图',
ylab = '核密度值')
#添加核密度图
lines(density(x), col = 'red', lty = 1, lwd = 2)
#添加正态分布图
x <- x[order(x)]
lines(x, dnorm(x, mean(x), sd(x)),
col = 'blue', lty = 2, lwd = 2.5)
#添加图例
legend('topright',
legend = c('核密度曲线','正态分布曲线'),
col = c('red','blue'), lty = c(1,2),
lwd = c(2,2.5), bty = 'n')
#绘制Q-Q图
qqnorm(x, xlab = '实际分布', ylab = '正态分布',
main = 'x的Q-Q图', col = 'blue')
qqline(x)
#绘制P-P图
P <- pnorm(x, mean(x), sd(x))
cdf <- 0
for(i in 1:length(x)){cdf[i] <- sum(x <= x[i])/length(x)}
plot(cdf, P, xlab = '实际分布', ylab = '正态分布',
main = 'x的P-P图', xlim = c(0,1),
ylim = c(0,1), col = 'blue')
abline(a = 0, b = 1)
par(opar)
}
#定量的shapiro检验
if (length(x) <= 5000) {
shapiro <- shapiro.test(x)
if(shapiro$p.value > alpha)
print(paste('定量结果为：', 'x服从正态分布，',
'P值 =',round(shapiro$p.value,5), '> 0.05'))
else
print(paste('定量结果为：', 'x不服从正态分布，',
'P值 =',round(shapiro$p.value,5), '<= 0.05'))
shapiro
}
else {
ks <- ks.test(x,'pnorm')
if(ks$p.value > alpha)
print(paste('定量结果为：', 'x服从正态分布，',
'P值 =',round(ks$p.value,5), '> 0.05'))
else
print(paste('定量结果为：', 'x不服从正态分布，',
'P值 =',round(ks$p.value,5), '<= 0.05'))
ks
}
}
