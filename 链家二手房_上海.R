# 导入开发所需的扩展包
library(dplyr)
library(Hmisc)
library(ggplot2)
library(caret)

house <- read.csv(file.choose(), stringsAsFactors = FALSE)
dim(house)
str(house)
summary(house)

load('.RData')
# 数据探索

# 户型分布
type_freq <- data.frame(table(house$户型))
type_p <- ggplot(data = type_freq, mapping = aes(x = reorder(Var1, -Freq),y = Freq)) + geom_bar(stat = 'identity', fill = 'steelblue') + theme(axis.text.x  = element_text(angle = 30, vjust = 0.5)) + xlab('户型') + ylab('套数')
type_p

# 把低于一千套的房型设置为其他
type <- c('2室2厅','2室1厅','3室2厅','1室1厅','3室1厅','4室2厅','1室0厅','2室0厅')
house$type.new <- ifelse(house$户型 %in% type, house$户型,'其他')
type_freq <- data.frame(table(house$type.new))
type_p <- ggplot(data = type_freq, mapping = aes(x = reorder(Var1, -Freq),y = Freq)) + geom_bar(stat = 'identity', fill = 'steelblue') + theme(axis.text.x  = element_text(angle = 30, vjust = 0.5)) + xlab('户型') + ylab('套数')
type_p

# 面积的正态性检验
norm.test(house$面积)

# 房价的正态性检验
norm.test(house$价格.W.)

# 楼层分布
unique(house$楼层)

# 把楼层分为低区、中区和高区三种
house$floow <- ifelse(substring(house$楼层,1,2) %in% c('低区','中区','高区'), substring(house$楼层,1,2),'低区')

# 各楼层类型百分比分布
percent <- paste(round(prop.table(table(house$floow))*100,2),'%',sep = '')
df <- data.frame(table(house$floow))
df <- cbind(df, percent)
df

# 上海各区房价均价
avg_price <- aggregate(house$单价.平方米., by = list(house$区域), mean)

p <- ggplot(data = avg_price, mapping = aes(x = reorder(Group.1, -x), y = x, group = 1)) + geom_area(fill = 'lightgreen') + geom_line(colour = 'steelblue', size = 2) + geom_point() + xlab('') + ylab('均价')
p

# 房屋建筑时间确实严重，我们按区域分组，使用众数填充
house$建筑时间[house$建筑时间 == ''] <- NA
# 自定义众数函数
stat.mode <- function(x, rm.na = TRUE){
  if (rm.na == TRUE){
    y = x[!is.na(x)]
  }
  res = names(table(y))[which.max(table(y))]
  return(res)
}

# 自定义函数，实现分组替补
my.impute <- function(data, category.col = NULL, 
                      miss.col = NULL, method = stat.mode){
  impute.data = NULL
  for(i in as.character(unique(data[,category.col]))){
    sub.data = subset(data, data[,category.col] == i)
    sub.data[,miss.col] = impute(sub.data[,miss.col], method)
    impute.data = c(impute.data, sub.data[,miss.col])
  }
  data[,miss.col] = impute.data
  return(data)
}

final_house <- subset(my.impute(house, '区域', '建筑时间'),select = c(区域,type.new,floow,面积,价格.W.,单价.平方米.,建筑时间))
final_house <- transform(final_house, builtdate2now = 2016-as.integer(substring(as.character(建筑时间),1,4)))
final_house <- subset(final_house, select = -建筑时间)

# 使用k-means聚类，探究上海的各个区域可以划分为几类

# 自定义函数
tot.wssplot <- function(data, nc, seed=1234){
  #假设分为一组时的总的离差平方和              
  tot.wss <- (nrow(data)-1)*sum(apply(data,2,var)) 
  for (i in 2:nc){
    #必须指定随机种子数
    set.seed(seed) 
    tot.wss[i] <- kmeans(data, centers=i, iter.max = 100)$tot.withinss
  }
  plot(1:nc, tot.wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares",col = 'blue',
       lwd = 2, main = 'Choose best Clusters')
}


standrad <- data.frame(scale(final_house[,c('面积','价格.W.','单价.平方米.')]))
myplot <- tot.wssplot(standrad, nc = 15)

# 根据图形，大致可以将数据聚为5类
set.seed(1234)
clust <- kmeans(x = standrad, centers = 5, iter.max = 100)
table(clust$cluster)

# 按聚类的结果，比较各类中房子的平均面积、价格和单价
aggregate(final_house[,3:5], list(clust$cluster), mean)

# 按照聚类的结果，查看各类中的区域分布
table(house$区域,clust$cluster)

# 各户型的平均面积
aggregate(final_house$面积, list(final_house$type.new), mean)

# 绘制面积与单价的散点图，并按聚类进行划分
p <- ggplot(data = final_house[,3:5], mapping = aes(x = 面积,y = 单价.平方米., color = factor(clust$cluster)))
p <- p + geom_point(pch = 20, size = 3)
p + scale_colour_manual(values = c("red","blue", "green", "black", "orange"))


# 构造楼层和聚类结果的哑变量
# 将几个离散变量转换为因子，目的便于下面一次性处理哑变量
final_house$cluster <- factor(clust$cluster)
final_house$floow <- factor(final_house$floow)
final_house$type.new <- factor(final_house$type.new)
# 筛选出所有因子型变量
factors <- names(final_house)[sapply(final_house, class) == 'factor']
# 将因子型变量转换成公式formula的右半边形式
formula <- f <- as.formula(paste('~', paste(factors, collapse = '+')))
dummy <- dummyVars(formula = formula, data = final_house)
pred <- predict(dummy, newdata = final_house)
head(pred)
# 将哑变量规整到final_house数据集中
final_house2 <- cbind(final_house,pred)
# 筛选出需要建模的数据
model.data <- subset(final_house2,select = -c(1,2,3,8,17,18,24))
# 直接对数据进行线性回归建模
fit1 <- lm(价格.W. ~ .,data = model.data)
summary(fit1)

library(car)
# Box-Cox转换
powerTransform(fit1)

fit2 <- lm(log(价格.W.) ~ .,data = model.data)

# 使用plot方法完成模型定性的诊断
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
plot(fit2)
par(opar)
