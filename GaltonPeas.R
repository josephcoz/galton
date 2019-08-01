#read in data
galton <- read.table(text = "'Parent' 'Offspring'
0.21 0.1726
0.20 0.1707
0.19 0.1637
0.18 0.1640
0.17 0.1613
0.16 0.1617
0.15 0.1598", header = TRUE, sep = "")

#check data correct
galton

#calculate correlation
galton_corr <- cor(galton[, 1], galton[, 2])
galton_corr
#when Parent diameter increases, Offspring diameter consistently increases
# visual check confirms calculated value, close to +1
# values are not too far from regression line


#create plot to check correlation
plot(galton[, 1], galton[, 2], xlab = 'Parent', ylab = 'Offspring',
main = 'Pea Plant Parent/Offspring Diameters')
# Add fit lines
abline(lm(galton[, 2] ~ galton[, 1]), col = "gray") # regression line (y~x) 

#Dr Grimshaw used ggplot2
library(ggplot2)
#can put ggplot2 scatter code here

#Analysis:

#Response variable: offspring (quantitative)
#Explanatory variable: parent (quantitative)

#model: offspring = beta0 + beta1 parent + epsilon, epsilon~N(0, sigma^2)

out_galton = lm(Offspring ~ Parent, data = galton)
out_galton

#table of estimates and standard errors...also ttest
summary(out_galton)

# ANOVA F-test
anova(out_galton)

#95% CI on beta1
confint(out_galton)

galton
#create a graphic showing the uncertainty associated with estimating 
# the inheritance effect
#qplot: q=quick
qplot(Parent, Offspring, data = galton, geom = 'smooth', formula = y ~ x,
      method = 'lm', se = TRUE, xlab = 'Diameter of Parent', ylab = 'Diameter of Offspring')

#prediction (mean or individual obs?)

#95% CI on E(offspring | parent = 0.20)
predict(out_galton, newdata=data.frame(Parent=0.20), interval="confidence")

#95% PI
predict(out_galton, newdata = data.frame(Parent = 0.18), interval = "prediction")

plot_df <- cbind(galton, predict(out_galton, interval="prediction"))
ggplot(plot_df, aes(Parent, Offspring)) +
    xlab('Diameter of Parent Pea') +
    ylab('Diameter of Offspring Pea') +
    geom_point() +
    geom_line(aes(y = fit), color = 'royalblue') +
    geom_line(aes(y = lwr), color = 'red', linetype = 'dashed') +
    geom_line(aes(y = upr), color='red',linetype='dashed')
#Recall Multiple R-squared from summary function