#Program Temp
#Author: Feixiao_L
#======================================================================================
#乱七八糟的R脚本
#======================================================================================
#阶梯法
#刺激感受函数
phi = function(X){return((exp(X)/(1+exp(X)))^2)}
#阶梯法模拟
SP = function(Xt, dX = 0.05){p = runif(1); if (p > phi(Xt)){return(Xt + dX)}else{return(Xt - dX)}}
set.seed(12345)
X = -5
for (i in 1:10000){S = SP(X[length(X)]); X = c(X, S)}
#绘制结果
x = 1:10001
plot(x, X, 'l')
abline(h = 0, lty = 2)
#abline(v = 0, lty = 2)

#考虑分布
fun_right = function(X, n, dx = 0.05){s = 1; for (i in 1:n){s = s * (1 - phi(X + (i - 1) * dx))}; return(s * phi(X + n * dx))}
fun_left = function(X, n, dx = 0.05){s = 1; for (i in 1:n){s = s * phi(X - (i - 1) * dx)}; return(s * (1 - phi(X - n * dx)))}
#模拟
foo = function(X, n, dx = 0.05){
	X_left = c()
	X_right = c()
	for (i in 1:n){X_left = c(X_left, fun_left(X, (n + 1 - i), dx)); X_right = c(X_right, fun_right(X, i, dx))}
	X_Prop = c(X_left, 0, X_right)
	X_Event = (-n:n) * dx + X
	#return(c(sum(X_Prop), sum(X_Event*X_Prop)))
	return(sum(X_Event*X_Prop))
}
fun = function(inf, sup, n, dx){X = c(); for (i in ((inf/dx):(sup/dx))){X = c(X, foo(i*dx, n, dx))}; return(X)}
EX = fun(-5, 5, 200, 0.1)
X = -50:50/10
plot(X, EX, 'l')
#1. 似乎一次反跳点的分布不是一个好求的东西
#2. 几乎可以肯定的是，一次反跳点的期望随着起始点和步长的变化而变化
#3. 给定步长情况下一次反跳点的期望与起始点之间的关系似乎与原始的移动概率函数的形状相近
#4. 目前看来，不管如何，一次反跳点期望等于自身的情况应该是对应于原始概率函数等于0.5的位置
#5. 目前看来，传统最小变化法在使用过程中不太适宜使用在高精度的场合，阶梯法则需要进一步考虑对随机过程的期望求解以求得高精度下的逼近值