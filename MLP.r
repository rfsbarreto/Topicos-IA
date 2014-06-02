rm(list = ls())
eta = 0.1
M = 2
Jm = c(2,1)
En = c(2,2)
W = vector('list', M)
V = vector('list', M)
U = vector('list', M)
delta = vector('list', M)
for (m in c(1:M)){
	W[[m]] = matrix(runif(Jm[m]*En[m]),Jm[m])
	V[[m]]
}
#Xd = c(1,1,1,0)
#Yd = c()
Xd = matrix(c(1,0,1,0,0,1,1,0,0,0,0,1,0,1,1,0,1,1,0,1,1,0,0,0,0,1,1,1,0,1,1,0,1,0,0,1,0,1,0,0,1,0,1,0,1,1,0,0,1,1,0,1,0,0,1,1),nrow=2,byrow=TRUE)
Yd = c(1,1,0,0,1,1,1,1,0,1,0,1,1,1,0,0,0,0,0,1,0,1,0,1,0,1,0,0)
L = length(Yd)
for(l in L){
	X = matrix(Xd[,l])
	V[[1]] = W[[1]] %*% X
	U[[1]] - tanh(V[[1]])
	for(m in 2:M){
		V[[m]] = W[[m]] %*% U[[m-1]]
		U[[m]] = tanh(V[[m]])
	}
	Y = U[[m]]
	E = (Y-Yd[,l]) %*% (Y-Yd[,l])
	delta[[m]] = (Yd[,l]-Y)*(1/cosh(V[[m]]))^2
	W[[m]] = W[[m]] + 2 * eta * delta[[m]] %*% U[[m-1]]
	for(m in M-1:1){
		delta[[m]] = t(W[[m+1]])%*%delta[[m+1]]*(1/cosh(V[[m]]))^2
		if(m==1)
			W[[m]] = W[[m]] + 2*eta*delta[[m]]%*%t(x)
		else		
		W[[m]] = W[[m]] + 2*eta*delta[[m]]%*%t(U[[m-1]])
	}
}

x=matrix(c(0,1))
U[[1]]= tanh(W[[1]]%*%x)
for(m in 2:m){
	U[[m=tanh(W[[m]]%*%U[[m-1]])]]
	U[[m]]
}


##Plotar Erro e 