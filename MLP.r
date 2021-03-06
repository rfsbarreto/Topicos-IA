# clear all
rm( list = ls() )

#---------------------------------------------------------------------
# aqui utilizei uma  função para aumentar as amostra em 100 vezes
# não será necessário porque vocês tem que fazer em função de um número
# de épocas muito grande
repmat = function(X,m,n){
  ##R equivalent of repmat (matlab)
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
}
#---------------------------------------------------------------------


eta = 0.7

M = 3

Jm = c( 5,5, 1)	

En = c( 5, 5, 5)

V = vector('list', M)
#print(V)
U = vector('list', M)
delta = vector('list', M)
W = vector('list', M)

for (m in c(1:M)){
  W[[m]] = matrix( runif( Jm[m] * En[m] ,-1,1),Jm[m])
}
print("WWWWW:")
print(W)

Xd = t(as.matrix(read.csv("Input_csv.csv",sep=";")))
Yd = t(as.matrix(read.csv("Output_csv.csv",sep=";")))

#Xd = matrix( c(1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1), nrow=2, byrow=TRUE)
#Yd = matrix( c(1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0), nrow = 1, byrow=TRUE )

#Xd = repmat(Xd, 1, 100) # aqui aumentei 100 vezes o tamanho original
#Yd = repmat(Yd, 1, 100) # aqui aumentei 100 vezes o tamanho original
Xd[2,]=(Xd[2,]/15)-1
Xd[3,]=(Xd[3,]-650)

print("Xd : ")

print(max(Xd[3,]))
print("Yd : ")
print(Yd)

L = length(Yd)
for (epoca in 1:1000){
	for (l in 1:L){
	  X = matrix( Xd[,l] )
	#  print(l)
	  if ( l <4)
#	   print("X:")
#	   print(X)
#	   print(nrow(X))
#	   print(nrow(W[[1]]))
#	   print(ncol(W[[1]]))
	  V[[1]] = W[[1]] %*% X
	  U[[1]] = tanh( V[[1]] )
	  for (m in 2:M){
	    V[[m]] = W[[m]] %*% U[[m-1]]
	    U[[m]] = tanh( V[[m]] )
	  }
	  Y = U[[m]]
	  E = t(Y - Yd[,l]) %*% (Y - Yd[,l]) #
	  delta[[M]] = (Yd[,l]-Y) * (1/cosh(V[[M]]) )^2 #sech(z) = 1/cosh(z)
	  
	  W[[M]] = W[[M]] + eta*delta[[M]] %*% t(U[[M-1]]) #
	  for (m in M-1:1){
	    delta[[m]] = t(W[[m+1]]) %*% delta[[m+1]] * (1/cosh(V[[m]]) )^2 #sech(z) = 1/cosh(z)
	    if( m == 1)
		W[[m]] = W[[m]] + eta*delta[[m]] %*% t(X)
	    else
		W[[m]] = W[[m]] + eta*delta[[m]] %*% t(U[[m-1]]) #
	  }
	}
}
#X = matrix( c(1, 1) )
print("uhuuu")
print("colsx: ")
print(ncol(X))
print(nrow(X))
print(ncol(W[[1]]))
print(nrow(W[[1]]))

i = runif(1,1,60)
print("i:")
print(i)
X=Xd[,i]


#c(2.3,7.37,1860,0.55,641)#
U[[1]] = tanh( W[[1]] %*% X )

for (m in 2:(M)){
  a= W[[m]] %*% U[[m-1]]
  if (m==M)
    print("A")
    print(a)
  U[[m]] = tanh( a)
}
a= W[[M]] %*% U[[M-1]]


print("Saida predita")
print( a )
print("Saida esperada")
print(Yd[i])
print("erro: ")
print(Yd[i]-a)
print(" pesos finais:")
print(W)
