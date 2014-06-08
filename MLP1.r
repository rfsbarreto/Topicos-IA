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

eta = 0.1 	#coeficiente aprendizagem

M =2		#Numero de camadas 

Jm = c( 5, 1)	#neuronios por camadas
#plot(Jm)
En = c(  5, 5) 	#pesos/camada

V = vector('list', M)		#vetor auxiliar, armazena calculo do somatorio dos pesos* entradas
#print(V)
U = vector('list', M)           #vetor auxiliar,armazena resultado da funçao ativacao
delta = vector('list', M) 	#representa o delta minusculo
W = vector('list', M)		#Vetor de Pesos

# Geraçao de pesos aleatorios
set.seed(10000)
for (m in c(1:M)){
  W[[m]] = matrix( runif( Jm[m] * En[m] ,-0.2,0.2),Jm[m])
}

o=W[[1]]

#leitura de dados
Xd_original = t(as.matrix(read.csv("Input_csv.csv",sep=";")))
Yd_original = t(as.matrix(read.csv("Output_csv.csv",sep=";")))

Xd=Xd_original
Yd=Yd_original

### normalizaçao dos dados 
Xd[1,]= (Xd[1,]-min(Xd[1,])) /(max(Xd[1,])-min(Xd[1,]))
Xd[2,]= (Xd[2,]-min(Xd[2,])) /(max(Xd[2,])-min(Xd[2,]))
Xd[3,]=(Xd[3,]-min(Xd[3,])) /(max(Xd[3,])-min(Xd[3,]))
Xd[4,]=(Xd[4,]-min(Xd[4,])) /(max(Xd[4,])-min(Xd[4,]))
Xd[5,]=(Xd[5,]-min(Xd[5,])) /(max(Xd[5,])-min(Xd[5,]))
Yd[1,]=(Yd[1,]-min(Yd[1,]))*2/ (max(Yd[1,])-min(Yd[1,]))-1


print(Xd)
print(Yd)
L =  length(Yd)
#L=45
ep=7
y2=sum(Yd)
y3=sum(head(t(Yd),L))
print(y2)
print(y3)
y_ = sum(head(t(Yd),n=L) )/L
print("Y_")
print(y_)
EE=matrix(0,ep,L)
E=matrix(0,ep,L)
E1=matrix(0,ep,1)
E2=matrix(0,ep,1)
E3=matrix(0,ep,1)
EE1=matrix(0,ep,1)
Erro_quad= matrix(0,ep,L)
Erro_abs= matrix(0,ep,L)
erro_min= matrix(0,ep,1)
erro_max= matrix(0,ep,1)
EErro_quad=matrix(0,ep,1)


for (epoca in 1:ep){
#	id = t(sample(L,replace=F))
	id = seq(1,L)
#	print(id)
	for (l in 1:L){  #1:L){
	  X = matrix( Xd[,l] )
	  V[[1]] = W[[1]] %*% X
	  #print(t(V[[1]]))
	  U[[1]] = tanh( V[[1]] )
	  #print(t(U[[1]]))
	  for (m in 2:M){
	    V[[m]] = W[[m]] %*% U[[m-1]]
	    U[[m]] = tanh( V[[m]] )
	  }
	  Y = U[[m]]
	  Erro_quad[epoca,l]=t(Y - Yd[,l]) %*% (Y - Yd[,l])
	 # Erro_quad[epoca,l]=Erro_quad[epoca,l]/2
	  Erro_abs[epoca,l]=t(  abs(Yd[,l]-Y)    )             
	  EE[epoca,l]=t(Yd[,l]-y_) %*% (Yd[,l]-y_)
	  delta[[M]] = (Yd[,l]-Y) * (1/cosh(V[[M]]) )^2 #sech(z) = 1/cosh(z)
	  #print("deltaM:")
	  #print(delta[[M]]) 
	  aux= 2*eta*delta[[M]] %*% t(U[[M-1]]) #
	 # print(aux)
	 # print(W[[M]])	
	  W[[M]] =W[[M]] + aux
	  for (m in M-1:1){
	    teste=0.0
	    #for ( p 
	    #teste=delta[M]*W
	    delta[[m]] = t(W[[m+1]]) %*% delta[[m+1]] * (1/cosh(V[[m]]) )^2 #sech(z) = 1/cosh(z)
	    if( m == 1)
		W[[m]] = W[[m]] + 2*eta*delta[[m]] %*% t(X)
	    else
		W[[m]] = W[[m]] + 2*eta*delta[[m]] %*% t(U[[m-1]]) #
	  }
	  #print("Y")
	  #print(delta)
	}
#	EErro_quad[epoca]=sum(Erro_quad[epoca,])
	E1[epoca]=sum(Erro_quad[epoca,])   #soma dos erros quadrados
        E3[epoca]=mean(Erro_quad[epoca,]) 
	E2[epoca]=mean(Erro_abs[epoca,])   # media dos erros absolutos	
	EE1[epoca]=sum(EE[epoca,])
	erro_min[epoca]=min(Erro_quad[epoca,])
    	erro_max[epoca]=max(Erro_quad[epoca,])
}

squared_R = c(rep(0,ep))
y_ = sum(Yd[,l] ) /L
for (i in 1:ep)
  squared_R[i]=1- ( E1[i] / EE1[i] )
print("vai ser true!")
#print(Erro_quad[,L])
print(sum(E[1,]))
plot(Erro_quad[,L],type='l')
plot(E3,ylab="Media do erro quadrado",type='l')
plot(E2,ylab="Media do erro Absoluto",type='l')
plot(squared_R,type='l')
plot(erro_min,type='l')
plot(erro_max,type='l')

#X = matrix( c(1, 1) )
#print()
print(" pesos finais:")
print(W)

classifica<- function(l) {       #X=c(2.3,7.37,1860,0.55,641)#c(0,30.7,5300,0.5,661)
#	X[1]= (X[1]-min(Xd_original[1,])) /(max(Xd_original[1,])-min(Xd_original[1,]))
#	X[2]= (X[2]-min(Xd_original[2,])) /(max(Xd_original[2,])-min(Xd_original[2,]))
#	X[3]=(X[3]-min(Xd_original[3,])) /(max(Xd_original[3,])-min(Xd_original[3,]))
#	X[4]=(X[4]-min(Xd_original[4,])) /(max(Xd_original[4,])-min(Xd_original[4,]))
#	X[5]=(X[5]-min(Xd_original[5,])) /(max(Xd_original[5,])-min(Xd_original[5,]))
#	Y[1]=(Y[1]-min(Yd_original[1,]))*2/ (max(Yd_original[1,])-min(Yd_original[1,]))-1

	U[[1]] = tanh( W[[1]] %*% matrix(Xd[,l]) )

	for (m in 2:(M)){
	  a= W[[m]] %*% U[[m-1]]
	 # if (m==M)
	  #  print("A")
	  #  print(a)
	  U[[m]] = tanh( a)
	}
	print("saidas:")
#	print(U)
	print(" Tamanho da particula")
	print(( (U[[m]]+1) * (max(Yd_original[1,])-min(Yd_original[1,])))/(2)+min(Yd_original[1,])) 
	
}

for ( i in 46:60)
  classifica(i)
#a= W[[M]] %*% U[[M-1]]
#print("U")
#print( U[m] )
#print(o==W[[1]])
#print("U")
#print(U)
