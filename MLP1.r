# clear all
rm( list = ls() )

#Funcao de mapeamento de tanh para os valores reais
unscale<- function(xl){
	 ( (xl/0.5+1) * (max(Yd_original[1,])-min(Yd_original[1,])))/(2)+min(Yd_original[1,])
	}
eta = 0.30 	#coeficiente aprendizagem

M =2		#Numero de camadas 

Jm = c( 5, 1)	#neuronios por camadas
#plot(Jm)
En = c(  5, 5) 	#pesos/camada

V = vector('list', M)		#vetor auxiliar, armazena calculo do somatorio dos pesos* entradas
#print(V)
U = vector('list', M)           #vetor auxiliar,armazena resultado da funçao ativacao
delta = vector('list', M) 	#representa o delta minusculo
W = vector('list', M)		#Vetor de Pesos
Wfinal= vector('list', M) 
som1=vector('list', M)

# Geraçao de pesos aleatorios
set.seed(10000)
for (m in c(1:M)){
  W[[m]] = matrix( runif( Jm[m] * En[m] ,-1.00,1.00),Jm[m])
  som1[[m]]=matrix(runif( Jm[m] * En[m] ,0,0),Jm[m])  
  Wfinal[[m]]=matrix(runif( Jm[m] * En[m] ,1000,1000),Jm[m])
}
print(som1)
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
Yd[1,]=0.5*((Yd[1,]-min(Yd[1,]))*2/ (max(Yd[1,])-min(Yd[1,]))-1)


#print(Xd)
#print(Yd)
L_in =  length(Yd)
print(L_in)
L=45
ep=1000
#print(sum(head(t(Yd),60))/60)
y_ = sum( c(head(t(Yd),L)) )/L
print("Y_")
print(y_)


###########inicializao de matrizes de erros
 EE=matrix(0,ep,L)
 E=matrix(0,ep,L)
 E1=matrix(0,ep,1)
 E2=matrix(0,ep,1)
 E3=matrix(0,ep,1)
 E4=matrix(110,ep,1)
 EE1=matrix(0,ep,1)
 Erro_quad= matrix(0,ep,L)
Erro_quad2=matrix(0,ep,L_in - L)
 Ys=matrix(0,ep,L)
 Erro_abs= matrix(0,ep,L)
 erro_max= matrix(0,ep,1)
erro_max1= matrix(0,ep,1)
erro_max2= matrix(110,ep,1)
Erro_abs1= matrix(0,ep,L)
 EErro_quad=matrix(0,ep,1)
#som1=0
###################}

for (epoca in 1:ep){
	id = t(sample(L,replace=F))
#	id = seq(1,L)
#	print(id)
	for (m in c(1:M)){
  	 som1[[m]]=matrix(runif( Jm[m] * En[m] ,0,0),Jm[m])
	}
	for (l in id){  #1:L){
	  X = matrix( Xd[,l] )
	  #print(W[[1]]==W[1])
	  V[[1]] = W[[1]] %*% X
	  #print(t(V[[1]]))
	  U[[1]] = tanh( V[[1]] )
	  #print(t(U[[1]]))
	  for (m in 2:M){
	    V[[m]] = W[[m]] %*% U[[m-1]]
	    U[[m]] = tanh( V[[m]] )
	  }
	  Y = U[[m]]
	  Ys[epoca,l]=Y
	  Erro_quad[epoca,l]=   (Y - Yd[,l])^2 #t(Y - Yd[,l]) %*% (Y - Yd[,l])
	  Erro_abs[epoca,l]=t(  abs(Yd[,l]-Y)    )             
	  EE[epoca,l]=t(Yd[,l]-y_) %*% (Yd[,l]-y_)
	  
          ## calculo dos deltas
	  delta[[M]] = (Yd[,l]-Y) * (1/(cosh(V[[M]]))^2) #sech(z) = 1/cosh(z)
	  for (m in M-1:1){
	    delta[[m]] = (t(W[[m+1]]) %*% delta[[m+1]]) * (1/cosh(V[[m]]) )^2 #sech(z) = 1/cosh(z)
	  }

          ##atualizacao dos erros
          for (m in M:1)
           if( m == 1){
             W[[m]] = W[[m]] + eta*delta[[m]] %*% t(X)
              som1[[m]]=som1[[m]] + delta[[m]] %*% t(X)
	   }else{
              W[[m]] = W[[m]] + eta*delta[[m]] %*% t(U[[m-1]]) #
	       som1[[m]]=som1[[m]] + delta[[m]] %*%t(U[[m-1]])
	   }

	}
   
        ## calculo dos erros
	E1[epoca]=sum(Erro_quad[epoca,])   #soma dos erros quadrados
        E3[epoca]=mean(Erro_quad[epoca,])  #media dos erros quadrados
	E2[epoca]=mean(Erro_abs[epoca,])   # media dos erros absolutos	
	EE1[epoca]=sum(EE[epoca,])        
    	erro_max[epoca]=max(Erro_quad[epoca,])
        erro_max1[epoca]=max(Erro_abs[epoca,])
'	for (m in M:1)
           if( m == 1)
              W[[m]] = W[[m]] + (eta/L)*(som1[[m]])
           else
             W[[m]] = W[[m]] + (eta/L)*(som1[[m]]) 
'
       for ( i in (L+1):L_in){
	  X = matrix( Xd[,i] )
          V[[1]] = W[[1]] %*% X
          U[[1]] = tanh( V[[1]] )
          for (m in 2:M){
            V[[m]] = W[[m]] %*% U[[m-1]]
            U[[m]] = tanh( V[[m]] )
          }
	  Y = U[[m]]
	  Erro_quad2[epoca,i-L]=t(Y - Yd[,i]) %*% (Y - Yd[,i])
          Erro_abs1[epoca,i-L]=t(  abs(Yd[,l]-Y)    )

      }
      erro_max2[epoca]=max(Erro_abs1[epoca,])

      E4[epoca]=mean(Erro_quad2[epoca,])
      if ((E4[epoca] <= min(E4)) && ( erro_max2[epoca] <= min(erro_max2)+0.25  )){
         Wfinal = W
         print("sai") 
         print(E4[epoca])
         print(min(E4))
	 print(erro_max2[epoca] )
         print(min(erro_max2))
	 print(epoca)
     }     
}
squared_R = c(rep(0,ep))
y_ = sum(Yd[,l] ) /L
for (i in 1:ep)
  squared_R[i]=1- ( E1[i] / EE1[i] )


#Plotagem dos graficos


print(sum(E[1,]))
plot(E3,ylab="Media do erro quadrado treinamento",type='l')
plot(E4,ylab="Media do erro quadrado validacao",type='l')
plot(E2,ylab="Media do erro Absoluto",type='l')
plot(squared_R,type='l')
#plot(unscale(Ys[,41]),type='l')#,xlim=c(0,1000),ylim=c(0,1))
plot((erro_max1),ylab="Erro absoluto Maximo do Treinamento",type='l')
plot((erro_max2),ylab="Erro Absoluto Maximo da Validacao",type='l')


#X = matrix( c(1, 1) )
#print()
print(" pesos finais:")
print(W)

result=matrix(c(0),nrow=15,ncol=3)
colnames(result)<- list(c("Previsto"),c("Observado"),c("Erro"))


classifica<- function(l) {  

	U[[1]] = tanh( Wfinal[[1]] %*% matrix(Xd[,l]) )

	for (m in 2:(M)){
	  a= Wfinal[[m]] %*% U[[m-1]]
	  U[[m]] = tanh( a)
	}
       #	print(( (U[[m]]+1) * (max(Yd_original[1,])-min(Yd_original[1,])))/(2)+min(Yd_original[1,])) 
	result[l-L,1]<<-unscale((U[[m]]))
        result[l-L,2]<<-Yd_original[,l]
        result[l-L,3]<<-result[l-L,2]-result[l-L,1] 
}

for ( i in 46:60)
  classifica(i)

print(result)
unscale(min(Yd[1,])) == min(Yd_original[1,])
unscale(max(Yd[1,])) ==max(Yd_original[1,]) 
#print(min(erro_min))
#print(unscale(min(erro_min)))

print(min(erro_max))
print(unscale(min(erro_max)))
unscale(0)
unscale(0.54)
#a= W[[M]] %*% U[[M-1]]
#print("U")
#print( U[m] )
#print(o==W[[1]])
#print("U")
#print(U)
