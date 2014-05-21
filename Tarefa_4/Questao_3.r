table = read.csv("input_q2.csv",header=F,sep=";")
#table
lines=18
att=63
y1 = c(1,-1,-1,-1,-1,-1,-1,
	1,-1,-1,-1,-1,-1,-1,
	1,-1,-1,-1,-1,-1,-1)
y2 = c(-1,1,-1,-1,-1,-1,-1,
	-1,1,-1,-1,-1,-1,-1,
	-1,1,-1,-1,-1,-1,-1)
y3 = c(-1,-1,1,-1,-1,-1,-1,
	-1,-1,1,-1,-1,-1,-1,
	-1,-1,1,-1,-1,-1,-1)
y4 = c(-1,-1,-1,1,-1,-1,-1,
	-1,-1,-1,1,-1,-1,-1,
	-1,-1,-1,1,-1,-1,-1)
y5 = c(-1,-1,-1,-1,1,-1,-1,
	-1,-1,-1,-1,1,-1,-1,
	-1,-1,-1,-1,1,-1,-1)
y6 = c(-1,-1,-1,-1,-1,1,-1,
	-1,-1,-1,-1,-1,1,-1,
	-1,-1,-1,-1,-1,1,-1)
y7 = c(-1,-1,-1,-1,-1,-1,1,
	-1,-1,-1,-1,-1,-1,1,
	-1,-1,-1,-1,-1,-1,1)

bias = 1
teta=0
w0 = c(rep(0,64))

tshld <- function(x){
        ifelse(x<(-1*teta),-1,ifelse(x>teta,1,0))
}

perceptron_letras<-function(letra,y,w){
eta = 1
n = 7

epoca = 0
epocaMax = 100
while(epoca < epocaMax)
{
	erro_count=0
#	cat("Época ",epoca,":\n\n");
	for(j in seq(1,lines)){
		somatorio_xiwi=0
		for(i in seq(1,att)){
			x=w[i]*table[j,i]
			somatorio_xiwi=somatorio_xiwi+x;
		}
		y1=tshld(somatorio_xiwi)

		if (y1 != y[j]){
			erro_count=erro_count+1;
			for(k in seq(1,63)){
				w[k]=w[k]+ table[j,k]*eta*y[j]
			}
			w[64]=w[64]+ eta*y[j]
		}
#		cat("\t\tpesos passo",j,"-> ", w,"\n");
	}
#	cat("pesos por epoca ")  

#	print(w)	
	epoca=epoca+1;

}
cat("Pesos da letra ",letra," ao final de 100 epócas: \n")
cat(w,"\n");
return(w)
}

w1 = perceptron_letras("A",y1,w0)
w2=perceptron_letras("B",y2,w0)
w3=perceptron_letras("C",y3,w0)
w4=perceptron_letras("D",y4,w0)
w5=perceptron_letras("E",y5,w0)
w6=perceptron_letras("F",y6,w0)
w7=perceptron_letras("G",y7,w0)

#w1

wfinal= data.frame(w1,w2,w3,w4,w5,w6,w7)
wfinal
classificador<- function(entrada){
	saida=c(rep(0,7))
#	print(entrada)
	for (i in 1:7){
		saida[i]=sum(entrada*wfinal[i])
#		print(saida)
		saida[i]=tshld(saida[i])
	}
#	print(saida)	
	return(saida)

}

#teste
ent= data.frame(c(-1,-1,1,1,-1,-1,-1,-1,-1,-1,1,-1,-1,-1,-1,-1,-1,1,-1,-1,-1,-1,-1,1,-1,1,-1,-1,-1,-1,1,-1,1,-1,-1,-1,1,1,1,1,1,-1,-1,1,-1,-1,-1,1,-1,-1,1,-1,-1,-1,1,-1,1,1,1,-1,1,1,1,1))
cat("Teste para Letra A, saida: ")
classificador(ent)

