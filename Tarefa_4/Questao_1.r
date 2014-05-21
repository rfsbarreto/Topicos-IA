table=read.csv("inputs_questao1.csv",sep=";")
table[1,4]
table
w=c(0,0,0)

eta=1
teta=0.2
A=0
peso=0
lines=4
att=3
erro_count=1

tshld <- function(x){
        ifelse(x<(-1*teta),-1,ifelse(x>teta,1,0))
}

epoca=0
while(epoca<10){
	erro_count=0
	cat("Época ",epoca,":\n\n");
	for(j in seq(1,lines)){
		somatorio_xiwi=0
		for(i in seq(1,att)){
			x=w[i]*table[j,i]
			somatorio_xiwi=somatorio_xiwi+x;
		}
		y=tshld(somatorio_xiwi)
	
		if (y!= table[j,4]){
			erro_count=erro_count+1;
			w[1]=w[1]+ table[j,1]*eta*table[j,4]
			w[2]=w[2]+ table[j,2]*eta*table[j,4]
			w[3]=w[3]+ table[j,3]*eta*table[j,4]
		}
		cat("\t\tpesos passo",j,"-> ", w,"\n");
	}
	cat("pesos por epoca ")  

	print(w)	
	epoca=epoca+1;
}

testar = function(t1,t2) {
    t = c(t1,t2,1)
    saida = 0
    for(i in 1:3){
        saida = saida+ w[i]*t[i]
    }
    
    return(tshld(saida))
}

cat("testando:\n")
testar(1,1)
testar(1,0)
testar(0,1)
testar(0,0)

