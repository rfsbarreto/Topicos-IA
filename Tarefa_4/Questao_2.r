table = read.csv("input_q2.csv",header=F,sep=";")
#table
lines=18
att=63
y = c(1,-1,-1,-1,-1,-1,-1,1,-1,-1,-1,-1,-1,-1,1,-1,-1,-1,-1,-1,-1)

bias = 1
teta=0
w = c(rep(0,64))

tshld <- function(x){
        ifelse(x<(-1*teta),-1,ifelse(x>teta,1,0))
}

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
			w[64]=w[64]+ table[j,k]*eta*y[j]
		}
#		cat("\t\tpesos passo",j,"-> ", w,"\n");
	}
#	cat("pesos por epoca ")  

#	print(w)	
	epoca=epoca+1;

}

print(w)

