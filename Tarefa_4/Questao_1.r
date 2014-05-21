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
        ifelse(x<teta,-1,ifelse(x==teta,0,1))
}

epoca=0
while(epoca<10){
	erro_count=0
	for(j in seq(1,lines)){
	#	print(j)
	
		somatorio_erro_quadrado=0
		somatorio_xiwi=0
		for(i in seq(1,att)){
			x=w[i]*table[j,i]
			somatorio_xiwi=somatorio_xiwi+x;
		}
		cat(somatorio_xiwi,"-> sm\n");
		y=tshld(somatorio_xiwi)
#		print(y)
		if (y!= table[j,4]){
			cat(y,table[j,4],"entrou\npesos: ");
			erro_count=erro_count+1;
			w[1]=w[1]+ table[j,1]*eta*table[j,4]
			w[2]=w[2]+ table[j,2]*eta*table[j,4]
			w[3]=w[3]+ table[j,3]*eta*table[j,4]
		}
#		j=(j+1)%%4;
	}
	print(w)	
	epoca=epoca+1;
}


