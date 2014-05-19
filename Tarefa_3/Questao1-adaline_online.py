x=[0,0.1,0.2,0.3,0.4,0.5,0.6]
y=[0,0.035,0.07,0.105,0.14,0.175,0.21]
eta=0.1
A=0
peso=0
N=7
b=0.35
for epoca in xrange(0,75):
#	print "Epoca:",epoca,"\n"
	dw=eta
	somatorio_erro_quadrado=0
	for i in xrange(0,N):
		erro=(y[i]-(x[i]*peso))
		erro_x=erro*x[i]
#		print aux
		somatorio_erro_quadrado+=pow(erro,2)	
		dw=eta*erro_x
#	for i in xrange(0,N):
		peso+=dw
#		if ((epoca+1)%3==0):
	
	eqm=(1/(2.0*N))*somatorio_erro_quadrado
#	if ((epoca+1)%3==0):
	print epoca,peso,eqm
