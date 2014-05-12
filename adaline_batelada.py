x=[0,0.1,0.2,0.3,0.4,0.5,0.6]
y=[0,0.035,0.07,0.105,0.14,0.175,0.21]
eta=0.7
A=0
peso=0
N=7
b=0.35
for epoca in xrange(0,75):
#dw=eta/N
	somatorio_erro_quadrado=0
	somatorio_erro_x=0
	for i in xrange(0,N):
		erro=(y[i]-(x[i]*peso))
		somatorio_erro_quadrado+=pow(erro,2)
		erro_x=erro*x[i]
		somatorio_erro_x+=erro_x
#		print "erro: ",erro," som: ",somatorio_erro_quadrado
	dw=(eta/N)*somatorio_erro_x
	eqm=(1/(2.0*N))*somatorio_erro_quadrado
#	for i in xrange(0,N):
	peso=peso+dw
#	if ((epoca+1)%3==0):
		print epoca,"peso:",peso,"EQM:",eqm
#print w0

