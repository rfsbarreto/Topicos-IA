from math import sin,cos,pi
x0,x1,x2,x3=[],[],[],[]
y0,y1,y2,y3=[],[],[],[]
resultados=[]
for i in xrange(0,20):
	x0.append(i*pi/10)
	x1.append(i*pi/10)
	x2.append(i*pi/10)
	x3.append(i*pi/10)
	y0.append(-1)
	y1.append(sin(x1[i]))
	y2.append(cos(x1[i]))
	y3.append(x1[i])
	resultados.append(-pi+0.565*sin(x1[i])+2.657*cos(x2[i])+0.674*x3[i])	
peso0,peso1,peso2,peso3=0,0,0,0
N=8
eta=0.1
#print "result:",resultados,"\n"
#print "x1:",x1,"\nx2:",x2,"\nx3:",x3,"x0\n",x0
#print "y1",y1,"\ny2",y2,"\ny3",y3,"y0\n",y0
#valores corretos
a1,a2,a3,a0=0.565,2.657,0.674,-pi


for epoca in xrange(0,750):
	somatorio_erro_quadrado=0
        somatorio_erro_x=0
	somatorio_erro_x0=0	
	#funcao 1
	print "Epoca",epoca,"\n"     	 
	for i in xrange(0,20):
#:		print "pesos:",peso0,peso1,peso2,peso3
                erro=(resultados[i]-(y1[i]*peso1+y2[i]*peso2+y3[i]*peso3-peso0))
		somatorio_erro_quadrado+=pow(erro,2)
                erro_x1=erro*y1[i]
		erro_x0=erro*y0[i]
		erro_x2=erro*y2[i]
		erro_x3=erro*y3[i]
      #          somatorio_erro_x+=erro_x
 #               print i,erro,y1[i],y2[i],y3[i],y0[i]
     #           somatorio_erro_x0+=erro_x0
	       # print "erro: ",erro," som: ",somatorio_erro_quadrado
       		dw1=eta*erro_x1
		dw0=eta*erro_x0
		dw2=eta*erro_x2
		dw3=eta*erro_x3
       		peso1+=dw1
        	peso2+=dw2
       		peso3+=dw3
        	peso0+=dw0
	eqm=(1/(2.0*N))*somatorio_erro_quadrado
	print "  pesos:  peso0->",peso0,"peso1->",peso1,"peso2->",peso2,"peso3->",peso3 
	print "  EQM:",eqm,"\n"
#       for i in xrange(0,N):
#        if ((epoca+1)%3==0):
	#        print epoca,"peso0:",peso0,"peso1:",peso1,"peso2",peso2,"peso3",peso3,"EQM:",eqm
#print w0



