import math


for i in xrange(0,20):
	x[i]= ((i/10)*3.14)


eta=2.8
A=0
w0=0
N=7
b=0.35




for epoca in xrange(0,75):
	dw=eta/N
	sum1=0
	for i in xrange(0,N):
		aux=(y[i]-(x[i]*w0))*x[i]
		print aux
		sum1+=aux
	print "soma: ",sum1
	dw=dw*sum1
#	for i in xrange(0,N):
	w0=w0+dw
	print w0,dw
print w0

