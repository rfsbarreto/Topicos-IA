x=[0,0.1,0.2,0.3,0.4,0.5,0.6]
y=[0,0,0,0,0,0,0]
eta=1
teta=0.2
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
	if ((epoca+1)%3==0):
		print epoca,"peso:",peso,"EQM:",eqm
#print w0



















'''
n=7
x = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6)
y = c(0,0.035,0.070,0.105,0.140,0.175,0.210)

EQMs = c(rep(0,175))
ws = c(rep(0,175))

w = c(rep(0,n))
saida = c(rep(0,n))
erro = c(rep(0,n))
dw = c(rep(0,n))

a = 0.2
precisao = 0

u <- function(x,w){
  sum(x*w)
} 

Thresh <- function(x,t){
	ifelse(x<t,-1,ifelse(x==t)0,1)
}

EQM <- function(y,w,n){
  (sum((y-w)^2))*(n/2)
}

w[1] = 0
saida[1] = w[1]*x[1]
erro[1] = y[1]-saida[1]
dw[1] = a*x[1]*erro[1]

EqmAnterior = EQM(y,w[1],n)
EqmAtual = 0
print(EqmAtual)
times = 0
epoca = 0
eqmc = 0
wc = 0
while(times < 2)
{
  EqmAnterior = EQM(y,w[1],n)
  ws[wc] = w[1]
  wc = wc + 1
  w[1] = w[n]+dw[n]
  saida[1] = w[1]*x[1]
  erro[1] = y[1]-saida[1]
  dw[1] = a*x[1]*erro[1]
  
  for(i in 2:n){
    ws[wc] = w[1]
    wc = wc + 1
    w[i] = w[i-1]+dw[i-1]
    saida[i] = w[i]*x[i]
    erro[i] = y[i]-saida[i]
    dw[i] = a*x[i]*erro[i]
  }
  
  ws[wc] = w[1]
  wc = wc + 1

  EqmAtual = EQM(y,w[1],n)
  EQMs[eqmc] = abs(EqmAtual - EqmAnterior)
  eqmc = eqmc+1
  if(abs(EqmAtual - EqmAnterior) <= precisao){
    times = times + 1
  }
  epoca = epoca + 1
}
'''
