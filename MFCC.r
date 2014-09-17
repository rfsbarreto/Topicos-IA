# clear all
rm( list = ls() )

## Inputs

Tw = 25          # analysis frame duration (ms)
Ts = 10          # analysis frame shift (ms)
alpha = 0.97     # preemphasis coefficient
NFFT = 512
samplerate=16000
Lfr =  300
Ufr =  8000   	 # frequency range to consider
M = 26           # number of filterbank channels 
C = 13           # number of cepstral coefficients
L = 22           # cepstral sine lifter parameter

## Helpers

split.overlap <- function(s, Tw, TS) {
  starts = seq(1, length(s)-Tw+1, by=Tw-TS)
  ends   = starts + Tw - 1
  sapply(1:length(starts), function(i) s[starts[i]:ends[i]])
}

hzToMel<- function(hz){ (1127*log(1+hz/700) )}
melToHz<- function(mel){ ( 700*exp(mel/1127)-700 )}

powspec <- function(frames) {
    complex_spec = rfft(frames)
	magSpec = abs(complex_spec)
	1/NFFT * magSpec^2}
	
	
	
melFilterBank <- function(signal){

lowmel = hzToMel(Lfr)
highmel = hzToMel(Ufr)
melpoints = seq(lowmel,highmel,length = M+2)
bin = floor((NFFT+1)*melToHz(melpoints)/samplerate)
print(bin)
fbank = matrix(0,M,(NFFT/2)+1)
    for (j in 1:M ) {

        for (i in as.integer(bin[j]):as.integer(bin[j+1])){
            fbank[j,i] = (i - bin[j])/(bin[j+1]-bin[j])}
        for (i in as.integer(bin[j+1]):as.integer(bin[j+2])){
            fbank[j,i] = (bin[j+2]-i)/(bin[j+2]-bin[j+1])}
	}
    fbank
}


#Etapas para MFCC
#
#
#1. Dividir o sinal em frames:

#Carregar amostras
speech <- scan(file.choose())

#Divisao



#2: Aplicar FFT
fourriedSpeech = powspec(split.overlap(speech,Tw,Ts))

#3: Computar Mel-Filterbanks
melBanks = melFilterBank(fourriedSpeech)
#4: Extrair energias do FB
energySpeech = sum(fourriedSpeech)
#extrair Feat
featSpeech = head((fourriedSpeech),(NFFT/2)+1) %*% t(melBanks)
featSpeech = log(featSpeech)
#aplicar DCT
featSpeech = DCT(featSpeech,type=1)	


	