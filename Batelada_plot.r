batelada = read.csv(file.choose(),sep=";")
plot(batelada$epoca~batelada$EQM)
X11()
#Windows()
plot(batelada$epoca~batelada$peso)