# MKLIST - Make a fiber list.

int	nfibers
real	width, sep, flux
file	temp

#nfibers = 300
#width = 2.0
#sep = 4.9
nfibers = j
width = x
sep = y

temp = mktemp ("tmp")
urand (nfibers, 1, ndigits=4, seed=1, scale_factor=0.5, > temp)
list = temp

for (i=1; i<=nfibers; i+=1) {
    if (fscan (list, flux) == EOF)
	break
    flux = 0.75 + flux
    printf ("%d %d %5.3f gauss %4.1f 0 %6.1f .002\n", i, mod(i,2),
	flux, width, sep*(i+1))
}

list = ""
delete (temp, verify=no)
