# HDIC_SORT -- sort the log exposure, density and weight information in order
# of increasing density value.  The sorting is done is place.  The four
# data values are assummed matched on input, that is, exposure[i] matches
# density[i] with weight[i] (and userwts[i]) for all array entries.

procedure hdic_sort (density, exposure, weights, userwts, whydel, sdev, nvals)

double	density[nvals]		# Density array
double	exposure[nvals]		# Log exposure array
double	weights[nvals]		# Weights array
double	userwts[nvals]		# Reference weights array
int	whydel[nvals]		# Flag array of reasons for deletion
double	sdev[nvals]		# Array of standard deviations
int	nvals			# Number of values to sort

int	i, j
double	temp
define	swap	{temp=$1;$1=$2;$2=temp}
int	itemp
define	iswap   {itemp=$1;$1=$2;$2=itemp}

begin
	# Bubble sort - inefficient, but sorting is done infrequently
	# an expected small sample size (16 pts typically). 

	for (i = nvals; i > 1; i = i - 1)
	    for (j = 1; j < i; j = j + 1) 
		if (density [j] > density[j+1]) {

		    # Out of order; exchange values
		    swap (exposure[j], exposure[j+1])
		    swap ( density[j],  density[j+1])
		    swap ( weights[j],  weights[j+1])
		    swap ( userwts[j],  userwts[j+1])
		   iswap (  whydel[j],   whydel[j+1])
		    swap (    sdev[j],     sdev[j+1])
		}
end
