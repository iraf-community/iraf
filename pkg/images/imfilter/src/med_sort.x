# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MED_ASHSRT -- Sort a real array in increasing order using the shell sort.

procedure med_ashsrt (a, npts)

real	a[ARB]			#U array to be sorted
int	npts			#I number of points

real	temp
int	j, k, d
define	swap {temp=$1;$1=$2;$2=temp}

begin
	switch (npts) {
	case 1:
	    ;
	case 2:
	    if (a[1] > a[2])
		swap (a[1], a[2])

	case 3:
	    if (a[1] > a[2])
		swap (a[1], a[2])
	    if (a[1] > a[3])
		swap (a[1], a[3])
	    if (a[2] > a[3])
		swap (a[2], a[3])

	case 4:
	    if (a[1] > a[2])
		swap (a[1], a[2])
	    if (a[1] > a[3])
		swap (a[1], a[3])
	    if (a[1] > a[4])
		swap (a[1], a[4])
	    if (a[2] > a[3])
		swap (a[2], a[3])
	    if (a[2] > a[4])
		swap (a[2], a[4])
	    if (a[3] > a[4])
		swap (a[3], a[4])

	case 5:
	    if (a[1] > a[2])
		swap (a[1], a[2])
	    if (a[1] > a[3])
		swap (a[1], a[3])
	    if (a[1] > a[4])
		swap (a[1], a[4])
	    if (a[1] > a[5])
		swap (a[1], a[5])
	    if (a[2] > a[3])
		swap (a[2], a[3])
	    if (a[2] > a[4])
		swap (a[2], a[4])
	    if (a[2] > a[5])
		swap (a[2], a[5])
	    if (a[3] > a[4])
		swap (a[3], a[4])
	    if (a[3] > a[5])
		swap (a[3], a[5])
	    if (a[4] > a[5])
		swap (a[4], a[5])

	default:
	    for (d = npts;  d > 1;  ) {
		if (d < 5)
		    d = 1
		else
		    d = (5 * d - 1) / 11
		do j = npts - d, 1, -1 {
		    temp = a[j]
		    do k = j + d, npts, d {
			if (temp <= a[k])
			    break
			a[k-d] = a[k]
		    }
		    a[k-d] = temp
		}
	    }
	}
end


# MED_GSHSRT -- Procedure to sort the indices of an array using the shell
# sort.

procedure med_gshsrt (a, index, npts)

real	a[ARB]			#I array to be sorted
int	index[ARB]		#O array of indices
int	npts			#I number of points in the array

int	j, k
int	d, temp
define	swap {temp=$1;$1=$2;$2=temp}

begin
	switch (npts) {
	case 1:
	    ;
	case 2:
	    if (a[index[1]] > a[index[2]])
		swap (index[1], index[2])

	case 3:
	    if (a[index[1]] > a[index[2]])
		swap (index[1], index[2])
	    if (a[index[1]] > a[index[3]])
		swap (index[1], index[3])
	    if (a[index[2]] > a[index[3]])
		swap (index[2], index[3])

	case 4:
	    if (a[index[1]] > a[index[2]])
		swap (index[1], index[2])
	    if (a[index[1]] > a[index[3]])
		swap (index[1], index[3])
	    if (a[index[1]] > a[index[4]])
		swap (index[1], index[4])
	    if (a[index[2]] > a[index[3]])
		swap (index[2], index[3])
	    if (a[index[2]] > a[index[4]])
		swap (index[2], index[4])
	    if (a[index[3]] > a[index[4]])
		swap (index[3], index[4])

	case 5:
	    if (a[index[1]] > a[index[2]])
		swap (index[1], index[2])
	    if (a[index[1]] > a[index[3]])
		swap (index[1], index[3])
	    if (a[index[1]] > a[index[4]])
		swap (index[1], index[4])
	    if (a[index[1]] > a[index[5]])
		swap (index[1], index[5])
	    if (a[index[2]] > a[index[3]])
		swap (index[2], index[3])
	    if (a[index[2]] > a[index[4]])
		swap (index[2], index[4])
	    if (a[index[2]] > a[index[5]])
		swap (index[2], index[5])
	    if (a[index[3]] > a[index[4]])
		swap (index[3], index[4])
	    if (a[index[3]] > a[index[5]])
		swap (index[3], index[5])
	    if (a[index[4]] > a[index[5]])
		swap (index[4], index[5])

	default:
	    for (d = npts;  d > 1;  ) {
		if (d < 5)
		    d = 1
		else
		    d = (5 * d - 1) / 11
		do j = npts - d, 1, -1 {
		    temp = index[j]
		    do k = j + d, npts, d {
			if (a[temp] <= a[index[k]])
			    break
			index[k-d] = index[k]
		    }
		    index[k-d] = temp
		}
	    }
	}
end
