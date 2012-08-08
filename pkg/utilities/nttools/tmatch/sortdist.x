#* HISTORY *
#* B.Simon	25-Aug-94	original
#  Phil Hodge	28-Sept-2005	in sortdist, add 'int cmpdist()'

# SORTDIST -- Sort the dist array 

procedure sortdist (ndist, dist, index)

int	ndist		# i: length of dist and index arrays
double	dist[ARB]	# i: indices of second table rows matching first
int	index[ARB]	# u: indices of first table rows, in sort order on exit
#--
int	cmpdist()
extern	cmpdist
pointer	sp, dist2

begin
	call smark (sp)
	call salloc (dist2, ndist, TY_DOUBLE)

	call amovd (dist, Memd[dist2], ndist)
	call gqsort (index, ndist, cmpdist, dist2)

	call sfree (sp)
end

# CMPDIST -- Compare two elements in the dist array

int procedure cmpdist (dist, ielem, jelem)

pointer	dist		# address of the dist array
int	ielem		# first element
int	jelem		# second element
#--
int	order

begin
	if (Memd[dist+ielem-1] < Memd[dist+jelem-1]) {
	    order = -1
	} else if (Memd[dist+ielem-1] > Memd[dist+jelem-1]) {
	    order = 1
	} else if (ielem < jelem) {
	    order = -1
	} else if (ielem < jelem) {
	    order = 1
	} else {
	    order = 0
	}
	return (order)
end
