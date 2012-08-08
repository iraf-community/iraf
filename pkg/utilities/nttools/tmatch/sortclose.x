#* HISTORY *
#* B.Simon	25-Aug-94	original
#  Phil Hodge	12-Jul-2005	in sortclose, add 'int cmpclose()'

# SORTCLOSE -- Sort the closest array 

procedure sortclose (nclosest, closest, index)

int	nclosest	# i: length of closest and index arrays
int	closest[ARB]	# i: indices of second table rows matching first
int	index[ARB]	# u: indices of first table rows, in sort order on exit
#--
int	cmpclose()
extern	cmpclose
pointer	sp, close2

begin
	call smark (sp)
	call salloc (close2, nclosest, TY_INT)

	call amovi (closest, Memi[close2], nclosest)
	call gqsort (index, nclosest, cmpclose, close2)

	call sfree (sp)
end

# CMPCLOSE -- Compare two elements in the close array

int procedure cmpclose (closest, ielem, jelem)

pointer	closest		# address of the closest array
int	ielem		# first element
int	jelem		# second element
#--
int	order

begin
	if (Memi[closest+ielem-1] < Memi[closest+jelem-1]) {
	    order = -1
	} else if (Memi[closest+ielem-1] > Memi[closest+jelem-1]) {
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
