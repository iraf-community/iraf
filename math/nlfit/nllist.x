# NL_LIST -- Procedure to order the list used when the NLFIT structure
# is initialized.

procedure nl_list (list, nlist, nfit)

int	list[ARB]		# list
int	nlist			# number of elements in the list
int	nfit			# number of active list elments

int	i, j, nfitp1, ifound

begin
	nfitp1 = nfit + 1

	do i = 1, nlist {
	    ifound = 0
	    do j = 1, nfit {
		if (list[j] == i)
		    ifound = ifound + 1
	    }
	    if (ifound == 0) {
		list[nfitp1] = i
		nfitp1 = nfitp1 + 1
	    } else if (ifound > 1)
	        call error (0, "Incorrect parameter ordering in plist")
	}

	if (nfitp1 != (nlist + 1))
	    call error (0, "Incorrect parameter ordering in plist")
end
