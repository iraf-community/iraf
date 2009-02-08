# MK_REMOVE -- Check the deletions for uniqueness and delete unwanted objects
# from the coordinates file.

procedure mk_remove (coords, deletions, cl, dl, ndelete)

char	coords[ARB]	# coordinate file name
char	deletions[ARB]	# deletions file name
int	cl		# coordinate file descriptor
int	dl		# deletions file descriptor
int	ndelete		# number of deletions

size_t	sz_val
int	i, ndel, nobj, obj, tcl, tdl, stat
long	l_val
pointer	sp, id, tclname, tdlname, line
real	xval, yval
int	fscan(), nscan(), open(), getline()

begin
	call smark (sp)
	sz_val = ndelete
	call salloc (id, sz_val, TY_INT)
	sz_val = SZ_FNAME
	call salloc (tclname, sz_val, TY_CHAR)
	call salloc (tdlname, sz_val, TY_CHAR)
	sz_val = SZ_LINE
	call salloc (line, sz_val, TY_CHAR)

	# Rewind both files to the beginning.
	l_val = BOF
	call seek (cl, l_val)
	call seek (dl, l_val)

	# Read in the ids of objects to be deleted.
	ndel = 0
	while (fscan (dl) != EOF) {
	    call gargi (Memi[id+ndel])
	    ndel = ndel + 1
	}

	# Sort the id numbers.
	sz_val = ndelete
	call asrti (Memi[id], Memi[id], sz_val)

	# Remove id numbers that are not unique.
	ndel = 1
	do i = 2, ndelete {
	    if (Memi[id+i-1] == Memi[id+i-2])
		next
	    ndel = ndel + 1
	    Memi[id+ndel-1] = Memi[id+i-1]
	}

	# Open two temporary files.
	call mktemp ("tcl", Memc[tclname], SZ_FNAME)
	call mktemp ("tdl", Memc[tdlname], SZ_FNAME)
	tcl = open (Memc[tclname], NEW_FILE, TEXT_FILE)
	tdl = open (Memc[tdlname], NEW_FILE, TEXT_FILE)

	nobj = 0
	do i = 1, ndel {

	    obj = Memi[id+i-1]

	    repeat {

		stat = getline (cl, Memc[line])
		if (stat == EOF)
		    break

		call sscan (Memc[line])
	        call gargr (xval)
	        call gargr (yval)
	        if (nscan () < 2) {
		    call putline (tcl, Memc[line])
		    next
		}

	        nobj = nobj + 1
		if (nobj < obj)
		    call putline (tcl, Memc[line])
		else
		    call putline (tdl, Memc[line])

	    } until (nobj >= obj)
	}

	# Copy the remainder of the file.
	while (getline (cl, Memc[line]) != EOF)
	    call putline (tcl, Memc[line])

	# Cleanup the coords file.
	call close (cl)
	call close (tcl)
	call delete (coords)
	call rename (Memc[tclname], coords)

	# Cleanup the delete file.
	call close (dl)
	call close (tdl)
	call delete (deletions)
	call rename (Memc[tdlname], deletions)

	call sfree (sp)
end
