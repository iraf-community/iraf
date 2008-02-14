# MK_REMOVE -- Check the deletions for uniqueness and delete unwanted objects
# from the coordinates file.

procedure mk_remove (coords, deletions, cl, dl, ndelete)

char	coords[ARB]	# coordinate file name
char	deletions[ARB]	# deletions file name
int	cl		# coordinate file descriptor
int	dl		# deletions file descriptor
int	ndelete		# number of deletions

int	i, ndel, nobj, obj, tcl, tdl, stat
pointer	sp, id, tclname, tdlname, line
real	xval, yval
int	fscan(), nscan(), open(), getline()

begin
	call smark (sp)
	call salloc (id, ndelete, TY_INT)
	call salloc (tclname, SZ_FNAME, TY_CHAR)
	call salloc (tdlname, SZ_FNAME, TY_CHAR)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Rewind both files to the beginning.
	call seek (cl, BOF)
	call seek (dl, BOF)

	# Read in the ids of objects to be deleted.
	ndel = 0
	while (fscan (dl) != EOF) {
	    call gargi (Memi[id+ndel])
	    ndel = ndel + 1
	}

	# Sort the id numbers.
	call asrti (Memi[id], Memi[id], ndelete)

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
