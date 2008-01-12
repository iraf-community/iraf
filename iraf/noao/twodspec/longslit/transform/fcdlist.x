include	<mach.h>
include	<error.h>

# FC_DLIST -- Fit Coordinates Deletion List Procedures.

# FC_DLREAD -- Fit Coordinates Deletion List Read.
# Read the deletion list file and match points in the list with the data
# and delete them.

procedure fc_dlread (x, y, w, npts)

real	x[npts]			# First coordinate to match
real	y[npts]			# Second coordinate to match
real	w[npts]			# Weight of coordinate
int	npts			# Number of coordinates

int	i, fd
real	r
char	file[SZ_FNAME]
real	xdel, ydel

int	access(), open(), fscan(), nscan()

begin
	call clgstr ("deletions", file, SZ_FNAME)

	if (access (file, READ_ONLY, TEXT_FILE) == NO)
	    return

	fd = open (file, READ_ONLY, TEXT_FILE)

	while (fscan (fd) != EOF) {
	    call gargr (xdel)
	    call gargr (ydel)

	    if (nscan() != 2)
		next

	    do i = 1, npts {
		r = sqrt ((x[i]-xdel)**2 + (y[i]-ydel)**2)
		if (r < 10*EPSILONR)
		    w[i] = 0.
#	        if (x[i] != xdel)
#		    next
#	        if (y[i] != ydel)
#		    next
#		w[i] = 0.
	    }
	}

	call close (fd)
end


# FC_DLWRITE -- Fit Coordinates Deletion List Write.

procedure fc_dlwrite (x, y, w, npts)

real	x[npts]			# First coordinate to match
real	y[npts]			# Second coordinate to match
real	w[npts]			# Weight of coordinate
int	npts			# Number of coordinates

int	i, fd
char	file[SZ_FNAME]

int	open()

begin
	call clgstr ("deletions", file, SZ_FNAME)

	if (file[1] == EOS)
	    return

	iferr (call delete (file))
	    ;
	iferr (fd = open (file, NEW_FILE, TEXT_FILE)) {
	    call erract (EA_WARN)
	    return
	}

	do i = 1, npts {
	    if (w[i] == 0.) {
		call fprintf (fd, "%g %g\n")
		    call pargr (x[i])
		    call pargr (y[i])
	    }
	}

	call close (fd)
end
