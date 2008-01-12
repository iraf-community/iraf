include	<error.h>
include <tbset.h>
include "sgraph.h"

# GG_RDCURVES -- Given the operand list as input, read in all the referenced
# lists and/or image sections, producing a list of vectors as output.  Return
# as the function value the number of curves.
#
# Modified: Code has been added to handle 3-D tables.  This code was 
#		pulled from B. Simon's library as adapted by I. Busko.
#		The cases dealing with the number of words have been 
#		modified to include branching to handle (in one case)
#		normal SDAS/text tables and (in the other case) 3-D tables.
#			W. Hack	(22 April 1997)

int procedure gg_rdcurves (oplist, x, y, size, npix, axis, rdmarks, erraxis) 

char	oplist[SZ_LINE]		# Operand list
pointer	x[MAX_CURVES]		# Pointer to x vector
pointer	y[MAX_CURVES]		# Pointer to y vector
pointer	size[MAX_CURVES]	# Pointer to vector of marker sizes
int	npix[MAX_CURVES]	# Number of values per vector
int	axis			# Axis for projection
bool	rdmarks			# Read mark sizes from list?
int	erraxis			# X or Y errors?

char	word[SZ_FNAME,4]	# Input line words
char	root[SZ_FNAME]		# Table root name
char	rowselect[SZ_FNAME]	# 3-D Table row selector
char	colselect[SZ_FNAME]	# 3-D Table column selector

char	xroot[SZ_FNAME]		# Table root name
char	xrows[SZ_FNAME]		# 3-D Table row selector
char	xcols[SZ_FNAME]		# 3-D Table column selector

char	yroot[SZ_FNAME]		# Table root name
char	yrows[SZ_FNAME]		# 3-D Table row selector
char	ycols[SZ_FNAME]		# 3-D Table column selector

pointer	tmp[MAX_CURVES]		# temp pointer for data

char	image[SZ_FNAME], table[SZ_FNAME], column[SZ_COLNAME]
#char	temp[SZ_FNAME]
pointer	project, sp
pointer	data[MAX_CURVES]
int	ncol, n

char	errcol[SZ_COLNAME]
char	xtable[SZ_FNAME], xcolumn[SZ_COLNAME]
char	ytable[SZ_FNAME], ycolumn[SZ_COLNAME]
int	ncurves, i, iw
pointer fi, ft, ftx, fty


int	gg_rdcurve(), imtopen(), imtgetim(), nscan()
pointer tbnopen()
int	tbnget()
int	gg_rd1col(), gg_rd2col(), gg_rdxycol()

begin
	 	call smark(sp)
		call salloc(project, MAX_CURVES, TY_INT)
		call aclri(Memi[project], MAX_CURVES)
	ncurves = 0

	# Parse the input
	call sscan (oplist) 
	do iw = 1, 4
	    call gargwrd (word[1,iw], SZ_FNAME)
	
	switch (nscan ()) {
	# How many words in input line?

	case 1:
	    # Image or list file;  Read all curves into memory.

	    fi = imtopen (word)
	    while (imtgetim (fi, image, SZ_FNAME) != EOF) {
		ncurves = ncurves + 1
		if (ncurves > MAX_CURVES)
		    call error (0, "Maximum 14 curves can be overplotted")
		i = ncurves
		iferr {
		    npix[i] = gg_rdcurve (image, 
			x[i], y[i], size[i], axis, rdmarks, erraxis) 
		} then {
		    call erract (EA_WARN)
		    ncurves = ncurves - 1
		}

	    }

	    call imtclose (fi)

	case 2:
	    # Single table column

	    ft = tbnopen (word[1,1])
	    call strcpy (word[1,2], column, SZ_COLNAME)

	    if (rdmarks)
		call clgstr ("errcolumn", errcol, SZ_COLNAME)

	    while (tbnget (ft, table,  SZ_FNAME) != EOF) {
	    	# Check to see if there are any row/col selectors 
	    	# appended to table name...
	    	call rdselect (table, root, rowselect, colselect, SZ_FNAME)

		ncurves = ncurves + 1
		if (ncurves > MAX_CURVES)
		    call error (0, "Maximum of 20 curves can be overplotted")
		i = ncurves

	    	if (rowselect[1] == EOS && colselect[1] == EOS) {
		# No row or column selectors specified, so we are working with
		# SDAS table...
			iferr {
			    npix[i] = gg_rd1col (table, column, 
				rdmarks, errcol, erraxis,
				x[i], y[i], size[i]) 
			} then {
			    call erract (EA_WARN)
			    ncurves = ncurves - 1
			}
		} else {
		# With row selector given, we are working with a 3-D table.
		# First we build the column selector from xcol/ycol names...  
		    if(rdmarks) {
		    	call sprintf(colselect,SZ_FNAME,"[c:%s,%s]")
				call pargstr(column)
				call pargstr(errcol)
		    } else {
		    	call sprintf(colselect,SZ_FNAME,"[c:%s]")
				call pargstr(column)
		    }
		# Now we append this selector to the table name...
		    call strcat(colselect, table, SZ_FNAME)

		    iferr {
			call om_rdtable (table, TY_REAL, Memi[project], data, npix[i], ncol, MAX_CURVES+1)
		# Fill in the X values (independent variable)
			# set aside the memory necessary for this array...
			call malloc (x[i], npix[i], TY_REAL)
			# increment values in array...
		        do n = 1, npix[i] {
  		      		Memr[x[i]+n-1] = n
		        }
			y[i]= data[1]
			if(rdmarks) size[i] = data[2]			
		    } then {
			call erract (EA_WARN)
			ncurves = ncurves - 1
		    }
		}
	    # End while loop
	    }
	    call tbnclose (ft)

	case 3:
	    # Single table;  X and Y column

	    ft  = tbnopen (word[1,1])
	    call strcpy (word[1,2], xcolumn, SZ_COLNAME)
	    call strcpy (word[1,3], ycolumn, SZ_COLNAME)

	    if (rdmarks)
		call clgstr ("errcolumn", errcol, SZ_COLNAME)


	    while (tbnget (ft, table,  SZ_FNAME) != EOF) {
	    	# Check to see if there are any row/col selectors 
	    	# appended to table name...
	    	call rdselect (table, root, rowselect, colselect, SZ_FNAME)

		ncurves = ncurves + 1
		if (ncurves > MAX_CURVES)
		    call error (0, "Maximum of 20 curves can be overplotted")
		i = ncurves

	    	if (rowselect[1] == EOS && colselect[1] == EOS) {
		# No row or column selectors specified, so we are working with
		# SDAS table...
		    iferr {
			npix[i] = gg_rdxycol (table, xcolumn, ycolumn, 
			      rdmarks, errcol, erraxis,
			      x[i], y[i], size[i]) 
		    } then {
			call erract (EA_WARN)
			ncurves = ncurves - 1
		    }

		} else {
		# With row selector given, we are working with a 3-D table.
		# First we build the column selector from xcol/ycol names...  
		    if(rdmarks) {
		    	call sprintf(colselect,SZ_FNAME,"[c:%s,%s,%s]")
				call pargstr(xcolumn)
				call pargstr(ycolumn)
				call pargstr(errcol)
		    } else {
		    	call sprintf(colselect,SZ_FNAME,"[c:%s,%s]")
				call pargstr(xcolumn)
				call pargstr(ycolumn)
		    }
		# Now we append this selector to the table name...
		    call strcat(colselect, table, SZ_FNAME)
		    iferr {
			call om_rdtable (table, TY_REAL, Memi[project], data, npix[i], ncol, MAX_CURVES+1)

			x[i] = data[1]
			y[i]= data[2]
			if(rdmarks) size[i] = data[3]
			
		    } then {
			call erract (EA_WARN)
			ncurves = ncurves - 1
		    }
		}
	    }
	    call tbnclose (ft)

	case 4:
	    # X table and column;  Y table and column

	    ftx = tbnopen (word[1,1])
	    call  strcpy (word[1,2], xcolumn, SZ_COLNAME)
	    fty = tbnopen (word[1,3])
	    call  strcpy (word[1,4], ycolumn, SZ_COLNAME)


	    if (rdmarks)
		call clgstr ("errcolumn", errcol, SZ_COLNAME)

	    while (tbnget (ftx, xtable, SZ_FNAME) != EOF &&
		   tbnget (fty, ytable, SZ_FNAME) != EOF) {
	    	# Check to see if there are any row/col selectors 
	    	# appended to table name...
	    	call rdselect (xtable, xroot, xrows, xcols, SZ_FNAME)
	    	call rdselect (ytable, yroot, yrows, ycols, SZ_FNAME)

		ncurves = ncurves + 1
		if (ncurves > MAX_CURVES)
		    call error (0, "Maximum of 20 curves can be overplotted")
		i = ncurves

	    	if ( (xrows[1] == EOS && xcols[1] == EOS) &&
		     (yrows[1] == EOS && ycols[1] == EOS) ) {
		# No row or column selectors specified, so we are working with
		# SDAS tables...
		    iferr {
			npix[i] = gg_rd2col (xtable, xcolumn, ytable, ycolumn, 
				rdmarks, errcol, erraxis,
				x[i], y[i], size[i]) 
		    } then {
			call erract (EA_WARN)
			ncurves = ncurves - 1
		    }
		} else {
		# We are now working with at least 1 3-D table
		# Is the X data coming from a 3-D table?

		    if (xrows[1] != EOS || xcols[1] != EOS) {
		    # With row selector given, we are working with a 3-D table.
		    # First we build the column selector from xcol/ycol names...

			    if(rdmarks) {
				    call sprintf(xcols,SZ_FNAME,"[c:%s,%s]")
					call pargstr(xcolumn)
					call pargstr(errcol)
			    } else {
				    call sprintf(xcols,SZ_FNAME,"[c:%s]")
					call pargstr(xcolumn)
			    }
			# Now we append these selectors to the table names...
			    call strcat(xcols, xtable, SZ_FNAME)
	
			    iferr {
				call om_rdtable (xtable, TY_REAL, Memi[project], data, npix[i], ncol, MAX_CURVES+1)
			# Fill in the X values (independent variable)
				x[i]= data[1]
				if(rdmarks) size[i] = data[2]			
			    } then {
				call erract (EA_WARN)
				ncurves = ncurves - 1
			    }

		    } else {
		    # read in X data as 1 column SDAS table

			iferr {
			    npix[i] = gg_rd1col (xtable, xcolumn, 
				rdmarks, errcol, erraxis,
				tmp[i], x[i], size[i]) 
			} then {
			    call erract (EA_WARN)
			    ncurves = ncurves - 1
			}
		    }

		# Now, let's check to see if the Y data comes from a 3-D table
		    if (yrows[1] != EOS || ycols[1] != EOS) {

		    # With row selector given, we are working with a 3-D table.
		    # First we build the column selector from xcol/ycol names...
			    if(rdmarks) {
				    call sprintf(ycols,SZ_FNAME,"[c:%s,%s]")
					call pargstr(ycolumn)
					call pargstr(errcol)
			    } else {
				    call sprintf(ycols,SZ_FNAME,"[c:%s]")
					call pargstr(ycolumn)
			    }
			# Now we append these selectors to the table names...
			    call strcat(ycols, ytable, SZ_FNAME)
	
			    iferr {
				call om_rdtable (ytable, TY_REAL, Memi[project], data, npix[i], ncol, MAX_CURVES+1)
			# Fill in the X values (independent variable)
				y[i]= data[1]
				if(rdmarks) size[i] = data[2]			
			    } then {
				call erract (EA_WARN)
				ncurves = ncurves - 1
			    }
		} else {
		# Read in Y data as a 1 column SDAS table...

			iferr {
			    npix[i] = gg_rd1col (ytable, ycolumn, 
				rdmarks, errcol, erraxis,
				tmp[i], y[i], size[i]) 
			} then {
			    call erract (EA_WARN)
			    ncurves = ncurves - 1
			}
		    }

		# End 3-D Tables if-then 
		}
	    # END while loop
	    }
	    call tbnclose (ftx)
	    call tbnclose (fty)
	}

	if (ncurves == 0)
	    call error (0, "No curves read")
	else
	    return (ncurves)
	    call sfree(sp)

end


int procedure gg_rdcurve (operand, x, y, size, axis, rdmarks, erraxis)

# GG_RDCURVE -- Read a curve into memory.  The operand may specify either
# list or image input; we determine which and then call the appropriate
# input routine to access the data.

#  9/11/91 Explicitly prevent reading marker data for image data.  ZGL

char	operand[SZ_LINE]	# Operand to be plotted
pointer	x, y, size		# Pointers to x, y and size arrays
int	axis			# Axis of image projection
bool	rdmarks			# Read marks from list?
int	erraxis			# X or Y errors?

int	gg_rdlist2(), gg_rdimage2(), gg_optype()

errchk	gg_rdlist2, gg_rdimage2, gg_optype

begin
	if (gg_optype (operand) == LIST_OP)
	    # Data from list (text) file columns
	    return (gg_rdlist2 (operand, x, y, size, rdmarks, erraxis))

	else {
	    # Data from image
	    # Make sure not to read marker sizes
	    rdmarks = false
	    return (gg_rdimage2 (operand, x, y, size, axis))
	}
end
