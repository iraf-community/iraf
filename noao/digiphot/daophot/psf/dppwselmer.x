include <tbset.h>
include "../lib/apseldef.h"

define	NCOLUMN 	5

define PS_DATA1STR "%-9d%10t%-10.3f%20t%-10.3f%30t%-12.3f%42t%-15.7g%80t \n"

# DP_XPSELMER -- Write the output photometry record to a text file.

procedure dp_xpselmer (tpout, id, x, y, mag, sky)

pointer	tpout		# pointer to the output table
int	id		# id of the star
real	x, y		# position of the star
real	mag		# magnitude of the star
real	sky		# value of sky

begin
	call fprintf (tpout, PS_DATA1STR)
	    call pargi (id)
	    call pargr (x)
	    call pargr (y)
	    call pargr (mag)
	    call pargr (sky)
end


# DP_TPSELMER -- Write out the PSF stars into an ST Table.

procedure dp_tpselmer (tp_out, id, x, y, mag, sky, colpoint, row)

int	tp_out			# the output table descriptor
int	id			# the object id
real	x			# the object x coordinate
real	y			# the object y coordinate
real	mag			# the object mangitude
real	sky			# the object sky value
int	colpoint[ARB]		# the column pointers
int	row			# current table row

begin
	# Write out the data.
	call tbrpti (tp_out, colpoint[1], id, 1, row)
	call tbrptr (tp_out, colpoint[2], x, 1, row)
	call tbrptr (tp_out, colpoint[3], y, 1, row)
	call tbrptr (tp_out, colpoint[4], mag, 1, row)
	call tbrptr (tp_out, colpoint[5], sky, 1, row)
end


# DP_XPSELPARS -- Add various parameters to the header of the photometry table.

procedure dp_xpselpars (tp, image, maxnpsf, scale, psfrad, fitrad)

pointer	tp			# pointer to the table
char	image[ARB]		# input image name
int	maxnpsf			# maximum number of psfstars
real	scale			# the image scale
real	psfrad			# the psf radius
real	fitrad			# the fitting radius

pointer	sp, str

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Add the image name nad maxnpsf parameters.
	call dp_imroot (image, Memc[str], SZ_FNAME)
	call dp_sparam (tp, "IMAGE", Memc[str], "imagename", "")
	call dp_iparam (tp, "MAXNPSF", maxnpsf, "number", "")
	call dp_rparam (tp, "NEWSCALE", scale, "units", "")
	call dp_rparam (tp, "PSFRAD", psfrad, "scaleunit", "")
	call dp_rparam (tp, "FITRAD", fitrad, "scaleunit", "")

	call sfree (sp)
end


define PS_NAME1STR "#N%4tID%10tXCENTER%20tYCENTER%30tMAG%42tMSKY%80t\\\n"
define PS_UNIT1STR "#U%4t##%10tpixels%20tpixels%30tmagnitudes%42tcounts\
%80t\\\n"
define PS_FORMAT1STR "#F%4t%%-9d%10t%%-10.3f%20t%%-10.3f%30t%%-12.3f%42t\
%%-15.7g%80t \n"

# DP_XPBANNER -- Create a new text file banner.

procedure dp_xpbanner (tp)

pointer	tp		# pointer to the output file

begin
	# Print out the banner file.
	call fprintf (tp, "#\n")
	call fprintf (tp, PS_NAME1STR)
	call fprintf (tp, PS_UNIT1STR)
	call fprintf (tp, PS_FORMAT1STR)
	call fprintf (tp, "#\n")
end


# DP_TPDEFCOL -- Define the columns for the output table

procedure dp_tpdefcol (tp, colpoint)

pointer	tp		# pointer to the output table
int	colpoint[ARB]	# array of column pointers

int	i
pointer	sp, colnames, colunits, colformat, col_dtype, col_len

begin
	# Allocate space for the table definition.
	call smark (sp)
	call salloc (colnames, NCOLUMN * (SZ_COLNAME + 1), TY_CHAR)
	call salloc (colunits, NCOLUMN * (SZ_COLUNITS + 1), TY_CHAR)
	call salloc (colformat, NCOLUMN * (SZ_COLFMT + 1), TY_CHAR)
	call salloc (col_dtype, NCOLUMN, TY_INT)
	call salloc (col_len, NCOLUMN, TY_INT)

	# Set up the column definitions.
	call strcpy (ID, Memc[colnames], SZ_COLNAME)
	call strcpy (XCENTER, Memc[colnames+SZ_COLNAME+1], SZ_COLNAME)
	call strcpy (YCENTER, Memc[colnames+2*SZ_COLNAME+2], SZ_COLNAME)
	call strcpy (MAG, Memc[colnames+3*SZ_COLNAME+3], SZ_COLNAME)
	call strcpy (SKY, Memc[colnames+4*SZ_COLNAME+4], SZ_COLNAME)

	# Define the column formats.
	call strcpy ("%6d", Memc[colformat], SZ_COLFMT)
	call strcpy ("10.3f", Memc[colformat+SZ_COLFMT+1], SZ_COLFMT)
	call strcpy ("10.3f", Memc[colformat+2*SZ_COLFMT+2], SZ_COLFMT)
	call strcpy ("12.3f", Memc[colformat+3*SZ_COLFMT+3], SZ_COLFMT)
	call strcpy ("15.7g", Memc[colformat+4*SZ_COLFMT+4], SZ_COLFMT)

	# Define the column units.
	call strcpy ("NUMBER", Memc[colunits], SZ_COLUNITS)
	call strcpy ("PIXELS", Memc[colunits+SZ_COLUNITS+1], SZ_COLUNITS)
	call strcpy ("PIXELS", Memc[colunits+2*SZ_COLUNITS+2], SZ_COLUNITS)
	call strcpy ("MAGNITUDES", Memc[colunits+3*SZ_COLUNITS+3], SZ_COLUNITS)
	call strcpy ("ADU", Memc[colunits+4*SZ_COLUNITS+4], SZ_COLUNITS)

	# Define the column data types.
	Memi[col_dtype] = TY_INT
	Memi[col_dtype+1] = TY_REAL
	Memi[col_dtype+2] = TY_REAL
	Memi[col_dtype+3] = TY_REAL
	Memi[col_dtype+4] = TY_REAL

	# Define the column lengths.
	do i = 1, NCOLUMN 
	    Memi[col_len+i-1] = 1
	
	# Define and create the table.
	call tbcdef (tp, colpoint, Memc[colnames], Memc[colunits],
	    Memc[colformat], Memi[col_dtype], Memi[col_len], NCOLUMN)
	call tbtcre (tp)

	call sfree (sp)
end


# DP_TPSELPARS -- Add various parameters to the header of the photometry table.

procedure dp_tpselpars (tp, image, maxnpsf, scale, psfrad, fitrad)

pointer	tp			# pointer to the table
char	image[ARB]		# the input image name
int	maxnpsf			# maximum number of psf stars
real	scale			# the image scale
real	psfrad			# the psf radius
real	fitrad			# the fitting radius

pointer	sp, str

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Add the min_group and max_group parameters.
	call dp_imroot (image, Memc[str], SZ_FNAME)
	call tbhadt (tp, "IMAGE", Memc[str])
	call tbhadi (tp, "MAXNPSF", maxnpsf)
	call tbhadr (tp, "SCALE", scale)
	call tbhadr (tp, "PSFRAD", psfrad)
	call tbhadr (tp, "FITRAD", fitrad)

	call sfree (sp)
end


# DP_WPSTARS -- Write the psf stars to the output file.

procedure dp_wpstars (tp_out, colpoint, text_file, ids, xcen, ycen, mag,
	sky, npsf)

int	tp_out			# the output file descriptor
int	colpoint[ARB]		# array of column pointers
bool	text_file		# is the output file a text file
int	ids[ARB]		# array of star ids
real	xcen[ARB]		# array of x coordinates
real	ycen[ARB]		# array of y coordinates
real	mag[ARB]		# array of magnitudes
real	sky[ARB]		# array of sky values
int	npsf			# the number of stars

int	istar, row

begin
	row = 0
	do istar = 1, npsf {
	    if (text_file)
		call dp_xpselmer (tp_out, ids[istar], xcen[istar], ycen[istar],
		    mag[istar], sky[istar])
	    else {
		row = row + 1
		call dp_tpselmer (tp_out, ids[istar], xcen[istar], ycen[istar],
		    mag[istar], sky[istar], colpoint, row)
	    }
	}
end
