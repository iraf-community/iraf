include	<tbset.h>

#* HISTORY *
#* B.Simon	24-Aug-1994	original
# Phil Hodge	 8-Apr-1999	Call tbfpri.

# TMATCH -- Find closest matching rows between two tables

procedure tmatch ()

#--
pointer	input1		# First input table
pointer	input2		# Second input table
pointer	output		# Output table
pointer	match1		# Columns from first table used to match
pointer	match2		# Columns from second table used to match
double	maxnorm		# Maximum value of norm for allowed match
pointer	incol1		# Columns from first table copied to output
pointer	incol2		# Columns from second table copied to output
pointer	factor		# Multiplicative factors used in computing norm
pointer	diagfile	# Diagnostic output file
pointer	nmcol1		# Columns from first table in diagnostic output
pointer	nmcol2		# Columns from second table in diagnostic output
bool	sphere		# Apply spherical correction to first column?

bool	fold
int	mxcol1, mxcol2, ncol1, ncol2, nrow1, nrow2
int	phu_copied	# set by tbfpri and ignored
pointer	sp, in1, in2, col1, col2, index1, index2, weight, dist, closest

data	fold	/ false /

string	mismatch  "Both lists of match columns must have same length"
string	nomatch   "Match columns not found in table"

bool	clgetb()
double	clgetd()
int	tbpsta()
pointer	tbtopn()

begin
	# Allocate memory for strings

	call smark (sp)
	call salloc (input1, SZ_FNAME, TY_CHAR)
	call salloc (input2, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (match1, SZ_FNAME, TY_CHAR)
	call salloc (match2, SZ_FNAME, TY_CHAR)
	call salloc (incol1, SZ_FNAME, TY_CHAR)
	call salloc (incol2, SZ_FNAME, TY_CHAR)
	call salloc (factor, SZ_FNAME, TY_CHAR)
	call salloc (diagfile, SZ_FNAME, TY_CHAR)
	call salloc (nmcol1, SZ_FNAME, TY_CHAR)
	call salloc (nmcol2, SZ_FNAME, TY_CHAR)

	# Read task parameters

	call clgstr ("input1", Memc[input1], SZ_FNAME)
	call clgstr ("input2", Memc[input2], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("match1", Memc[match1], SZ_FNAME)
	call clgstr ("match2", Memc[match2], SZ_FNAME)
	maxnorm = clgetd ("maxnorm")

	call clgstr ("incol1", Memc[incol1], SZ_FNAME)
	call clgstr ("incol2", Memc[incol2], SZ_FNAME)
	call clgstr ("factor", Memc[factor], SZ_FNAME)
	call clgstr ("diagfile", Memc[diagfile], SZ_FNAME)
	call clgstr ("nmcol1", Memc[nmcol1], SZ_FNAME)
	call clgstr ("nmcol2", Memc[nmcol2], SZ_FNAME)
	sphere = clgetb ("sphere")

	# Open input tables and get list of match colums

	in1 = tbtopn (Memc[input1], READ_ONLY, NULL)
	in2 = tbtopn (Memc[input2], READ_ONLY, NULL)

	mxcol1 = tbpsta (in1, TBL_NCOLS)
	mxcol2 = tbpsta (in2, TBL_NCOLS)

	call salloc (col1, mxcol1, TY_INT)
	call salloc (col2, mxcol2, TY_INT)

	call tctexp (in1, Memc[match1], mxcol1, ncol1, Memi[col1])
	call tctexp (in2, Memc[match2], mxcol2, ncol2, Memi[col2])

	if (ncol1 != ncol2)
	    call error (1, mismatch)

	if (ncol1 == 0)
	    call error (1, nomatch)

	if (ncol1 < 2)
	    sphere = false

	# Sort input tables

	call allrows (in1, nrow1, index1)
	call allrows (in2, nrow2, index2)
	
	call tbtsrt (in1, ncol1, Memi[col1], fold, nrow1, Memi[index1])
	call tbtsrt (in2, ncol2, Memi[col2], fold, nrow2, Memi[index2])

	call salloc (weight, ncol1, TY_DOUBLE)
	call salloc (dist, nrow1, TY_DOUBLE)
	call salloc (closest, nrow1, TY_INT)

	# Compute weights from list of factors or table column units

	call getweight (ncol1, Memi[col1], Memi[col2], 
			Memc[factor], Memd[weight])

	# Compute closest match between the two tables

	call getmatch (in1, in2, ncol1, Memi[col1], Memi[col2], Memd[weight], 
		       nrow1, Memi[index1], nrow2, Memi[index2], maxnorm, 
		       sphere, Memi[closest], Memd[dist])

	# Write output table

	call tbfpri (Memc[input1], Memc[output], phu_copied)
	call putmatch (Memc[output], Memc[incol1], Memc[incol2], in1, in2, 
		       nrow1, Memi[closest])

	# Write diagnostic info

	call infomatch (Memc[diagfile], in1, in2, Memc[nmcol1], Memc[nmcol2], 
			maxnorm, nrow1, Memi[closest], Memd[dist])

	# Clean up

	call mfree (index1, TY_INT)
	call mfree (index2, TY_INT)
	call tbtclo (in1)
	call tbtclo (in2)
	call sfree (sp)
end
