# This file contains ttr_trans and ttr_flip.  The former copies data
# from one table to another and transposes rows and columns, while the
# latter copies data without transposing.  Either routine may also flip
# rows and/or columns, i.e. first input row to last output row, or first
# input column to last input column.
#
# Phil Hodge, 30-Nov-1994  Subroutines created.

# ttr_trans -- copy data from input to output
# This routine transposes a table.

procedure ttr_trans (itp, otp, icp, ocp,
		irows, icols, orows, ocols, op, dtype, nelem)

pointer itp		# i: pointer to input table struct
pointer otp		# i: pointer to output table struct
pointer icp[icols]	# i: array of pointers to input column descriptors
pointer ocp[irows]	# i: array of pointers to output column descriptors
int	irows		# i: number of rows in input table
int	icols		# i: number of columns in input table
int	orows		# i: number of rows in output table
int	ocols		# i: number of columns in output table
int	op[2]		# i: mapping of (columns,rows) from input to output
int	dtype		# i: data type of column
int	nelem		# i: length of array stored at each row,column
#--
pointer sp
pointer buf		# scratch for copying array entries
int	clen		# length of char string (= -dtype)
int	i, j		# loop indices for input table
int	oi, oj		# loop indices for output table
int	oj_start	# starting value for oj
int	oi_incr, oj_incr	# increments in oi, oj

# buffers for copying one element:
pointer cbuf
double	dbuf
real	rbuf
int	ibuf
short	sbuf
bool	bbuf

int	nret		# number of array elements actually read and written
int	tbagtd(), tbagtr(), tbagti(), tbagts(), tbagtb(), tbagtt()
errchk	tbegtd, tbegtr, tbegti, tbegts, tbegtb, tbegtt,
	tbeptd, tbeptr, tbepti, tbepts, tbeptb, tbeptt,
	tbagtd, tbagtr, tbagti, tbagts, tbagtb, tbagtt,
	tbaptd, tbaptr, tbapti, tbapts, tbaptb, tbaptt

begin
	call smark (sp)

	# Assign values for the beginning and increment for the loops
	# on oi and oj.
	if (op[1] > 0) {
	    oi = 1
	    oi_incr = 1
	} else {
	    oi = ocols			# = irows
	    oi_incr = -1
	}
	if (op[2] > 0) {
	    oj_start = 1
	    oj_incr = 1
	} else {
	    oj_start = orows		# = icols
	    oj_incr = -1
	}

	if (dtype < 0)
	    clen = -dtype

	if (nelem == 1) {

	    if (dtype == TY_REAL) {
		do j = 1, irows {
		    oj = oj_start	# oj, not oi, because we're transposing
		    do i = 1, icols {
			call tbegtr (itp, icp[i], j, rbuf)
			call tbeptr (otp, ocp[oi], oj, rbuf)
			oj = oj + oj_incr
		    }
		    oi = oi + oi_incr
		}
	    } else if (dtype == TY_DOUBLE) {
		do j = 1, irows {
		    oj = oj_start
		    do i = 1, icols {
			call tbegtd (itp, icp[i], j, dbuf)
			call tbeptd (otp, ocp[oi], oj, dbuf)
			oj = oj + oj_incr
		    }
		    oi = oi + oi_incr
		}
	    } else if (dtype == TY_INT) {
		do j = 1, irows {
		    oj = oj_start
		    do i = 1, icols {
			call tbegti (itp, icp[i], j, ibuf)
			call tbepti (otp, ocp[oi], oj, ibuf)
			oj = oj + oj_incr
		    }
		    oi = oi + oi_incr
		}
	    } else if (dtype == TY_SHORT) {
		do j = 1, irows {
		    oj = oj_start
		    do i = 1, icols {
			call tbegts (itp, icp[i], j, sbuf)
			call tbepts (otp, ocp[oi], oj, sbuf)
			oj = oj + oj_incr
		    }
		    oi = oi + oi_incr
		}
	    } else if (dtype == TY_BOOL) {
		do j = 1, irows {
		    oj = oj_start
		    do i = 1, icols {
			call tbegtb (itp, icp[i], j, bbuf)
			call tbeptb (otp, ocp[oi], oj, bbuf)
			oj = oj + oj_incr
		    }
		    oi = oi + oi_incr
		}
	    } else if (dtype < 0) {
		call salloc (cbuf, SZ_LINE, TY_CHAR)
		do j = 1, irows {
		    oj = oj_start
		    do i = 1, icols {
			call tbegtt (itp, icp[i], j, Memc[cbuf], SZ_LINE)
			call tbeptt (otp, ocp[oi], oj, Memc[cbuf])
			oj = oj + oj_incr
		    }
		    oi = oi + oi_incr
		}
	    } else {
		call error (1, "invalid data type")
	    }

	} else {			# each entry is an array

	    if (dtype > 0)
		call salloc (buf, nelem, dtype)

	    if (dtype == TY_REAL) {
		do j = 1, irows {
		    oj = oj_start
		    do i = 1, icols {
			nret = tbagtr (itp, icp[i], j, Memr[buf], 1, nelem)
			call tbaptr (otp, ocp[oi], oj, Memr[buf], 1, nret)
			oj = oj + oj_incr
		    }
		    oi = oi + oi_incr
		}
	    } else if (dtype == TY_DOUBLE) {
		do j = 1, irows {
		    oj = oj_start
		    do i = 1, icols {
			nret = tbagtd (itp, icp[i], j, Memd[buf], 1, nelem)
			call tbaptd (otp, ocp[oi], oj, Memd[buf], 1, nret)
			oj = oj + oj_incr
		    }
		    oi = oi + oi_incr
		}
	    } else if (dtype == TY_INT) {
		do j = 1, irows {
		    oj = oj_start
		    do i = 1, icols {
			nret = tbagti (itp, icp[i], j, Memi[buf], 1, nelem)
			call tbapti (otp, ocp[oi], oj, Memi[buf], 1, nret)
			oj = oj + oj_incr
		    }
		    oi = oi + oi_incr
		}
	    } else if (dtype == TY_SHORT) {
		do j = 1, irows {
		    oj = oj_start
		    do i = 1, icols {
			nret = tbagts (itp, icp[i], j, Mems[buf], 1, nelem)
			call tbapts (otp, ocp[oi], oj, Mems[buf], 1, nret)
			oj = oj + oj_incr
		    }
		    oi = oi + oi_incr
		}
	    } else if (dtype == TY_BOOL) {
		do j = 1, irows {
		    oj = oj_start
		    do i = 1, icols {
			nret = tbagtb (itp, icp[i], j, Memb[buf], 1, nelem)
			call tbaptb (otp, ocp[oi], oj, Memb[buf], 1, nret)
			oj = oj + oj_incr
		    }
		    oi = oi + oi_incr
		}
	    } else if (dtype < 0) {
		call salloc (buf, (clen+1) * nelem, TY_CHAR)	# add 1 for EOS
		do j = 1, irows {
		    oj = oj_start
		    do i = 1, icols {
			nret = tbagtt (itp, icp[i], j, Memc[buf], clen,
					1, nelem)
			call tbaptt (otp, ocp[oi], oj, Memc[buf], clen, 1, nret)
			oj = oj + oj_incr
		    }
		    oi = oi + oi_incr
		}
	    } else {
		call error (1, "invalid data type")
	    }
	}

	call sfree (sp)
end

# ttr_flip -- copy data from input to output
# This routine copies a table without transposing.
# irows and icols are the numbers of rows and columns in both the
# input and output tables.
# Note that if we are reversing the order of the columns (horizontal flip),
# the last column of the input table was defined first, so the flip in
# column order is taken care of by the relative order of the elements of
# the arrays icp and ocp.

procedure ttr_flip (itp, otp, icp, ocp, irows, icols, op)

pointer itp		# i: pointer to input table struct
pointer otp		# i: pointer to output table struct
pointer icp[icols]	# i: array of pointers to input column descriptors
pointer ocp[ARB]	# i: array of pointers to output column descriptors
int	irows		# i: number of rows in input table
int	icols		# i: number of columns in input table
int	op[2]		# i: mapping of (columns,rows) from input to output
#--
int	j		# loop index for input row number
int	oj, oj_incr	# loop index and increment for output row number
errchk tbrcpy, tbrcsc

begin
	# Assign values for the beginning and increment for the loop
	# on output row number.
	if (op[2] > 0) {
	    oj = 1
	    oj_incr = 1
	} else {
	    oj = irows
	    oj_incr = -1
	}

	# Copy the data from input to output.
	if (op[1] > 0) {

	    # Retain column order.
	    do j = 1, irows {
		call tbrcpy (itp, otp, j, oj)
		oj = oj + oj_incr
	    }

	} else {					# op[1] < 0

	    # Reverse column order.
	    do j = 1, irows {
		call tbrcsc (itp, otp, icp, ocp, j, oj, icols)
		oj = oj + oj_incr
	    }
	}
end
