include <fset.h>		# to check whether input or output is redirected
include <tbset.h>
define	MAX_RANGES (SZ_LINE/2)	# max number of ranges of row numbers

# tlinear -- first order fit to y or x and y columns by linear regression
#
# E.B. Stobie   15-Feb-1989  Task created.
# Phil Hodge     4-Oct-1995  Use table name template routines tbnopen, etc.
# Phil Hodge    24-Sep-1997  Replace IS_INDEF with IS_INDEFD.
# Phil Hodge     8-Apr-1999  Call tbfpri.
# Phil Hodge     8-Jun-1999  Set input/output to STDIN/STDOUT if redirected.
# Phil Hodge    38-Aug-2000  Completely exclude points with weight of zero.

procedure tlinear()

pointer inlist, outlist		# scr for input & output lists of names
char	xcol[SZ_COLNAME]	# x column name
char    ycol[SZ_COLNAME]        # y column name
char    wcol[SZ_COLNAME]        # weight column name
char    scol[SZ_COLNAME]        # standard deviations column name
char	outcoly[SZ_COLNAME]	# column name for fitted y values
char	outcolr[SZ_COLNAME]	# column name for y residual values
char    cyu[SZ_COLUNITS]        # column units for y
char    cxu[SZ_COLUNITS]        # column units for x
char    cwu[SZ_COLUNITS]        # column units for w
char    csu[SZ_COLUNITS]        # column units for s
char    cyf[SZ_COLFMT]          # column format for y
char    cxf[SZ_COLFMT]          # column format for x
char    cwf[SZ_COLFMT]          # column format for w
char    csf[SZ_COLFMT]          # column format for s
#--
pointer sp
pointer list1, list2		# for lists of input and output tables
pointer itp, otp		# ptr to table descriptor
pointer xcptr			# ptr to x column descriptor
pointer ycptr                   # ptr to y column descriptor
pointer wcptr                   # ptr to weighting column descriptor
pointer scptr                   # ptr to standard deviations
pointer ocpx, ocpy              # ptr to output x and y columns
pointer ocpw, ocps              # ptr to output w and s columns
pointer ocpf, ocpr		# ptr to col descr for output columns
pointer intab, outtab		# scr for names of input & output tables
pointer range_string		# string which gives ranges of row numbers
pointer points                  # ptr to valid points array
pointer as, bs, chi2s           # storage for fitted results
pointer siga2s, sigb2s          # storage for errors
pointer nptss, nrowss           # storage for no pts
pointer srms, rmss              # storage for rms and mean of residuals

double	s, sx, sy               # intermediate variables for fit
double  xval, yval              # x and y values to be fitted
double  wval, sval              # weighting values
double  fval, rval              # fitted values and residuals
double  wgt, xpt, ypt           # actual values used in fit
double  a, b, siga2, sigb2      # coefficients and their sigmas
double  chi2, sigdat            # chi squared 
double  avx, t, st2             # intermediate values used in fit
double  sr, sr2, srx            # intermediate values for rms
double  rms, srm                # mean and rms of residuals
double  yres                    # individual fitted y values and residuals

int	junk, i
int	nrows, count     	# number of rows, number of tables
int     nvalues, stat
int     row, npts
int     maxtab                  # maximum number of tables in input list
int     ranges[3,MAX_RANGES]
int     cxn, cyn, cwn, csn      # column number
int     cxl, cyl, cwl, csl      # lendata
int     cxdt, cydt, cwdt, csdt  # datatype
int     cxfl, cyfl, cwfl, csfl  # length of format
int	phu_copied		# set by tbfpri and ignored

bool	listout			# is the output ASCII rather than a table?
bool    done, point
bool	xpoint, weight, stdev

int	fstati()
pointer tbtopn(), tbnopen()
int     tbnget(), tbnlen()
int     decode_ranges(), get_next_number()
int     tbpsta()
bool	streq()

begin
	# Allocate scratch for lists of names and for table names.
	call smark (sp)
	call salloc (inlist, SZ_FNAME, TY_CHAR)
	call salloc (outlist, SZ_FNAME, TY_CHAR)
	call salloc (intab, SZ_FNAME, TY_CHAR)
	call salloc (outtab, SZ_FNAME, TY_CHAR)
	call salloc (range_string, SZ_FNAME, TY_CHAR)

	# Get task parameters.

	if (fstati (STDIN, F_REDIR) == YES)
	    call strcpy ("STDIN", Memc[inlist], SZ_FNAME)
	else
	    call clgstr ("intable", Memc[inlist], SZ_FNAME)

	if (fstati (STDOUT, F_REDIR) == YES)
	    call strcpy ("STDOUT", Memc[outlist], SZ_FNAME)
	else
	    call clgstr ("outtable", Memc[outlist], SZ_FNAME)

	call clgstr ("xcol", xcol, SZ_COLNAME)
        call clgstr ("ycol", ycol, SZ_COLNAME)
        call clgstr ("wcol", wcol, SZ_COLNAME)
        call clgstr ("scol", scol, SZ_COLNAME)
	call clgstr ("rows", Memc[range_string], SZ_FNAME)

	listout = streq (Memc[outlist], "STDOUT")	# ASCII output?
	if ( ! listout ) {
	    call clgstr ("outcoly", outcoly, SZ_COLNAME)
	    call clgstr ("outcolr", outcolr, SZ_COLNAME)
	}

	# Expand the input table list.
	list1 = tbnopen (Memc[inlist])

	if ( ! listout ) {
	    # Expand the output table list.
	    list2 = tbnopen (Memc[outlist])
	    if (tbnlen (list1) != tbnlen (list2)) {
		call tbnclose (list1)
		call tbnclose (list2)
		call error (1,
			"Number of input and output tables not the same")
	    }
	}

        # allocate arrays for results
        count = 0
        maxtab = 200
        call malloc (as, maxtab, TY_DOUBLE)
        call malloc (bs, maxtab, TY_DOUBLE)
        call malloc (chi2s, maxtab, TY_DOUBLE)
        call malloc (siga2s, maxtab, TY_DOUBLE)
        call malloc (sigb2s, maxtab, TY_DOUBLE)
        call malloc (srms, maxtab, TY_DOUBLE)
        call malloc (rmss, maxtab, TY_DOUBLE)
        call malloc (nrowss, maxtab, TY_INT)
        call malloc (nptss, maxtab, TY_INT)

	# Do for each input table.
	while (tbnget (list1, Memc[intab], SZ_FNAME) != EOF) {

	    itp = tbtopn (Memc[intab], READ_ONLY, NULL)
	    call tbcfnd (itp, ycol, ycptr, 1)
	    if (ycptr == NULL) {
		call tbtclo (itp)
		call eprintf ("column not found in %s\n")
		    call pargstr (Memc[intab])
		if ( ! listout )	# skip next output table
		    junk = tbnget (list2, Memc[outtab], SZ_FNAME)
		next
	    }

            call tbcfnd (itp, xcol, xcptr, 1)
            if (xcptr != NULL) xpoint = true
               else xpoint = false
               
            call tbcfnd (itp, wcol, wcptr, 1)
            if (wcptr != NULL ) {
               weight = true
               stdev = false

             } 
             else {
                weight = false
                call tbcfnd (itp, scol, scptr, 1)
                if (scptr != NULL ) stdev = true
                   else stdev = false
             }

        if (decode_ranges (Memc[range_string], ranges, MAX_RANGES, nvalues)
           != OK)
           call error (1, "bad range of row numbers")
	# Create scratch for fitted values and residuals
            nrows = tbpsta (itp, TBL_NROWS)
            call malloc (points, nrows, TY_BOOL)
            
        # xpoint = true  use xcolumn, else use row for x
        # weight = true  use weights
        # stdev  = true  (only if weight = false)  use standard deviations

            do i = 1, nrows {
               Memb[points+i-1] = false
               }

            row = 0
            npts = 0
            s = 0.
            sx = 0.
            sy = 0.

            stat = get_next_number (ranges, row)
            done = (stat == EOF) || (row > nrows)
     
            while (! done) {

               wgt = 1.
               xpt = row
               call tbegtd (itp, ycptr, row, yval)

               if (!IS_INDEFD(yval)) {
                  point = true
                  ypt = yval

                  if (xpoint) {
                     call tbegtd (itp, xcptr, row, xval)
                     if (!IS_INDEFD(xval)) xpt = xval
                        else point = false
                  }

                  if (weight) {
                     call tbegtd (itp, wcptr, row, wval)
                     if (!IS_INDEFD(wval)) wgt = wval
                        else point = false
                     if (wgt == 0.d0)
                        point = false
                  }
                  
                  if (stdev) {
                     call tbegtd (itp, scptr, row, sval)
                     if (!IS_INDEFD(sval)) wgt = 1./(sval*sval)
                        else point = false
                  }
               }
               else point = false

                  if (point) {

                     Memb[points+row-1] = true
                     npts = npts + 1
                     s = s + wgt
                     sx = sx + xpt * wgt
                     sy = sy + ypt * wgt
                  }
               
               stat=get_next_number(ranges,row)
               done=(stat == EOF) || (row > nrows)
            }
 
            if (npts > 1) {

               avx = sx/s
               t = 0.
               st2 = 0.
               b = 0.

               do i = 1, nrows {
                  
                  if (Memb[points+i-1]) {
                     row = i
                     xpt = i
                     wgt = 1.

                     call tbegtd (itp, ycptr, row, ypt)
                     if (xpoint) {
                        call tbegtd (itp, xcptr, row, xval)
                        xpt = xval
                     }
                     if (weight) {
                        call tbegtd (itp, wcptr, row, wval)
                        wgt = wval
                     }
                     if (stdev) {
                        call tbegtd (itp, scptr, row, sval)
                        wgt = 1. / (sval*sval)
                     }

                     t = xpt - avx
                     st2 = st2 + t * t * wgt              
                     b = b + t * ypt * wgt
                  }
               }
                
               if (st2 > 0.) {

                  b = b / st2
                  a = (sy - sx * b) / s
                  siga2 = sqrt ((1. + (sx*sx) / (s * st2)) / s)
                  sigb2 = sqrt (1. / st2)
                  chi2 = 0.
                  sr = 0.
                  sr2 = 0.
 
                  do i = 1, nrows {
                  
                     if (Memb[points+i-1]) {
                        row = i
                        xpt = i
                        wgt = 1.

                        call tbegtd (itp, ycptr, row, ypt)
                        if (xpoint) {
                           call tbegtd (itp, xcptr, row, xval)
                           xpt = xval
                        }
                        if (weight) {
                           call tbegtd (itp, wcptr, row, wval)
                           wgt = wval
                        }
                        if (stdev) {
                           call tbegtd (itp, scptr, row, sval)
                           wgt = 1. / (sval*sval)
                        }
                        yres = ypt - (a + b * xpt)
                        chi2 = chi2 + yres * yres * wgt
                        sr = sr + yres
                        sr2 = sr2 + yres * yres
                     }
                  }

                  sigdat = 1.
                  if (!weight && !stdev) sigdat = sqrt (chi2 / (npts - 2))
                  siga2 = siga2 * sigdat
                  sigb2 = sigb2 * sigdat

                  srm = sr / npts
                  srx = sr2 - (sr*sr)/npts
                  rms = sqrt (srx / (npts - 1))

                  # Save fit values
                  Memd[as+count] = a
                  Memd[bs+count] = b
                  Memd[chi2s+count] = chi2
                  Memd[siga2s+count] = siga2
                  Memd[sigb2s+count] = sigb2
                  Memd[srms+count] = srm
                  Memd[rmss+count] = rms
                  Memi[nptss+count] = npts
                  Memi[nrowss+count] = nrows
                  count = count + 1

                  if (! listout) {

                     # Create output table & define columns.
                     junk = tbnget (list2, Memc[outtab], SZ_FNAME)
                     call tbfpri (Memc[intab], Memc[outtab], phu_copied)
                     otp = tbtopn (Memc[outtab], NEW_FILE, NULL)
                     if (xpoint) {
                        call tbcinf (xcptr, cxn, xcol, cxu, cxf, cxdt, cxl,
                                     cxfl)
                        call tbcdef (otp, ocpx, xcol, cxu, cxf, cxdt, cxl, 1)
                     }
                     call tbcinf (ycptr, cyn, ycol, cyu, cyf, cydt, cyl, cyfl)
                     call tbcdef (otp, ocpy, ycol, cyu, cyf, cydt, cyl, 1)  
                     if (weight) {
                        call tbcinf (wcptr, cwn, wcol, cwu, cwf, cwdt, cwl,
                                     cwfl)
                        call tbcdef (otp, ocpw, wcol, cwu, cwf, cwdt, cwl, 1)
                     }
                     if (stdev) {
                        call tbcinf (scptr, csn, scol, csu, csf, csdt, csl,
                                     csfl)
                        call tbcdef (otp, ocps, scol, csu, csf, csdt, csl, 1)
                     }
                     call tbcdef (otp, ocpf,
	                          outcoly, "", "", TY_DOUBLE, 1, 1)
	             call tbcdef (otp, ocpr,
                                  outcolr, "", "", TY_DOUBLE, 1, 1)
                     call tbtcre (otp)

	             # Put info records in the header.
	             call tbhadt (otp, "intable", Memc[intab])
                     if (xpoint) call tbhadt (otp, "xcol", xcol)
                     call tbhadt (otp, "ycol", ycol)
                     if (weight) call tbhadt (otp, "wcol", wcol)
                     if (stdev)  call tbhadt (otp, "scol", scol)
	             call tbhadi (otp, "nrows", nrows)
 
                     call tbhadd (otp, "a", a)
                     call tbhadd (otp, "b", b)
                     call tbhadd (otp, "siga2", siga2)
                     call tbhadd (otp, "sigb2", sigb2)
                     call tbhadd (otp, "chi2", chi2)

	             # Write the values into the output table, and close it.
                     do i = 1, nrows { 

                        point = true
                        row = i
                        xpt = i
                        if (xpoint) {
                           call tbegtd (itp, xcptr, row, xval)
                           call tbeptd (otp, ocpx, row, xval)
                           if (IS_INDEFD (xval)) point = false
                              else xpt = xval
                        }
                        if (point) {
                           fval = a + b * xpt
                           call tbeptd (otp, ocpf, row, fval)    
                           call tbegtd (itp, ycptr, row, yval)
                           call tbeptd (otp, ocpy, row, yval)
                           if (!IS_INDEFD (yval)) {
                              rval = yval - fval
                              call tbeptd (otp, ocpr, row, rval)
                           }
                           if (weight) {
                              call tbegtd (itp, wcptr, row, wval)
                              call tbeptd (otp, ocpw, row, wval)
                           }
                           if (stdev) {
                              call tbegtd (itp, scptr, row, sval)
                              call tbeptd (otp, ocps, row, sval)
                           }
                        }
                
                     }   
		         call tbtclo (otp)
                  }
               }
               else {
                  call printf("Must have at least 2 unique x values.")
                  call printf("   Cannot fit!\n")
               }
            }
            call mfree (points, TY_BOOL)
	    call tbtclo (itp)
	}
	call tbnclose (list1)
	if ( ! listout )
	    call tbnclose (list2)

        if (count > 0) {
           call printf ("#          Fit by linear regression (y = a + bx)\n")
           call printf (" \n")
           call printf ("# Table  pts in row    pts in fit              a")
           call printf ("                       b\n")
           call printf (" \n")

           do i = 1, count {
              call printf ("%6d %10d %13d    %18.8g     %18.8g\n")
                 call pargi (i)
                 call pargi (Memi[nrowss+i-1])
                 call pargi (Memi[nptss+i-1])
                 call pargd (Memd[as+i-1])
                 call pargd (Memd[bs+i-1])
           }
           call printf (" \n")
           call printf (" \n")
      
           call printf("# Table    siga2         sigb2           chi2")
           call printf("     residual rms    residual mean\n")
           call printf (" \n")
           do i = 1, count {
              call printf ("    %d %13.7g %13.7g %13.7g %13.7g     %13.7g\n")
                 call pargi (i)
                 call pargd (Memd[siga2s+i-1])
                 call pargd (Memd[sigb2s+i-1])
                 call pargd (Memd[chi2s+i-1])
                 call pargd (Memd[rmss+i-1])
                 call pargd (Memd[srms+i-1])
           }
        }
        call mfree (as, TY_DOUBLE)
        call mfree (bs, TY_DOUBLE)
        call mfree (chi2s, TY_DOUBLE)
        call mfree (siga2s, TY_DOUBLE)
        call mfree (sigb2s, TY_DOUBLE)
        call mfree (srms, TY_DOUBLE)
        call mfree (rmss, TY_DOUBLE)
        call mfree (nptss, TY_INT)
        call mfree (nrowss, TY_INT)
	call sfree (sp)
end
