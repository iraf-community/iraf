include <error.h>
include <fset.h>
include <printf.h>
include <mach.h>
include	"vt.h"

define	SZ_VTRECFD	5120		# length, in chars, of full disk recs
define	YABUF		20000			# Yet Another BUFfer
define	SWAP		{temp=$1;$1=$2;$2=temp}
define	MAX_RANGES	100

# TCOPY -- This is an asynchronous tape to tape copy routine.  It considers
# the input and output to be streaming devices. 
# The user specifies which files on tape s/he wants and a root name for the
# output file names.

procedure t_tcopy()

char	inputfile[SZ_FNAME]
char	files[SZ_LINE]
char	outputfile[SZ_FNAME]

char	tapename[SZ_FNAME]
int	filerange[2 * MAX_RANGES + 1]
int	nfiles, filenumber, numrecords, whichfile
bool	verbose 

int	decode_ranges(), mtfile()
int	get_next_number(), tapecopy(), mtneedfileno()
bool	clgetb()
errchk	tapecopy

begin
	call fseti (STDOUT, F_FLUSHNL, YES)

        # Get input file(s).
        call clgstr ("inputfile", inputfile, SZ_FNAME)
	if (mtfile (inputfile) == NO || mtneedfileno (inputfile) == NO) {
	    call strcpy ("1", files, SZ_LINE)
        } else {
	    call clgstr ("files", files, SZ_LINE)
	}

        if (decode_ranges (files, filerange, MAX_RANGES, nfiles) == ERR)
	    call error (0, "Illegal file number list.")

	# Get the output file from the cl.
	call clgstr ("outputfile", outputfile, SZ_FNAME)

	# See if the output is mag tape, if not, error.
	if (mtfile (outputfile) == NO)
	    call error (1, "Outputfile should be magnetic tape.")

	# If no tape file number is given, then ask whether the tape
	# is blank or contains data.  If blank then start at [1], else
	# start at [EOT].

	if (mtneedfileno(outputfile) == YES)
	    if (!clgetb ("new_tape"))
		call mtfname (outputfile, EOT, outputfile, SZ_FNAME)
	    else
		call mtfname (outputfile, 1, outputfile, SZ_FNAME)

	# Get verbose flag.
	verbose = clgetb ("verbose")

        # Loop over files
	whichfile = 1
	filenumber = 0
        while (get_next_number (filerange, filenumber) != EOF) {

	    # Assemble the appropriate tape file name.
	    if (mtneedfileno (inputfile) == NO)
		call strcpy (inputfile, tapename, SZ_FNAME)
	    else
		call mtfname (inputfile, filenumber, tapename, SZ_FNAME)

	    if (whichfile > 1) {
	        # Assemble the appropriate output file name.
		call mtfname (outputfile, EOT, outputfile, SZ_FNAME)
	    }

	    if (verbose) {
		call printf ("reading %s, writing %s\n")
    		    call pargstr(tapename)
    		    call pargstr(outputfile)
	    }

	    iferr {
	        numrecords = tapecopy (tapename, outputfile)
	    } then {
		call eprintf ("Error copying file: %s\n")
		    call pargstr (tapename)
		call erract (EA_WARN)
		next
	    } else if (numrecords == 0) {
	        call printf ("Tape at EOT\n")
		break
	    }
	    whichfile = whichfile + 1

        }  # End while.
end


# TAPECOPY -- This is the actual tape to tape copy routine.

int procedure tapecopy (infile, outfile)

char	infile[SZ_FNAME]
char	outfile[SZ_FNAME]

pointer	bufa, bufb, temp
int	bufsz, numrecords
int	nbytes, lastnbytes, in, out
int	fstati(), mtopen(), awaitb()
errchk	mtopen, areadb, awriteb, awaitb

begin
	# Open input file, see if it has anything in it.  If not, return.
	in = mtopen (infile, READ_ONLY, 0)

	bufsz = fstati (in, F_MAXBUFSIZE)	# Maximum output buffer size.
	if (bufsz == 0)				# If no max, set a max.
	    bufsz = YABUF

	call malloc (bufa, bufsz, TY_CHAR) 	# Allocate output buffer.
	call malloc (bufb, bufsz, TY_CHAR)	# Other output buffer

	call areadb (in, Memc[bufa], bufsz, 0)
	nbytes = awaitb (in)
	if (nbytes == EOF) {
	    call close (in)
	    call mfree (bufa, TY_CHAR)
	    call mfree (bufb, TY_CHAR)
	    return (EOF)
	}

	# Open the output file.
	out = mtopen (outfile, WRITE_ONLY, 0)

	lastnbytes = 0				# Last record size memory.
	numrecords = 0				# Number of records read.

	if (nbytes > 0) {
	    if (nbytes > SZ_VTRECFD*SZB_SHORT &&
		nbytes < SZ_VTRECFD*SZB_SHORT+600)
		nbytes = SZ_VTRECFD*SZB_SHORT
	    call awriteb (out, Memc[bufa], nbytes, 0)
	    call areadb (in, Memc[bufb], bufsz, 0)
	    numrecords = numrecords + 1
	}

	SWAP (bufa, bufb)

	# Main Loop.
	repeat {
	    if (awaitb (out) != nbytes) {
		call printf ("Write error, record = %d.\n")
		    call pargi (numrecords+1)
	    }

	    nbytes = awaitb (in)
	    if (nbytes == ERR) {
		call printf ("Read error, record = %d.\n")
		    call pargi (numrecords+1)
		nbytes = lastnbytes
	    }
	    lastnbytes = nbytes

	    if (nbytes > 0) {
		if (nbytes > SZ_VTRECFD*SZB_SHORT &&
		    nbytes < SZ_VTRECFD*SZB_SHORT+600)
		    nbytes = SZ_VTRECFD*SZB_SHORT
	        call awriteb (out, Memc[bufa], nbytes, 0)
	        call areadb (in, Memc[bufb], bufsz, 0)
	        numrecords = numrecords + 1
	    }

	    SWAP (bufa, bufb)

	} until (nbytes == 0)	# all done

	call mfree (bufa, TY_CHAR)
	call mfree (bufb, TY_CHAR)
	call close (in)
	call close (out)

	return (numrecords)
end
