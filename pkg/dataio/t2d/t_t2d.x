# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <fset.h>
include <mach.h>

define	OBUF_PAD	35536
define	SZ_OBUF		100000
define	SWAP		{temp=$1;$1=$2;$2=temp}
define	MAX_RANGES	200

# T2D -- This is an asynchronous tape to disk copy routine.
# T2d sets up a large output buffer (many blocks long) and reads from
# the input device directly into this output buffer, keeping track of where in
# the output buffer it is.  When it reaches a predetermined point in the output
# buffer, it writes an integral number of blocks to the output device, moves
# the leftover input data to the beginning of the alternate buffer and
# continues reading. (until EOF, then it writes out whatever is left).
# The user specifies which files on tape he or she wants and a root name
# for the output file names.

procedure t_t2d()

char	input[SZ_FNAME]
char	files[SZ_LINE]
char	ofroot[SZ_FNAME]		# Root file name, output files.

char	tapename[SZ_FNAME]
char	dfilename[SZ_FNAME]		# Disk file name.
int	filerange[2 * MAX_RANGES + 1]
int	nfiles, filenumber, numrecords
bool	verbose 
bool	errignore

int	mtfile(), strlen(), decode_ranges(), mtneedfileno()
int	get_next_number(), tape2disk()
bool	clgetb()

begin
	call fseti (STDOUT, F_FLUSHNL, YES)

        # Get input file(s).
        call clgstr ("input", input, SZ_FNAME)
        if (mtfile(input) == NO || mtneedfileno(input) == NO)
	    call strcpy ("1", files, SZ_LINE)
        else
	    call clgstr ("files", files, SZ_LINE)

        if (decode_ranges (files, filerange, MAX_RANGES, nfiles) == ERR)
	    call error (0, "Illegal file number list.")

	# Get root output filename, the verbose flag, and the error-ignore flag.
	call clgstr ("ofroot", ofroot, SZ_FNAME)
	verbose = clgetb ("verbose")
	errignore = clgetb ("errignore")
	filenumber = 0

        if (mtfile(input) == YES && mtneedfileno(input) == YES) {
            # Loop over files
            while (get_next_number (filerange, filenumber) != EOF) {

	        # Assemble the appropriate tape file name.
		call mtfname (input, filenumber, tapename, SZ_FNAME)

	        # Assemble the appropriate disk file name.
	        call strcpy (ofroot, dfilename, SZ_FNAME)
	        call sprintf (dfilename[strlen(ofroot) + 1], SZ_FNAME, "%03d")
		    call pargi (filenumber)

	        # Print out the tape file we are trying to read.
	        if (verbose) {
		    call printf ("%s ")
    		        call pargstr (tapename)
		    call flush (STDOUT)
	        }


	        # Do the tape to disk transfer.
	        iferr {
	            numrecords = tape2disk (tapename, dfilename, errignore)
	        } then {
		    call eprintf ("Error reading file: %s\n")
		        call pargstr (tapename)
		    call erract (EA_WARN)
		    next
	        } else if (numrecords == 0) {
		    call deletefg (dfilename, YES, YES)
		    if (verbose)
	                call printf ("Tape at EOT\n")
		    break
	        } else if (verbose){
		    call printf (" wrote `%s'\n")
    		        call pargstr(dfilename)
	        }

            }  # End while.

	} else {

	    # Print out the tape file we are trying to read.
	    if (verbose) {
		call printf ("%s ")
    		    call pargstr(input)
		call flush (STDOUT)
	    }

	    # Do the tape to disk transfer.
	    iferr {
	        numrecords = tape2disk (input, ofroot, errignore)
	    } then {
	        call eprintf ("Error reading file: %s\n")
		    call pargstr (input)
		call erract (EA_WARN)
	    } else if (numrecords == 0) {
		call deletefg (input, YES, YES)
		if (verbose)
	            call printf ("Tape at EOT\n")
	    } else if (verbose){
		call printf (" wrote `%s'\n")
    		    call pargstr(ofroot)
	    }
	}
end


# TAPE2DISK -- This is the actual tape to disk copy routine.

int procedure tape2disk (infile, outfile, errignore)

char	infile[SZ_FNAME]
char	outfile[SZ_FNAME]
bool	errignore

bool	inblock
int	blksize, mxbufszo, numblks, cutoff, obufsize, temp, numrecords
int	inblksize, innumblks, toread, mxbufszi
long	ooffset
int	nchars, stat, in, out, lastnchars
pointer	op, otop, bufa, bufb

int	fstati(), mtopen(), open(), await()

begin
	# Open the input and output files.
	in = mtopen (infile, READ_ONLY, 0)
	out = open (outfile, NEW_FILE, BINARY_FILE)

	# Find out how big the blocks are on the output device. Calculate
	# an output buffer size which is an integral number of blocks long
	# and is long enough to permit many input reads per output write.
	# Here, I use the maximum output buffer size.

	blksize = fstati (out, F_BLKSIZE)	# Outputfile block size
	mxbufszo = fstati (out, F_MAXBUFSIZE)	# Maximum output buffer size
	mxbufszi = fstati (in, F_MAXBUFSIZE)	# Maximum in buffer size
	if (mxbufszo <= 0)			# if no max, set a max
	    mxbufszo = SZ_OBUF
	if (mxbufszi <= 0)			# if no max, set a max
	    mxbufszi = OBUF_PAD
	numblks = mxbufszo / blksize		# No. blocks in 'out' buffer

	# Find out if the input device is blocked and if it is, the block
	# size.
	inblksize = fstati (in, F_BLKSIZE)	# Inputfile block size
	inblock = true
	if (inblksize == 0)
	    inblock = false

	# Put an extra OBUF_PAD chars in the output buffer to allow for
	# overruns on the last input read before we do an output write.

	cutoff = numblks * blksize
	obufsize = cutoff + OBUF_PAD

	call malloc (bufa, obufsize, TY_CHAR) 	# Allocate output buffer.
	call malloc (bufb, obufsize, TY_CHAR)	# Other output buffer
	op = bufa				# Movable pointer inside buffer
	otop = bufa + cutoff			# Point to full position (top)

	ooffset = 1				# Output offset.
	nchars = 0				# Number of chars.
	numrecords = 0				# Number of records read.
	lastnchars = 0

	# Main Loop.
	repeat {
	    # A series of reads of the input file are required to fill the
	    # output buffer.

	    if (inblock) {
		innumblks = (cutoff - (op - bufa)) / inblksize
		toread = (innumblks+1) * inblksize

		call aread (in, Memc[op], toread, 1)
		nchars = await (in)
		if (nchars <= 0) {
		    if (nchars == ERR) {
			# report read error
			call eprintf ("error on read\n")
			call flush (STDERR)

			# If errignore, do not move pointer, else,
			# assume data.
			if (!errignore)
			    nchars = toread
		    }
		    # If we found the EOF
		    if (nchars == 0) {
			cutoff = op - bufa
		    }
		} else if (nchars < toread) {
		    numrecords = numrecords + 1
		    cutoff = op - bufa + nchars
		    nchars = 0
		} else {
		    numrecords = numrecords + 1
		    op = op + nchars
		}

	    } else {

		repeat {
		    call aread (in, Memc[op], mxbufszi, 1)
		    nchars = await (in)

		    if (nchars <= 0) {
			if (nchars == ERR) {
			    # report read error
			    call eprintf ("error on read\n")
			    call flush (STDERR)

			    # If errignore, do not move pointer, else,
			    # assume data.
			    if (!errignore)
				nchars = lastnchars
			}
			# If we found the EOF
			if (nchars == 0) {
			    cutoff = op - bufa
			    break
			}
		    }

		    if (nchars > 0) {
			numrecords = numrecords + 1
			lastnchars = nchars
			op = op + nchars
		    }

		} until (op >= otop)
	    }  # end of 'if (inblock)'

	    # Wait for last write to finish and initiate next write.
	    stat = await (out)
	    if (stat == ERR)
		call eprintf ("error on write\n")
	    call awrite (out, Memc[bufa], cutoff, ooffset)
	    ooffset = ooffset + cutoff	    # Update the output offset.

	    # Copy leftover buffer elements into the bottom of other buffer.
	    if ((op - otop) > 0)
		call amovc (Memc[otop], Memc[bufb], op - otop)
	    op = bufb + (op - otop)

	    # Swap buffers
	    SWAP (bufa, bufb)
	    otop = bufa + cutoff
	} until (nchars == 0)	# all done

	stat = await (out)	# wait for final write to finish.
	if (stat == ERR)
	    call eprintf ("error on write\n")

	call close (in)
	call close (out)
	call mfree (bufa, TY_CHAR)
	call mfree (bufb, TY_CHAR)

	return (numrecords)
end
