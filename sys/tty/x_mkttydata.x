# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<syserr.h>
include	"tty.h"

task	mkttydata = t_mkttydata

.help mkttydata
.nf -------------------------------------------------------------------------
MKTTYDATA -- System Manager's program to compile the TERMCAP entries for a
list of terminals.  Output is an SPP format include file to be used in TTYLOAD
to statically declare and initialize the TERMCAP entries for the named
devices, eliminating the need to scan the TERMCAP file for those devices.

Compilation of selected termcap entries to speed up accesses to the
termcap database for frequently referenced terminals.  We read and preprocess
the entries for the named terminals from the termcap file, producing the source
code for the TTYLOAD procedure as output.  The termcap entry for each device
is included in the source for TTYLOAD as a static data structure.  TTYLOAD is
subsequently compiled and placed in the library with the other TTY routines.
At run time, TTYODES first tries to load the termcap database entry using
TTYLOAD, and if that fails it goes and reads the termcap file.

N.B.: The TTY interface may be used for any termcap format file, regardless
of whether or not the database describes terminals.
.endhelp ---------------------------------------------------------------------

# Tunable parameters.

define	MAX_DEVICES	25		# initial max termcap entries
define	INC_DEVICES	25		# increment if overflow occurs
define	SZ_SBUF		4096		# initial size of string buffer
define	INC_SZSBUF	2048		# increment if overflow occurs
define	NI_PERLINE	5		# number of datastmt ints per line
define	NC_PERLINE	8		# number of datastmt chars per line

# Device descriptor structure (contains the extracted termcap entries).
# There are no upper limits on the number of devices or upon the sizes of
# any of the substructures.

define	LEN_TCSTRUCT	8

define	TC_NDEVICES	Memi[$1]	# number of termcap entries
define	TC_MAXDEVICES	Memi[$1+1]	# initial max termcap entries
define	TC_DEVNAME_P	Memi[$1+2]	# pointer to devname index array
define	TC_CAPLIST_P	Memi[$1+3]	# pointer to caplist index array
define	TC_SBUF		Memi[$1+4]	# pointer to string buffer
define	TC_SZSBUF	Memi[$1+5]	# current size of string buffer
define	TC_NEXTCH	Memi[$1+6]	# offset of next avail char in sbuf
define	TC_TCFNAME	Memi[$1+7]	# name of termcap file

define	TC_DEVNAME	Memi[TC_DEVNAME_P($1)+$2-1]
define	TC_CAPLIST	Memi[TC_CAPLIST_P($1)+$2-1]

# MKTTYDATA -- Given the name of a termcap format file and a list of device
# names, call TTYOPEN to fetch the termcap entry of the device.
# Move the entry for the device into the dev structure and continue until
# the entries for all devices have been read.  Write out the source code for
# the data structures of these devices.  This output file is "included"
# when TTYLOAD is later compiled, cacheing the termcap entries for the
# named devices in memory.

procedure t_mkttydata()

bool	verbose
int	devlist, fd, ndev, buflen
pointer	sp, termcap_file, output_file, devname, tc, tty
bool	clgetb()
int	clpopnu(), clgfil(), tc_putstr(), open(), tc_dummy_ttyload()
pointer	ttyopen()
extern	tc_dummy_ttyload()
errchk	open, tc_write_data_declarations, clgfil, tc_putstr, malloc, realloc

begin
	call smark (sp)
	call salloc (termcap_file, SZ_FNAME, TY_CHAR)
	call salloc (output_file, SZ_FNAME, TY_CHAR)
	call salloc (devname, SZ_FNAME, TY_CHAR)
	call salloc (tc, LEN_TCSTRUCT, TY_STRUCT)

	# Open the list of devices to be compiled into the cache.  CLGFIL is
	# useful for reading the list even though the list elements are not
	# filenames, because it can expand comma a delimited list passed as
	# a string as well as read from a list file.  The list is not sorted
	# so that the caller can order the devices in the order in which they
	# will most frequently be referenced (though really it matters little).
	# Get the names of the input and output files.

	devlist = clpopnu ("devlist")
	call clgstr ("termcap_file", Memc[termcap_file], SZ_FNAME)
	call clgstr ("output_file",  Memc[output_file],  SZ_FNAME)
	verbose = clgetb ("verbose")

	# Initialize the TC descriptor structure. Allocate the variable sized
	# buffers.

	ndev   = 0
	buflen = MAX_DEVICES

	TC_NDEVICES(tc)   = 0
	TC_MAXDEVICES(tc) = MAX_DEVICES
	TC_SZSBUF(tc)     = SZ_SBUF
	TC_NEXTCH(tc)     = 0

	iferr {
	    call malloc (TC_DEVNAME_P(tc), buflen, TY_INT)
	    call malloc (TC_CAPLIST_P(tc), buflen, TY_INT)
	    call malloc (TC_SBUF(tc), SZ_SBUF, TY_CHAR)
	} then
	    call erract (EA_FATAL)

	# Store the name of the termcap file in the descriptor.  The descriptor
	# is only valid if TTYLOAD is called with the exact same filename.

	TC_TCFNAME(tc) = tc_putstr (tc, Memc[termcap_file])

	# Fetch the termcap entry for each device in the list.  This is not
	# done very efficiently, but it does not matter since this program
	# is infrequently run.  Accumulate the entries in the TC structure.

	while (clgfil (devlist, Memc[devname], SZ_FNAME) != EOF) {
	    # Fetch entry from termcap file.

	    iferr (tty = ttyopen (Memc[termcap_file], Memc[devname],
		tc_dummy_ttyload)) {

		call erract (EA_WARN)
		next

	    } else if (verbose) {
		call eprintf ("%4d %s: %d chars\n")
		    call pargi (ndev + 1)
		    call pargstr (Memc[devname])
		    call pargi (T_CAPLEN(tty))
	    }

	    ndev = ndev + 1
	    TC_NDEVICES(tc) = ndev

	    # Make room for more devices if necessary.
	    if (ndev > TC_MAXDEVICES(tc)) {
		TC_MAXDEVICES(tc) = TC_MAXDEVICES(tc) + INC_DEVICES
		buflen = TC_MAXDEVICES(tc)
		iferr {
		    call realloc (TC_DEVNAME_P(tc), buflen, TY_INT)
		    call realloc (TC_CAPLIST_P(tc), buflen, TY_INT)
		} then
		    call erract (EA_FATAL)
	    }

	    # Add entry to descriptor.
	    TC_DEVNAME(tc,ndev) = tc_putstr (tc, Memc[devname])
	    TC_CAPLIST(tc,ndev) = tc_putstr (tc, T_CAPLIST(tty))

	    call ttyclose (tty)
	}

	call clpcls (devlist)

	# Write the output file (an SPP "include" file) containing data
	# declarations for the data structures in the TC structure.

	iferr (call delete (Memc[output_file]))
	    ;
	fd = open (Memc[output_file], NEW_FILE, TEXT_FILE)
	call tc_write_data_declarations (fd, tc, Memc[termcap_file])
	call close (fd)

	call mfree (TC_DEVNAME_P(tc), TY_INT)
	call mfree (TC_CAPLIST_P(tc), TY_INT)
	call mfree (TC_SBUF(tc), TY_CHAR)
	call sfree (sp)
end


# TC_PUTSTR -- Put a string (incl EOS) in the string buffer at nextch.
# If there is not enough space in the buffer, reallocate a larger buffer.
# Return the index of the string in the string buffer.

int procedure tc_putstr (tc, str)

pointer	tc
char	str[ARB]
int	nextch, nchars, strlen()
errchk	realloc

begin
	# Null strings are not stored and cause a null index to be returned.
	nchars = strlen (str)
	if (nchars == 0)
	    return (0)

	nextch = TC_NEXTCH(tc)
	if (nextch + nchars + 1 > TC_SZSBUF(tc)) {
	    TC_SZSBUF(tc) = TC_SZSBUF(tc) + INC_SZSBUF
	    call realloc (TC_SBUF(tc), TC_SZSBUF(tc), TY_CHAR)
	}

	call strcpy (str, Memc[TC_SBUF(tc) + nextch], ARB)
	TC_NEXTCH(tc) = nextch + nchars + 1

	return (nextch)
end


# TC_WRITE_DATA_DECLARATIONS -- Write the SPP data declarations required to
# declare and initialize the following data structures:
#
#	int	ndevices	# number of devices in cache
#	int	devname[]	# 0-indexed offset into sbuf of device name
#	int	devcaps[]	# 0-indexed offset into sbuf of termcap entry
#	char	sbuf[]		# string buffer

procedure tc_write_data_declarations (fd, tc, termcap_file)

int	fd			# output file
pointer	tc			# TC descriptor
char	termcap_file[ARB]	# name of source file

int	ndevices, dev
pointer	sbuf
int	strlen()

begin
	ndevices = TC_NDEVICES(tc)
	sbuf = TC_SBUF(tc)

	# Write a comments section naming the devices represented by the
	# data declarations which follow.

	call fprintf (fd,
	    "# TERMCAP data declarations for %d devices from '%s'\n")
	    call pargi (TC_NDEVICES(tc))
	    call pargstr (termcap_file)

	do dev = 1, ndevices {
	    call fprintf (fd, "#%15s (size %d+1 chars)\n")
		call pargstr (Memc[sbuf+TC_DEVNAME(tc,dev)])
		call pargi (strlen (Memc[sbuf+TC_CAPLIST(tc,dev)]))
	}

	# Output the object declarations.

	call fprintf (fd, "\n")
	call fprintf (fd, "int\ttermcap_filename, ndevices, i\n")
	call fprintf (fd, "int\tdevname[%d], devcaps[%d]\n")
	    call pargi (ndevices)
	    call pargi (ndevices)
	
	# Do not add 1 char for the EOS; SPP compiler automatically does so.
	call fprintf (fd, "char\tsbuf[%d]\n")
	    call pargi (TC_NEXTCH(tc))

	# Output the data initialization declarations.

	call fprintf (fd, "\n")
	call fprintf (fd, "data\tndevices /%d/\n")
	    call pargi (ndevices)
	call fprintf (fd, "data\ttermcap_filename /%d/\n")
	    call pargi (TC_TCFNAME(tc) + 1)

	call tc_init_datai (fd, "devname", Memi[TC_DEVNAME_P(tc)], ndevices)
	call tc_init_datai (fd, "devcaps", Memi[TC_CAPLIST_P(tc)], ndevices)
	call fprintf (fd, "\n")
	call tc_init_datac (fd, "sbuf",    Memc[TC_SBUF(tc)], TC_NEXTCH(tc)+1)
end


# TC_INIT_DATAI -- Write a series of data statements to initialize an
# integer array.  A single large statement is not used due to variation
# in the permissible number of continuation statements permitted by
# different compilers.

procedure tc_init_datai (fd, varname, array, npix)

int	fd		# output file
char	varname[ARB]	# name of variable to be initialized
int	array[npix]	# array values
int	npix

int	i, j, i1, i2

begin
	for (j=1;  j <= npix;  j = j + NI_PERLINE) {
	    i1 = j
	    i2 = min (j + NI_PERLINE - 1, npix)

	    # Begin new data statement.
	    call fprintf (fd, "data\t(%s(i),i=%2d,%2d)\t/")
		call pargstr (varname)
		call pargi (i1)
		call pargi (i2)

	    # Output data values.  NOTE: the TC_SBUF offsets are zero-indexed
	    # offsets into Mem, but the SBUF array in the include file is a
	    # static Fortran array which requires 1-indexed offsets, so we
	    # add one before writing out the offsets.

	    for (i=i1;  i <= i2;  i=i+1) {
		if (i > i1)
		    call fprintf (fd, ", ")
		call fprintf (fd, "%d")
		    call pargi (array[i] + 1)
	    }

	    # Terminate statement.
	    call fprintf (fd, "/\n")
	}
end


# TC_INIT_DATAC -- Write a series of data statements to initialize a
# char array.  A single large statement is not used due to variation
# in the permissible number of continuation statements permitted by
# different compilers.

procedure tc_init_datac (fd, varname, str, nchars)

int	fd			# output file
char	varname[ARB]		# name of variable to be initialized
char	str[nchars]		# array values
int	nchars

int	i, j, i1, i2

begin
	for (j=1;  j <= nchars;  j = j + NC_PERLINE) {
	    i1 = j
	    i2 = min (j + NC_PERLINE - 1, nchars)

	    # Begin new data statement.
	    call fprintf (fd, "data\t(%s(i),i=%2d,%2d)\t/")
		call pargstr (varname)
		call pargi (i1)
		call pargi (i2)

	    # Output data values.
	    for (i=i1;  i <= i2;  i=i+1) {
		if (i > i1)
		    call fprintf (fd, ", ")
		call fprintf (fd, "%3d")
		    call pargc (str[i])
	    }

	    # Terminate statement.
	    call fprintf (fd, "/\n")
	}
end


# TC_DUMMY_TTYLOAD -- Since we are rebuilding a TTYLOAD, we cannot pass
# a real one to TTYOPEN.  This dummy procedure returns 0 to TTYOPEN for
# all devices, forcing TTYOPEN to open and scan the termcap file to fetch
# the termcap entry for a device.

int procedure tc_dummy_ttyload (termcap_file, devname, outstr, maxch)

char	termcap_file[ARB]
char	devname[ARB]
char	outstr[maxch]
int	maxch

begin
	outstr[1] = EOS
	return (0)
end
