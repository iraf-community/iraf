# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<time.h>
include	<fset.h>
include <mii.h>

#------------------------------------------------------------------------------
# FITSSPP.X - IRAF/SPP interface for FITSIO.
# These routines are part of the FITSIO library and are designed to run in
# the IRAF/SPP environment.
#
#     FTOPNX -- Open or create a file.  NOTE:  calls ieesnan[rd]
#     FTCLSX -- Close a file opened with FTOPNX.
#     FTFLSH -- dummy routine to flush a file to disk.  Not needed in IRAF.
#
#     FTGSDT -- Get the current date and time.
#
#     FTMBYT -- move internal file pointer to specified byte
#     FTMOFF -- offset internal file pointer to specified byte
#
#     FTPI2B -- Write an array of Integer*2 bytes to the output FITS file.
#     FTPI4B -- Write an array of Integer*4 bytes to the output FITS file.
#     FTPR4B -- Write an array of Real*4 bytes to the output FITS file.
#     FTPR8B -- Write an array of Real*8 bytes to the output FITS file.
#
#     FTGI2B -- Read an array of Integer*2 bytes from the input FITS file.
#     FTGI4B -- Read an array of Integer*4 bytes from the input FITS file.
#     FTGR4B -- Read an array of Real*4 bytes from the input FITS file.
#     FTGR8B -- Read an array of Real*8 bytes from the input FITS file.
#
#     FTUPCH -- Convert input string  to upper case.
#
#     FTPBYT -- Write a byte sequence to a file.  
#     FTPCBF -- Write a sequence of characters to a file.  (see unix/ or [.vms])
#
#     FTGBYT -- Read a byte sequence from a file.
#     FTGCBF -- Read a sequence of characters from a file. (see unix/ or [.vms])
#
#     FTWRIT -- Write a sequence of bytes to a file
#     FTREAD -- Read a sequence of bytes from a file
#------------------------------------------------------------------------------
#   This software was prepared by High Energy Astrophysic Science Archive
#   Research Center (HEASARC) at the NASA Goddard Space Flight Center. Users
#   shall not, without prior written permission of the U.S. Government,
#   establish a claim to statutory copyright.  The Government and others acting
#   on its behalf, shall have a royalty-free, non-exclusive, irrevocable,
#   worldwide license for Government purposes to publish, distribute,
#   translate, copy, exhibit, and perform such material. 
#------------------------------------------------------------------------------

define	SZ_FITSREC	1440		# FITS record size in chars

# FTOPNX -- Open or create a file.

procedure ftopnx (funit, pkname, oldnew, rwmode, block, status)

int	funit		#I Fortran I/O unit number
%       character*(*) pkname  
int	oldnew		#I file status: 0 =  existing file; else  new file
int	rwmode		#I file access mode: 0 = readonly; else = read/write
int	block		#O FITS record blocking factor 
int	status		#U returned error status (0=ok)

bool	firsttime
int	mode, i, nbuff, fd
char    fname[SZ_PATHNAME]
int	access(), open()
int	fstati()
include	"fitsspp.com"
data	firsttime /true/

begin
	if (status > 0)
	    return

	# Initialize fitsspp common.
	if (firsttime) {
            nxtfld=0
	    call aclri (buflun, NB)
	    firsttime = false
	}

        #  Determine at run time what type of machine we are running on.  
        call ftarch(compid)
        if (compid == 4)
              compid=3

	# Set the values for real and double undefined values, and turn on
	# conversion between IEEE NaN and IRAF INDEF.  (added by PEH)
	# NOTE:  This has a global effect for any task in any executable
	# that is linked with this (ftopnx) routine.
	call ieesnanr (INDEFR)
	call ieesnand (INDEFD)

	# Check for valid unit number.
	if (funit < 1 || funit > 199) {
	    status = 101
	    return
	}

	# Find available buffer slot for this file.
	nbuff = ERR
	do i = 1, NB {
	    if (buflun[i] == 0) {
                 nbuff = i
                 break
	    }
	}

	# Error: no vacant buffer slots left.
	if (nbuff == ERR) {
	    status = 102
	    return
	}

	# Convert Fortran string to an SPP string.
	call f77upk (pkname, fname, SZ_PATHNAME)

	# Get the file access mode.
	if (oldnew == 0) {
	    # Test if file exists.
	    if (access (fname, 0,0) == NO) {
		# Error: file doesn't exist.
		status = 103
		return
	    }

	    # Set the access mode.
	    if (rwmode == 0)
		mode = READ_ONLY
	    else
		mode = READ_WRITE

	    # Set the FITS blocking factor.
	    block = 1
        } else
	    mode = NEW_FILE

	# Open the file.
	iferr (fd = open (fname, mode, BINARY_FILE)) {
	    if (oldnew == 0)
		status = 104
	    else
		status = 105
	    return
	}

	# advise fio that the I/O will be primarily sequential
	call fseti (fd, F_ADVICE, SEQUENTIAL)

	# Store the current size of the file
	filesize[nbuff] = fstati (fd, F_FILESIZE)

	# Initialize the HDU parameters
	bufnum[funit] = nbuff
	chdu[nbuff] = 1
	hdutyp[nbuff] = 0
	maxhdu[nbuff] = 1
	hdstrt[nbuff,1] = 0
	hdend[nbuff] = 0
	nxthdr[nbuff] = 0
	# Data start location is undefined.
	dtstrt[nbuff] = -1000000000

	buflun[nbuff] = funit
        reclen[nbuff] = 2880
	recnum[nbuff] = 0
	bytnum[nbuff] = 2880

	wrmode[nbuff] = (rwmode != 0)

	bufid[funit] = fd
end

# FTCLSX -- Close a file opened with FTOPNX.

procedure ftclsx (iunit, keep, status)

int	iunit 		#I Fortran I/O unit number
bool	keep		#I keep the file (or delete it)?
int	status		#U returned error status (0=ok)

int	fd
int	nbuff
char    fname[SZ_PATHNAME]
include	"fitsspp.com"

begin
	fd = bufid[iunit]
	nbuff = bufnum[iunit]

	if (keep) {
	    iferr (call close(fd))
#	        set error code, if it has not previous been set
	        if (status <= 0) status = 110
        } else {
	    call fstats (fd, F_FILENAME, fname, SZ_PATHNAME)
	    iferr (call close(fd))
#	        set error code, if it has not previous been set
	        if (status <= 0) status = 110

#           now delete the file
            call delete (fname)
	    }

	bufnum[iunit] = 0
	buflun[nbuff] = 0
end

# FTFLSH -- dummy routine to flush a file to disk.  Not needed in IRAF.

procedure ftflsh (nbuff, status)

int	nbuff			#I number of the buffer to be written 
int	status			#U output error status

begin
end

# FTGSDT -- Get the current date and time.

procedure ftgsdt (dd, mm, yy, status)

int	dd		#O day of the month (1-31)
int	mm		#O month of the year (1-12)
int	yy		#O last 2 digits of the year (1992 = 92, 2001 = 01)
int	status		#U returned error status

int	itime
int	tm[LEN_TMSTRUCT]
int	clktime()

begin
	if (status > 0)
	    return

	itime = clktime (0)
	call brktime (itime, tm)

	dd = TM_MDAY(tm)
	mm = TM_MONTH(tm)
	yy = mod (TM_YEAR(tm), 100)
end

# FTMBYT -- move internal file pointer to specified byte

procedure ftmbyt (iunit, bytno, igneof, status)

int     iunit   	#I  fortran I/O unit number
int     bytno   	#I  byte to move to
bool	igneof   	#I  ignore moves past EOF?
int     status  	#U  output error status

int	nbuff
include	"fitsspp.com"

begin
	if (status > 0)
		return

        nbuff = bufnum[iunit]

	recnum[nbuff] = (bytno / reclen[nbuff]) + 1
        bytnum[nbuff] = mod ((bytno), reclen[nbuff])

	if ((bytno >= (filesize[nbuff] * SZB_CHAR)) && !(igneof) )
		status = 107
end

# FTMOFF -- offset internal file pointer to specified byte

procedure ftmoff (iunit, offset, igneof, status)

int     iunit   	#I  fortran I/O unit number
int     offset   	#I  number of byte to move
bool	igneof   	#I  ignore moves past EOF?
int     status  	#U  output error status

int	nbuff,bytno
include	"fitsspp.com"

begin
	if (status > 0)
		return

        nbuff = bufnum[iunit]
        bytno = ((recnum[nbuff]-1) * reclen[nbuff]) + bytnum[nbuff] + offset

	recnum[nbuff] = (bytno / reclen[nbuff]) + 1
        bytnum[nbuff] = mod ((bytno), reclen[nbuff])

	if ((bytno >= (filesize[nbuff] * SZB_CHAR)) && !(igneof) )
		status = 107
end

# FTPI2B -- Write an array of Integer*2 bytes to the output FITS file.
#           Does any required translation from internal machine format to FITS.

procedure ftpi2b (ounit, nvals, incre, i2vals, status)

int     ounit   	#I  fortran I/O unit number
int     nvals   	#I  number of pixels in the i2vals array
int     incre   	#I  byte increment between values
short   i2vals[ARB]  	#I  array of input integer*2 values
int     status  	#U  output error status

int	i
int	offset

begin
        call miipak(i2vals,i2vals,nvals,TY_SHORT,MII_SHORT)

        if (incre .le. 2)
                call ftpbyt(ounit,nvals*2,i2vals,status)
        else   {
#               offset is the number of bytes to move between each value
                offset=incre-2
                call ftpbyt(ounit,2,i2vals,status)
                do  i=2,nvals  {
                        call ftmoff(ounit,offset,true,status)                
                        call ftpbyt(ounit,2,i2vals[i],status)
                }
        }
end


# FTPI4B -- Write an array of Integer*4 bytes to the output FITS file.
#           Does any required translation from internal machine format to FITS.

procedure ftpi4b (ounit, nvals, incre, i4vals, status)

int     ounit   	#I  fortran I/O unit number
int     nvals   	#I  number of pixels in the i4vals array
int     incre   	#I  byte increment between values
int     i4vals[ARB]  	#I  array of input integer*4 values
int     status  	#U  output error status

int	i
int	offset

begin
        call miipak(i4vals,i4vals,nvals,TY_INT,MII_LONG)

        if (incre .le. 4)
                call ftpbyt(ounit,nvals*4,i4vals,status)
        else   {
#               offset is the number of bytes to move between each value
                offset=incre-4
                call ftpbyt(ounit,4,i4vals,status)
                do i=2,nvals  {
                        call ftmoff(ounit,offset,true,status)                
                        call ftpbyt(ounit,4,i4vals[i],status)
                }
        }
end


# FTPR4B -- Write an array of Real*4 bytes to the output FITS file.
#           Does any required translation from internal machine format to FITS.

procedure ftpr4b (ounit, nvals, incre, r4vals, status)

int     ounit   	#I  fortran I/O unit number
int     nvals   	#I  number of pixels in the r4vals array
int     incre   	#I  byte increment between values
real    r4vals[ARB]  	#I  array of input real*4 values
int     status  	#U  output error status

int	i
int	offset

begin
        call miipak(r4vals,r4vals,nvals,TY_REAL,MII_REAL)

        if (incre .le. 4)
                call ftpbyt(ounit,nvals*4,r4vals,status)
        else   {
#               offset is the number of bytes to move between each value
                offset=incre-4
                call ftpbyt(ounit,4,r4vals,status)
                do i=2,nvals  {
                        call ftmoff(ounit,offset,true,status)                
                        call ftpbyt(ounit,4,r4vals[i],status)
                }
        }
end


# FTPR8B -- Write an array of Real*8 bytes to the output FITS file.
#           Does any required translation from internal machine format to FITS.

procedure ftpr8b (ounit, nvals, incre, r8vals, status)

int     ounit   	#I  fortran I/O unit number
int     nvals   	#I  number of pixels in the r8vals array
int     incre   	#I  byte increment between values
double  r8vals[ARB]  	#I  array of input real*8 values
int     status  	#U  output error status

int	i
int	offset

begin
        call miipak(r8vals,r8vals,nvals,TY_DOUBLE,MII_DOUBLE)

        if (incre .le. 8)
                call ftpbyt(ounit,nvals*8,r8vals,status)
        else   {
#               offset is the number of bytes to move between each value
                offset=incre-8
                call ftpbyt(ounit,8,r8vals,status)
                do i=2,nvals  {
                        call ftmoff(ounit,offset,true,status)                
                        call ftpbyt(ounit,8,r8vals[i],status)
                }
        }
end


# FTGI2B -- Read an array of Integer*2 bytes from the input FITS file.
#           Does any required translation from FITS to internal machine format

procedure ftgi2b (iunit, nvals, incre, i2vals, status)

int     iunit   	#I  fortran I/O unit number
int     nvals   	#I  number of pixels in the i2vals array
int     incre   	#I  byte increment between values
short   i2vals[ARB]  	#O  array of output integer*2 values
int     status  	#U  output error status

int	i
int	offset

begin
        if (incre .le. 2)
                call ftgbyt(iunit,nvals*2,i2vals,status)
        else   {
#               offset is the number of bytes to move between each value
                offset=incre-2
                call ftgbyt(iunit,2,i2vals,status)
                do  i=2,nvals  {
                        call ftmoff(iunit,offset,false,status)                
                        call ftgbyt(iunit,2,i2vals[i],status)
                }
        }
        call miiupk(i2vals,i2vals,nvals,MII_SHORT,TY_SHORT)
end


# FTGI4B -- Read an array of Integer*4 bytes from the intput FITS file.
#           Does any required translation from FITS to internal machine format

procedure ftgi4b (iunit, nvals, incre, i4vals, status)

int     iunit   	#I  fortran I/O unit number
int     nvals   	#I  number of pixels in the i4vals array
int     incre   	#I  byte increment between values
int     i4vals[ARB]  	#O  array of output integer*4 values
int     status  	#U  output error status

int	i
int	offset

begin
        if (incre .le. 4)
                call ftgbyt(iunit,nvals*4,i4vals,status)
        else   {
#               offset is the number of bytes to move between each value
                offset=incre-4
                call ftgbyt(iunit,4,i4vals,status)
                do  i=2,nvals  {
                        call ftmoff(iunit,offset,false,status)                
                        call ftgbyt(iunit,4,i4vals[i],status)
                }
        }
        call miiupk(i4vals,i4vals,nvals,MII_LONG,TY_INT)
end


# FTGR4B -- Read an array of Real*4 bytes from the intput FITS file.
#           Does any required translation from FITS to internal machine format

procedure ftgr4b (iunit, nvals, incre, r4vals, status)

int     iunit   	#I  fortran I/O unit number
int     nvals   	#I  number of pixels in the r4vals array
int     incre   	#I  byte increment between values
real    r4vals[ARB]  	#O  array of output real*4 values
int     status  	#U  output error status

int	i
int	offset

begin
        if (incre .le. 4)
                call ftgbyt(iunit,nvals*4,r4vals,status)
        else   {
#               offset is the number of bytes to move between each value
                offset=incre-4
                call ftgbyt(iunit,4,r4vals,status)
                do  i=2,nvals  {
                        call ftmoff(iunit,offset,false,status)                
                        call ftgbyt(iunit,4,r4vals[i],status)
                }
        }
        call miiupk(r4vals,r4vals,nvals,MII_REAL,TY_REAL)
end


# FTGR8B -- Read an array of Real*8 bytes from the intput FITS file.
#           Does any required translation from FITS to internal machine format

procedure ftgr8b (iunit, nvals, incre, r8vals, status)

int     iunit   	#I  fortran I/O unit number
int     nvals   	#I  number of pixels in the r8vals array
int     incre   	#I  byte increment between values
double  r8vals[ARB]  	#O  array of output real*8 values
int     status  	#U  output error status

int	i
int	offset

begin
        if (incre .le. 8)
                call ftgbyt(iunit,nvals*8,r8vals,status)
        else   {
#               offset is the number of bytes to move between each value
                offset=incre-8
                call ftgbyt(iunit,8,r8vals,status)
                do  i=2,nvals  {
                        call ftmoff(iunit,offset,false,status)                
                        call ftgbyt(iunit,8,r8vals[i],status)
                }
        }
        call miiupk(r8vals,r8vals,nvals,MII_DOUBLE,TY_DOUBLE)
end

# FTUPCH -- Convert input string (a Fortran character string) to upper case.

procedure ftupch (fstr)

%       character fstr*(*)
char	sstr[SZ_LINE]

begin
	call f77upk (fstr, sstr, SZ_LINE)
	call strupr (sstr)
	call f77pak (sstr, fstr, SZ_LINE)
end

# FTPBYT -- Write a byte sequence to a file.  The sequence may begin on any
# byte boundary and may be any number of bytes long.

procedure ftpbyt (iunit, nbytes, array, status)

int	iunit		#I fortran unit number
int	nbytes		#I number of bytes to be transferred
char	array[ARB]	#I input data buffer
int	status		#U output error status

int	fd, nbuff, fpos, hdtype
int	bytes_per_record
include	"fitsspp.com"

begin
	# Special cases.
        if (status > 0)
	    return
        if (nbytes <= 0) {
	    status = 306
	    return
        }

	fd = bufid[iunit]

	# Get byte index in file.
        nbuff = bufnum[iunit]
	bytes_per_record = reclen[nbuff]
        hdtype = hdutyp[nbuff]

	# zero indexed byte position in the file
	fpos = bytes_per_record * (recnum[nbuff]-1) + bytnum[nbuff]

	# Write the data.
	iferr (call ftwrit (fd, array, hdtype, fpos, nbytes,
               filesize[nbuff])) {
	    status = 107
	    return
	}

	# Update the FITSIO common to track the new file position.
	fpos = fpos + nbytes

	recnum[nbuff] = (fpos / bytes_per_record)+1
        bytnum[nbuff] = mod (fpos, bytes_per_record)
end

# FTGBYT -- Read a byte sequence from a file.  The sequence may begin on any
# byte boundary and may be any number of bytes long.  An error status is
# returned if less than the requested amount of data is read.

procedure ftgbyt (iunit, nbytes, array, status)

int	iunit		#I fortran unit number
int	nbytes		#I number of bytes to be transferred
char	array[ARB]	#O output data buffer
int	status		#U output error status

int	bytes_per_record
int	fd, nbuff, fpos, nb
int	ftread()
include	"fitsspp.com"

begin
	# Special cases.
        if (status > 0 || nbytes == 0)
	    return
        if (nbytes < 0) {
	    status = 306
	    return
        }

	fd = bufid[iunit]

	# Get byte index in file.
        nbuff = bufnum[iunit]
	bytes_per_record = reclen[nbuff]

	# zero indexed byte position in the file
	fpos = bytes_per_record * (recnum[nbuff]-1) + bytnum[nbuff]

	# Read the data.
	iferr (nb = ftread (fd, array, fpos, nbytes)) {
	    status = 107
	    return
	} else if (nb != nbytes) {
	    status = 107
          }

	# Update the FITSIO common to track the new file position.
	fpos = fpos + max (0, nb)

	recnum[nbuff] = (fpos / bytes_per_record)+1
        bytnum[nbuff] = mod (fpos, bytes_per_record)
end

# FTWRIT -- Write a sequence of bytes to a file at the indicated
# position.  The sequence can begin at any byte and can be any number of
# bytes long.
#
# This routine could be implemented more efficiently using fwritep to
# directly access the file buffer for unaligned transfers, but so long
# as most transfers are aligned the following code is as fast as anything.

procedure ftwrit (fd, ibuf, hdtype, fpos, nbytes, fsize)

int	fd			#I file descriptor
char	ibuf[ARB]		#I data buffer
int	hdtype			#I type of HDU (1=ASCII table)
int	fpos			#I starting byte (0 index) in output file
int	nbytes			#I number of bytes to transfer
int	fsize			#I current size of the file

char	ch
pointer	sp, bp
int	start_char, endchr
int	nchars, boff, junk, bufsize, nc
errchk	getc, seek, write, malloc
char	getc()

bool	initialized
char	blanks[SZ_FITSREC], zeros[SZ_FITSREC]
data	initialized /false/

begin
	call smark (sp)

	# The first time we are called initialize the empty (blank or
	# zero fill) FITS records.

	if (!initialized) {
	    bufsize = SZ_FITSREC * SZB_CHAR
	    call malloc (bp, bufsize, TY_CHAR)

	    ch = ' '
	    call amovkc (ch, Memc[bp], bufsize)
	    call achtcb (Memc[bp], blanks, bufsize)
	    call aclrc (zeros, SZ_FITSREC)

	    call mfree (bp, TY_CHAR)
	    initialized = true
	}

        # Get index of first and last file chars.
        start_char = fpos / SZB_CHAR + 1
        endchr = (fpos+nbytes - 1) / SZB_CHAR + 1
        nchars = endchr - start_char + 1
	boff = mod (fpos, SZB_CHAR)

	# If write starting point is beyond the end of file,
	# then insert fill bytes from the current end of file to
	# the starting point.  

	if (start_char > fsize+1) {

	    # Extend the file, using blank or zero fill.  Blank fill is
	    # used for ascii tables (hdtype=1) otherwise zero fill is used.

	    call seek (fd, fsize + 1)
	    while (fsize < start_char) {
		nc=min(start_char-fsize, SZ_FITSREC)
		if (hdtype == 1)
		    call write (fd, blanks, nc)
		else
		    call write (fd, zeros, nc)

		fsize = fsize + nc
	    }
	}

	# If things are nicely aligned write data directly to the output file

	if (boff == 0 && mod(nbytes,SZB_CHAR) == 0) {
	    call seek (fd, start_char)
	    call write (fd, ibuf, nchars)

	} else {

	    # Allocate intermediate buffer.
	    call salloc (bp, nchars, TY_CHAR)

	    # Get any partial chars at ends of sequence.
	    if (boff > 0) {
	        call seek (fd, start_char)
	        junk = getc (fd, Memc[bp])
	    }
	    if (mod (fpos+nbytes, SZB_CHAR) != 0) {
		if (endchr > fsize) {
		    # off end of file, so add correct fill value to last char
		    if (hdtype == 1)
			Memc[bp+nchars-1]=blanks[1]
		    else
			Memc[bp+nchars-1]=0
		} else {
		    # read existing byte in file, and insert the char
		    call seek (fd, endchr)
		    junk = getc (fd, Memc[bp+nchars-1])
		}
	    }

	    # Insert data segment into buffer.
	    call bytmov (ibuf, 1, Memc[bp], boff + 1, nbytes)

	    # Write edited sequence to output file.
	    call seek (fd, start_char)
	    call write (fd, Memc[bp], nchars)
 	}

	fsize=max(fsize,endchr)

	# Now, if file is not a multiple of 2880 bytes long, pad it with fill

	nc=SZ_FITSREC - mod(fsize, SZ_FITSREC)
	if (nc .ne. SZ_FITSREC) {

		call seek (fd, fsize + 1)
		if (hdtype == 1)
		    call write (fd, blanks, nc)
		else
		    call write (fd, zeros, nc)
		fsize = fsize + nc
	}

	call sfree (sp)
end

# FTREAD -- Read a sequence of bytes from a file at the indicated
# position.  The sequence can begin at any byte and can be any number of
# bytes long.
#
# This routine could be implemented more efficiently using freadp to
# directly access the file buffer for unaligned transfers, but so long
# as most transfers are aligned the following code is as fast as anything.

int procedure ftread (fd, obuf, fpos, nbytes)

int	fd			#I file descriptor
char	obuf[ARB]		#O output buffer
int	fpos			#I starting byte (zero index) in input file
int	nbytes			#I number of bytes to transfer

pointer	sp, bp
int	start_char, endchr
int	nchars, boff, iostat, nout
int	read()
errchk	read

begin
        # Get index of first and last file chars.
        start_char = fpos / SZB_CHAR + 1
        endchr = (fpos+nbytes  - 1) / SZB_CHAR + 1
        nchars = endchr - start_char + 1
	boff = mod (fpos, SZB_CHAR)

	# If things are nicely aligned read data directly into the output
	# buffer and we are done.

	call seek (fd, start_char)
	if (boff == 0 && mod(nbytes,SZB_CHAR) == 0)
	    return (read (fd, obuf, nchars) * SZB_CHAR)

	# Allocate intermediate buffer.
	call smark (sp)
	call salloc (bp, nchars, TY_CHAR)

	# Read raw file segment.
	iostat = read (fd, Memc[bp], nchars)
	if (iostat == EOF) {
	    call sfree (sp)
	    return (0)
	}

	# Extract and return desired bytes.
	nout = min (nbytes, iostat * SZB_CHAR - boff)
	call bytmov (Memc[bp], boff + 1, obuf, 1, nout)

	call sfree (sp)
	return (nout)
end
