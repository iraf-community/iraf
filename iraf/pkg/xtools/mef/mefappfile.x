include <pkg/mef.h>

# MEFFAPPFILE.X --  Set of routines to append a FITS units to an FITS file.
#	meff_app_file(mefi, mefo)
#	mef_pakwr (out, card)
#	mef_wrpgcount (out)
#	mef_wrblank (out, nlines)


# MEF_APP_FILE -- Append a FITS file to an existant file. This means the
# first input unit needs to be changed from a Primary to an Extension Unit.

procedure mef_app_file (mefi, mefo)

pointer	mefi	#I input mef descriptor	
pointer	mefo	#O output mef descriptor	

char    dname[1]
long	off
int	status
bool	in_phdu
int	access(), mef_rdhdr_gn()

errchk mef_rdhdr_gn

begin

	# If output file does not exist create a dummy extension
	if (access(MEF_FNAME(mefo), 0,0) == NO) {
	   dname[1] = EOS
	   call mef_dummyhdr (MEF_FD(mefo),dname)
	   MEF_ACMODE(mefo) = APPEND
	}

	in_phdu = true      # The input file has a PHDU

	# Read the first input header unit (PHDU) and change to extension
	# unit while writing to output file.
	status = mef_rdhdr_gn (mefi,0)
	if (status == EOF)
	    call error (13, "EOF encountered on input file")
	call mef_wrhdr (mefi, mefo, in_phdu)

	# Check for dataless unit; if so the data pointer is at the
	# end of the last header block.

	if (MEF_POFF(mefi) == INDEFL)
	    off = MEF_HOFF(mefi) + ((MEF_HSIZE(mefi)+2879)/2880)*1440
	else
	    off = MEF_POFF(mefi)

	# Now copy the data
	call seek (MEF_FD(mefi), off)
	call fcopyo (MEF_FD(mefi), MEF_FD(mefo))
end


# MEF_PAKWR -- Pack a character buffer and write to the output buffer.

procedure mef_pakwr (out, card)

int 	out		#I Output file descriptor	
char 	card[ARB]	#I Input FITS card

size_t	sz_val

begin
	sz_val = 80
	# arg2 : incompatible pointer
	call achtcb (card, card, sz_val)
	sz_val = 40
	call write(out, card, sz_val)
end


# MEF_WRPGCOUNT -- Write PCOUNT and GCOUNT to the output buffer.

procedure mef_wrpgcount (out)

int	out		#I file descriptor

char	line[80]

begin
	call mef_encodei ("PCOUNT", 0, line, "No 'random' parameters")
	call mef_pakwr (out, line)
	call mef_encodei ("GCOUNT", 1, line, "Only one group")
	call mef_pakwr (out, line)
end


# MEF_WRBLANK --  Write a number of blank lines into the output buffer.
# we reach the END card in the 1st block but we run out
# to the 2nd block in the output file. Now fill it up
# with blank.

procedure mef_wrblank (out, olines)

int	out		#I output file descriptor
int	olines		#I number of blank lines

size_t	sz_val
int	nlines, i
char    card[80]
int	imod()

begin
	   nlines = 36 - imod(olines,36) 

	   do i =1, 80
	      card[i] = ' '

	   sz_val = 80
	   # arg2 : incompatible pointer
	   call achtcb (card, card, sz_val)
	   sz_val = 40
	   for(i=1; i<=nlines; i=i+1) 
	      call write(out, card, sz_val)
	   return
end
