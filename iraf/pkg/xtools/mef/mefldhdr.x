include <error.h>
include <mach.h>
include <ctype.h>
include <mii.h>
include <pkg/mef.h>

# MEF_LOAD_HEADER -- Load a FITS header from a file descriptor into a
# spool file.

int procedure mef_load_header (mef, spool, group)

pointer mef			#I FITS descriptor
int	spool			#I spool output file descriptor
int	group			#I Currrent group

pointer lbuf, sp, fb
int	nchars, index, ncards, pcount, in
int	mef_read_card(), mef_kctype()
int	note()

errchk  mef_read_card
begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (fb, FITS_BLOCK_BYTES, TY_CHAR)

	MEF_EXTNAME(mef) = EOS
	MEF_EXTVER(mef) = INDEFL

	in = MEF_FD(mef)
	MEF_HOFF(mef) = note(in)

	# Read successive lines of the FITS header. 
	pcount = 0
	ncards = 0
	repeat {
	    # Get the next input line.
	    nchars = mef_read_card (in, Memc[fb], Memc[lbuf], ncards)
	    if (nchars == EOF) {
		call close (spool)
		return (EOF)
            }
	    ncards = ncards + 1
	    # A FITS header card already has 80 chars, just add the newline.
	    Memc[lbuf+LEN_CARD] = '\n'
	    Memc[lbuf+LEN_CARD+1] = EOS
	    call putline (spool, Memc[lbuf])

	    # Process the header card.
	    switch (mef_kctype (Memc[lbuf], index)) {
	    case END:
		MEF_HSIZE(mef) = ncards*LEN_CARDNL
		break
	    case SIMPLE:
		call strcpy ("SIMPLE", MEF_EXTTYPE(mef), SZ_EXTTYPE)
	    case XTENSION:
		call mef_gvalt (Memc[lbuf], MEF_EXTTYPE(mef), SZ_EXTTYPE)
	    case EXTNAME:
		call mef_gvalt (Memc[lbuf], MEF_EXTNAME(mef), LEN_CARD)
	    case EXTVER:
		call mef_gvali (Memc[lbuf], MEF_EXTVER(mef))
	    case PCOUNT:
		call mef_gvali (Memc[lbuf], pcount)
		MEF_PCOUNT(mef) = pcount
	    case BITPIX:
		call mef_gvali (Memc[lbuf], MEF_BITPIX(mef))
	    case NAXIS:
		call mef_gvali (Memc[lbuf], MEF_NDIM(mef))
	    case NAXISN:
		 call mef_gvali (Memc[lbuf], MEF_NAXIS(mef,index))
	    case OBJECT:
		call mef_gvalt (Memc[lbuf], MEF_OBJECT(mef), MEF_SZVALSTR)
	    default:
		if (ncards == 1) {
		    call sprintf(Memc[lbuf], SZ_LINE,
		    "Header does not start with SIMPLE nor XTENSION: %s[%d]")
		       call pargstr(MEF_FNAME(mef))
		       call pargi(group)
		    call error (13, Memc[lbuf])	
		}
	    }
	}

	call sfree (sp)
	return (OK)
end


# MEF_GET_CARD -- Read a FITS header card.

int procedure mef_read_card (fd, ibuf, obuf, ncards)

int	fd			#I Input file descriptor
char	ibuf[ARB]		#I input buffer
char    obuf[ARB]		#O Output buffer
int	ncards			#I ncards read so far

int	ip, nchars_read
int	read()
errchk	read

begin
	# We read one FITS block first, read card from it until 36
	# cards have been processed, where we read again.

	if (mod (ncards, 36) == 0) {
	    nchars_read = read (fd, ibuf, FITS_BLKSZ_CHAR)
	    if (nchars_read ==  EOF)
	        return (EOF)
	    call miiupk (ibuf, ibuf, FITS_BLOCK_BYTES, MII_BYTE, TY_CHAR)
	    ip = 1
	}
	
	call amovc (ibuf[ip], obuf, LEN_CARD)
	ip = ip + LEN_CARD

	return (LEN_CARD)
end
