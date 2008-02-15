# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <psset.h>
include "psio.h"

define	PSPAGE_ENV	"pspage"


# PS_OPEN -- Initialize the PSTOOLS structure.

pointer procedure ps_open (fd, default_footer)

int	fd					#I output file descriptor
int	default_footer				#I option flags

pointer	ps
int	scale
char	page[SZ_FNAME], version[SZ_FNAME]
int	envgets()
bool	streq()
errchk	calloc, syserr

begin
	# Allocate the structure.
	iferr {
	    call calloc (ps, LEN_PSSTRUCT, TY_STRUCT)

	    call calloc (PS_HLE(ps), SZ_WORD, TY_CHAR)
	    call calloc (PS_HCE(ps), SZ_WORD, TY_CHAR)
	    call calloc (PS_HRE(ps), SZ_WORD, TY_CHAR)
	    call calloc (PS_FLE(ps), SZ_WORD, TY_CHAR)
	    call calloc (PS_FCE(ps), SZ_WORD, TY_CHAR)
	    call calloc (PS_FRE(ps), SZ_WORD, TY_CHAR)
	
	    call calloc (PS_WBPTR(ps), SZ_LINE, TY_CHAR)
	} then
	    call syserr (SYS_PSOPEN)

	# Set the output file descriptor
	PS_FD(ps) = fd

	# Initialize default values of the struct.
	call aclrc (page, SZ_FNAME)
	if (envgets (PSPAGE_ENV, page, SZ_FNAME) != 0) {
	    call strlwr (page)
	    if (streq (page, "letter"))
	        call ps_page_size (ps, PAGE_LETTER)
	    else if (streq (page, "legal"))
	        call ps_page_size (ps, PAGE_LEGAL)
	    else if (streq (page, "a4"))
	        call ps_page_size (ps, PAGE_A4)
	    else if (streq (page, "b5"))
	        call ps_page_size (ps, PAGE_B5)
	} else
	    call ps_page_size (ps, PAGE_LETTER)

	PS_FONTSZ(ps)	= FONT_SIZE		# default font size
	PS_JUSTIFY(ps)	= YES			# justify text?

	# Set the margin values.
	scale = PPI * RESOLUTION
	PS_PLMARGIN(ps)	= LMARGIN * scale	# perm. L margin     (points)
	PS_PRMARGIN(ps)	= RMARGIN * scale	# perm. R margin     (points)
	PS_PTMARGIN(ps)	= TMARGIN * scale	# perm. T margin     (points)
	PS_PBMARGIN(ps)	= BMARGIN * scale	# perm. B margin     (points)

	PS_CLMARGIN(ps)	= PS_PLMARGIN(ps)	# current L margin   (points)
	PS_CRMARGIN(ps)	= PS_PRMARGIN(ps)	# current R margin   (points)

	# Set the right margin in pixel coords.
	PS_CRMPOS(ps)   = (PS_PWIDTH(ps) * RESOLUTION) - PS_CRMARGIN(ps)
	PS_PRMPOS(ps)   = PS_CRMPOS(ps)
	PS_CURPOS(ps)   = PS_PLMARGIN(ps)

	PS_LMARGIN(ps)	= LMARGIN		# page left margin   (inches)
	PS_RMARGIN(ps)	= RMARGIN		# page right margin  (inches)
	PS_TMARGIN(ps)	= TMARGIN		# page top margin    (inches)
	PS_BMARGIN(ps)	= BMARGIN		# page bottom margin (inches)

	PS_XPOS(ps)	= PS_PLMARGIN(ps)
	PS_YPOS(ps)	= (RESOLUTION * PS_PHEIGHT(ps)) - PS_PTMARGIN(ps)

	PS_CFONT(ps)	= F_ROMAN		# font initializations
	PS_PFONT(ps)	= F_ROMAN
	PS_SFONT(ps)	= NULL
	PS_CFONT_CH(ps)	= 'R'
	PS_SFONT_CH(ps)	= EOS

	# Compute the width of the line.
	PS_LINE_WIDTH(ps) = (PS_PWIDTH(ps) * RESOLUTION) - 
	    PS_PLMARGIN(ps) - PS_PRMARGIN(ps) 

	# Set the footer flags.
	PS_PNUM(ps) = 1
	PS_NUMBER(ps) = YES
	if (default_footer == YES) {
	    call aclrc (version, SZ_FNAME)
	    if (envgets ("version", version, SZ_FNAME) != 0)
		call strcpy (version, FLEDGE(ps), SZ_FNAME)
	    else
	        call strcpy ("NOAO/IRAF", FLEDGE(ps), SZ_WORD)
	    call strcpy (" ", FCENTER(ps), SZ_FNAME)
	} 

	return (ps)
end
