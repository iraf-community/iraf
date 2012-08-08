# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "psio.h"


# PS_WRITE_PROLOG -- Write the PS prolog given the current postscript struct.
# This initializes a flag keeping other changes from taking effect once
# this is called.

procedure ps_write_prolog (ps)

pointer	ps					#I PSIO descriptor

int	fd, sz_font
char	buf[SZ_LINE]

int	itoc(), ps_centerPos(), ps_rjPos()
long	clktime()

begin
	fd = PS_FD(ps)
	call cnvtime (clktime(long(0)), buf, SZ_LINE)

	# Write the header stuff.
	call fprintf (fd, "%%!PS-Adobe-1.0\n")
	call fprintf (fd, "%%%%Creator: IRAF PostScript Translator\n")
	call fprintf (fd, "%%%%CreationDate: %s\n") 	; call pargstr (buf)
	call fprintf (fd, "%%%%Pages: (atend)\n")
	call fprintf (fd, "%%%%DocumentFonts: (atend)\n")
	call fprintf (fd, "%%%%EndComments\n")
	call fprintf (fd, "%%%%BeginProlog\n")


	# Initialize page values.
	call fprintf (fd,
	    "/inch\t{ 72 mul \t} def\t\t\t%% 72 points per inch\n")
	call fprintf (fd,
	    "/PL \t{ %d  \t\t} def\t\t\t%% set page height\n")
	    call pargi (PS_PHEIGHT(ps))
	call fprintf (fd,
	    "/FtrY\t{ 20  \t\t} def\t\t\t%% footer Y position\n")
	call fprintf (fd,
	    "/HdrY\t{ PL 40 sub  \t} def\t\t\t%% header Y position\n")
	call fprintf (fd,
	    "/xOrg\t%d \t   def\t\t\t\t%% left margin (inches)\n")
	    call pargi (PS_PLMARGIN(ps))
	call fprintf (fd,
	    "/yOrg\t%d \t   def\t\t\t\t%% top margin (inches)\n")
	    call pargi (PS_PTMARGIN(ps))
	call fprintf (fd,
	    "/Page\t0 \t   def\t\t\t\t%% page number\n")
	call fprintf (fd,
	    "/pnum\t4 string def\t\t\t\t%% sizeof pnum cvs buffer\n")
	call fprintf (fd,
	    "/res\t%4.2f\tdef\t\t\t\t%% pixel resolution factor\n")
	    call pargi (RESOLUTION)
	call fprintf (fd, "\n")


	# Create the font array to be used.
	sz_font = PS_FONTSZ(ps)
	call fprintf (fd, "/FS \t{ findfont exch scalefont } bind def ")
	call fprintf (fd, "\t%% find and scale a font\n")
	call fprintf (fd, "/Fonts [\t\t\t\t\t%% create an array of fonts\n")
	call fprintf (fd, "\t%d /Times-Roman FS\n")  ; call pargi (sz_font)
	call fprintf (fd, "\t%d /Times-Bold FS\n")   ; call pargi (sz_font)
	call fprintf (fd, "\t%d /Times-Italic FS\n") ; call pargi (sz_font)
	call fprintf (fd, "\t%d /Courier FS\n")      ; call pargi (sz_font-1)
	call fprintf (fd, "\t11 /Times-Bold FS\n")   
	    if (sz_font >= 6 && sz_font <= 10)
		call pargi (sz_font+2)
	    else
		call pargi (sz_font)
	call fprintf (fd, "] def\n")

	# Set the fonts.
	call fprintf (fd,
	    "/SF { setfont       \t} bind def\n")
	call fprintf (fd,
	    "/R  { Fonts 0 get SF\t} bind def\t\t%% roman font\n")
	call fprintf (fd,
	    "/B  { Fonts 1 get SF\t} bind def\t\t%% bold font\n")
	call fprintf (fd,
	    "/I  { Fonts 2 get SF\t} bind def\t\t%% italic font\n")
	call fprintf (fd,
	    "/T  { Fonts 3 get SF\t} bind def\t\t%% teletype font\n")
	call fprintf (fd,
	    "/HD { Fonts 4 get SF\t} bind def\t\t%% header font\n")

	# Define line motion bindings.
	call fprintf (fd, "/H  { res div\n")
	call fprintf (fd, "\tcurrentpoint exch pop\n")
	call fprintf (fd, "\tmoveto \t\t} def\t\t\t%% horizontal position\n")
	call fprintf (fd, "/V  { res div\n")
	call fprintf (fd, "\tcurrentpoint pop exch\n")
	call fprintf (fd, "\tmoveto \t\t} def\t\t\t%% vertical position\n")
	call fprintf (fd, "/S  { exch H show\t} bind def\t\t%% show\n")


	# Write the page header routine.
	call fprintf (fd, "/BP {\t\t\t\t\t\t%% Begin page (header).\n")
	if (HLEDGE(ps) != EOS) {
	    call fprintf (fd, "\txOrg %d div HdrY moveto B (%s) show\n")
		call pargi (RESOLUTION)
		call pargstr (HLEDGE(ps))
	}
	if (HCENTER(ps) != EOS) {
	    call fprintf (fd, "\t%d %d div HdrY moveto B (%s) show\n")
		call pargi (ps_centerPos(ps, HCENTER(ps)))
		call pargi (RESOLUTION)
		call pargstr (HCENTER(ps))
	}
	if (HREDGE(ps) != EOS) {
	    call fprintf (fd, "\t%d %d div HdrY moveto B (%s) show\n")
		call pargi (ps_rjPos(ps, HREDGE(ps)))
		call pargi (RESOLUTION)
		call pargstr (HREDGE(ps))
	}
	call fprintf (fd, "\txOrg yOrg moveto\n")
	call fprintf (fd, "} bind def\n")


	# Write the page footer routine.
	call fprintf (fd, "/EP {\t\t\t\t\t\t%% End page (footer).\n")
	call fprintf (fd,
	    "\t/Page Page 1 add def\t\t\t%% increment page number\n")
	if (FLEDGE(ps) != EOS) {
	    call fprintf (fd, "\txOrg %d div FtrY moveto R (%s) show\n")
		call pargi (RESOLUTION)
		call pargstr (FLEDGE(ps))
	}
	if (FCENTER(ps) != EOS) {
	    call fprintf (fd, "\t%d %d div FtrY moveto R (%s) show\n")
		call pargi (ps_centerPos(ps, FLEDGE(ps)))
		call pargi (RESOLUTION)
		call pargstr (FCENTER(ps))
	}
	if (PS_NUMBER(ps) == YES) {
	    if (itoc (PS_PNUM(ps), buf, SZ_LINE) != 0) {
		call fprintf (fd,
		    "\t%d %d div FtrY moveto R Page pnum cvs show\n")
		    call pargi (ps_rjPos(ps, buf))
		    call pargi (RESOLUTION)
	    }
	} else if (FREDGE(ps) != EOS) {
	    call fprintf (fd, "\t%d %d div FtrY moveto R (%s) show\n")
		call pargi (ps_rjPos(ps, FREDGE(ps)))
		call pargi (RESOLUTION)
		call pargstr (FREDGE(ps))
	}
	call fprintf (fd, "\tshowpage\t\t\t\t%% show the page\n")
	call fprintf (fd, "} bind def\n")


	# Finish the prolog header and flush the output.
	call fprintf (fd, "%%%%EndProlog\n")
	call fprintf (fd, "%%%%Page: 1 1\n")
	call fprintf (fd, "%%----------\n")
	call fprintf (fd, "initgraphics\n")
	call fprintf (fd, "R\n")
	call fprintf (fd, "BP\n")
	call flush (fd)

	# Set the flag indicating we've written the prolog
	PS_INITIALIZED(ps) = YES
end


# PS_TRAILER - Write the postscript trailer.

procedure ps_trailer (ps)

pointer	ps					#I PSIO descriptor

int	fd

begin
	fd = PS_FD(ps)

	call fprintf (fd, "EP\n")
	call fprintf (fd, "%% end of document\n")
	call fprintf (fd, "%%%%Trailer\n")
	call fprintf (fd, "%%%%DocumentFonts: ")
	call fprintf (fd, "Times-Roman Times-Bold Times-Italic Courier\n")
	call fprintf (fd, "%%%%Pages: %d\n")
	    call pargi(PS_PNUM(ps))

	call flush (fd)
end
