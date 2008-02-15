# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fio.h>
include <fset.h>
include "../lib/ids.h"
include <gki.h>
include <ctotok.h>
include <error.h>
include "cv.h"

# Captain Video

procedure t_cv()

pointer	gp
char	device[SZ_FNAME]
char	command[SZ_LINE]

pointer	gopen(), sp
int	dd[LEN_GKIDD]

int	scan, tok, envgets()

include	"cv.com"

begin
	call smark (sp)
	call salloc (cv_stack, CVLEN, TY_SHORT)

	if (envgets ("stdimage", device, SZ_FNAME) == 0)
	    call error (EA_FATAL,
		"variable 'stdimage' not defined in environment")

	call ids_open (device, dd)
	call gki_inline_kernel (STDIMAGE, dd)
	gp = gopen ( device, READ_WRITE, STDIMAGE)

	call fseti (STDIMAGE, F_TYPE, SPOOL_FILE)
	call fseti (STDIMAGE, F_CANCEL, OK)
	call ids_grstream (STDIMAGE)

	# to do:
	# initialize local variables: image display size, etc
	# instead of defines such as MCXSCALE, etc
	cv_maxframes = CV_MAXF
	cv_maxgraph  = CV_MAXG
	cv_xcen	     = CV_XCEN
	cv_ycen	     = CV_YCEN
	cv_xres	     = CV_XRES
	cv_yres	     = CV_YRES
	cv_zres	     = CV_ZRES
	cv_gp	     = gp
	cv_xcon	     = real(GKI_MAXNDC+1)/CV_XRES
	cv_ycon	     = real(GKI_MAXNDC+1)/CV_YRES
	cv_grch	     = CV_GRCHNUM
	cv_xwinc     = -1.			# Flag: Don't know what lut is

        repeat {
	    call printf (":-) ")
	    call flush (STDOUT)
	    if (scan() == EOF)
		break
	    call gargtok(tok, command, SZ_LINE)
	    if ((tok == TOK_EOS) || (tok == TOK_NEWLINE))
		next
	    # decode next command
	    call strlwr(command)
	    switch (command[1]) {
	        case 'x', 'q':
		    break
		    
		
		case 'b':
		    call blink

		case 'c':
		    if (command[2] == 'l')
		        call clear
		    else
		        call rdcur
		    
		case 'd':
		    call display(command[2])

		case 'e':		# erase means clear
		    call clear

		case 'h', '?':
		    call help

		# case 'l':
		#   call load

		case 'm':
		    call match

		case 'o':
		    call offset

		case 'p':
		    if ( command[2] == 's')
			call map(command[2])		# pseudo color
		    else
		        call pan

		case 'r':
		    if (command[2] == 'e')
			call reset
		    else
		        call range

		case 's':
		    if (command[2] == 'n')
		        call snap
		    else
		        call split
		
		case 't':
		    call tell

		case 'w':
		    if (command[2] == 'r')
			call text
		    else
		        call window

		case 'z':
		    call zoom
		    
		default:
		    call eprintf("unknown command: %s\n")
		        call pargstr(command[1])
			     
	    }  # end switch statement

	} # end repeat statment

	# all done

	call gclose ( gp )
	call ids_close
	call sfree (sp)
end


# HELP -- print informative message

procedure help()

begin
	call eprintf ("--- () : optional; [] : select one; N : number; C/F/Q : see below\n")
	call eprintf ("b(link) N F (C Q) (F (C Q)..)	blink		N = 10 is one second\n")
	call eprintf ("c(ursor) [on off F]		cursor\n")
	call eprintf ("di F (C Q) [on off]		display image\n")
	call eprintf ("dg C (F Q) [on off]		display graphics\n")
	call eprintf ("e(rase) [N a(ll) g(raphics) F]	erase (clear)\n")
	#call eprintf ("l(oad)				load a frame\n")
	call eprintf ("m(atch) (o) F (C) (to) (F) (C)	match (output) lookup table\n")
	call eprintf ("o(ffset)  C N			offset color	N: 0 to +- 4095\n")
	call eprintf ("p(an) (F) 			pan images\n")
	call eprintf ("ps(eudo) (o) (F C) (rn sn)	pseudo color mapping   rn/sn: random n/seed n\n")
	call eprintf ("r(ange) N (C) (N C ...)		scale image	N: 1-8\n")
	call eprintf ("re(set) [r i t a]		reset display	registers/image/tables/all\n")
	call eprintf ("sn(ap) (C)			snap a picture\n")
	call eprintf ("s(plit) [c o px,y nx,y]		split picture\n")
	call eprintf ("t(ell)				tell display state\n")
	call eprintf ("w(indow) (o) (F C)		window (output) frames\n")
	call eprintf ("wr(ite) [F C] text		write text to frame/graphics\n")
	call eprintf ("z(oom) N (F) 			zoom frames	N: 1-8\n")
	call eprintf ("x   or   q			exit/quit\n")
	call eprintf ("--- C: letter c followed by r/g/b/a or, for snap r,g,b,m,bw,rgb,\n")
	call eprintf ("---   or for dg r/g/b/y/p/m/w, as 'cr', 'ca', or 'cgb'\n")
	call eprintf ("--- F: f followed by a frame number or 'a' for all\n")
	call eprintf ("--- Q: q followed by quadrant number or t,b,l,r for top, bottom,...\n")
end
