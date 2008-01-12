include	<gset.h>
include <smw.h>

define	OPTIONS	"|label|mabove|mbelow|"
define	LABEL	1	# Label at cursor position
define	MABOVE	2	# Tick mark plus label above spectrum
define	MBELOW	3	# Tick mark plus label below spectrum


# SPLABEL -- Add a label.

procedure splabel (option, sh, gp, x, y, label, format)

char	option[ARB]		#I Label option
pointer	sh			#I Spectrum object
pointer	gp			#I Graphics object
real	x, y			#I Label position
char	label[ARB]		#I Label
char	format[ARB]		#I Format

int	op, pix, color, markcolor, strdic(), gstati()
real	mx, my, x1, x2, y1, y2
pointer	sp, fmt, lab
double	shdr_wl()

define	TICK	.03	# Tick size in NDC
define	GAP	.02	# Gap size in NDC

begin
	call smark (sp)
	call salloc (fmt, SZ_LINE, TY_CHAR)
	call salloc (lab, SZ_LINE, TY_CHAR)

	op = strdic (option, Memc[lab], SZ_LINE, OPTIONS)
	if (op == 0) {
	    call sfree (sp)
	    return
	}
	call ggwind (gp, x1, x2, y1, y2)
	if ((x < min (x1, x2)) || (x > max (x1, x2))) {
	    call sfree (sp)
	    return
	}

	# Set label position and draw tick mark.
	switch (op) {
	case LABEL:
	    call gctran (gp, x, y, mx, my, 1, 0)
	    call gctran (gp, mx, my, x1, y2, 0, 1)
	    markcolor = gstati (gp, G_TICKLABELCOLOR)
	    if (format[1] == EOS)
		call strcpy ("h=c;v=c;s=1.0", Memc[fmt], SZ_LINE)
	    else
		call strcpy (format, Memc[fmt], SZ_LINE)

	case MABOVE:
	    pix = max (2, min (SN(sh)-3, int (shdr_wl (sh, double (x)))))
	    y1 = max (Memr[SY(sh)+pix-2], Memr[SY(sh)+pix-1],
		Memr[SY(sh)+pix], Memr[SY(sh)+pix+1])
	    call gctran (gp, x, y1, mx, my, 1, 0)
	    call gctran (gp, mx, my + GAP, x1, y1, 0, 1)
	    call gctran (gp, mx, my + GAP + TICK, x1, y2, 0, 1)

	    color = gstati (gp, G_PLCOLOR)
	    markcolor = gstati (gp, G_TICKLABELCOLOR)
	    call gseti (gp, G_PLCOLOR, markcolor)
	    call gline (gp, x1, y1, x1, y2)
	    call gseti (gp, G_PLCOLOR, color)

	    call gctran (gp, mx, my + TICK + 2 * GAP, x1, y2, 0, 1)
	    if (format[1] == EOS)
	        call strcpy ("u=180;h=c;v=b;s=0.5", Memc[fmt], SZ_LINE)
	    else
		call strcpy (format, Memc[fmt], SZ_LINE)

	case MBELOW:
	    pix = max (2, min (SN(sh)-3, int (shdr_wl (sh, double (x)))))
	    y1 = min (Memr[SY(sh)+pix-2], Memr[SY(sh)+pix-1],
		Memr[SY(sh)+pix], Memr[SY(sh)+pix+1])
	    call gctran (gp, x, y1, mx, my, 1, 0)
	    call gctran (gp, mx, my - GAP, x1, y1, 0, 1)
	    call gctran (gp, mx, my - GAP - TICK, x1, y2, 0, 1)

	    color = gstati (gp, G_PLCOLOR)
	    markcolor = gstati (gp, G_TICKLABELCOLOR)
	    call gseti (gp, G_PLCOLOR, markcolor)
	    call gline (gp, x1, y1, x1, y2)
	    call gseti (gp, G_PLCOLOR, color)

	    call gctran (gp, mx, my - TICK - 2 * GAP, x1, y2, 0, 1)
	    if (format[1] == EOS)
	        call strcpy ("u=0;h=c;v=t;s=0.5", Memc[fmt], SZ_LINE)
	    else
		call strcpy (format, Memc[fmt], SZ_LINE)
	}

	# Draw the label.
	if (label[1] != EOS) {
	    color = gstati (gp, G_TXCOLOR)
	    call gseti (gp, G_TXCOLOR, markcolor)
	    if (label[1] == '%') {
		call sprintf (Memc[lab], SZ_LINE, label)
		    call pargr (x)
		call gtext (gp, x1, y2, Memc[lab], Memc[fmt])
	    } else
		call gtext (gp, x1, y2, label, Memc[fmt])
	    call gseti (gp, G_TXCOLOR, color)
	}

	call gflush (gp)
	call sfree (sp)
end
