# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math.h>
include <imio.h>
include <imhdr.h>
include <ctype.h>
include <mwset.h>
include "../lib/skywcs/skywcs.h"
include "wcspix.h"


# Image class data.
define  LEN_IMGDATA	15
define  IMG_WP          Memi[$1  ]              # wcspix back-pointer
define  IMG_IM          Memi[$1+1]              # image pointer
define  IMG_BPM         Memi[$1+2]              # bad pixel mask pointer
define  IMG_MW          Memi[$1+3]              # image wcs pointer
define  IMG_CO          Memi[$1+4]              # skywcs transform pointer
define  IMG_CTW         Memi[$1+5]              # mwcs log->world transform ptr
define  IMG_CTP         Memi[$1+6]              # mwcs log->phys transform ptr
define  IMG_CTA         Memi[$1+7]              # mwcs log->amplifier transform
define  IMG_CTD         Memi[$1+8]              # mwcs log->detector transform
define  IMG_ROT         Memr[$1+9]              # rotation angle
define  IMG_SCALE       Memr[$1+10]             # plate scale
define  IMG_LINEAR      Memi[$1+11]             # linear coords


define	IMG_DEBUG	FALSE


# IMG_INIT --  Initialize the object structure.

procedure img_init (cp, wp)

pointer	cp					#i cache pointer
pointer	wp					#i WCSPIX structure

pointer	img					# data pointer

begin
	if (IMG_DEBUG) call printf ("img_init: \n")

	# Allocate the image data structure if not previously allocated.
	if (C_DATA(cp) == NULL) {
	    iferr (call calloc (C_DATA(cp), LEN_IMGDATA, TY_STRUCT))
	        return
	}

	img = C_DATA(cp)
	IMG_WP(img) = wp
	IMG_IM(img) = NULL
	IMG_MW(img) = NULL
	IMG_CO(img) = NULL
	IMG_CTW(img) = NULL
	IMG_CTP(img) = NULL
	IMG_ROT(img) = 0.0
	IMG_SCALE(img) = 0.0
	IMG_LINEAR(img) = YES
end


# IMG_CACHE --  Cache an image in the object cache.

procedure img_cache (cp, objid, regid, ref)

pointer	cp					#i cache pointer
int	objid					#i object id
int	regid					#i region id
char	ref[ARB]				#i object reference

pointer	img, im, wp
int     stat
char	alert[SZ_LINE]

pointer immap(), ds_pmmap(), mw_sctran()
pointer	img_amp_wcs(), img_det_wcs()
int     imaccf(), sk_decim()

errchk  immap, ds_pmmap(), mw_sctran, sk_decim

begin
	if (IMG_DEBUG) call printf ("img_cache: \n")

        # Now map the image and WCS.
	img = C_DATA(cp)
	wp  = IMG_WP(img)

        iferr (IMG_IM(img) = immap (ref, READ_ONLY, 0)) {
	    # Send alert to the GUI.
            call sprintf (alert, SZ_FNAME, "Unable to cache\n%s")
                call pargstr (ref)
            call xim_alert (alert, "", "")
            return
        }

        IMG_CO(img)  = NULL
        IMG_CTW(img) = NULL
        IMG_CTP(img) = NULL
	iferr {
            stat = sk_decim (IMG_IM(img), "world", IMG_MW(img), IMG_CO(img))
            if (stat == ERR || IMG_MW(img) == NULL)
                IMG_LINEAR(img) = YES

            if (IMG_MW(img) != NULL) {
                IMG_CTW(img) = mw_sctran (IMG_MW(img), "logical", "world", 03B)
                IMG_CTP(img) = mw_sctran (IMG_MW(img), "logical", "physical",
		    03B)

	        # Get the amplifier transformation values if present.
	        im = IMG_IM(img)
	        if (imaccf(im,"ATM1_1") == YES &&
	            imaccf(im,"ATM2_2") == YES &&
	            imaccf(im,"ATV1") == YES &&
	            imaccf(im,"ATV2") == YES)
		        IMG_CTA(img) = img_amp_wcs (im, IMG_MW(img))

	        if (imaccf(im,"DTM1_1") == YES &&
	            imaccf(im,"DTM2_2") == YES &&
	            imaccf(im,"DTV1") == YES &&
	            imaccf(im,"DTV2") == YES)
		        IMG_CTD(img) = img_det_wcs (im, IMG_MW(img))
	    }

	} then {
	    # Send alert to the GUI.
            call sprintf (alert, SZ_FNAME, "Unable to decode image WCS\n%s")
                call pargstr (ref)
            call xim_alert (alert, "", "")
            IMG_LINEAR(img) = YES
	}

	# See if we can find a bad pixel mask.
	if (WP_BPM(wp) == YES) {
            iferr (IMG_BPM(img) = ds_pmmap ("BPM", IMG_IM(img)))
	        IMG_BPM(img) = NULL
	}

        C_OBJID(cp) = objid
        C_REGID(cp) = regid
        C_NREF(cp)  = C_NREF(cp) + 1
        call strcpy (ref, C_REF(cp), 128)
end


# IMG_UNCACHE --  Uncache an image in the object cache.

procedure img_uncache (cp, id)

pointer	cp					#i cache pointer
int	id					#i image id

pointer	img

begin
	if (IMG_DEBUG) call printf ("img_uncache: \n")

        C_OBJID(cp) = NULL
        C_NREF(cp) = 0
        call strcpy ("", C_REF(cp), SZ_FNAME)

	img = C_DATA(cp)
	if (IMG_MW(img) != NULL)
            call mw_close (IMG_MW(img))
        if (IMG_BPM(img) != NULL)
            call imunmap (IMG_BPM(img))
        if (IMG_IM(img) != NULL)
            call imunmap (IMG_IM(img))

        IMG_IM(img)     = NULL
        IMG_BPM(img)    = NULL
        IMG_MW(img)     = NULL
        IMG_CTW(img)    = NULL
        IMG_CTP(img)    = NULL
        IMG_CO(img)     = NULL
        IMG_ROT(img)    = 0.0
        IMG_SCALE(img)  = 0.0
        IMG_LINEAR(img) = NO

	call mfree (C_DATA(cp), TY_STRUCT)
        C_DATA(cp) = NULL
end


# IMG_WCSTRAN -- Translate object source (x,y) coordinates to the
# desired output WCSs.  Message is returned as something like:
#
#        set value {
#            { object <objid> }  { region <regionid> }
#            { pixval <pixel_value> [<units>] }
#            { bpm    <bpm_pixel_value> }
#            { coord  <wcsname> <x> <y> [<xunits> <yunits>] }
#            { coord  <wcsname> <x> <y> [<xunits> <yunits>] }
#            		:
#        }


procedure img_wcstran (cp, id, x, y)

pointer	cp					#i cache pointer
int	id					#i image id
real	x, y					#i source coords

pointer img, im, wp, co
double  dx, dy, wx, wy, pixval
real	rx, ry
int     i, bpm

# Use static storage to avoid allocation overhead.
char	buf[SZ_LINE]
char	msg[SZ_LINE], wcs[LEN_WCSNAME], xc[LEN_WCSNAME], yc[LEN_WCSNAME]
char	xunits[LEN_WCSNAME], yunits[LEN_WCSNAME]

double	sk_statd()

begin
	if (IMG_DEBUG) call printf ("img_wcstran: \n")

	img = C_DATA(cp)			# initialize
	co  = IMG_CO(img)
	wp  = IMG_WP(img)
	im  = IMG_IM(img)

	# Get the translation to the image section.
	dx  = (double(x) - sk_statd(co,S_VXOFF)) / sk_statd(co,S_VXSTEP)
	dy  = (double(y) - sk_statd(co,S_VYOFF)) / sk_statd(co,S_VYSTEP)
	rx = dx
	ry = dy

	# Read the pixel data.
	call img_get_data (cp, id, rx, ry, pixval, bpm)

	# Begin formatting the message.
	call aclrc (msg, SZ_LINE)
	call sprintf (msg, SZ_LINE, "wcstran { object %d } { region %d } ")
	    call pargi (C_OBJID(cp))
	    call pargi (C_REGID(cp))

	call sprintf (buf, SZ_LINE, "{ pixval %9.9g } { bpm %d }\n")
	    call pargd (pixval)
	    call pargi (bpm)
	call strcat (buf, msg, SZ_LINE)

	# Now loop over the requested systems and generate a coordinate
	# for each.
	for (i=1; i <= MAX_WCSLINES; i=i+1) {

	    # Get the coordinate value.
	    call img_get_coord (img, dx, dy, SYSTEMS(wp,i), WCSNAME(wp,i),
		wx, wy)

	    # Get the system name, labels, and formats strings for the WCS.
	    call img_coord_labels (cp, i, wcs, xunits, yunits)

	    # Format the values as requested.
	    call img_coord_fmt (cp, i, wx, wy, xc, yc)

	    # Format the coord buffer and append it to the message.
	    call sprintf (buf, SZ_LINE,
		"{coord {%9s} {%12s} {%12s} {%4s} {%4s}}\n")
	            call pargstr (wcs)
	            call pargstr (xc)
	            call pargstr (yc)
	            call pargstr (xunits)
	            call pargstr (yunits)
	    call strcat (buf, msg, SZ_LINE)
	}

	# Now send the completed message.
	call wcspix_message (msg);
end


# IMG_WCSLIST -- List the WCSs available for the given image.

procedure img_wcslist (cp, id)

pointer	cp					#i cache pointer
int	id					#i image id

pointer img, im, mw
char	msg[SZ_LINE]

begin
	if (IMG_DEBUG) call printf ("img_wcslist: \n")

	img = C_DATA(cp)			# initialize
	mw  = IMG_MW(img)
	im  = IMG_IM(img)

	call strcpy ("wcslist {None Logical World Physical line ", msg, SZ_LINE)

	# See if we can do amplifier/detector coords by checking for ATM/ATV
	# and DTM/DTV keywords.

	if (IMG_CTA(img) != NULL)
	    call strcat (" Amplifier ", msg, SZ_LINE)
	if (IMG_CTD(img) != NULL)
	    call strcat (" Detector ", msg, SZ_LINE)
	if (IMG_CTA(img) != NULL || IMG_CTD(img) != NULL)
	    call strcat (" CCD ", msg, SZ_LINE)
	call strcat (" line ", msg, SZ_LINE)

	# If we have a MWCS pointer list the sky projections.
	if (mw != NULL)
	    call strcat (SKYPROJ, msg, SZ_LINE)

	# Close the message.
	call strcat ("}", msg, SZ_LINE)
	
	call wcspix_message (msg)
end


# IMG_GET_DATA -- Get data from the image.

procedure img_get_data (cp, id, x, y, pixval, bpm_pix)

pointer	cp					#i cache pointer
int	id					#i image id
real	x, y					#i source coords
double	pixval					#o central pixel value
int	bpm_pix					#o bad pixel mask value

pointer img, wp, im, bpm, pix
int     nl, nc, ix, iy
int     size, x1, x2, y1, y2

pointer imgs2r(), imgs2i()

begin
	if (IMG_DEBUG) call printf ("img_get_data: \n")

	img  = C_DATA(cp)
	wp   = IMG_WP(img)
	im   = IMG_IM(img)
	bpm  = IMG_BPM(img)
	nc   = IM_LEN(im,1)
	nl   = IM_LEN(im,2)
	size = WP_PTABSZ(wp)

	# Sanity check on the cursor image position.
	if (x < 0.0 || y < 0.0 || x > nc || y > nl)
	    return

	# Bounds checking.  Rather than deal with out of bounds pixels we'll
	# adjust the center pixel so we get the same size raster up to each
	# boundary.

	ix = int (x + 0.5) 		;     iy = int (y + 0.5)
	ix = max (size/2+1, ix)		;     iy = max (size/2+1, iy)
	ix = min (ix, (nc-(size/2)-1))  ;     iy = min (iy, (nl-(size/2)-1))

	# Compute the box offset given the center and size.
	x1 = ix - size / 2 + 0.5
	x2 = ix + size / 2 + 0.5
	y1 = iy - size / 2 + 0.5
	y2 = iy + size / 2 + 0.5

	# Get the image pixels
        x1 = max (1, x1)
        x2 = min (nc, x2)
        y1 = max (1, y1)
        y2 = min (nl, y2)
	pix = imgs2r (im, int(x1), int(x2), int(y1), int(y2))

	if (bpm != NULL && WP_BPM(wp) == YES)
	    bpm_pix = Memi[imgs2i (bpm, ix, ix, iy, iy)]
	else
	    bpm_pix = 0

	# Compute the image pixel associated with the requested coords.
	pixval = Memr[pix + ((size/2)*size) + (size/2)] * 1.0d0

	# Send the pixel table.
	if (WP_PTABSZ(wp) > 1)
	    call img_send_pixtab (Memr[pix], WP_PTABSZ(wp), x1, x2, y1, y2)
end


# IMG_OBJINFO -- Get header information from the image.

procedure img_objinfo (cp, id, template)

pointer	cp					#i cache pointer
int	id					#i image id
char	template[ARB]				#i keyword template

pointer	im, img

define  WCS_TEMPLATE "WCSDIM,CTYPE*,CRPIX*,CRVAL*,CD*,CROTA2,LTV*,LTM*,WSV*,WAT*,RA*,DEC*,EQUINOX,EPOCH,MJD*,DATE-OBS"

begin
	if (IMG_DEBUG) call printf ("img_objinfo: \n")

	# Send the full header (or keyword filtered header), only the WCS
	# keywords, and a plain-text explanation of the WCS.

	img = C_DATA(cp)
	im  = IMG_IM(img)

	call img_send_header  (im, "imghdr", template)
	call img_send_header  (im, "wcshdr", WCS_TEMPLATE)
	call img_send_wcsinfo (im, cp)
	call img_send_compass (im, cp)
end



#==============================================================================

# IMG_SEND_HEADER -- Send an image header to the named GUI object.  Keywords
# are filtered according to a specified template

procedure img_send_header (im, object, template)

pointer	im					#i image descriptor
char	object[ARB]				#i object for the message
char	template[ARB]				#i keyword template

pointer	sp, hdr, lbuf, line, field, keyw, dict
pointer	ip, lp, list
int	nlines, in, out, i, hdr_size
bool	keyw_filter

int     stropen(), getline(), stridx(), imgnfn(), strdic()
pointer	imofnlu()
bool	streq()
errchk  stropen, getline, putci, putline, imgnfn, imofnlu, strdic

define  USER_AREA       Memc[($1+IMU-1)*SZ_STRUCT + 1]
define  SZ_KEYW		8

begin
        hdr_size = (LEN_IMDES + IM_LENHDRMEM(im) - IMU) * SZ_STRUCT - 1
        hdr_size = hdr_size + SZ_LINE

	call smark (sp)
	call salloc (hdr, hdr_size, TY_CHAR)
	call salloc (dict, hdr_size, TY_CHAR)
	call salloc (field, SZ_LINE, TY_CHAR)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (keyw, SZ_KEYW, TY_CHAR)

	in  = stropen (USER_AREA(im), hdr_size, READ_ONLY)
	out = stropen (Memc[hdr], hdr_size, WRITE_ONLY)
	call fprintf (out, "%s {")
	    call pargstr (object)

	# Build up a dictionary of header keywords based on the template.
	keyw_filter = (!streq (template, "*"))
	if (keyw_filter) {
	    list = imofnlu (im, template)
	    call strcpy ("|", Memc[dict], hdr_size)
            while (imgnfn (list, Memc[field], SZ_FNAME) != EOF) {
	        call strcat (Memc[field], Memc[dict], hdr_size)
	        call strcat ("|", Memc[dict], hdr_size)
	    }
	    call imcfnl (list)
	}


        # Copy header records to the output, stripping any trailing
        # whitespace and clipping at the right margin.  We also filter
	# against the keyword dictionary found above.

	nlines = 0
        while (getline (in, Memc[lbuf]) != EOF) {

	    call aclrc (Memc[line], SZ_LINE)

	    # Escape any brackets passed to the Tcl.
	    ip = lbuf
	    lp = line	
            while (Memc[ip] != EOS && Memc[ip] != '\n') {
		if (stridx (Memc[ip], "[{") > 0) {
		    Memc[lp] = '\\'
		    lp = lp + 1
		}
		Memc[lp] = Memc[ip]
	  	ip = ip + 1
	  	lp = lp + 1
	    }
            Memc[lp] = '\n'
            Memc[lp+1] = EOS
            
	    # See whether the line matches a keyword we want to output.
	    if (keyw_filter) {
		for (i=0; i < SZ_KEYW && !IS_WHITE(Memc[line+i]); i=i+1)
		    Memc[keyw+i] = Memc[line+i]
		Memc[keyw+i] = '\0'

		# If not in the dictionary skip to the next line.
	        if (strdic (Memc[keyw], Memc[keyw], SZ_KEYW, Memc[dict]) == 0)
		    next
	    }

            call putci (out, ' ')
            call putline (out, Memc[line])

	    # Send the header in small chunks so we don't overflow the
	    # message buffer.
	    nlines = nlines + 1
	    if (mod(nlines,10) == 0) {
		call fprintf (out, "}")
		call close (out)
		call wcspix_message (Memc[hdr]);
	        call aclrc (Memc[hdr], hdr_size)
		out = stropen (Memc[hdr], hdr_size, WRITE_ONLY)
		call fprintf (out, "%s {")
	    	    call pargstr (object)
	    }
        }
	call fprintf (out, "}")

	call close (in)
	call close (out)

	# Send the final message.
	call wcspix_message (Memc[hdr])

	# Pad a few lines for the GUI
	call sprintf (Memc[hdr], SZ_LINE, "%d { \n\n\n }")
	    call pargstr (object)
	call wcspix_message (Memc[hdr])

	call sfree (sp)
end


# IMG_SEND_COMPASS -- Send information about the image WCS in a plain-english
# string.

procedure img_send_compass (im, cp)

pointer	im					#i image descriptor
pointer	cp					#i cache element pointer

pointer	sp, buf, img, co
double	cx, cy, cx1, cy1, dx, dy, x1, y1
double	cosa, sina, angle
int	i, j, comp_x, comp_y
long    axis[IM_MAXDIM], lv[IM_MAXDIM], pv1[IM_MAXDIM], pv2[IM_MAXDIM]

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call aclrc (Memc[buf], SZ_LINE)

	# Get the data pointer.
	img = C_DATA(cp)
	co  = IMG_CO(img)

	# Get world coords at the image corners.
	if (IMG_CTW(img) != NULL) {

	    if (IMG_ROT(img) > 0.0)
		angle = -IMG_ROT(img)
	    else
		angle = IMG_ROT(img) + 360.0
            cosa = cos (DEGTORAD(angle))
            sina = sin (DEGTORAD(angle))

	    # Image center position
	    cx = IM_LEN(im,1) / 2.0d0
	    cy = IM_LEN(im,2) / 2.0d0
            call mw_c2trand (IMG_CTW(img), cx, cy, cx1, cy1)

	    # Extend a unit vector up from the center assuming it's North
	    # and rotate it by the wcs angle.
            dx  = cx + ( 10.0 * sina)
            dy  = cy + ( 10.0 * cosa)
            call mw_c2trand (IMG_CTW(img), dx, dy, x1, y1)

	    # Check new point Y value relative to the center position.
	    if (y1 >= cy1)
	        comp_y = 1			# North is up
	    else
	        comp_y = -1			# North is down

	    # Extend a unit vector left from the center assuming it's East
	    # and rotate it by the wcs angle.
            dx  = cx + (-10.0 * cosa)
            dy  = cy + ( 10.0 * sina)
            call mw_c2trand (IMG_CTW(img), dx, dy, x1, y1)

	    # Check new point X value relative to the center position.
	    if (x1 >= cx1)
	        comp_x = 1			# East is left and we have a WCS
	    else
	        comp_x = -1			# East is right

	} else {
            # Determine the logical to physical mapping by evaluating two
            # points and determining the axis reduction if any.  pv1 will be
            # the offset and pv2-pv1 will be the scale.

            lv[1] = 0; lv[2] = 0; call imaplv (im, lv, pv1, 2)
            lv[1] = 1; lv[2] = 1; call imaplv (im, lv, pv2, 2)

            i = 1
            axis[1] = 1;  axis[2] = 2
            do j = 1, IM_MAXDIM {
                if (pv1[j] != pv2[j]) {
                    axis[i] = j
                    i = i + 1
                }
	    }
            comp_x = - (pv2[axis[1]] - pv1[axis[1]])
            comp_y =   (pv2[axis[2]] - pv1[axis[2]])
	}
	
	call sprintf (Memc[buf], SZ_LINE, "compass %d %g %d %d %s\0")
	    call pargi (C_OBJID(cp))
	    call pargr (IMG_ROT(img))
	    call pargi (comp_x)
	    call pargi (comp_y)
	    if (IMG_MW(img) != NULL)
		call pargstr ("E N")
	    else
		call pargstr ("X Y")

	call wcspix_message (Memc[buf])
	call sfree (sp)
end


# IMG_SEND_WCSINFO -- Send information about the image WCS in a plain-english
# string.

procedure img_send_wcsinfo (im, cp)

pointer	im					#i image descriptor
pointer	cp					#i cache element pointer

pointer	sp, co, img, mw
pointer	buf, proj, radecstr
int	fd, radecsys, ctype, wtype, ndim
double 	crpix1, crpix2, crval1, crval2, cval1, cval2
double	xscale, yscale, xrot, yrot
double	r[IM_MAXDIM], w[IM_MAXDIM], cd[IM_MAXDIM,IM_MAXDIM], 

int	idxstr(), sk_stati(), stropen(), mw_stati()
double	sk_statd(), sl_epj(), sl_epb()
bool	fp_equald()

errchk	stropen

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (proj, SZ_FNAME, TY_CHAR)
	call salloc (radecstr, SZ_FNAME, TY_CHAR)
	
	# Open a string on a file.
	fd = stropen (Memc[buf], SZ_LINE, WRITE_ONLY)

	# Get the data pointer.
	img = C_DATA(cp)

	# Get the coordinate transform descriptor.
	co = IMG_CO(img)
	radecsys = sk_stati (co, S_RADECSYS)
	ctype = sk_stati (co, S_CTYPE)
	wtype = sk_stati (co, S_WTYPE)

	mw = IMG_MW(img)
	if (mw != NULL) {
	    # Now get the mwcs Rterm (CRPIXi), Wterm (CRVALi), and CD matrix.
	    ndim = mw_stati (mw, MW_NPHYSDIM)
	    call wcs_gfterm (mw, r, w, cd, ndim)
	    crpix1 = r[1]
	    crpix2 = r[2]
	    crval1 = w[1]
	    crval2 = w[2]

	    xscale = sqrt (cd[1,1]**2 + cd[2,1]**2) * 3600.0d0
	    yscale = sqrt (cd[1,2]**2 + cd[2,2]**2) * 3600.0d0
	    xrot   = 0.0
	    yrot   = 0.0
	    if (!fp_equald (cd[1,1], 0.0d0))
	        xrot   = DRADTODEG(atan ( cd[2,1] / cd[1,1]))
	    if (!fp_equald (cd[2,2], 0.0d0))
	        yrot   = DRADTODEG(atan (-cd[1,2] / cd[2,2]))
	} else {
	    ndim = 2
	    xscale = 1.0
	    yscale = 1.0
	    xrot   = 0.0
	    yrot   = 0.0
	}

	if (IMG_DEBUG) {
	    call printf("WCS Info:\n=========\n")
	    call printf("R term: %g %g\n"); call pargd(r[1]); call pargd(r[2])
	    call printf("W term: %g %g\n"); call pargd(w[1]); call pargd(w[2])
	    call printf("    cd: %g %g\n        %g %g\n")
	        call pargd(cd[1,1]); call pargd(cd[1,2])
	        call pargd(cd[2,1]); call pargd(cd[2,2])
	    call printf(" scale: %g %g\n");call pargd(xscale);call pargd(yscale)
	    call printf("   rot: %g %g\n");call pargd(xrot);call pargd(yrot)
	}

	IMG_SCALE(img) = (xscale + yscale) / 2.0d0
	#IMG_ROT(img)  = (xrot + yrot) / 2.0d0
	IMG_ROT(img)   = xrot
	

	# Now format a WCS text panel such as
	#
	#      Projection:  TAN                     System:  Equatorial FK5
	#     Ra/Dec axes:  1/2                 Dimensions:  512 x 512
	#
	#      Center Pos:  RA:  13:29:52.856          Dec:  +47:11:40.39
	#   Reference Pos:  RA:  13:29:52.856          Dec:  +47:11:40.39
	# Ref pixel coord:   X:  250.256                 Y:  266.309
	#     Plate Scale:  0.765194             Rot Angle:  1.02939
	#         Equinox:  J2000.000                Epoch:  J1987.25775240
	#             MJD:  46890.39406

	# Get some preliminary values.
        if (idxstr (radecsys, Memc[radecstr], SZ_FNAME, EQTYPE_LIST) <= 0)
            call strcpy ("FK5", Memc[radecstr], SZ_FNAME)
        call strupr (Memc[radecstr])

        if (idxstr (wtype, Memc[proj], SZ_FNAME, WTYPE_LIST) <= 0)
            call strcpy ("logical", Memc[proj], SZ_FNAME)
        call strupr (Memc[proj])

	call fprintf (fd, "wcsinfo {\n")

	call fprintf (fd, 
	    "      Projection:  %-6s\t            System:  %s %s\n")
	    call pargstr (Memc[proj])
	    switch (ctype) {
	    case CTYPE_EQUATORIAL:
		call pargstr ("Equatorial")
	        call pargstr (Memc[radecstr])
	    case CTYPE_ECLIPTIC:
		call pargstr ("Ecliptic")
	        call pargstr ("")
	    case CTYPE_GALACTIC:
		call pargstr ("Galactic")
	        call pargstr ("")
	    case CTYPE_SUPERGALACTIC:
		call pargstr ("SuperGalactic")
	        call pargstr ("")
	    default:
		call pargstr ("Linear")
	        call pargstr ("")
	    }

	call fprintf (fd, "     Ra/Dec axes:  %d/%d")
		call pargi (sk_stati (co, S_PLNGAX))
		call pargi (sk_stati (co, S_PLATAX))
	call fprintf (fd, "                  Dimensions:  %d x %d\n\n")
		call pargi (IM_LEN(im,1))
		call pargi (IM_LEN(im,2))

	call fprintf (fd, 
	    "      Center Pos: %3s:  %-12H           %3s:  %-12h\n")
	    if (ctype == CTYPE_EQUATORIAL)
		call pargstr (" RA")
	    else
		call pargstr ("Lon")
	    call pargd (cval1)
	    if (ctype == CTYPE_EQUATORIAL)
		call pargstr ("Dec")
	    else
		call pargstr ("Lat")
	    call pargd (cval2)

	call fprintf (fd, 
	    "   Reference Pos: %3s:  %-12H           %3s:  %-12h\n")
	    if (ctype == CTYPE_EQUATORIAL)
		call pargstr (" RA")
	    else
		call pargstr ("Lon")
	    call pargd (crval1)
	    if (ctype == CTYPE_EQUATORIAL)
		call pargstr ("Dec")
	    else
		call pargstr ("Lat")
	    call pargd (crval2)

	call fprintf (fd, 
	    " Reference Pixel:   X:  %-9.4f                Y:  %-9.4f\n")
	        call pargd (crpix1)
	        call pargd (crpix2)

	call fprintf (fd, 
	    "     Plate Scale:  %-8f              Rot Angle:  %-8f\n")
	        call pargr (IMG_SCALE(img))
	        call pargr (IMG_ROT(img))

	call fprintf (fd, 
	    "         Equinox:  %s%8f                 Epoch:  %s%.6f\n")
	    switch (radecsys) {
	    case EQTYPE_FK5, EQTYPE_ICRS:
		call pargstr ("J") ; call pargd (sk_statd(co,S_EQUINOX))
		call pargstr ("J") ; call pargd (sl_epj(sk_statd(co,S_EPOCH)))
	    default:
		if (IMG_LINEAR(img) == YES) {
		    call pargstr (" ") ; call pargd (INDEFD)
		    call pargstr (" ") ; call pargd (INDEFD)
		} else {
		    call pargstr ("B")
		    call pargd (sk_statd(co,S_EQUINOX))
		    call pargstr ("B")
		    call pargd (sl_epb(sk_statd(co,S_EPOCH)))
		}
	    }

	call fprintf (fd, "             MJD:  %.6f\n")
	    call pargd (sk_statd(co,S_EPOCH))

	call fprintf (fd, "}\n \n \n")

	# Close the formatted string and send the message.
	call close (fd)
	call wcspix_message (Memc[buf])

	call sfree (sp)
end


# IMG_SEND_PIXTAB -- Send a 'pixtab' message.  Format of the message is
#
#  	pixtab {
#  	  { {pix} {pix} ...  }	    	# pixel table values
#  	  { {x1}  {x2}  ...  }		# column label values
#  	  { {y1}  {y2}  ...  }		# row label values
#  	  { <mean>  <stdev>  }		# pixtab statistics
#  	}
#

procedure img_send_pixtab (pixtab, size, x1, x2, y1, y2)

real	pixtab[ARB]				#i pixtab array
int	size					#i pixtab size
int	x1, x2, y1, y2				#i raster boundaries

pointer	sp, buf, el
int	i, j, npix
real	pix, sum, sum2, mean, var, stdev, x, y

define  SZ_PIXTAB	(6*SZ_LINE)

begin
	call smark (sp)
	call salloc (buf, SZ_PIXTAB, TY_CHAR)
	call salloc (el, SZ_FNAME, TY_CHAR)

	# Begin the pixtab message.  
	call strcpy ("pixtab {\n{\ntable {\n", Memc[buf], SZ_PIXTAB)

	# Format the pixels into a table for presentation.  Do the y-flip
	# here so the pixels are in order for the List widget in the GUI.
	# Accumulate the pixel statistics so we don't have to do it in the
	# GUI where it's slower.

	sum = 0.0
	sum2 = 0.0
	npix = size * size

	for (i=size - 1; i >= 0; i=i-1) {
	    for (j=1; j <= size; j=j+1) {
	        pix =  pixtab[(i * size) + j]
		sum = sum + pix
		sum2 = sum2 + (pix * pix)

	        call sprintf (Memc[el], SZ_FNAME, " {%10.1f}")
	            call pargr (pix)

		call strcat (Memc[el], Memc[buf], SZ_PIXTAB)
	    }
	    call strcat ("\n", Memc[buf], SZ_PIXTAB)
	}
	call strcat ("}\n}\n", Memc[buf], SZ_PIXTAB)


	# Do the row and column label parts of the message.
	call strcat ("{", Memc[buf], SZ_PIXTAB)
	for (x = x1; x <= x2; x = x + 1.) {
	    call sprintf (Memc[el], SZ_FNAME, " {%10.1f}")
	        call pargr (x)
	    call strcat (Memc[el], Memc[buf], SZ_PIXTAB)
	}
	call strcat ("}\n", Memc[buf], SZ_PIXTAB)

	call strcat ("{", Memc[buf], SZ_PIXTAB)
	for (y = y2; y >= y1; y = y - 1.) {
	    call sprintf (Memc[el], SZ_FNAME, " {%10.1f}")
	        call pargr (y)
	    call strcat (Memc[el], Memc[buf], SZ_PIXTAB)
	}
	call strcat ("}\n", Memc[buf], SZ_PIXTAB)


	# Compute the statistics for the raster.
	mean = sum / real(npix)
	var = (sum2 - sum * mean) / real(npix - 1)
	if (var <= 0)
	    stdev = 0.0
	else
	    stdev = sqrt (var)

	call sprintf (Memc[el], SZ_FNAME, " { %10.2f %10.4f }\n")
	    call pargr (mean)
	    call pargr (stdev)
	call strcat (Memc[el], Memc[buf], SZ_PIXTAB)


	# Close the message.
	call strcat ("}", Memc[buf], SZ_PIXTAB) 	

	# Send the formatted message.
	call wcspix_message (Memc[buf])

	call sfree (sp)
end


# IMG_AMP_WCS -- Create a WCS transformation for the amplifier coordinates.

pointer procedure img_amp_wcs (im, mw)

pointer	im					#i image pointer
pointer	mw					#i MWCS descriptor

pointer	ct
double	r[IM_MAXDIM], w[IM_MAXDIM], cd[IM_MAXDIM,IM_MAXDIM]

double	imgetd()
pointer	mw_sctran()

begin
	r[1]    = 0.0d0
	r[2]    = 0.0d0
	w[1]    = imgetd (im, "ATV1")
	w[2]    = imgetd (im, "ATV2")
	cd[1,1] = imgetd (im, "ATM1_1")
	cd[1,2] = 0.0d0
	cd[2,1] = 0.0d0
	cd[2,2] = imgetd (im, "ATM2_2")

	# Create a new named system.
	call mw_newsystem (mw, "amplifier", 2)

	# Set the new Wterm for the system.
	call mw_swtermd (mw, r, w, cd, 2)

	# Set up the transform.
	ct = mw_sctran (mw, "logical", "amplifier", 03B)

	# Reset the default world system.
	call mw_sdefwcs (mw)

	return (ct)
end


# IMG_DET_WCS -- Create a WCS transformation for the detector coordinates.

pointer procedure img_det_wcs (im, mw)

pointer	im					#i image pointer
pointer	mw					#i MWCS descriptor

pointer	ct
double	r[IM_MAXDIM], w[IM_MAXDIM], cd[IM_MAXDIM,IM_MAXDIM]

double	imgetd()
pointer	mw_sctran()

begin
	r[1]    = 0.0d0
	r[2]    = 0.0d0
	w[1]    = imgetd (im, "DTV1")
	w[2]    = imgetd (im, "DTV2")
	cd[1,1] = imgetd (im, "DTM1_1")
	cd[1,2] = 0.0d0
	cd[2,1] = 0.0d0
	cd[2,2] = imgetd (im, "DTM2_2")

	# Create a new named system.
	call mw_newsystem (mw, "detector", 2)

	# Set the new Wterm for the system.
	call mw_swtermd (mw, r, w, cd, 2)

	# Set up the transform.
	ct = mw_sctran (mw, "logical", "detector", 03B)

	# Reset the default world system.
	call mw_sdefwcs (mw)

	return (ct)
end


# IMG_COORD_LABELS -- Get the WCS name, coord labels and format strings for
# the specified object.

procedure img_coord_labels (cp, line, wcsname, xunits, yunits)

pointer	cp					#i cache pointer
pointer	line					#i WCS output line
char	wcsname[ARB]				#o WCS name string
char	xunits[ARB], yunits[ARB]		#o WCS coord labels

pointer	img, co, wp
pointer	sp, proj, radecstr
            
int	strcmp(), sk_stati(), idxstr()

begin
	img = C_DATA(cp)			# initialize ptrs
	co  = IMG_CO(img)
	wp  = IMG_WP(img)

	if (SYSTEMS(wp,line) == SYS_WORLD) {
	    switch (sk_stati(co,S_CTYPE)) {
	    case CTYPE_EQUATORIAL:
	        call strcpy ("  RA", xunits, LEN_WCSNAME)
	        call strcpy (" Dec", yunits, LEN_WCSNAME)
	    case CTYPE_ECLIPTIC:
	        call strcpy ("ELon", xunits, LEN_WCSNAME)
	        call strcpy ("ELat", yunits, LEN_WCSNAME)
	    case CTYPE_GALACTIC:
	        call strcpy ("GLon", xunits, LEN_WCSNAME)
	        call strcpy ("GLat", yunits, LEN_WCSNAME)
	    case CTYPE_SUPERGALACTIC:
	        call strcpy ("SLon", xunits, LEN_WCSNAME)
	        call strcpy ("SLat", yunits, LEN_WCSNAME)
	    }
	} else if (SYSTEMS(wp,line) == SYS_SKY) {
	    call strcpy (WCSNAME(wp,line), wcsname, LEN_WCSNAME)
	    call strlwr (wcsname)
	    if (strcmp (wcsname,"ecliptic") == 0) {
	        call strcpy ("ELon", xunits, LEN_WCSNAME)
	        call strcpy ("ELat", yunits, LEN_WCSNAME)
	    } else if (strcmp (wcsname,"galactic") == 0) {
	        call strcpy ("GLon", xunits, LEN_WCSNAME)
	        call strcpy ("GLat", yunits, LEN_WCSNAME)
	    } else if (strcmp (wcsname,"supergalactic") == 0) {
	        call strcpy ("SLon", xunits, LEN_WCSNAME)
	        call strcpy ("SLat", yunits, LEN_WCSNAME)
	    } else {
	        call strcpy ("  RA", xunits, LEN_WCSNAME)
	        call strcpy (" Dec", yunits, LEN_WCSNAME)
	    }
	} else {
	    call strcpy ("X", xunits, LEN_WCSNAME)
	    call strcpy ("Y", yunits, LEN_WCSNAME)
	}


	# Now get the format strings.  For systems other than the image 
	# default just use the WCS string as the name, otherwise format a
	# string giving more information about the system.
	if (SYSTEMS(wp,line) != SYS_WORLD)
	    call strcpy (WCSNAME(wp,line), wcsname, LEN_WCSNAME)

	else {
	    call smark (sp)
	    call salloc (radecstr, SZ_FNAME, TY_CHAR)
	    call salloc (proj, SZ_FNAME, TY_CHAR)

	    call sprintf (wcsname, LEN_WCSNAME, "%s-%s-%s")

	    switch (sk_stati(co,S_CTYPE)) {
	    case CTYPE_EQUATORIAL:      call pargstr ("EQ")
	    case CTYPE_ECLIPTIC:        call pargstr ("ECL")
	    case CTYPE_GALACTIC:        call pargstr ("GAL")
	    case CTYPE_SUPERGALACTIC:   call pargstr ("SGAL")
	    default:                    call pargstr ("UNKN")
	    }

            if (sk_stati(co,S_CTYPE) == CTYPE_EQUATORIAL) {
                if (idxstr(sk_stati(co,S_RADECSYS), Memc[radecstr], 
                    SZ_FNAME, EQTYPE_LIST) <= 0)
                        call strcpy ("FK5", Memc[radecstr], SZ_FNAME)
                call strupr (Memc[radecstr])
                call pargstr (Memc[radecstr])
            } else {
                if (sk_stati(co,S_CTYPE) == CTYPE_SUPERGALACTIC)
                    call pargstr ("-")
                else
                    call pargstr ("--")
            }

            if (idxstr(sk_stati(co,S_WTYPE), Memc[proj], SZ_FNAME,
		WTYPE_LIST) <= 0)
                    call strcpy ("linear", Memc[proj], SZ_FNAME)
            call strupr (Memc[proj])
            call pargstr (Memc[proj])

	    call sfree (sp)
	}

	# Now fix up the WCS system name.
	if (strcmp (wcsname, "fk4") == 0 ||
	    strcmp (wcsname, "fk5") == 0 ||
	    strcmp (wcsname, "icrs") == 0 ||
	    strcmp (wcsname, "gappt") == 0 ||
	    strcmp (wcsname, "fk4-no-e") == 0) {
		call strupr (wcsname)

	} else if (IS_LOWER(wcsname[1]))
	    wcsname[1] = TO_UPPER(wcsname[1])
end


# IMG_COORD_FMT --  Format the coordinate strings.

procedure img_coord_fmt (cp, line, xval, yval, xc, yc)

pointer	cp					#i object cache pointer
int	line					#i output line number
double	xval, yval				#i input coords
char	xc[ARB], yc[ARB]			#o formatted coord strings

pointer	img, co, wp
char	xfmt[LEN_WCSNAME], yfmt[LEN_WCSNAME]

int	sk_stati()
bool	streq()

begin
	img = C_DATA(cp)			# initialize ptrs
	co  = IMG_CO(img)
	wp  = IMG_WP(img)

	# Convert coords to the requested format.
	if (FORMATS(wp,line) == FMT_DEFAULT) {
	    if (IMG_MW(img) == NULL) {
	        call strcpy ("%10.2f", xfmt, LEN_WCSNAME)
	        call strcpy ("%10.2f", yfmt, LEN_WCSNAME)
	    } else {
	        if (SYSTEMS(wp,line) == SYS_WORLD ||
		    SYSTEMS(wp,line) == SYS_SKY) {

	            if (streq(WCSNAME(wp,line),"ecliptic") ||
	                streq(WCSNAME(wp,line),"galactic") ||
	                streq(WCSNAME(wp,line),"supergalactic"))
	                    call strcpy ("%h", xfmt, LEN_WCSNAME)
	            else
	                call strcpy ("%.2H", xfmt, LEN_WCSNAME)
	            call strcpy ("%.1h", yfmt, LEN_WCSNAME)
	        } else {
	            call strcpy ("%10.2f", xfmt, LEN_WCSNAME)
	            call strcpy ("%10.2f", yfmt, LEN_WCSNAME)
	        }
	    }

	} else if (FORMATS(wp,line) == FMT_HMS) {
	    if (sk_stati(co, S_CTYPE) == CTYPE_EQUATORIAL)
	        call strcpy ("%.2H", xfmt, LEN_WCSNAME)
	    else
	        call strcpy ("%.1h", xfmt, LEN_WCSNAME)
	    call strcpy ("%h", yfmt, LEN_WCSNAME)
	} else {
	    call strcpy ("%10.2f", xfmt, LEN_WCSNAME)
	    call strcpy ("%10.2f", yfmt, LEN_WCSNAME)
	}

	# Convert the value to the requested format
        call sprintf (xc, LEN_WCSNAME, xfmt)
	    if (FORMATS(wp,line) != FMT_RAD)
	        call pargd (xval)
	    else
	        call pargd (DEGTORAD(xval))

        call sprintf (yc, LEN_WCSNAME, yfmt)
	    if (FORMATS(wp,line) != FMT_RAD)
	        call pargd (yval)
	    else
	        call pargd (DEGTORAD(yval))
end


# IMG_GET_COORD -- Given an x,y position in the image return the coordinate in
# the given system.

procedure img_get_coord (img, x, y, system, wcsname, wx, wy)

pointer	img					#i IMG struct pointer
double	x, y					#i input image position
int	system					#i coordinate system requested
char	wcsname[ARB]				#i desired WCS name
double	wx, wy					#o output coordinates

double 	ox, oy
real 	epoch
pointer	im, co, nco
char	buf[SZ_LINE]
int	stat

real	imgetr()
int	imaccf(), sk_stati(), sk_decwstr()
bool	streq()

errchk imgetr

begin	
	im  = IMG_IM(img)
	co  = IMG_CO(img)

	wx = x						# fallback values
	wy = y

	switch (system) {
	case SYS_NONE:
	    wx = x
	    wy = y
	case SYS_PHYSICAL:
            if (IMG_CTP(img) != NULL)
                call mw_c2trand (IMG_CTP(img), x, y, wx, wy)
	case SYS_WORLD:
            if (IMG_CTW(img) != NULL)
                call mw_c2trand (IMG_CTW(img), x, y, wx, wy)
	case SYS_AMP:
            if (IMG_CTA(img) != NULL)
                call mw_c2trand (IMG_CTA(img), x, y, wx, wy)
	case SYS_CCD:
	    ;					# TBD
	case SYS_DETECTOR:
            if (IMG_CTD(img) != NULL)
                call mw_c2trand (IMG_CTD(img), x, y, wx, wy)
	case SYS_SKY:
	    # Note Ecliptic/GAPPT coords need an epoch value.
	    if (streq (wcsname, "ecliptic") || streq (wcsname, "gappt")) {
		if (imaccf (im, "EPOCH") == YES) {
		    epoch = imgetr (im, "EPOCH")
		    if (epoch == 0.0 || IS_INDEFR(epoch))
		        epoch = 1950.0
		} else
		    epoch = 1950.0

		call sprintf (buf, SZ_LINE, "%s %.1f")
		    call pargstr (wcsname)
		    call pargr (epoch)
	    } else
		call strcpy (wcsname, buf, SZ_LINE)

	    stat = sk_decwstr (buf, nco, co)
	    if (stat != ERR) {
        	if (IMG_CTW(img) != NULL)
        	    call mw_c2trand (IMG_CTW(img), x, y, ox, oy)
	   	call sk_lltran (co, nco, DEGTORAD(ox), DEGTORAD(oy),
		    INDEFD, INDEFD, 0.0d0, 0.0d0, wx, wy)
		if (sk_stati(co,S_PLATAX) < sk_stati(co,S_PLNGAX)) {
		    wx = RADTODEG(wy) 		# transposed image
		    wy = RADTODEG(wx)
		} else {
		    wx = RADTODEG(wx)		# regular image
		    wy = RADTODEG(wy)
		}
	    }
	case SYS_OTHER:
	    ;					# TBD

	default: 					# default coords
	    wx = x
	    wy = y
	}
end
