# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctype.h>
include "wcspix.h"


# Unknown class data.
define  LEN_UNKDATA     1
define  UNK_WP          Memi[$1  ]              # wcspix back-pointer


# UNK_INIT --  Initialize the object structure.

procedure unk_init (cp, wp)

pointer	cp					#i cache pointer
pointer	wp					#i WCSPIX structure

begin
	# Allocate the image data structure if not previously allocated.
	if (C_DATA(cp) == NULL) {
	    iferr (call calloc (C_DATA(cp), LEN_UNKDATA, TY_STRUCT))
	        return
	}

	UNK_WP(C_DATA(cp)) = wp
end


# UNK_CACHE --  Cache an image in the object cache.  Since we don't know
# what this is we simply setup so that a query to the object id will still
# return a result of some kind rather than ignore it.  In most cases this
# just means the input arguments are echoed back (e.g. coords), or default
# values such as a rotation value can be retrieved.

procedure unk_cache (cp, objid, regid, ref)

pointer	cp					#i cache pointer
int	objid					#i object id
int	regid					#i region id
char	ref[ARB]				#i object reference

begin
	C_OBJID(cp) = objid
	C_REGID(cp) = regid
	C_NREF(cp)  = C_NREF(cp) + 1
	call strcpy (ref, C_REF(cp), 128)
end


# UNK_UNCACHE --  Uncache an unknown image in the object cache.

procedure unk_uncache (cp, id)

pointer	cp					#i cache pointer
int	id					#i image id

begin
	C_OBJID(cp) = NULL
	C_NREF(cp)  = 0
	call strcpy ("", C_REF(cp), SZ_FNAME)

	call mfree (C_DATA(cp), TY_STRUCT)
	C_DATA(cp)  = NULL
end


# UNK_WCSTRAN -- Translate object source (x,y) coordinates to the
# desired output WCSs.  Message is returned as something like:
#
#        set value {
#            { object <objid> }  { region <regionid> }
#            { pixval <pixelvalue> [<units>] }
#            { coord <wcsname> <x> <y> [<xunits> <yunits>] }
#            { coord <wcsname> <x> <y> [<xunits> <yunits>] }
#        }


procedure unk_wcstran (cp, id, x, y)

pointer	cp					#i cache pointer
int	id					#i image id
real	x, y					#i source coords

pointer wp
int     i

# Use static storage to avoid allocation overhead.
char	buf[SZ_LINE], msg[SZ_LINE]

begin
	wp  = UNK_WP(C_DATA(cp))

	# Begin formatting the message.
	call aclrc (msg, SZ_LINE)
	call sprintf (msg, SZ_LINE, "wcstran { object %d } { region %d } ")
	    call pargi (C_OBJID(cp))
	    call pargi (C_REGID(cp))
	call strcat ("{ pixval 0.0 } { bpm 0 } \n", msg, SZ_LINE)


	# Now loop over the requested systems and generate a coordinate
	# for each.
	for (i=1; i <= MAX_WCSLINES; i=i+1) {

	    # Format the coord buffer and append it to the message.
	    call sprintf (buf, SZ_LINE, "{coord {%9s} {%12g} {%12g} {X} {Y}}\n")
	        call pargstr ("UNKN")
	        call pargr (x)
	        call pargr (y)
	    call strcat (buf, msg, SZ_LINE)
	}

	# Now send the completed message.
	call wcspix_message (msg)
end


# UNK_WCSLIST -- List the WCSs available for the given image.

procedure unk_wcslist (cp, id)

pointer	cp					#i cache pointer
int	id					#i image id

begin
	#call wcspix_message ("wcslist {None Logical}")
end


# UNK_GETDATA -- Get data from the image.

procedure unk_getdata (cp, id, x, y, pixval)

pointer	cp					#i cache pointer
int	id					#i image id
real	x, y					#i source coords
real	pixval					#o central pixel value

pointer wp, pix
int     size, x1, x2, y1, y2

begin
	wp = UNK_WP(C_DATA(cp))
	size = WP_PTABSZ(wp)

	# Compute the box offset given the center and size.
	x1 = x - size / 2 + 0.5
	x2 = x + size / 2 + 0.5
	y1 = y - size / 2 + 0.5
	y2 = y + size / 2 + 0.5

	pixval = 0.0

	# Send the pixel table.
	if (size > 1) {
	    call calloc (pix, size *  size, TY_REAL)
	    call img_send_pixtab (Memr[pix], size, x1, x2, y1, y2)
	    call mfree (pix, TY_REAL)
	}
end


# UNK_OBJINFO -- Get header information from the image.

procedure unk_objinfo (cp, id, template)

pointer	cp					#i cache pointer
int	id					#i image id
char	template[ARB]				#i keyword template

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	# Send a default (X,Y) compass indicator.
	call aclrc (Memc[buf], SZ_LINE)
	call sprintf (Memc[buf], SZ_LINE, "compass %d 0.0 -1 1 X Y\0")
	    call pargi (C_OBJID(cp))
	call wcspix_message (Memc[buf])

	call sfree (sp)
end
