include	<ctype.h>
include	<imhdr.h>
include "slitpic.h"

# T_SLITPIC -- generates image to be used as a mask for making aperture plates.
# Positions of slits have already been calculated and are read from "tape1".
# If the user wants to generate a dicomed print of the mask with crtpict, a
# command file to be used as input to task crtpict can be generated.

procedure t_slitpic ()

pointer	im
char	site[SZ_LINE], pix_date[SZ_LINE], output_root[SZ_FNAME], tape1[SZ_FNAME]
char	serial_numbers[SZ_LINE], cmd_root[SZ_FNAME], cmd_file[SZ_FNAME]
char	id_string[LEN_IDSTRING], suffix[SZ_LINE], image_name[SZ_FNAME]
int	serial[3, MAX_RANGES], stat, find_slits(), jj, junk
int	nserial, fd, this_number, n_slits, slits[MAX_SLITS, N_PARAMS]
real    pixel_scale, plate_scale, slit_width

pointer	immap()
bool	clgetb()
int	decode_ranges(), strncmp(), open(), itoc()
real	clgetr()

begin
	# Get parameters from cl
	call clgstr ("site", site, SZ_LINE)
	if (strncmp (site, "kpno", 1) == 0 || strncmp (site, "KPNO", 1) == 0)
	    plate_scale = PSCALE
	else if (strncmp (site, "ctio",1) == 0 || strncmp (site, "CTIO",1) == 0)
	    plate_scale = CPSCALE
	else {
	    call eprintf ("Unknown site: %s Try again.\n")
		call pargstr (site)
	    return
	}

	call clgstr ("output_root", output_root, SZ_FNAME)
	call clgstr ("pix_date", pix_date, SZ_LINE)
	pixel_scale = clgetr ("pixel_scale")
	slit_width = clgetr ("slit_width")
	call clgstr ("tape1", tape1, SZ_FNAME)
	fd = open (tape1, READ_ONLY, TEXT_FILE)

	# Serial numbers to be processed are entered as a range.
	call clgstr ("serial_numbers", serial_numbers, SZ_LINE)
	if (decode_ranges (serial_numbers, serial, MAX_RANGES, nserial) == ERR)
	    call error (0, "Error in specifying range of serial numbers")

	for (jj = 1; jj <= nserial; jj = jj + 1) {
	    stat = find_slits (fd, serial, pixel_scale, plate_scale, 
		slit_width, slits, n_slits, id_string, this_number)

	    if (stat == EOF)
    		return

	    # Generate unique output file names if more than one serial number
	    call strcpy (output_root, image_name, SZ_FNAME)
	    call strcpy (cmd_root, cmd_file, SZ_FNAME)
	    if (nserial > 1) {
		junk = itoc (this_number, suffix, SZ_LINE)
		call strcat (suffix, image_name, SZ_FNAME)
		call strcat (suffix, cmd_file, SZ_FNAME)
	    } 

	    im = immap (image_name, NEW_IMAGE, LEN_USER_AREA)
	    call strupr (pix_date)
	    call sprintf (IM_TITLE(im), SZ_LINE,
		"SN=%d, SW=%0.2f, PS=%0.4f, PD=%s, %s")
		call pargi (this_number)
		call pargr (slit_width)
		call pargr (pixel_scale)
		call pargstr (pix_date)
		call pargstr (id_string)
	    call write_image (im, slits, n_slits, plate_scale, pixel_scale)
	    if (clgetb ("crtpict"))
	        call write_crtpict_cards (cmd_file, site, this_number, 
		    slit_width, image_name, pixel_scale, pix_date)
	    call imunmap (im)
	}
end


int procedure find_slits (fd, serial, pixel_scale, plate_scale, slit_width,
    slits, n_slits, id_string, this_number)

int	fd
int	serial[ARB]
int	slits[MAX_SLITS, N_PARAMS]
real	slit_width
int	this_number

char	keyword[LEN_KEYWORD], card_image[SZ_LINE], equal[LEN_KEYWORD]
char	id_string[LEN_IDSTRING]
int	serial_number, i, n_slits, ip, dummy, limit, j, jnext
real	xpos_lo, xpos_hi, ypos, pixel_scale, plate_scale
bool	streq(), is_in_range()
int	fscan(), ctor()

begin
	# Read card images until a SERIAL keyword is found:
	repeat {
	    if (fscan (fd) == EOF) 
	        return (EOF)
	    call gargwrd (keyword, LEN_KEYWORD)
	    if (streq (keyword, "SERIAL")) {
		call gargwrd (equal, LEN_KEYWORD)
		call gargi (serial_number)
		call printf ("Serial number %d seen\n")
		    call pargi (serial_number)
		call flush (STDOUT)
		if (is_in_range (serial, serial_number)) {
		    this_number = serial_number
		    break
		}
	    }
	}

	# Now positioned at proper entry, find NS2 keyword and slit locations:
	# This assumes keyword OBJECT always preceedes NS2.
	repeat {
	    if (fscan (fd) == EOF) 
		return (EOF)
	    call gargwrd (keyword, LEN_KEYWORD)
	    if (streq (keyword, "OBJECT")) {
		call gargwrd (equal, LEN_KEYWORD)
		call gargwrd (id_string, LEN_IDSTRING)
		next
	    }
	    if (streq (keyword, "NS2")) {
		call gargwrd (equal, LEN_KEYWORD)
		call gargi (n_slits)
		break
	    }
	}

	do i = 1, n_slits {
	    if (fscan (fd) == EOF)
		return (EOF)
	    else
	        call gargstr (card_image, SZ_LINE)

	    ip = START_COLUMN
	    dummy = ctor (card_image, ip, xpos_lo)
	    ip = ip + 8
	    dummy = ctor (card_image, ip, xpos_hi)
	    dummy = ctor (card_image, ip, ypos)
	    call calculate_slit_pos (xpos_lo, xpos_hi, ypos, slits, i,
		pixel_scale, plate_scale, slit_width)
	}
	
	# Sort slits array in order of increasing x - bubble sort
	for (limit = n_slits - 1; limit >= 1; limit = limit - 1) {
	    do j = 1, limit {
	        jnext = j + 1
	        if (slits [j,2] >= slits [jnext, 2])
		    call swap (jnext, j, slits)
	    }
	}
end


# CALCULATE_SLIT_POS -- calculate position of slit and store results
# in array "slits".  This procedure is called once for each slit.

procedure calculate_slit_pos (xplo, xphi, yp, slits, slit_num, pixel_scale, 
    plate_scale, slit_width)

real	xplo, xphi, yp
int	slits[MAX_SLITS, N_PARAMS], slit_num

int	x_lo, x_hi, ycen, ys, y_lo, y_hi
int	upper_ys, lower_ys
real	pixel_scale, plate_scale, slit_width

begin
	x_lo = int ((XY_ZERO_PT + xplo) / pixel_scale * plate_scale + 0.5) + 1
	x_hi = int ((XY_ZERO_PT + xphi) / pixel_scale * plate_scale + 0.5) - 1
	ycen = int ((XY_ZERO_PT +   yp) / pixel_scale * plate_scale + 0.5) 
	ys   = int ((slit_width / pixel_scale) + 0.5)

	# The following 4 statements were added june25,1985 at Jim's request,
	# and are intended to correct for rounding problems with slit width.
	lower_ys = ys / 2
	upper_ys = lower_ys
	if ((ys - lower_ys) > lower_ys)
	    upper_ys = lower_ys + 1

	# Next 2 statements modified at time of above change
	#y_lo = ycen - ys
	#y_hi = ycen + ys - 1
	y_lo = ycen - lower_ys
	y_hi = ycen + upper_ys - 1

	slits [slit_num, 1] = slit_num
	slits [slit_num, 2] = x_lo
	slits [slit_num, 3] = x_hi
	slits [slit_num, 4] = y_lo
	slits [slit_num, 5] = y_hi
end


# SWAP -- swaps entries in input array; used for bubble sort.

procedure swap (new, old, slits)

int	new, old			# New and old indices to be swapped
int	slits [MAX_SLITS, N_PARAMS]	# Array of slit endpoints and index

int	n
real	temp[N_PARAMS]

begin
	do n = 1, N_PARAMS {
	    temp[n] = slits [new, n]
	    slits [new, n] = slits [old, n]
	    slits [old, n] = temp[n]
	}
end


# WRITE_IMAGE -- writes two dimensional image of slit mask.  Slits and the
# area outside the circular field are clear; other mask areas are saturated.
# All pixel values are either clear (0) or saturated (255).

procedure write_image (im, slits, n_slits, plate_scale, pixel_scale)

pointer	im, sp, row
int	slits[MAX_SLITS, N_PARAMS]
int	n_slits
real	plate_scale, pixel_scale

int	center, size, n, mask_radius, edge_1, edge_2, k, i
pointer	impl2s()

begin
	# First, set some image header parameters
	call smark (sp)
	size = int ((XY_ZERO_PT * 2.0 * plate_scale / pixel_scale) + 2.0 + 0.5)
	call salloc (row, size, TY_SHORT)
	IM_PIXTYPE(im) = TY_SHORT
	IM_LEN(im, 1) = size
	IM_LEN(im, 2) = size

	center = (size / 2) + 1
	do n = 1, size {
	    mask_radius = int (sqrt (real ((center**2) - ((center - n)**2))))
	    edge_1 = center - mask_radius
	    edge_2 = center + mask_radius

	    do i = 1, edge_1 - 1
		Mems[row+i-1] = CLEAR

	    do i = edge_2 + 1, size
		Mems[row+i-1] = CLEAR

	    do i = edge_1, edge_2
		Mems[row+i-1] = SATURATE

	    do i = 1, n_slits {
		if ((n >= slits[i,2]) && (n <= slits[i,3])) {
		    # Set slitlet area to 0
		    edge_1 = slits [i, 4]
		    edge_2 = slits [i, 5]
		    do k = edge_1, edge_2 - 1
			Mems[row+k-1] = CLEAR
		}
	    }

	    # Now output accumulated row to IRAF image
	    call amovs (Mems[row], Mems[impl2s(im, n)], size)
	}
	call sfree (sp)
end


procedure write_crtpict_cards (cmd_file, site, this_number, slit_width,
    image_name, pixel_scale, date)

char	cmd_file[SZ_FNAME], site[SZ_LINE], image_name[SZ_FNAME], date[SZ_LINE]
int	this_number
real	slit_width, pixel_scale

begin
	# Generate command cards for execution of crtpict
end
