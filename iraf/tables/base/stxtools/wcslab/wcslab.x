include <gset.h>
include <imhdr.h>
include <math.h>
include <mwset.h>
include "wcslab.h"
include "wcs_desc.h"
include <ctype.h>


# WCSLAB -- Procedure to draw labels and grids in sky projection coordinates.
#
# Description
#  Wcslab produces a labelling and grid based on the MWCS of a 
#  specified image.
#
#  The only things necessary to run this routine are:
#    1) Open an image and pass the image descriptor in im.
#    2) Open the graphics device and set the desired viewport (with a
#       gsview call).
#    3) Make sure that the wlpars pset is available.
#
#  Upon return, the graphics system will be in the state that it had been
#  left in and a "virtual viewport" will be returned in the arguments
#  left, right, bottom, top.  This viewport defines the region where labels
#  and/or titles were written.  If any graphics is performed within this
#  region, chances are that something will be overwritten.  If any other
#  graphics remain outside this region, then what was produced by this
#  subroutine will remain untouched.
#
# Bugs 
#  Can only handle sky projections for Right Ascension/Declination.   This
#  should be able to deal with any of the projections for this system, but
#  has only been tested with the Tangent projection.

procedure wcslab (mw, log_x1, log_x2, log_y1, log_y2, gp, title)

pointer	mw                        # I:   the wcs descriptor
real    log_x1, log_x2            # I/O: the viewport
real    log_y1, log_y2            # I/O: the viewport
pointer gp                        # I:   the graphics descriptor
char    title[ARB]                # I:   the image title

pointer	wd
real	junkx1, junkx2, junky1, junky2 
bool	clgetb()
pointer wl_create()
errchk	clgstr

begin
	# Allocate the descriptor.
	wd = wl_create()

	# Set the title name.
	call strcpy (title, WL_TITLE(wd), SZ_LINE)

	# Set the WCS descriptor. If the descriptor is NULL or if
	# the use_wcs parameter is yes, retrieve the parameter 
	# specified wcs.
	if (mw == NULL)
	    call wl_wcs_params (mw, log_x1, log_x2, log_y1, log_y2)
	else if (clgetb ("usewcs"))
	    call wl_wcs_params (mw, junkx1, junkx2, junky1, junky2)
	WL_MW(wd) = mw

	# Determine axis types.
	call wl_get_system_type (WL_MW(wd), WL_SYSTEM_TYPE(wd),
            WL_LOGICAL_CENTER(wd,1), WL_WORLD_CENTER(wd,1), WL_AXIS_FLIP(wd))
	if (IS_INDEFI(WL_SYSTEM_TYPE(wd)))
    	    call error (0, "WCSLAB: Image WCS is unsupported\n")

	# Get the parameters.
	call wl_gr_inparams (wd)

	# Copy the graphics descriptor.
	WL_GP(wd) = gp

	# Set the plot window in pixels (the logical space of the WCS).
	WL_SCREEN_BOUNDARY(wd,LEFT) = log_x1
	WL_SCREEN_BOUNDARY(wd,RIGHT) = log_x2
	WL_SCREEN_BOUNDARY(wd,BOTTOM) = log_y1
	WL_SCREEN_BOUNDARY(wd,TOP) = log_y2

	# Plot and label the coordinate grid.
	call wl_wcslab (wd)

	# Return the possibly modified graphics descriptor and viewport.
	gp = WL_GP(wd)
	call gsview (gp, WL_NEW_VIEW(wd,LEFT), WL_NEW_VIEW(wd,RIGHT),
	    WL_NEW_VIEW(wd,BOTTOM), WL_NEW_VIEW(wd,TOP))

	# Save the current parameters.
	if (WL_REMEMBER(wd) == YES)
	    call wl_gr_remparams (wd)

  	# Release the memory.
  	call wl_destroy (wd)
end


# WL_CREATE -- Create a WCSLAB descriptor and initialize it.
#
# Description
#   This routine allocates the memory for the WCSLAB descriptor and
#   subarrays and initializes values.
#
# Returns
#   the pointer to the WCSLAB descriptor.

pointer procedure wl_create()

int	i,j
pointer	wd

begin
	# Allocate the descriptor memory.
	call malloc (wd, WL_LEN, TY_STRUCT)

	# Allocate the subarrays.
	call malloc (WL_AXIS_TITLE_PTR(wd), SZ_LINE * N_DIM, TY_CHAR)
	call malloc (WL_AXIS_TITLE_SIDE_PTR(wd), N_SIDES * N_DIM, TY_BOOL)
	call malloc (WL_BEGIN_PTR(wd), N_DIM, TY_DOUBLE)
	call malloc (WL_END_PTR(wd), N_DIM, TY_DOUBLE)
	call malloc (WL_LABEL_ANGLE_PTR(wd), MAX_LABEL_POINTS, TY_DOUBLE)
	call malloc (WL_LABEL_AXIS_PTR(wd), MAX_LABEL_POINTS, TY_INT)
	call malloc (WL_LABEL_POSITION_PTR(wd), N_DIM * MAX_LABEL_POINTS,
	    TY_DOUBLE)
	call malloc (WL_LABEL_SIDE_PTR(wd), N_DIM * N_SIDES, TY_BOOL)
	call malloc (WL_LABEL_VALUE_PTR(wd), MAX_LABEL_POINTS, TY_DOUBLE)
	call malloc (WL_LOGICAL_CENTER_PTR(wd), N_DIM, TY_DOUBLE)
	call malloc (WL_MAJ_I_PTR(wd), N_DIM, TY_DOUBLE)
	call malloc (WL_MIN_I_PTR(wd), N_DIM, TY_INT)
	call malloc (WL_NV_PTR(wd), N_SIDES, TY_REAL)
	call malloc (WL_SCREEN_BOUNDARY_PTR(wd), N_SIDES, TY_DOUBLE)
	call malloc (WL_TITLE_PTR(wd), SZ_LINE, TY_CHAR)
	call malloc (WL_WORLD_CENTER_PTR(wd), N_DIM, TY_DOUBLE)

	# Initialize the simple values (should be the same as the parameter
	# file).
	WL_POLAR_LABEL_POSITION(wd) = INDEF
	WL_AXIS_TITLE_SIZE(wd) = 1.5
	WL_LABEL_SIZE(wd) = 1.0
	WL_MAJ_TICK_SIZE(wd) = .03
	WL_MIN_TICK_SIZE(wd) = .01
	WL_TITLE_SIZE(wd) = 2.0
	WL_GRAPH_TYPE(wd) = INDEFI
	WL_MAJ_LINE_TYPE(wd) = GL_SOLID
	WL_MIN_LINE_TYPE(wd) = GL_DOTTED
	WL_TITLE_SIDE(wd) = TOP
	WL_ALWAYS_FULL_LABEL(wd) = NO
	WL_LABEL_ROTATE(wd) = YES
	WL_LABON(wd) = YES
	WL_LABOUT(wd) = YES
	WL_MAJ_GRIDON(wd) = YES
	WL_MIN_GRIDON(wd) = NO
	WL_REMEMBER(wd) = NO
	WL_TICK_IN(wd) = YES

	# Initialize any strings.
	call strcpy ("imtitle", WL_TITLE(wd), SZ_LINE)

	# Initialize the axis dependent values.
	do i = 1, N_DIM {
	    WL_AXIS_TITLE(wd,i) = EOS
	    WL_AXIS_TITLE_SIDE(wd,i) = INDEFI
	    WL_BEGIN(wd,i) = INDEFD
	    WL_END(wd,i) = INDEFD
	    WL_MAJOR_INTERVAL(wd,i) = INDEFD
	    WL_MINOR_INTERVAL(wd,i) = 5
    	    do j = 1, N_SIDES
      	        WL_LABEL_SIDE(wd,j,i) = false
	}

        # Return the descriptor.
	return (wd) 
end


# WL_WCS_PARAMS -- Read the WCS descriptor from the parameters.
#
# Description
#  This procedure returns the WCS descriptor created from task parameters
#  and the logical space that will be graphed.
#
# Bugs
#  This only deals with two axes.

procedure wl_wcs_params (mw, log_x1, log_x2, log_y1, log_y2)

pointer mw              # O: The MWCS descriptor.
real    log_x1, log_x2, # O: The extent of the logical space to graph.
real	log_y1, log_y2

real    cd[2,2], r[2], w[2]
pointer sp, input, pp
pointer clopset(), mw_open()
real    clgpsetr()

begin
        call smark (sp)
        call salloc (input, SZ_LINE, TY_CHAR)

	# Open the pset.
	pp = clopset ("wcspars")

        # Create an MWCS descriptor.
        mw = mw_open (NULL, 2)

        # Get the types.
        call clgpset (pp, "ctype1", Memc[input], SZ_LINE)
        call wl_decode_ctype (mw, Memc[input], 1)
        call clgpset (pp, "ctype2", Memc[input], SZ_LINE)
        call wl_decode_ctype (mw, Memc[input], 2)

        # Get the reference coordinates.
        r[1] = clgpsetr (pp, "crpix1")
        r[2] = clgpsetr (pp, "crpix2")
        w[1] = clgpsetr (pp, "crval1")
        w[2] = clgpsetr (pp, "crval2")

        # Get the CD matrix.
        cd[1,1] = clgpsetr (pp, "cd1_1")
        cd[1,2] = clgpsetr (pp, "cd1_2")
        cd[2,1] = clgpsetr (pp, "cd2_1")
        cd[2,2] = clgpsetr (pp, "cd2_2")

        # Set the Wterm.
        call mw_swtermr (mw, r, w, cd, 2)

        # Get the extent of the logical space.
        log_x1 = clgpsetr (pp, "log_x1")
        log_x2 = clgpsetr (pp, "log_x2")
        log_y1 = clgpsetr (pp, "log_y1")
        log_y2 = clgpsetr (pp, "log_y2")

	# Close the pset.
	call clcpset (pp)

        call sfree (sp)
end


# WL_DECODE_CTYPE -- Decode the ctype string into axis type and system type.
#
# Description
#   The CTYPE is what is found in FITS keywords CTYPEn.  The value may
#   contain two pieces of information, always the system type and possibly
#   an individual axis type.  For systems such as plain old linear systems
#   just a system type is defined.  However, for celestial systems, both
#   types are defined in the form "axistype-systemtype".  There may be
#   any number of '-' in between the values.

procedure wl_decode_ctype (mw, input, axno)

pointer mw              # I: the MWCS descriptor
char    input[ARB]      # I: the string input
int     axno            # I: the axis being worked on

int     i, input_len
int     strncmp(), strldx(), strlen()
string  empty ""

begin

        input_len = strlen (input)

        # Fix some characters.
        do i = 1, input_len {
          if (input[i] == ' ' || input[i] == '\'')
            break
          else if (IS_UPPER(input[i]))
            input[i] = TO_LOWER(input[i])
          else if (input[i] == '_')
            input[i] = '-'
        }

        # Determine the type of function on this axis.
        if (strncmp (input, "linear", 6) == 0) {
          call mw_swtype (mw, 1, 2, "linear", empty)

        } else if (strncmp (input, "ra--", 4) == 0) {
          i = strldx ("-", input) + 1
          call mw_swtype (mw, 1, 2, input[i], empty)
          call mw_swattrs (mw, axno, "axtype", "ra")

        } else if (strncmp (input, "dec-", 4) == 0) {
          i = strldx ("-", input) + 1
          call mw_swtype (mw, 1, 2, input[i], empty) 
          call mw_swattrs (mw, axno, "axtype", "dec")

        } else {
          # Since we have to be able to read any FITS header, we have
          # no control over the value of CTYPEi.  If the value is
          # something we don't know about, assume a LINEAR axis, using
          # the given value of CTYPEi as the default axis label.
          call mw_swtype (mw, 1, 2, "linear", empty)
          call mw_swattrs (mw, axno, "label", input)
        }
        
end


# WL_GET_SYSTEM_TYPE -- Determine type of transformation the MWCS represents.
#
# Note
#   For some systems, the axis mapping reverses the order to make
#   the rest of the code tractable.  The only problem is that when graphing,
#   the graph routines need to "fix" this reversal.  Also note that this
#   occurs only for systems that have distinct axis types, such as RA and
#   DEC.
#
# Bugs
#   A potential problem:  For a WCS that has more axes than necessary
#   for the sky projections, those axis are set such that during
#   transformations, the first index position is used.  For the one
#   example I have seen, the "third" axis is time and this interpretation
#   works.  But, I am sure something will fall apart because of this.

procedure wl_get_system_type (mw, system_type, logical_center, world_center,
	flip)

pointer mw                    # I: the MWCS descriptor.
int	system_type           # O: the transformation type:
                              #    RA_DEC     -> tan, sin, or arc projection
                              #                  in right ascension and
			      #                  declination
                              #    LINEAR     -> any regular linear system
                              #    INDEFI     -> could not be determined
double  logical_center[N_DIM] # O: the center point in the logical system.
double  world_center[N_DIM]   # O: the center point in the world system.
int	flip                  # O: true if the order of the axes have been
                              #    changed by axis mappins

double	tmp_logical[MAX_DIM], tmp_world[MAX_DIM]
int	wcs_dim, axis, index_sys1, index_sys2, found_axis
int	axno[MAX_DIM], axval[MAX_DIM], found_axis_list[N_DIM]
pointer	sp, axtype, cd, cur_type
int	mw_stati(), strncmp(), strdic()
errchk	mw_gwattrs

begin
	# Get some memory.
	call smark (sp)
	call salloc (axtype, SZ_LINE, TY_CHAR)
	call salloc (cur_type, SZ_LINE, TY_CHAR)
	call salloc (cd, MAX_DIM, TY_DOUBLE)

	# Get the dimensionality of the WCS.
	call mw_seti (mw, MW_USEAXMAP, NO)
	wcs_dim = mw_stati (mw, MW_NDIM)

	# Initialize the two dimensions.
	index_sys1 = INDEFI
	index_sys2 = INDEFI

	# Look through the possible supported axis types.  When a type has
	# exactly N_DIM axes defined, that will be the one used.

	for (system_type = 1; system_type <= NUMBER_OF_SUPPORTED_TYPES;
	    system_type = system_type + 1) {

	    # Determine the string that should be looked for.
	    switch (system_type)  {
	    case RA_DEC:
	        call strcpy (RA_DEC_DICTIONARY, Memc[cur_type], SZ_LINE)
	    case LINEAR:
	        call strcpy (LINEAR_DICTIONARY, Memc[cur_type], SZ_LINE)
	    }

	    # Initialize the number of found axis.
	    found_axis = 0

	    # Examine each axis to determine whether the current axis type is
	    # the one to use.
	    for (axis = 1; axis <= wcs_dim; axis = axis + 1) {

		# If the current physical axis is not mapped, ignore it.
		# This statement is causing a problem in 2.10.3, not sure
		# why but am removing it for now.
		#if (axno[axis] == 0)
		    #next

	        ifnoerr (call mw_gwattrs( mw, axis, "wtype", Memc[axtype],
	            SZ_LINE)) {
        	    call strlwr (Memc[axtype]) 

		    # If this axis type matches the one being looked for, add
		    # it to the axis list. If there are too many axis of the
		    # current type found, don't add to the found axis list.

		    if (strdic (Memc[axtype], Memc[axtype], SZ_LINE,
                         Memc[cur_type]) > 0) {
		        found_axis = found_axis + 1
		        if (found_axis <= N_DIM)
			    found_axis_list[found_axis] = axis
		    }
	        }
	    }

	    # Check to see whether we have the right number axes.
	    if (found_axis == N_DIM)
		break

	}

	# If any axes were found, then further check axis types.
	# Depending on the axis type, there may be need to distinguish
	# between the two possible axis further.

	if  (found_axis == N_DIM) 
	    switch (system_type) {
	    case RA_DEC:
	        for (axis = 1; axis <= N_DIM; axis = axis + 1)
		    ifnoerr (call mw_gwattrs (mw, found_axis_list[axis], 
                        "axtype", Memc[axtype], SZ_LINE)) {
		    call strlwr( Memc[axtype] )
		    if (strncmp (Memc[axtype], "ra", 2) == 0)
			index_sys1 = found_axis_list[axis]
		    else if (strncmp (Memc[axtype], "dec", 3) == 0)
			index_sys2 = found_axis_list[axis]
		}

	    # The "default" seems to be the LINEAR case for MWCS.
	    # Since no other information is provided, this is all we know.
	    default:
		index_sys1 = found_axis_list[1]
		index_sys2 = found_axis_list[2]
	    }

	# If either axis is unknown, something is wrong.  If the WCS has two
	# axes defined, then make some grand assumptions.  If not, then there
	# is nothing more to be done.

	if (IS_INDEFI (index_sys1) || IS_INDEFI (index_sys2)) {
	    if (wcs_dim >= N_DIM) {
	        index_sys1 = 1
	        index_sys2 = 2
	    } else
	        call error (0, "Wcslab: Fewer than two defined axes")
	}

	# Zero the axis values and set any "unknown" axis to always use the
	# "first" position in that axis direction.  This will more than likely
	# be a problem, but no general solution comes to mind this second.

	call amovki (0, axno, wcs_dim)
	call amovki (0, axval, wcs_dim)

	# Setup so that the desired axes are set as the X and Y axis.
	axno[index_sys1] = X_DIM
	axno[index_sys2] = Y_DIM
	call mw_saxmap (mw, axno, axval, wcs_dim)

	# Recover the center points of the Logical and World systems.
	call mw_gwtermd (mw, tmp_logical, tmp_world, Memd[cd], wcs_dim)

	logical_center[X_DIM] = tmp_logical[index_sys1]
	logical_center[Y_DIM] = tmp_logical[index_sys2]
	world_center[X_DIM] = tmp_world[index_sys1]
	world_center[Y_DIM] = tmp_world[index_sys2]

	# Check for reversal of axes
	if (index_sys1 > index_sys2)
	    flip = YES
	else
	    flip = NO

	# Release the memory.
	call sfree (sp) 
end


# WL_GR_INPARAMS -- Read in the graphics parameters for wcslab.
#	
# Description
#  Read all the parameters in and make some decisions about what
#  will be done.

procedure wl_gr_inparams (wd) 

pointer wd                       # I: the WCSLAB descriptor

pointer sp, aline, pp
bool	clgpsetb(), streq()
double	wl_string_to_internal()
int	btoi(), strdic(), wl_line_type(), clgpseti()
pointer	clopset()
real	clgpsetr()

begin
	# Get some memory.
	call smark (sp)
	call salloc (aline, SZ_LINE, TY_CHAR)

	# Open the pset.
	pp = clopset ("wlpars")

	# Get the title if other than the default.
	call clgpset (pp, "title", Memc[aline], SZ_LINE)
	if (! streq (Memc[aline], "imtitle"))
	    call strcpy (Memc[aline], WL_TITLE(wd), SZ_LINE)

	# Get the axis titles.
	call clgpset (pp, "axis1_title", WL_AXIS_TITLE(wd,AXIS1), SZ_LINE)
	call clgpset (pp, "axis2_title", WL_AXIS_TITLE(wd,AXIS2), SZ_LINE)

	# Get the parameters.
	WL_ALWAYS_FULL_LABEL(wd) = btoi (clgpsetb (pp,"full_label"))
	WL_AXIS_TITLE_SIZE(wd) = clgpsetr (pp, "axis_title_size")
	WL_LABEL_ROTATE(wd) = btoi (clgpsetb (pp, "rotate"))
	WL_LABEL_SIZE(wd) = clgpsetr (pp, "label_size")
	WL_LABON(wd) = btoi (clgpsetb (pp, "dolabel"))
	WL_LABOUT(wd) = btoi (clgpsetb (pp, "labout"))
	WL_MAJ_GRIDON(wd) = btoi (clgpsetb (pp, "major_grid"))
	WL_MAJ_TICK_SIZE(wd) = clgpsetr (pp, "major_tick")
	WL_MIN_GRIDON(wd) = btoi (clgpsetb (pp, "minor_grid"))
	WL_MINOR_INTERVAL(wd,AXIS1) = clgpseti (pp, "axis1_minor")
	WL_MINOR_INTERVAL(wd,AXIS2) = clgpseti (pp, "axis2_minor")
	WL_MIN_TICK_SIZE(wd) = clgpsetr (pp, "minor_tick")
	WL_REMEMBER(wd) = btoi (clgpsetb (pp, "remember"))
	WL_TICK_IN(wd) = btoi (clgpsetb (pp, "tick_in"))
	WL_TITLE_SIZE(wd) = clgpsetr (pp, "title_size")

	# Set what type of graph will be plotted.
	call clgpset (pp, "graph_type", Memc[aline], SZ_LINE)
	call strlwr (Memc[aline])
	WL_GRAPH_TYPE(wd) = strdic (Memc[aline], Memc[aline], SZ_LINE,
	    GRAPHTYPES)
	if (WL_GRAPH_TYPE(wd) <= 0)
	    WL_GRAPH_TYPE(wd) = INDEFI

	# Get which sides labels will appear on.
	call clgpset (pp, "axis1_side", Memc[aline], SZ_LINE)
	call strlwr (Memc[aline])
	call wl_label_side (Memc[aline], WL_LABEL_SIDE(wd,1,AXIS1))

	call clgpset (pp, "axis2_side", Memc[aline], SZ_LINE)
	call strlwr (Memc[aline])
	call wl_label_side (Memc[aline], WL_LABEL_SIDE(wd,1,AXIS2))

	# Get the polar justification direction.
	call clgpset (pp, "justify", Memc[aline], SZ_LINE)
	call strlwr (Memc[aline])
	WL_POLAR_LABEL_DIRECTION(wd) = strdic (Memc[aline], Memc[aline],
	    SZ_LINE, GRAPHSIDES)
	if (WL_POLAR_LABEL_DIRECTION(wd) <= 0)
	    WL_POLAR_LABEL_DIRECTION(wd) = INDEFI

  	# Decode the graphing parameters.
  	call clgpset (pp, "axis1_int", Memc[aline], SZ_LINE)
	WL_MAJOR_INTERVAL(wd,AXIS1) = wl_string_to_internal (Memc[aline], 
            WL_SYSTEM_TYPE(wd), AXIS1)
	call clgpset (pp, "axis1_beg", Memc[aline], SZ_LINE)
	WL_BEGIN(wd,AXIS1) = wl_string_to_internal (Memc[aline],
	    WL_SYSTEM_TYPE(wd), AXIS1)
	call clgpset (pp, "axis1_end", Memc[aline], SZ_LINE)
	WL_END(wd,AXIS1) = wl_string_to_internal (Memc[aline],
	    WL_SYSTEM_TYPE(wd), AXIS1)

	call clgpset (pp, "axis2_int", Memc[aline], SZ_LINE)
	WL_MAJOR_INTERVAL(wd,AXIS2) = wl_string_to_internal (Memc[aline], 
            WL_SYSTEM_TYPE(wd), AXIS2)
	call clgpset (pp, "axis2_beg", Memc[aline], SZ_LINE)
	WL_BEGIN(wd,AXIS2) = wl_string_to_internal(Memc[aline],
	    WL_SYSTEM_TYPE(wd), AXIS2 )
	call clgpset (pp, "axis2_end", Memc[aline], SZ_LINE)
	WL_END(wd,AXIS2) = wl_string_to_internal (Memc[aline],
	    WL_SYSTEM_TYPE(wd), AXIS2)

	# Get the polar label position.
	call clgpset (pp, "axis2_dir", Memc[aline], SZ_LINE)
	WL_POLAR_LABEL_POSITION(wd) = wl_string_to_internal( Memc[aline],
	    WL_SYSTEM_TYPE(wd), AXIS1)

	# Get the axis titles.
	call clgpset (pp, "axis1_title_side", Memc[aline], SZ_LINE)
	call strlwr (Memc[aline])
	WL_AXIS_TITLE_SIDE(wd,AXIS1) = strdic (Memc[aline], Memc[aline],
	    SZ_LINE, GRAPHSIDES)
	if (WL_AXIS_TITLE_SIDE(wd,AXIS1) <= 0)
	    WL_AXIS_TITLE_SIDE(wd,AXIS1) = INDEFI

	call clgpset (pp, "axis2_title_side", Memc[aline], SZ_LINE) 
	call strlwr (Memc[aline])
	WL_AXIS_TITLE_SIDE(wd,AXIS2) = strdic (Memc[aline], Memc[aline],
	    SZ_LINE, GRAPHSIDES)
	if (WL_AXIS_TITLE_SIDE(wd,AXIS2) <= 0)
	    WL_AXIS_TITLE_SIDE(wd,AXIS2) = INDEFI

	# Decode the grid line types.
	call clgpset (pp, "major_line", Memc[aline], SZ_LINE)
	WL_MAJ_LINE_TYPE(wd) = wl_line_type (Memc[aline])
	call clgpset (pp, "minor_line", Memc[aline], SZ_LINE)
	WL_MIN_LINE_TYPE(wd) = wl_line_type (Memc[aline])

	# Get the title side.
	call clgpset (pp, "title_side", Memc[aline], SZ_LINE)
	call strlwr (Memc[ aline])
	WL_TITLE_SIDE(wd) = strdic (Memc[aline], Memc[aline], SZ_LINE,
	    GRAPHSIDES)

	# Close the pset.
	call clcpset (pp)

	# Free memory.
	call sfree (sp)
end


# WL_GR_REMPARAMS -- Write out the graphing parameters.

procedure wl_gr_remparams (wd)

pointer wd  		# I: the WCSLAB descriptor.

pointer sp, output, pp
pointer	clopset()

begin
	# Get some memory.
	call smark (sp)
	call salloc (output, SZ_LINE, TY_CHAR)

	# Open the pset.
	pp = clopset ("wlpars")

	# Set the graph type.
	switch (WL_GRAPH_TYPE(wd)) {
	case NORMAL:
	    call clppset (pp, "graph_type", "normal")
	case POLAR:
	    call clppset (pp, "graph_type", "polar")
	case NEAR_POLAR:
	    call clppset (pp, "graph_type", "near_polar")
	default:
	    call clppset (pp, "graph_type", "default")
	}

	# Write back the labelling parameters.
	call wl_internal_to_string (WL_MAJOR_INTERVAL(wd,AXIS1),
	    WL_SYSTEM_TYPE(wd), AXIS1, Memc[output])
	call clppset (pp, "axis1_int", Memc[output])
	call wl_internal_to_string (WL_BEGIN(wd,AXIS1), WL_SYSTEM_TYPE(wd), 
	    AXIS1, Memc[output])
	call clppset (pp, "axis1_beg", Memc[output])
	call wl_internal_to_string (WL_END(WD,AXIS1), WL_SYSTEM_TYPE(wd), 
            AXIS1, Memc[output])
	call clppset (pp, "axis1_end", Memc[output])
	call wl_internal_to_string (WL_MAJOR_INTERVAL(wd,AXIS2),
	    WL_SYSTEM_TYPE(wd), AXIS2, Memc[output])
	call clppset (pp, "axis2_int", Memc[output])
	call wl_internal_to_string (WL_BEGIN(wd,AXIS2), WL_SYSTEM_TYPE(wd), 
            AXIS2, Memc[output])
	call clppset (pp, "axis2_beg", Memc[output])
	call wl_internal_to_string (WL_END(wd,AXIS2), WL_SYSTEM_TYPE(wd), 
            AXIS2, Memc[output])
	call clppset (pp, "axis2_end", Memc[output])
	call wl_internal_to_string (WL_POLAR_LABEL_POSITION(wd),
	    WL_SYSTEM_TYPE(wd), AXIS1, Memc[output])
	call clppset (pp, "axis2_dir", Memc[output])

	# Write back labelling justification.
	call wl_side_to_string (WL_POLAR_LABEL_DIRECTION(wd), Memc[output],
	    SZ_LINE)
	call clppset (pp, "justify", Memc[output])

	# Put the axis title sides out. 
	call wl_side_to_string (WL_AXIS_TITLE_SIDE(wd,AXIS1), Memc[output],
	    SZ_LINE)
	call clppset (pp, "axis1_title_side", Memc[output])
	call wl_side_to_string (WL_AXIS_TITLE_SIDE(wd,AXIS2), Memc[output],
	    SZ_LINE )
	call clppset (pp, "axis2_title_side", Memc[output])

	# Put the label sides out.
	call wl_put_label_sides (WL_LABEL_SIDE(wd,1,AXIS1), Memc[output],
	    SZ_LINE )
	call clppset (pp, "axis1_side", Memc[output])
	call wl_put_label_sides (WL_LABEL_SIDE(wd,1,AXIS2), Memc[output],
	    SZ_LINE)
	call clppset (pp, "axis2_side", Memc[output])

	# Close the pset.
	call clcpset (pp)

	# Free memory.
	call sfree (sp) 
end


# WL_DESTROY -- Deallocate the WCSLAB descriptor.

procedure wl_destroy (wd)

pointer wd  	# I: the WCSLAB descriptor to be destroyed

begin
	# Deallocate all the subarrays.
	call mfree (WL_WORLD_CENTER_PTR(wd), TY_DOUBLE)
	call mfree (WL_TITLE_PTR(wd), TY_CHAR)
	call mfree (WL_SCREEN_BOUNDARY_PTR(wd), TY_DOUBLE)
	call mfree (WL_NV_PTR(wd), TY_REAL)
	call mfree (WL_MIN_I_PTR(wd), TY_INT)
	call mfree (WL_MAJ_I_PTR(wd), TY_DOUBLE)
	call mfree (WL_LOGICAL_CENTER_PTR(wd), TY_DOUBLE)
	call mfree (WL_LABEL_VALUE_PTR(wd), TY_DOUBLE)
	call mfree (WL_LABEL_SIDE_PTR(wd), TY_BOOL)
	call mfree (WL_LABEL_POSITION_PTR(wd), TY_DOUBLE)
	call mfree (WL_LABEL_AXIS_PTR(wd), TY_INT)
	call mfree (WL_LABEL_ANGLE_PTR(wd), TY_DOUBLE)
	call mfree (WL_END_PTR(wd), TY_DOUBLE)
	call mfree (WL_BEGIN_PTR(wd), TY_DOUBLE)
	call mfree (WL_AXIS_TITLE_SIDE_PTR(wd), TY_BOOL)
	call mfree (WL_AXIS_TITLE_PTR(wd), TY_CHAR)

	# Now deallocate the structure.
	call mfree (wd, TY_STRUCT)
end


# WL_LABEL_SIDE -- Decode string into set of booleans  sides.

procedure wl_label_side (input, flag)

char	input[ARB]     # I: string listing the sides to be labeled
bool	flag[N_SIDES]  # O: the flags indicating which sides wll be labeled

int	i 
int	strmatch()

begin
	# Initialize all the flags to false.
	do i = 1, N_SIDES
	    flag[i] = false

	# Now set each side that is in the list.
	if (strmatch (input, "right") != 0)
	    flag[RIGHT] = true
	if (strmatch (input, "left") != 0)
	    flag[LEFT] = true
	if (strmatch (input, "top") != 0)
	    flag[TOP] = true
	if (strmatch (input, "bottom") != 0)
	    flag[BOTTOM] = true
end


# WL_STRING_TO_INTERVAL -- Convert from a string to a number.
#
# Description
#  Since (ideally) the wcslab task should be able to handle any sky
#  map transformation, there are a number of potential units that can be
#  transformed from.  The specification of coordinates in these systems
#  are also quite varied.  Thus, for input purposes, coordinates are entered
#  as strings.  This routine decodes the strings to a common unit (degrees)
#  based on the type of system being graphed.
#
# Function Returns
#  This returns the single coordinate value converted to a base system 
#  (degrees).

double procedure wl_string_to_internal (input, axis_type, which_axis)

char	input[ARB]	# I; the string containing the numerical value
int	axis_type   	# I: the type of wcs
int	which_axis	# I: the axis number

double	value
int	strlen(), nscan()

begin
	# It is possible that the value was not defined.
	if (strlen (input)  <= 0)
	    value = INDEFD

	# Decode based on the system.
	else
	    switch (axis_type) {

	    # The RA and DEC systems.
	    case RA_DEC:

	        # Since SPP FMTIO can handle the HH:MM:SS format, just let it
		# read in the value.  However, there is no way to distinquish
		# H:M:S from D:M:S. If the axis being read is RA, assume that
		# it was H:M:S.

	        call sscan (input)
	            call gargd (value)

		# If the axis is Longitude == RA, then convert the hours to
		# degrees.
		if (nscan() < 1) {
		    value = INDEFD
		} else {
		    if  (which_axis == AXIS1)
		        value = HRSTODEG (value)
		}

	    # Default- unknown system, just read the string as a double
	    # precision and return it.
	    default:
		call sscan (input)
		    call gargd (value)
		if (nscan() < 1)
		    value = INDEFD
	    }

	return (value)
end


# WL_LINE_TYPE -- Decode a string into an IRAF GIO polyline type.

int procedure wl_line_type (line_type_string)

char	line_type_string[ARB]  # I: the string specifying the line type
                               #      "solid" -> GL_SOLID
                               #      "dotted" -> GL_DOTTED
                               #      "dashed" -> GL_DASHED
                               #      "dotdash" -> GL_DOTDASH
int	type
bool	streq()

begin
	if (streq (line_type_string, "solid"))
	    type = GL_SOLID
	else if (streq (line_type_string, "dotted"))
	    type = GL_DOTTED
	else if  (streq( line_type_string, "dashed"))
	    type = GL_DASHED
	else if  (streq (line_type_string, "dotdash"))
	    type = GL_DOTDASH
	else {
	    call eprintf ("Pattern unknown, using 'solid'.\n")
	    type = GL_SOLID
	}

	return (type)
end


# WL_INTERNAL_TO_STRING - Convert internal representation to a string.

procedure wl_internal_to_string (value, system_type, which_axis, output)

double	value          # I: the value to convert
int	system_type    # I: the wcs type
int	which_axis     # I: the axis
char	output[ARB]    # O: the output string

begin
	# If the value is undefined, write an empty string.
	if (IS_INDEFD (value))
	    output[1] = EOS

	# Else, convert the value depending on the axis types.
	else
	    switch (system_type)  {

	    # Handle the RA, DEC
	    case RA_DEC:

		# If this is Axis1 == Right Ascension, then convert to hours.
		if  (which_axis == AXIS1)
		    value = value / 15.0D0

		call sprintf (output, SZ_LINE, "%.6h")
		    call pargd (value)

	    # Else, just write a value.
	    default:
		call sprintf (output, SZ_LINE, "%.7g")
		call pargd (value)
	    }

end


# WL_SIDE_TO_STRING -- Convert a side to its string representation.

procedure wl_side_to_string (side, output, max_len)

int	side              # I: the side to convert
char	output[max_len]   # O: the string representation of the side
int	 max_len          # I: the maximum length of the output string

begin
	switch (side) {
	case RIGHT:
	    call strcpy ("right", output, max_len)
	case LEFT:
	    call strcpy ("left", output, max_len)
	case TOP:
	    call strcpy ("top", output, max_len)
	case BOTTOM:
	    call strcpy ("bottom", output, max_len)
	default:
	    call strcpy ("default", output, max_len)
	}
end


# WL_PUT_LABEL_SIDES -- Create a string containing the sides specified.

procedure wl_put_label_sides (side_flags, output, max_len)

bool	side_flags[N_SIDES]  # I: the boolean array of sides
char	output[ARB]          # O: the output comma separated list of sides
int	max_len              # I: maximum length of the output string

int	i
pointer sp, side
int	strlen()

begin
	# Get memory.
	call smark (sp)
	call salloc (side, max_len, TY_CHAR)

	# Build the list.
	output[1] = EOS
	do i = 1, N_SIDES
	    if (side_flags[i]) {
	        if (strlen (output) != 0)
                    call strcat (",", output, max_len)
		call wl_side_to_string (i, Memc[side], max_len)
		call strcat (Memc[side], output, max_len)
	    }

	if (strlen (output) == 0)
	    call strcat ("default", output, max_len)

	# Free memory.
	call sfree (sp)
end
