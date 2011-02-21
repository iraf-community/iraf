# WCS_DESC - The definition of the WCSLAB descriptor memory structure.
#
# Description
#   This include file defines the memory structures and macros needed to
#   access elements of a WCSLAB descriptor.  The descriptor provides all
#   the necessary elements for the routine wcslab to produce a labeled
#   graph.
#
# History
#   9May91 - Created the descriptor.  Jonathan D. Eisenhamer, STScI.
#  15May91 - Modified the descriptor to contain only pointers to arrays.
#            Two routines, wcs_create and wcs_destroy are required to
#            create the arrays that are pointed to in the descriptor. 
#            Also seperated the include file from the wcslab.h file. jde
#  12Jun91 - Rewrote some of the labelling parameters. jde
#  20Jun91 - Redesigned much of the parameters. jde
#---------------------------------------------------------------------------

# Value of opposite axis that polar labels should appear along.
define WL_POLAR_LABEL_POSITION   Memd[P2D($1)]

# The rotation between the Logical and World coordinate systems.
define WL_ROTA                   Memd[P2D($1+2)]

# Size of the axis titles.
define WL_AXIS_TITLE_SIZE        Memr[P2R($1+4)]

# The offset required to properly calculate positions in the image display.
define WL_IMAGE_X_OFF            Memr[P2R($1+5)]
define WL_IMAGE_Y_OFF            Memr[P2R($1+6)]

# Size of the grid labels.
define WL_LABEL_SIZE             Memr[P2R($1+7)]

# Major tick mark size.
define WL_MAJ_TICK_SIZE          Memr[P2R($1+8)]

# Minor tick mark size.
define WL_MIN_TICK_SIZE          Memr[P2R($1+9)]

# Magnification of the text size for the title.
define WL_TITLE_SIZE             Memr[P2R($1+10)]

# The side in polar/near-polar plots not to put Axis 1 labels.
define WL_BAD_LABEL_SIDE         Memi[$1+11]

# The type of graph that will be produced.  The possible value are:
#
#   UNKNOWN    -> Graph type will be determined
#   NORMAL     -> Approximate a cartesian grid
#   POLAR      -> Graph center on a pole
#   NEAR_POLAR -> Graph very close to a pole

define WL_GRAPH_TYPE             Memi[$1+12]

# Number of segments each line should be broken into to plot it.
define WL_LINE_SEGMENTS          Memi[$1+13]

# The grid line type for major grids.  The possible values are to standard 
# IRAF GIO polyline types.
define WL_MAJ_LINE_TYPE          Memi[$1+14]

# The grid line type for minor grids.  The possible values are to standard 
# IRAF GIO polyline types.
define WL_MIN_LINE_TYPE          Memi[$1+15]

# The number of label points.
define WL_N_LABELS               Memi[$1+16]

# The graphic WCS that is set to NDC units.
define WL_NDC_WCS                Memi[$1+17]

# The graphic WCS used to plot the grid lines.
define WL_PLOT_WCS               Memi[$1+18]

# The direction of the latitude labelling on polar graphs. Possible values are:
#
#   BOTTOM -> Towards the bottom of the graph.
#   TOP -> Towards the top of the graph.
#   RIGHT -> Towards the right of the graph.
#   LEFT -> Towards the left of the graph.

define WL_POLAR_LABEL_DIRECTION  Memi[$1+19]

# The possible axis types.  The possible values are:
#
#   RA_DEC_TAN - The tangential display in right ascension and declination.
#   LINEAR     - General linear systems.

define WL_SYSTEM_TYPE            Memi[$1+20]

# Define which side of the graph will have the title.
define WL_TITLE_SIDE             Memi[$1+21]

# True if the axis mapping has reversed the order of the axis relative
# to the logical system.
define WL_AXIS_FLIP              Memi[$1+22]

# TRUE if the labels should always be printed in full form.
define WL_ALWAYS_FULL_LABEL      Memi[$1+23]

# TRUE if the grid labels should rotate with the grid lines.
define WL_LABEL_ROTATE           Memi[$1+26]

# True if coordinate labels are to be written.
define WL_LABON                  Memi[$1+27]

# True if we are to write labels outside the window borders.  Else, write 
# them inside.
define WL_LABOUT                 Memi[$1+28]

# True if we are to draw the major grid lines.
define WL_MAJ_GRIDON             Memi[$1+29]

# True if we are to draw the minor grid lines.
define WL_MIN_GRIDON             Memi[$1+30]

# True if the graph parameters should be written back out to the 
# parameter file.
define WL_REMEMBER               Memi[$1+31]

# TRUE if tick marks should point into the graph.
define WL_TICK_IN                Memi[$1+32]

# Titles to label each axis.
define WL_AXIS_TITLE_PTR         Memi[$1+33]
define WL_AXIS_TITLE             Memc[WL_AXIS_TITLE_PTR($1)+(($2-1)*SZ_LINE)] 

# The sides the axis titles will appear.
define WL_AXIS_TITLE_SIDE_PTR    Memi[$1+34]
define WL_AXIS_TITLE_SIDE        Memi[WL_AXIS_TITLE_SIDE_PTR($1)+$2-1]

# Beginning values to start labeling the axes.
define WL_BEGIN_PTR              Memi[$1+35]
define WL_BEGIN                  Memd[WL_BEGIN_PTR($1)+$2-1]

# The name of the graphics device.
#define WL_DEVICE_PTR             Memi[$1+36]
#define WL_DEVICE                 Memc[WL_DEVICE_PTR($1)]

# Value to stop labeling the axes.
define WL_END_PTR                Memi[$1+37]
define WL_END                    Memd[WL_END_PTR($1)+$2-1]

# The graphics descriptor.
define WL_GP                     Memi[$1+38]

# The angle of text at this label point.
define WL_LABEL_ANGLE_PTR        Memi[$1+40]
define WL_LABEL_ANGLE            Memd[WL_LABEL_ANGLE_PTR($1)+$2-1]

# Which axis the label represents.
define WL_LABEL_AXIS_PTR         Memi[$1+41]
define WL_LABEL_AXIS             Memi[WL_LABEL_AXIS_PTR($1)+$2-1]

# The positions of tick mark/grid labels.
define WL_LABEL_POSITION_PTR     Memi[$1+42]
define WL_LABEL_POSITION         Memd[WL_LABEL_POSITION_PTR($1)+$2-1+(($3-1)*MAX_LABEL_POINTS)]
#
# NOTE:  If the axis are transposed, the positions represented here are
#        the corrected, transposed values.

# The sides the labels for each axis should appear on.
define WL_LABEL_SIDE_PTR         Memi[$1+43]
define WL_LABEL_SIDE             Memb[WL_LABEL_SIDE_PTR($1)+$2-1+(($3-1)*N_SIDES)]

# The value of the label.
define WL_LABEL_VALUE_PTR        Memi[$1+44]
define WL_LABEL_VALUE            Memd[WL_LABEL_VALUE_PTR($1)+$2-1]

# The center of the transformations in the logical system.  
define WL_LOGICAL_CENTER_PTR     Memi[$1+45]
define WL_LOGICAL_CENTER         Memd[WL_LOGICAL_CENTER_PTR($1)+$2-1]

# The coordinate transformation from Logical to World.
define WL_LWCT                   Memi[$1+46]

# Major grid intervals for the axis.
define WL_MAJ_I_PTR              Memi[$1+47]
define WL_MAJOR_INTERVAL         Memd[WL_MAJ_I_PTR($1)+$2-1]

# The minor intervals for the axis.
define WL_MIN_I_PTR              Memi[$1+48]
define WL_MINOR_INTERVAL         Memi[WL_MIN_I_PTR($1)+$2-1]

# Remember the extent of the labels around the plot box.
define WL_NV_PTR                 Memi[$1+49]
define WL_NEW_VIEW               Memr[WL_NV_PTR($1)+$2-1]

# The MWL structure.
define WL_MW                     Memi[$1+50]

# The values of the sides of the screen.  The indexes are defined as follows:
#
#   TOP -> Y-axis value at the top of display.
#   BOTTOM -> Y-axis value at bottom of display
#   RIGHT  -> X-axis value at right of display.
#   LEFT   -> X-axis value at left of display.
#
define WL_SCREEN_BOUNDARY_PTR    Memi[$1+51]
define WL_SCREEN_BOUNDARY        Memd[WL_SCREEN_BOUNDARY_PTR($1)+$2-1]

# The title that will be placed on the plot.
define WL_TITLE_PTR              Memi[$1+52]
define WL_TITLE                  Memc[WL_TITLE_PTR($1)]

# The coordinate transformation from World to Logical.
define WL_WLCT                   Memi[$1+53]

# The center of the transformations in the world system.
define WL_WORLD_CENTER_PTR       Memi[$1+54]
define WL_WORLD_CENTER           Memd[WL_WORLD_CENTER_PTR($1)+$2-1]

# The length of this structure.
define WL_LEN                    55+1

#---------------------------------------------------------------------------
# End of wcs_desc
#---------------------------------------------------------------------------
