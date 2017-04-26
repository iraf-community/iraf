.help psiescape.h 1May92 plot
.ih
.NAME
psiescape.h -- Define the special GIO escape instructions.
.ih
DESCRIPTION
The following escape instructions are defined for the PostScript GKI
Interpreter:

  PS_CODE - Send the raw PostScript code to the output.
  PS_IMAGE_RED_LUT - Download a new lookup table for the Red component
                     of an image.
  PS_IMAGE_GREEN_LUT - Download a new lookup table for the Green component
                       of an image.
  PS_IMAGE_BLUE_LUT  - Download a new lookup table for the Blue component
                       of an image.
  PS_GR_RED_LUT      - Download a new lookup table for the Red component
                       for the graphics.
  PS_GR_GREEN_LUT    - Download a new lookup table for the Green component
                       for the graphics.
  PS_GR_BLUE_LUT     - Download a new lookup table for the Blue component
                       for the graphics.
  PS_ROMAN_FONT      - Specify a new font for the normal font.
  PS_GREEK_FONT      - Specify a new font for the greek font.
  PS_ITALIC_FONT     - Specify a new font for the italic font.
  PS_BOLD_FONT       - Specify a new font for the bold font.
  PS_VARIABLE_SPACE  - Change whether characters are mono-spaced or
                       variable-spaced.
  PS_DASH, PS_DOT,
  PS_SPACE           - Change the sizes of a dash, a dot, and the space
                       between them.
  PS_FILL_PATTERN    - Add/change fill patterns.

The size of the instruction array should have the following minimums:
  - For the PS_CODE instruction, the array should be the length of the string
    being passed.
  - The size of each image LUT array is PS_IMAGE_LUT_SIZE.
  - The size of each graphics LUT array is PS_GR_LUT_SIZE.
  - The font arrays should be the length of the string containing the name
    of the font to use.
  - For the PS_VARIABLE_SPACE, the size is PS_VARIABLE_SPACE_SIZE.
  - For PS_FILL_PATTERN, the size is PS_FILL_SIZE.
  
.ih
SEE ALSO
t_psikern
.endhelp
#---------------------------------------------------------------------------

# Define the escape instructions.
define	PS_CODE               	1001
define 	PS_IMAGE_RED_LUT      	1002
define 	PS_IMAGE_GREEN_LUT    	1003
define 	PS_IMAGE_BLUE_LUT     	1004
define 	PS_GR_RED_LUT         	1005
define 	PS_GR_GREEN_LUT       	1006
define 	PS_GR_BLUE_LUT        	1007
define 	PS_ROMAN_FONT         	1008
define 	PS_GREEK_FONT         	1009
define 	PS_ITALIC_FONT        	1010
define 	PS_BOLD_FONT          	1011
define 	PS_VARIABLE_SPACE     	1012
define 	PS_DOT                	1013
define 	PS_DASH               	1014
define 	PS_SPACE              	1015
define 	PS_FILL_PATTERN       	1016
define	PS_IMAGE_LUT		1017
define	PS_GRAPHICS_LUT		1018

# Define the sizes of the instruction arrays.
define 	PS_MAX_LUT_VALUE	255
define 	PS_IMAGE_LUT_SIZE	256
define 	PS_GR_LUT_SIZE		16
define 	PS_VARIABLE_SPACE_SIZE	1
define 	PS_FILL_SIZE		9

# Define how to pack/unpack a LUT defined as reals from 0-1 into
# a short array from 0-PS_MAX_LUT_VALUE.
define 	PS_PACKLUT    		(int($1*PS_MAX_LUT_VALUE))
define 	PS_UNPACKLUT  		($1/real(PS_MAX_LUT_VALUE))
