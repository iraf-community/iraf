.TL
The "cv" Display Package
.AU
Richard Wolff
.DA
.PP
The \fIcv\fR program is used to control the image display from within
\fIIRAF\fR.  It differs from most \fIIRAF\fR programs since it has its
own prompt and its own internal "language".  Each of the available commands
is described in the following paragraphs, but first a few comments on the
command structure seem in order.  Commands are distinguished by their
first letter, except for a few instances where the second letter is needed.
The rest of the command name can be typed if you wish.  Commands often
require specification of frames numbers, colors, quadrants, or numeric
values.  In most cases, the order is unimportant,  but, zoom, for instance,
does require the zoom power right after the command name.  The order given
in the \fIhelp\fR command will always work.
.PP
A frame list is indicated in the \fIhelp\fR listing with an \fBF\fR.  This
is to be replaced in the typed command by an \fBf\fR followed (no spaces)
with a list of the pertinent image planes.  Thus, \fBf1\fR means
.I "frame 1"
while \fBf42\fR means
.I "frames 4"
and \fI2\fR.  In most cases, the leading \fBf\fR can be omitted.
The specification \fBfa\fR means \fIall frames\fR.  In those
cases in the \fIhelp\fR menu where the frame specification is optional,
omitting the frame list is the same as typing \fBfa\fR; that is, operate
on \fIall\fR frames.
.PP
A color specification is a \fBc\fR followed by a set of letters.
The letter \fBa\fR means \fIall\fR, just as in the frame specification.
The letters \fBr, b,\fR and \fBg\fR are the other possibilities for all
commands other than \fIdg\fR and \fIsnap\fR.  For displaying graphics
planes (\fBdg\fR), the other possibilities are \fBy, p, m, w\fR which
stand for \fIyellow, purple, mauve,\fR and \fIwhite\fR.  (\fIMauve\fR is
the wrong name and will get changed.)  The \fIsnap\fR command accepts, in
addition to the standard three colors, \fBm, bw,\fR and \fBrgb\fR, which
stand for \fImonochrome, black and white,\fR and \fIfull color\fR.  (See
the discussion under \fIsnap\fR for further explanation.)
An omitted color specification is the same as \fIall colors\fR.
.PP
Quadrants are given by a \fBq\fR followed by numbers from the set one through
four, or the letter \fBa\fR as in the frame and color cases.  Quadrants are
numbered in the standard way, with the upper right being \fI1\fR, the upper
left \fI2\fR, etc.  Adjacent quadrants may be referenced by \fBt, b, l,\fR
and \fBr\fR, standing for \fItop, bottom, left,\fR and \fIright\fR.  An
omitted quadrant specification is the same as \fIall quadrants\fR.  Quadrants
are effective only if the split screen command has set the split point to
something other than the "origin".
.sp
.SH
\fBblink\fR N F (C Q) (F C Q)
.IP
The blink rate is given by \fBN\fR, which is in tenths of a second.  Although
current timing routines in \fIIRAF\fR do not recognize partial seconds,
for the NOAO 4.2BSD UNIX implementation, a non-portable timing routine is
used so that tenth seconds are usable.
Erratic timing is pretty much the rule when the system load is large.
One frame must be given,
followed by any color or quadrant specification, and then
optionally followed by any number of similar triads.  A specification of
\fI10 f12 f3 f3 f4\fR would display frames one and two for one second, then
frame three for two one second intervals, then frame 4, and then recycle.
The first blink cycle may appear somewhat odd as the code "settles in",
but the sequence should become regular after that (except for timing
problems due to system load).  In split screen mode, it is necessary to
specify all the frames together with quadrants, which leads to a lot of
typing:  The reason is that blink simply cycles through a series of
\fBdi\fR commands, and hence it requires the same information as that
command.
.SH
\fBcursor\fR [on off F]
.IP
This command is used to turn the cursor on or off, and to read coordinates
and pixel values from a frame.  Pixel coordinates for a feature are those
of the image as loaded into the display, and do not change as the image
is panned or zoomed.  Fractional pixel positions are given for zoomed
images, with a minimum number of decimal places printed (but the same number
for both the \fIx\fR and \fIy\fR coordinates).
For an unpanned, unzoomed image plane, the lower left corner
of the \fIscreen\fR is (1,1)
even if the image you loaded is smaller than 512x512, occupies only
a portion of the display screen, and does not extend to the lower left
corner of the screen.  This defect will likely be remedied
when the \fIcv\fR package is properly integrated into \fIIRAF\fR.
Pixel information can be read from a frame that is not being displayed.
.SH
\fBdi\fR F (C Q) [on off]
.IP
The \fId\fRisplay \fIi\fRmage command turns specified frames on (or off).
Turning a frame off does not erase it.  A frame need not have all colors
turned on, nor appear in all quadrants of a split screen display.
.SH
\fBdg\fR C (F Q) [on off]
.IP
The \fId\fRisplay \fIg\fRraphics command turns specific graphics planes
on or off.  For the IIS display, neither the frame nor the quadrant
parameters are relevant.  A side-effect of this command is that it
resets the graphics hardware to the \fIcv\fR standard: red cursor and
seven graphics planes, each colored differently.  If the display is in
a "weird" state that is not cured with the \fIreset r/t\fR commands,
and a \fIreset i\fR would destroy images of interest, try a \fIdg ca on\fR
command followed by \fIdg ca off\fR.
.SH
\fBerase\fR [F all graphics]
.IP
This command erases the specified frame, or all the graphics planes, or
all data planes.  The command \fBclear\fR is a synonym.
.SH
\fBmatch\fR (o) (F) (C) (to) (F) (C)
.IP
This command allows the user to copy a look-up table to a specified set
of tables, and hence, to match the mapping function of frames (and/or
colors) to a reference table.  If the \fBo\fR parameter is omitted, the
match is among the look-up tables associated with particular frames;
otherwise, the \fIouput\fR tables are used (hence, the \fBo\fR).  In the
latter case, only colors are important; the frame information should
be omitted.  For the individual frame tables, colors can be omitted, in
which case a match of frame one to two means to copy the three tables
of frame two (red, green, and blue) to those of frame one.  Only one
reference frame or color should be given, but \fImatch f23 cgb f1 cr\fR
is legal and means to match the green and blue color tables of both
frames two and three to the red table of frame one.
.SH
\fBoffset\fR C N
.IP
The value N, which can range from -4095 to +4095 is added to the data
pipeline for color \fBC\fR, thus offsetting the data.  This is useful
if one needs to change the data range that is mapped into the useful part
of the output tables.
.SH
\fBpan\fR (F)
.IP
When invoked, this command connects the trackball to the specified frames
and allows the user to move (pan/roam/scroll) the image about the screen.
This function is automatically invoked whenever the zoom factor is changed.
.SH
\fBpseudo\fR (o) (F C) (rn sn)
.IP
Look-up tables are changed with the \fIwindow\fR and the \fIpseudocolor\fR
commands.  Windowing provides linear functions and is discussed under that
command; \fIpseudo\fR provides pseudo-coloring capabilities.  Pseudo-color
maps are usually best done in the output tables, rather than in the
look-up tables associated with particular frames; hence, \fBps o\fR is
the more likely invocation of the start of the command line.  A color
(or colors) can be specified for "output" pseudocolor, in which case, only
those colors will be affected.  For frame look-up tables,
the frame must be specified.
.IP
Two mappings are provided.  One uses a set of randomly selected colors
mapped to a specified number of pixel value ranges.  The other uses
triangle color mappings.  The former is invoked with the \fI(rn sn)\fR
options.  In this case, the number following \fBr\fR gives the number of
ranges/levels into which the input data range is to be divided; to
each such range, a randomly selected color is assigned.  The number
following \fBs\fR is a seed for the random number generator;  changing
this while using the same number of levels gives different color mappings.
The default seed is the number of levels.  If only the seed is given (\fBr\fR
omitted), the default number of levels is 8.  This mapping is used when
a contour type display is desired:  each color represents an intensity range
whose width is inversely proportional to the number of levels.
.IP
The triangle mapping uses a different triangle in each of the three look-up
tables (either the sets associated with the specified frames, or the output
tables).  The initial tables map low intensity to blue, middle values to
green, and high values to red, as shown in the diagram. (The red and blue
triangles are truncated as their centers are on a table boundary.)
.sp
.KS
.PS
B: box
move
G: box
move
R: box
move to B.sw left 0.375
line dotted to B.nw
line dashed to B.s
move to G.sw
line dashed to G.n
line dashed to G.se
move to R.s
line dashed to R.ne
line dotted to R.se right 0.375
"blue" at B.s below
"green" at G.s below
"red" at R.s below
.PE
.sp
.KE
.IP
Once invoked, the program then allows the user to adjust the triangle
mapping.  In
response to the prompt line, select the color to be changed and move the
trackball:  the center of the triangle is given by the \fIx\fR cursor
coordinate and the width by the \fIy\fR coordinate.  Narrow functions
(small \fIy\fR) allow one to map colors to a limited range of intensity.
When the mapping is satisfactory, a press of any button "fixes" the
mapping and the user may then either select another color or exit.
Before selecting a color, place the cursor at approximately the default
position for the mapping (or where it was for the last mapping of that
color under the current command); otherwise, the color map will change
suddenly when the color is selected via the trackball buttons.
.SH
\fBrange\fR N (C) (N C ...)
.IP
This command changes the range function in the specified color pipeline
so that the data is scaled by (divided by) the value \fBN\fR.  For the
IIS, useful range values are 1,2,4 and 8;  anything else will be changed
to the next lowest legal value.
.SH
\fBreset\fR [r i t a]
.IP
Various registers and tables are reset with this command.  If the \fBr\fR
option is used, the registers are reset.  This means that zoom is set to
one, all images are centered, split screen is removed, the range values are
set to one and the offset values are set to zero.  Also, the cursor is
turned on and its shape is set.  Option \fBi\fR causes all the image and
graphics planes to be erased and turned off.  Option \fBt\fR resets all
the look-up tables to their default linear, positive slope, form, and
removes any color mappings by making all the output tables the same, and
all the frame specific tables the same.  Option \fBa\fR does \fIall\fR
the above.
.SH
\fBsnap\fR (C)
.IP
This command creates an \fIIRAF\fR image file whose contents are a
512x512 digital snapshot of the image display screen.  If no color
is specified,
or if \fIcm\fR (color monochromatic) is given,
the snapshot is of the \fIblue\fR image, which, if you
have a black and white image, is the same as the red or the green
image.  Specifying \fBcg\fR for instance will take a snapshot of the
image that you would get had you specified \fIcg\fR for each frame
turned on by the \fIdi\fR command.  Color is of interest only when
the window or pseudo color commands have made the three colors distinguishable.
If the "snapped" image is intended to be fed to the Dicomed film
recorder, a black and white image is all that is usually provided and so
a color snap is probably not appropriate.
In the case of the "no color/monochromatic" snap, the graphics planes are
all added together, while, if a real color is given, only the graphics
planes that have some of that color are included in the image.
The color \fBrgb\fR can be
given, in which case the red, green, and blue images are weighted equally
to produce a single image file.  This image does not represent well what
you see, partly because of the equal weight given all colors: some
mapping of eye sensitivity is probably what is required, but it is not
implemented.
.IP
The program operates by first determining zoom, pan, offset, tables, etc,
and, for each quadrant of the split screen, which images planes are active.
Then, for each line of the display, those images are read out from the display's
memory and the transformations done in hardware are duplicated pixel by pixel
in software.  The word "active" needs a bit of explanation.  Any image plane
whose pixels are contributing to the image is active.  No image is active if
it has been turned off (by the \fIdi\fR) command (or if all images were
turned off and the one of interest not subsequently turned back on).  If the
image is all zeroes, or if it is not but split screen is active and the
part of the image being displayed is all zeroes, it is not contributing to
the output.  However, the snap program cannot tell that an active image is
not contributing anything useful,
and so it dutifully reads out each pixel and adds zeroes to the output.
The moral of this is that frames of no interest should be (turned) off before
snap is called (unless you don't have anything better to do than wait for
computer prompts).  When split screen is active, frames are read only for
the quadrants in which they are active.
.IP
The fastest snaps are for single images that are zoomed but not panned
and which are displayed (and snapped) in black and white, or snapped
in a single color.
.SH
\fBsplit\fR [c o px,y nx,y]
.IP
This command sets the split screen point.  Option \fBc\fR is shorthand for
\fIcenter\fR, which is the normal selection.  Option \fBo\fR stands for
\fIorigin\fR, and is the split position that corresponds to no split screen.
If you wish to specify the split point in pixels, use the \fBpx,y\fR form, in
which the coordinates are given as integers.  If you prefer to specify
the point in NDC (which range from 0 though 1.0), use the \fBnx,y\fR form
in which the coordinates are decimal fractions.
.IP
A peculiarity of the IIS hardware is that if no split screen is desired,
the split point must be moved to the upper left corner of the display, rather
than to the lower left (the \fIIRAF\fR 1,1 position).  This means that no
split screen (the \fBo\fR option, or what you get after \fBre r\fR) is really
split screen with only quadrant \fBfour\fR displayed:  if you use the \fIdi\fR
command with quadrant specification, only quadrant 4 data will be seen.
.SH
\fBtell\fR
.IP
This command displays what little it knows about the display status.  At
present, all it can say is whether any image plane is being displayed, and
if any are, what is the number of one of them.  This rather weak performance
is the result of various design decisions both within \fIcv\fR and the
\fIIRAF\fR display code, and may be improved.
.SH
\fBwindow\fR (o) (F C)
.IP
This command operates just as the \fIpseudo\fR command, except that it
applies a linear mapping to the output look-up tables (if option \fBo\fR
is used) or to the frame specific tables.  The mapping is controlled by
the trackball, with the \fIy\fR cursor coordinate supplying the slope
of the map, and \fIx\fR the offset.  If different mappings are given to
each color, a form of pseudo-color is generated.
.SH
\fBwrite\fR [F C] text
.IP
This command writes the given text into either an image plane (or planes)
or into the specified color graphics bit plane(s).  The user is prompted 
to place the cursor at the (lower left) corner of the text, which is
then written to the right in roman font.  The user is also asked for
a text size (default 1.0).  If the text is written into a graphics
plane, and a \fBsnap\fR is requested with no color specification, then
text in any graphics plane will be included in the image.  A color snap,
on the other hand, will include graphics text to the extent that the
text is displayed in that color.
Text written into an image plane
will have the same appearance as any "full on" pixel; that is, text
in an image plane is written at maximum intensity,
overwrites the image data,
and is affected by look-up tables, offsets,
and so forth, like any other image pixels.
.SH
\fBzoom\fR N (F)
.IP
This command zooms the display to the power given by \fBN\fR.  For the
IIS, the power must be 1,2,4, or 8; anything else is changed to the next
lower legal value.  The model 70 zooms all planes together.  The center
of the zoom is determined by the cursor position relative to the first
frame specified (if none, the lowest numbered active one).  Once the zoom
has taken place, the \fIpan\fR routine is called for the specified frames.
