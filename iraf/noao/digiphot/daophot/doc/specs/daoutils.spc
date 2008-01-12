.help daoutils Jan89 "Utility Package for DAOPHOT"
.sh
1. Introduction

The DAOUTILS package will provide a set of tools for working
with the output from DAOPHOT. These tools will provide the user with
the ability to select data upon ranges and limits for any of the
fields in a daophot output table. The package will also provide tools
for merging results contained in different output files.

.sh
2. Requirements
.ls 4
.ls (1)
The tasks in the DAOUTILS package shall take as input the output ST Tables
from the DAOPHOT tasks. The convert task in the daophot package shall be
used to convert the output from apphot tasks into the proper format for use
with the daoutils package.
.le
.ls (2)
The tasks in the package which produce tabular output shall use the
ST Tables for their output and those tasks which read output from other
DAOUTILS tasks will be able to read ST Tables. 
.le
.ls (3)
The DAOUTILS  package shall include tasks to inspect and edit the results 
from the photometry routines. These shall include tasks such as interactively
rejecting particular stars from the results, 
producing plots of errors versus brightness, errors versus position etc. There
will also be a task for merging the results contained in several different
Tables. It shall also be possible to interactively examine the photometry
results with various graphical and/or display tools for inspecting/editing the
results. It shall also be possible to construct growth curves from the
aperture photometry for the purpose of calibrating  the daophot magnitudes
to total magnitudes. There shall also be routines for calibrating the
photometry by the use of standard stars.
.le
.ls (4) 
The tasks shall be able to be run in batch mode as well as interative
mode. In batch mode use of a graphics terminal or image display shall not
be required.
.le
.ls (5)
The DAOPHOT package shall be written in the SPP language in conformance with
the standards and conventions of IRAF. The code shall be portable and
device independent.
.le
.le
.sh
2.1 Limitations of the Initial DAOUTILS Package

The DAOUTILS package will have the following limitations:
.ls
.ls (1)
The initial version of DAOUTILS will not make direct use of the interactive
image display. It will make use of cursor readback however.
.le
.le

.sh
3. Specifications

The DAOUTILS package will take the output from the DAOPOHT package as input
and provide a variety of tools which will allow the use to examine, edit and
calibrate the output from DAOPHOT. The output from DAOUTILS will consist of
ST Tables, graphical displays and printed summaries.

The CL callable part of the DAOUTILS package will consist of the following 
tasks:

.ks
.nf
	merge     -- Merge the results from different runs of daophot
	daograph  -- Graph the results of daophot stored in ST Tables
	gtedit     -- Interactive graphical data editor
	examine   -- Interactively examine the output from daophot
	growth    -- aperture growth curves <--> PSF magnitudes
	cmd       -- Color-magnitude and color-color plots
	calibrate -- do photometric calibration

.fi
.ke

In addition the ttools package provided as part of STSDAS provides for
many generic tools for working with the ST Tables are will be usable for
many of the data selection tasks which most users will wish to apply to 
daophot output.

.sh
3.1 The GTEDIT Task

The user will be able to plot any two columns of a Table versus each other
and with the cursor interactively delete records. The user will move the
graphics cursor near the point on the graph which represents the record
which he wishes to delete from the input record. The user will also be able
to specify 'areas' of the plot for which all records pointed to by points
in the indicated sections will be deleted. The user will also be
able to undelete records. The records will not actually be deleted until the
task ends. The user will also be able to interactively change which columns 
are being plotted versus each other. The user will also have the option of
editing the table in place or to create a new output table which will contain
the edited results of the input table.

.sh
3.1.1 GEDIT Parameters

GEDIT will have input parameters which control the input and initial
operation of the task.

.ks
.nf
Positional or query mode parameters:

	input	- filename
	xcolumn - column name (string)
	ycolumn - column name (string)
.fi
.ke

.ks
.nf
Hidden parameters:

	inplace 	boolean		"false"
	output  	filename	""
	reject		filename	""
.fi
.ke

The function and format of these parameters is decsribed in more detail
below.

.ls
.ls 16 input
The name of the input table which contains the output from DAOPHOT.
.le
.ls xcolumn
The name of the column in the input table which will be used for the
X axis of the plot
.le
.ls ycolumn
The name of the column in the input table which will be used for the
Y axis of the plot
.le
.ls inplace
Controls whether the input table is modified in place or whether a new 
table is created.
.le
.ls output
If inplace is false then the value of this parameter will be the name of
the output table which will contain the edited output.
.le
.ls reject
The name of the output file containing those objects which have been
deleted. If this parameter is NULL then the records which have been
deleted are not saved.
.le
.le

.sh
3.1.2 Interactive GEDIT Commands

Once GEDIT has plotted the two columns specified as the input there are
several commands available to the user. These allow the user to delete/undelete
points, change which columns are plotted, view a particular record from the
input table and exit GEDIT.

.ks
.nf
In the interactive mode the following cursor keys are active:

	x  -- delete the record represented by the point nearest the cursor
	>  -- delete all records represented by Y values > than the cursor
	<  -- delete all records represented by Y values < than the cursor
	+  -- delete all records represented by X values > than the cursor
	-  -- delete all records represented by X values < than the cursor
	b  -- mark the corner of box containing records to be deleted.
	u  -- undelete the record(s) deleted by the last delete operation
	q  -- exit GEDIT
.fi
.ke

.ks
.nf
In addition the following colon commands are available:

	:xcol <name>  Use the column <name> as the X axis
	:ycol <name>  Use the column <name> as the Y axis
.fi
.ke

.sh
3.1.3 GEDIT OUTPUT

The output from GEDIT is a direct copy of the input table with the records
which the user marked for deletion removed. If the parameter 'inplace' was
set to true then the edited table replaces the original table, otherwise a
new table is created. Is it important that no records be deleted until the 
user exits GEDIT and confirms that the update is to take place.

.sh
3.2 TGRAPH

This task is will produce plots from the DAOPHOT output tables. It is similar
to the sgraph task in STSDAS but does not have the option of plotting image 
sections. It does have the ability to plot error bars and columns of data from
two different tables.

The following is the help for sgraph:

.ih
NAME
graph -- graph one or more lists, image sections, or tables
.ih
USAGE
graph input
.ih
PARAMETERS
.ls input
List of operands to be graphed.  May be STDIN, or one or more image
sections, tables and columns, or lists.  SDAS table input is specified
by:  a table name and column name, a table and two column names, or a
pair of table and column names, separated by white space. 
.le
.ls stack = no
If stack = yes, plot multiple curves on separate axes (WCS) stacked 
vertically rather than on the same axes.
.le
.ls wx1=0., wx2=0., wy1=0., wy2=0.
The range of user coordinates spanned by the plot.  If the range of values
in x or y = 0, the plot is automatically scaled from the minimum to
maximum data value along the degenerate dimension.
.le
.ls vx1=0., vx2=0., vy1=0., vy2=0.
NDC coordinates (0-1) of the device plotting viewport.  If not set by 
the user, a suitable viewport which allows sufficient room for all labels 
is used.
.le
.ls pointmode = no
If \fBpointmode\fR = yes, plot points or markers at data values, rather than 
connected lines.
.le
.ls marker = "box"
Marker to be drawn if \fBpointmode\fR = yes.  Markers are "point", "box", 
"cross", "plus" or "circle".
.le
.ls szmarker = 0.005
The size of a marker in NDC coordinates (0 to 1 spans the screen).
If zero and the input operand is a list, marker sizes are taken individually
from the third column of each list element.  If positive, all markers are
of size \fBszmarker\fR.  If negative and the input operand is a list,
the size of a marker is the third column of each list element times the
absolute value of \fBszmarker\fR.
.le
.ls xlabel = "", ylabel = ""
Label for the X-axis or Y-axis.
.le
.ls title = "imtitle"
Plot title.  If \fBtitle\fR  = "imtitle"
and the first operand in \fBinput\fR is an image, the image title is used
as the plot title.
.le
.ls box = yes
Draw axes at the perimeter of the plotting window.
.le
.ls fill = yes
Fill the output viewport regardless of the device aspect ratio?
.le
.ls axis = 1
Axis along which the projection is to be computed, if an input operand is
an image section of dimension 2 or higher.  Axis 1 is X (line average),
2 is Y (column average), and so on.
.le
.ls erraxis = 0
If pointmode = no and erraxis is 1 or 2, the axis parallel to which
error bars are plotted (1 ==> X, 2 ==> Y). 
.le
.ls errcolumn = ""
The column(s) in the input table to be used as the error amplitude(s).
If one column name, then symmetrical symmetrical error bars are drawn,
centered on the data points with the total size, in WC specified by the
values in the column from the same table specified in parameter input.
Two names specify two table columns for the lower and upper errors,
respectively. 
.le
.ls pattern = "solid"
The line pattern for the first curve.  Subsequent curves on the same 
plot cycle through the set of patterns:  solid, dashed, dotted, dotdash.
.le
.ls crvstyle = "straight"
The style of the plotted curve:  "straight" is the usual straight 
line connection between points, "pseudohist" consists of horizontal 
segments at each data point connected by vertical segments, "fullhist" 
is a bar graph or histogram style plot.
.le
.ls transpose = no
Swap the X and Y axes of the plot.  If enabled, the axes are transposed 
after the optional linear transformation of the X-axis.
.le
.ls xflip = no, yflip = no
Flip the axis?  That is, plot and label X values increasing right to
left instead of left to right and/or Y values top to bottom instead of
bottom to top. 
.le
.ls logx = no, logy = no
Log scale the X or Y axis.  Zero or negative values are indefinite and
will not be plotted, but are tolerated.
.le
.ls ticklabels = yes
Label the tick marks.
.le
.ls majrx=5, minrx=5, majry=5, minry=5
Number of major tick marks on each axis; number of minor tick marks between
major tick marks.  Ignored if log scaling is in effect for an axis.
.le
.ls lintran = no
Perform a linear transformation of the X-axis upon input.  Used to assign
logical coordinates to the indices of pixel data arrays (image sections).
.le
.ls p1=0, p2=0, q1=0, q2=1
If \fBlintran\fR is enabled, pixel index P1 is mapped to Q1, and P2 to Q2.
If P1 and P2 are zero, P1 is set to 1 and P2 to the number of pixels in
the input array.
.le
.ls round = no
Extend the axes up to "nice" values.
.le
.ls append = no
Append to an existing plot.  
.le
.ls device = "stdgraph"
The output device.
.le
.ih
DESCRIPTION
\fBGraph\fR graphs one or more lists, image sections, or table columns;
lists and image sections may be mixed in the input list at will.  If the
curves are not all the same length the plot will be scaled to the
longest curve and all curves will be plotted left justified.  If an
image section operand has more than one dimension the projection
(average) along a designated axis will be computed and plotted.  By
default, a unique dash pattern is used for each curve, up to a maximum
of 4. 

List input may be taken from the standard input or from a file,
and consists of a sequence of Y values, X and Y values, or X, Y,
and marker size values, one pair of coordinates per line in the list.
Blank lines, comment lines, and extra columns are ignored.
The first element in the list determines whether the list is a Y list
or and X,Y list; it is an error if an X,Y list has fewer than two
coordinates in any element.  INDEF valued elements appear as gaps
in the plot.

SDAS table input is specified by a table name and column name, a table 
and two columns, or a pair of table and column names separated by white 
space.  The table name may be a file name template.  Note that this is a 
single string, so that it must be quoted if entered on the command line.

Error bars may be plotted for data from list or table input.  Errors may
be in X or in Y and separate upper and lower errors may be specified. 
If `pointmode' is "no" then the parameter `erraxis' specifies if the
errors are in X or in Y if its value is 1 or 2, respectively.  If
`pointmode' is "no" and `erraxis' is zero, a polyline is plotted (see 
below).  If the input data come from a list, then the third (size)
column specifies the amplitude of symmetrical error bars.  If there is a
fourth column, the third and fourth columns specify the amplitudes of
the lower and upper errors, respectively.  If the input data are in a
table, the parameter `errcol' specifies the source of the errors. 
If `errcol' contains a single word, it is the column in the same table
as the input data containing the amplitudes of symmetrical errors.  If
`errcol' contains two words, they specify the columns in the same table
containing the amplitudes of the lower and upper errors, respectively.
If the X and Y data come from different tables, then `erraxis' specifies
which table contains the error column or columns.  Error data may not
come from image data.  The `append' parameter may be used
to overplot several curves of different style. 

Different line types and curve styles may be selected.  The string
parameter `crvstyle' may take on one of the values:  "none", "straight",
"pseudohist", or "fullhist", specifying the style of connections between
data points;  the string parameter `pattern' can take on one of the
values "solid", "dashed", "dotted", "dotdash" to indicate the style
of the first line drawn.  Subsequent lines drawn on the same graph cycle
the available styles. 

If \fBappend\fR is enabled, previous values for \fBbox\fR,
\fBfill\fR, \fBround\fR, the plotting viewport (\fBvx1\fR, \fBvx2\fR, 
\fBvy1\fR, \fBvy2\fR), and the plotting window (\fBwx1\fR, \fBwx2\fR, 
\fBwy1\fR, \fBwy2\fR) are used.

By default, the plot drawn will fill the device viewport, if the viewport
was either specified by the user or automatically calculated by 
\fIgraph\fR.  Setting
the value of \fBfill\fR  to "no" means the viewport will be adjusted so 
that equal numbers of data values in x and y will occupy equal lengths 
when plotted.  That is, when \fBfill = no\fR, a unity aspect ratio is 
enforced, and plots
appear square regardless of the device aspect ratio.  On devices with non 
square full device viewports (e.g., the vt640), a plot drawn by \fIgraph\fR
appears extended in the x direction unless \fBfill\fR = no.

.ih
EXAMPLES
Plot the output of a list processing filter:

	cl> ... list_filter | graph

Plot a graph entered interactively from the terminal:

	cl> graph STDIN

Overplot two lists:

	cl> graph list1,list2

Graph line 128 of image "pix":

	cl> graph pix[*,128]

Graph the average of columns 50 through 100:

	cl> graph pix[50:100,*] axis=2

Graph two columns from a table against each other:

	cl> graph "table xcol ycol"

Graph a list in point plot mode:

	cl> graph list po+

Annotate a graph:

.nf
	cl> graph pix[*,10],pix[*,20] xlabel=column\
	>>> ylabel=intensity title="lines 10 and 20 of pix"
.fi

Direct the graph to the standard plotter device:

	cl> graph list device=stdplot
.ih
BUGS
Indefinites are not recognized when computing image projections.

End sgraph help.

.sh
3.3 The MERGE Task

This task will merge the results from a maximum of four DAOPHOT output
files based upon positional coincidence. This task will work on files
which could have been run images with different scales, orientations etc.
MERGE will need to transform the coordinate systems of the input photometry
lists to a common coordinate system. There will be two ways of inputing the
transformations to be applied to the coordinate lists: 1) Simple X, Y shifts
or 2) transformations as determined by the GEOMAP task. This will allow the
most common cases to handled in a simple way without having to run the GEOMAP
task.

The user will be able to specify a match radius which will determine how
close to each other two objects must be to be considered a coincidence.
The user will also be able to specify the number of lists an object must
be identified in to be included in the output file. For example, if the
user was merging photometry results from four different output tables
he could request that the output table would contain entries for matches
between any two or more of the four tables. The output Table from MERGE
will simply contain pointers to the corresponding rows of the various
input tables and not contain a copy of the complete row from each table
for those objects which were matched. The user will then run another task,
MSELECT to select the fields he wants from the original photometry files.
This will allow the user to select only the fields he wants and to do the
selection more than once without remerging the individual files.

.sh
3.3.1 MERGE Parameters

MERGE shall have parameters which control the input and functioning of the
task.

.ks
.nf
Positional or query parameters:

	ptable1  -- Input photometry table #1
	ptable2  -- Input photometry table #2
	ptable3  -- Input photometry table #3
	ptable4  -- Input photometry table #4
	matchrad -- Matching radius for coincidence
	nmatch   -- minimum number of matches to be included in output
	merge_table -- output table containing merge pointers
.fi
.ke

.ks
.nf
Hidden Parameters:

	gmfile12  -- geomap database name for transforming #1 -> #2
	gmrec12	  -- geomap database record for transforming #1 -> #2
	gmfile13  -- geomap database name for transforming #1 -> #3
	gmrec13	  -- geomap database record for transforming #1 -> #3
	gmfile14  -- geomap database name for transforming #1 -> #4
	gmrec14	  -- geomap database record for transforming #1 -> #4
	dx12  -- X offset (x2 - x1)
	dy12  -- Y offset (y2 - y1)
	dx13  -- X offset (x3 - x1)
	dy13  -- Y offset (y3 - y1)
	dx14  -- X offset (x4 - x1)
	dy14  -- Y offset (y4 - y1)
.fi
.ke

These parameters perform the following functions:

.ls 4
.ls 16 ptable1
The name of the first input photometry table. This should be a standard
DAOPHOT output table or one whose column names have been modified to agree 
with the DAOPHOT standard.
.le
.ls ptable2
The name of the second input photometry table.
.le
.ls ptable3
The name of the third input photometry table.
.le
.ls ptable4
The name of the fourth input photometry table.
.le
.ls matchrad
The matching radius for determining whether two objects should be identified
with the same object and potentially included in the output table.
.le
.ls nmatch
The minumum number of matches among the input photometry files for an object 
to be included in the output Table. If there were four input files and
\fInmatch\fR was set to two then an object would be included in the output
if it was matched in files 1/2, 1/3, 1/4, 2/3, 2/4 or 3/4. If \fInmatch\fR
was three then the object would have to be identified in files 1/2/3, 
2/3/4, 1/2/4 or 1/3/4
.le
.ls gmfile12
The name of the \fIGEOMAP\fR database containing the record which specifies 
the transformation between photometry list 2 and photometry list 2 in the 
sense of mapping objects in list #2 into the coordinate frame of the
list #1
.le
.ls gmrec12
The name of \fIGEOMAP\fR database record within \fIgmfile12\fR which
describes the transformation from coordinate system #2 to system #1
.le
.ls gmfile13
The name of the \fIGEOMAP\fR database containing the record which specifies 
the transformation between photometry list 3 and photometry list 3 in the 
sense of mapping objects in list #3 into the coordinate frame of the
list #1
.le
.ls gmrec13
The name of \fIGEOMAP\fR database record within \fIgmfile13\fR which
describes the transformation from coordinate system #3 to system #1
.le
.ls gmfile14
The name of the \fIGEOMAP\fR database containing the record which specifies 
the transformation between photometry list 4 and photometry list 4 in the 
sense of mapping objects in list #4 into the coordinate frame of the
list #1
.le
.ls gmrec14
The name of \fIGEOMAP\fR database record within \fIgmfile14\fR which
describes the transformation from coordinate system #4 to system #1
.le
.ls dx12
The X offset between coordinate system #2 and coordinate system #1
in the sense of (x2 - x1).
.le
.ls dy12
The Y offset between coordinate system #2 and coordinate system #1
in the sense of (y2 - y1).
.le
.ls dx13
The X offset between coordinate system #3 and coordinate system #1
in the sense of (x3 - x1).
.le
.ls dy13
The Y offset between coordinate system #3 and coordinate system #1
in the sense of (y3 - y1).
.le
.ls dx14
The X offset between coordinate system #4 and coordinate system #1
in the sense of (x4 - x1).
.le
.ls dy14
The Y offset between coordinate system #4 and coordinate system #1
in the sense of (y4 - y1).
.le
.le

If the parameters specifying the \fIGEOMAP\fR database file name for
a particular transformation is empty then the corresponding parameters
describing the transformation in terms of a simple shift are queried.

.sh
3.3.2
The Output from MERGE

\fIMERGE\fR will produce an STSDAS Table as output with the table
containing pointers to records in the input Tables which identify the
objects which are positionally coincident. The output Table will contain
columns with the names \fIpoint0\fR, \fIpoint1\fR, \fIpoint2\fR and
\fIpoint3\fR. If an object is identified in a particular input photometry
list and also meets the \fInmatch\fR criterion then the entry in the 
output Table will contain the row number in the corrsponding input Table.
For those objects which are included in the output Table but which are not
identified in a particular input Table the corrsponding entry will be
INDEF.

.ks
.nf
Sample Output Format (ASCII representation)

 Point0		Point1		Point2		Point3

 1		3		INDEF		2
 3		2		2		4
 4		5		4		INDEF
 INDEF		8		7		6

 .fi
 .ke

 In the above sample the first matched object corrsponds to rows 1, 3 and
 2 in input Tables 0, 1 and 3 respectively. This object was not matched
 in input Table 0. 

The header of the Table output by MERGE shall contain the necessary information
for tracking the original data files, the data and time the merge was done etc.
The MSELECT task should check that the original photometry files have not been
modified since MERGE was run to ensure data integrity.

.sh
3.4 MSELECT

This task is used to select fields from the photometry Tables which
were used as input to the \fIMERGE\fR task. The output from \fIMERGE\fR
is a set of pointers to the appropriate records in the input tables.
Using \fIMSELECT\fR on an output file from \fIMERGE\fR will produce an
output file which will contain the specified fields for each of the
objects which have been matched. If an object has not been identified
in a particular Table then the values for each output field are set to
INDEF.

.sh
3.4.1 MSELECT Parameters

MERGE will have parameters wich will control the input and functioning
of the task.

.ks
.nf
Positional Parameters:

	input		filename	""
	fields		string		""
	output		string		""
.fi
.ke

.ks
.nf
Hidden Parameters:

	tables		string		""
.fi
.ke

These parameters perform the following functions:

.ls 4
.ls 16 input
This specifies the input Table which must be an output Table from the
\fIMERGE\fR task. \fIMSELECT\fR will get the names of the actual
photometry Tables from the header of the input merge table.
.le
.ls fields
A list of fields to extract from the input photometry tables. 
.le
.ls output
The name of the output table. This table will contain entries for
each of the selected fields of each photometry table. The values for
each field in the output table will be the values of the selected fields
for each of the input photometry tables. For those entries which were not 
nmatched the corresponding entries will be INDEF.
.le
.ls tables
This parameter is used to select output for only a subset of the input
photometry tables. 
.le

.endhelp
