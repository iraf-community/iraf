.TL
IRAF Graphics with the Diversified Computer Systems EM4010 Terminal Emulator
.AU
Steve Rooke
April 1987

.NH
Introduction
.PP
The popular vtxxx and Tektronix 4010 terminal emulator, \fBem4010\fP,
a product of Diversified Computer Systems, Inc., may be used as a graphics
terminal within IRAF.  In its current form, the emulator has certain features
that make its use within IRAF a bit awkward, though it is fully functional.
The user must first make sure the emulator's Setup Mode features are set
properly, then must inform the IRAF graphics system that it is talking to
an \fBem4010\fP terminal.

.PP
Since it is intended for a class of desktop personal computer that does
not have separate hardware graphics and text memories, the \fBem4010\fP
\fIemulates\fP individual graphics and text planes.
In general you will want to enter IRAF from the emulator's \fBvt220\fP
(7-bit) mode, although the \fBvt102\fP and \fBvt100\fP modes probably work
equally well.
IRAF needs to know what kind of terminal you are using, requiring that
\fBem4010\fP entries exist in both the \fLdev$termcap\fP and \fLdev$graphcap\fP
files, and that you notify the system about the terminal with the \fLstty\fP
task.
Once inside IRAF, the system will take care of switching the emulator between
text and graphics modes.

.NH
The Termcap and Graphcap Entries
.PP
IRAF stores information about text terminals in the file \fItermcap\fP in
the \fIdev\fP directory, and information about graphics terminals
in the file \fIgraphcap\fP.
You can determine whether your system currently supports the \fBem4010\fP
by searching those files for `em4010':
.nf
.sp 1
.in 5
\fLcl> match em4010 dev$termcap
cl> match em4010 dev$graphcap\fP
.in 0
.sp 1
.fi
If there is no match, the IRAF system manager will need to install the
entries, which may be obtained from the IRAF group at NOAO or edited in
from the Appendix.
The \fItermcap\fP and \fIgraphcap\fP entries should be placed with the
other terminal entries and the device cache rebuilt, for the best performance.
See \fLcl> help mktty\fP and the comments at the beginning of each file
for information on caching device entries.
If it is not possible to rebuild the cache (e.g. if the system's source
files have been stripped) performance will be better if the entries are
inserted near the beginning of the file.
Note the comments in \fLhelp mktty\fP about modifying entries that
have already been cached.

.NH
EM4010 Setup Mode Settings
.PP
In order to use the \fBem4010\fP for graphics within IRAF,
you must first establish the correct Tektronix mode settings.
To enter \fISetup Mode\fP from vt220 mode (vt100, vt102, etc.),
first press \fBALT F1\fP, then select the Tektronix mode page
with \fBSHIFT F3\fP.  If you are already in the emulator's graphics
mode, simply press \fBALT F1\fP.
Remember to save the new settings; look for the function key
menu entry for \fIsave\fP at the bottom of the screen.
Also note that \fBALT H\fP may be used from either vtxxx or 4010
mode to obtain on-line help about using the emulator.

.NH 2
Graphics Board Settings
.PP
Make sure you have the graphics board setting corresponding to
the actual hardware in your terminal.  For the most efficient and
accurate graphics, you will need to know this setting when you set
your terminal type (see `Identifying the Terminal to IRAF' below).

.NH 2
Options
.PP
The option settings should be as follows:
.sp 1
.TS
center, box;
c s
c|c
l|a.
Tektronix Setup Settings for IRAF
_
Option	Value
_
GIN Mode Input Terminator	^M (CR)
Transparent Mode Entry	^X
Exit String	^E
Clear VT Screen on Exit	No
Auto Switch	Enable
Text Mode	Replace
.TE
The `^' above means enter the named letter as a control character.
Note that, although you enter the \fIGraphics Input (GIN)\fP terminator
as `control-M', the carriage return, it is echoed as a back arrow (\fB\(<-\fP).
You may wish to experiment with the Text Mode setting.  In \fIOverstrike\fP
mode some plots may look better or more familiar, but it may become hard to
read the status line.
.NH
Identifying the Terminal to IRAF
.PP
Once you have the emulator set up properly, you need to inform IRAF that it
is talking to the \fBem4010\fP.  The best way to do this is with the command:
.sp 1
.nf
.in 5
\fLcl> stty em4010\fP
.in 0
.fi
.sp 1
If the \fBem4010\fP is your normal, default terminal, place the
`\fLstty em4010\fP'
command into your login.cl or loginuser.cl file.
.PP
The standard termcap and graphcap entries for device \fBem4010\fP will
suffice in most cases.  However, if you have a graphics board other than
the Hercules, in particular the IBM high or medium resolution boards, or
the AT&T 6300 monochrome graphics board, you should set the device as
follows, to be sure you always get the right size text characters
while in graphics mode:
.sp 1
.TS
center, box;
c s
c|c
l|a.
EM4010 Device Names for IRAF
_
Graphics Board	IRAF Device Name
_
Hercules monochrome graphics board	em4010he
IBM high resolution board	em4010ih
IBM medium resolution board	em4010im
IBM enhanced graphics board	em4010ieh
Tecmar Master graphics board, non-interlaced	em4010tn
Tecmar Master graphics board, interlaced	em4010ti
AT&T 6300 monochrome graphics board	em4010att
.TE
For example, for the AT&T board, use:
.sp 1
.nf
.in 5
\fLcl> stty em4010att\fP
.in 0
.fi
.sp 1
(or just `\fLstty em4010att\fP' in your login.cl file).

.NH
Caveats:  Undesirable Features or Emulator Bugs
.PP
The current version of the \fBem4010\fP emulator has several features
which can be viewed as bugs or design deficiencies.  None of these features
is serious enough to limit use of the \fBem4010\fP, though they may
make certain functions somewhat awkward.

.NH 2
Losing the Graphics Buffer
.PP
When in IRAF's \fIcursor mode\fP, the emulator stays in graphics mode for most 
functions, including those utilizing the single status line at the bottom
of the screen.
However, those functions that require temporarily flipping
to text mode with the paging system, such as \fIcursor mode\fP's
`\fB?\fP', \fB:.help\fP, etc., cause the graphics buffer to be lost.
If you exit the pager with a carriage return rather than
\fBq\fP, the graphics screen will be refreshed automatically by IRAF.

.PP
Certain IRAF graphics tasks may try to write text output while you are
still in graphics mode, without using the pager employed
by the likes of \fB:.help\fP.
On some terminals this works fine, with the text output scrolling up
over the graphics, or appearing in a separate window.
However, in the \fBem4010\fP, since the text output is written into
the emulated text `plane', there is an automatic `exit-graphics', causing the
graphics buffer to be erased.
The effect of this is that the graphics screen will be cleared \fIand\fP
you will not have had time to see the text output, though you should still be in
\fIGIN\fP mode (you still have the crosshair).
To redraw the screen at this point, type \fBR\fP.

.PP
To first view the text output manually,
press \fBALT V\fP; you will now see the full contents of the text buffer.
To go back to graphics, press \fBALT G\fP followed by an \fBR\fP
and \fIfive carriage returns\fP to redraw the screen.
This is because the emulator has forgotten that you were in \fIGIN\fP mode,
while IRAF still expects it to be in that state, not knowing
anything about your temporary manual switch to vtxxx mode.

.NH 2
Overstrike vs. Replacement Text
.PP
An apparent emulator bug is that while in Tektronix text mode \fIoverstrike\fP,
the feature that allows us to erase an existing status line before
writing to it afresh is disabled.  This means that you can \fBeither\fP have
all text written to the graphics plane overlaid on the plot as with
the \fIvt640\fP and most Tektronix terminals, and never erase the
status line, \fBor\fP erase the status line normally but have text 
\fIreplace\fP whatever it writes over.
Here, replacing means there will be a blank border around each character,
so for example in a contour plot the text characters labelling the
contour levels will wipe out any contour lines immediately underneath them.
The latter will still be preferable to most users, because the single
status lines are used frequently by IRAF applications.

.NH 2
Unimplemented Tektronix Features
.PP
The emulator does not implement broken lines (dashed, dotted, etc.) at all.
Nor does it emulate different sizes of hardware text, though one can always
set text quality to `high' in \fIcursor mode\fP, and let the
IRAF graphics system generate characters in software
(see \fLhelp cursors\fP).

.NH
Future Developments
.PP
We are working with the people at Diversified Computer Systems to 
try to get them to modify the emulator, particularly to prevent the
graphics buffer from being erased upon exit from graphics mode, and to
cause the emulator to remember when graphics was exited from \fIGIN\fP mode.
We will also try to get them to implement broken lines.

.bp
.SH
Appendix -- EM4010 Termcap and Graphcap Entries
.sp 1
.SH
dev$termcap
.LP
.sp 1
.ft L
.nf
# Diversified Computer Systems vt220/tek4010 emulator (em4010) series
em4010|em4010he|em4010ih|em4010im|em4010ieh|em4010tn|em4010ti|em4010att:\\
	:gd:li#24:co#80:cl=50\\E[H\\E[2J^X:is=^]^X\\E[1;24r\\E[24;1H:tc=vt220:
.sp 1
.fi
.SH
dev$graphcap
.LP
.sp 1
.ft L
.nf
# Diversified Computer Systems vt220/tek4010 emulator (em4010) series
em4010he|em4010 emulator for Hercules monochrome graphics board:\\
	:xr#720:yr#348:xs#.19:ys#.15:ar#.77:li#38:ch#.026:tc=em4010:
em4010ih|em4010 emulator for IBM high-res board:\\
	:xr#640:yr#200:li#33:ch#.030:tc=em4010:
em4010im|em4010 emulator for IBM medium-res board:\\
	:xr#320:yr#200:li#33:co#64:ch#.030:cw#.016:tc=em4010:
em4010ieh|em4010 emulator for IBM enhanced graphics board:\\
	:xr#640:yr#350:li#38:ch#.026:tc=em4010:
em4010tn|em4010 emulator for Tecmar Master graphics board, non-interlaced:\\
	:xr#720:yr#352:li#38:ch#.026:tc=em4010:
em4010ti|em4010 emulator for Tecmar Master graphics board, interlaced:\\
	:xr#720:yr#704:li#38:ch#.026:tc=em4010:
em4010att|em4010 emulator for AT&T 6300 monochrome graphics board:\\
	:xr#640:yr#400:li#42:ch#.024:tc=em4010:
em4010|Tek 4010 emulator for IBM PC-compatibles (default Hercules):\\
	:xr#720:yr#348:xs#.19:ys#.15:ar#.77:li#38:ch#.026:lt#1:nc#1:th#1:\\
	:Y1#40:Y2#739:RC=^]\\E"4g:CW=^E\\E[24;0H\\E[K:\\
	:GD=^X\\E[24;1h\\E[24;1f\\E[0K:tc=vt640:
.sp 1
.ft P
.nf
