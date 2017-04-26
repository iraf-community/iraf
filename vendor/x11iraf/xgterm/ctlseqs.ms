.\"#! troff -ms $1		-*- Nroff -*-
.\" "Xterm Control Sequences" document
.\" $XConsortium: ctlseqs.ms,v 1.8 91/07/21 15:30:31 gildea Exp $
.\" 
.\" Copyright 1991 by the Massachusetts Institute of Technology
.\"
.\" Permission to use, copy, modify, and distribute this documentation
.\" for any purpose and without fee is hereby granted, provided 
.\" that the above copyright notice appears in all copies,
.\" and that the name of M.I.T. not be used in advertising
.\" or publicity pertaining to distribution of the software without specific, 
.\" written prior permission.  M.I.T. makes no representations about the 
.\" suitability of the software described here for any purpose, nor
.\" about the accuracy of this documentation.  It is provided "as is"
.\" without express or implied warranty.
.\"
.\" The X Window System is a Trademark of MIT.
.\"
.\" Originally written by Edward Moy, University of California,
.\" Berkeley, edmoy@violet.berkeley.edu, for the X.V10R4 xterm.
.\" The X Consortium staff has since updated it for X11.
.\"
.\" Run this file through troff and use the -ms macro package.
.ND
.de St
.sp
.nr PD 0
.nr PI 1.5i
.nr VS 16
..
.de Ed
.nr PD .3v
.nr VS 12
..
.rm CH
.ds LH Xterm Control Sequences
.nr s 6*\n(PS/10
.ds L \s\nsBEL\s0
.ds E \s\nsESC\s0
.ds T \s\nsTAB\s0
.ds X \s\nsETX\s0
.ds N \s\nsENQ\s0
.ds e \s\nsETB\s0
.ds C \s\nsCAN\s0
.ds S \s\nsSUB\s0
.nr [W \w'\*L'u
.nr w \w'\*E'u
.if \nw>\n([W .nr [W \nw
.nr w \w'\*T'u
.if \nw>\n([W .nr [W \nw
.nr w \w'\*X'u
.if \nw>\n([W .nr [W \nw
.nr w \w'\*N'u
.if \nw>\n([W .nr [W \nw
.nr w \w'\*e'u
.if \nw>\n([W .nr [W \nw
.nr w \w'\*C'u
.if \nw>\n([W .nr [W \nw
.nr w \w'\*S'u
.if \nw>\n([W .nr [W \nw
.nr [W +\w'\|\|'u
.de []
.nr w \w'\\$2'
.nr H \\n([Wu-\\nwu
.nr h \\nHu/2u
.ds \\$1 \(br\v'-1p'\(br\v'1p'\h'\\nhu'\\$2\h'\\nHu-\\nhu'\(br\l'-\\n([Wu\(ul'\v'-1p'\(br\l'-\\n([Wu\(rn'\v'1p'\|
..
.[] Et \v'-1p'\*X\v'1p'
.[] En \v'-1p'\*N\v'1p'
.[] Be \v'-1p'\*L\v'1p'
.[] Bs \v'-1p'\s\nsBS\s0\v'1p'
.[] Ta \v'-1p'\*T\v'1p'
.[] Lf \v'-1p'\s\nsLF\s0\v'1p'
.[] Vt \v'-1p'\s\nsVT\s0\v'1p'
.[] Ff \v'-1p'\s\nsFF\s0\v'1p'
.[] Cr \v'-1p'\s\nsCR\s0\v'1p'
.[] So \v'-1p'\s\nsSO\s0\v'1p'
.[] Si \v'-1p'\s\nsSI\s0\v'1p'
.[] Eb \v'-1p'\*e\v'1p'
.[] Ca \v'-1p'\*C\v'1p'
.[] Su \v'-1p'\*S\v'1p'
.[] Es \v'-1p'\*E\v'1p'
.[] Fs \v'-1p'\s\nsFS\s0\v'1p'
.[] Gs \v'-1p'\s\nsGS\s0\v'1p'
.[] Rs \v'-1p'\s\nsRS\s0\v'1p'
.[] Us \v'-1p'\s\nsUS\s0\v'1p'
.[] ! !
.[] # #
.[] (( (
.[] ) )
.[] * *
.[] + +
.[] 0 0
.[] 1 1
.[] 2 2
.[] 3 3
.[] 4 4
.[] 5 5
.[] 6 6
.[] 7 7
.[] 8 8
.[] 9 9
.[] : :
.[] ; ;
.[] = =
.[] > >
.[] ? ?
.[] @ @
.[] A A
.[] cB B
.[] C C
.[] D D
.[] E E
.[] F F
.[] H H
.[] J J
.[] K K
.[] L L
.[] M M
.[] N N
.[] O O
.[] P P
.[] R R
.[] S S
.[] T T
.[] Z Z
.[] [[ [
.[] bS \\e
.[] ] ]
.[] ^ ^
.[] _ _
.[] ` \`
.[] a a
.[] b b
.[] c c
.[] d d
.[] f f
.[] g g
.[] h h
.[] i i
.[] j j
.[] k k
.[] l l
.[] m m
.[] n n
.[] o o
.[] p p
.[] q q
.[] r r
.[] s s
.[] t t
.[] x x
.[] | |
.[] } }
.[] c~ ~
.[] Sc \fIc\fP
.ds Cc \fIC\fP
.ds Cb \fIC\v'.3m'\h'-.2m'\s-2b\s0\v'-.3m'\fP
.ds Cx \fIC\v'.3m'\h'-.2m'\s-2x\s0\v'-.3m'\fP
.ds Cy \fIC\v'.3m'\h'-.2m'\s-2y\s0\v'-.3m'\fP
.ds Ps \fIP\v'.3m'\h'-.2m'\s-2s\s0\v'-.3m'\fP
.ds Pm \fIP\v'.3m'\h'-.2m'\s-2m\s0\v'-.3m'\fP
.ds Pt \fIP\v'.3m'\h'-.2m'\s-2t\s0\v'-.3m'\fP
.ds Ix \fIx\fP
.ds Iy \fIy\fP
.ds Iw \fIw\fP
.ds Ih \fIh\fP
.ds Ir \fIr\fP
.ds Ic \fIc\fP
.nr LL 6.5i
.TL
Xterm Control Sequences
.AU
Edward Moy
.AI
University of California, Berkeley
.sp
Revised by
.AU
Stephen Gildea
.AI
MIT X Consortium

.am NP
.ds CF %
..
.SH
Definitions
.IP \*(Sc
The literal character \fIc\fP.
.IP \*(Cc
A single (required) character.
.IP \*(Ps
A single (usually optional) numeric parameter, composed of one of more digits.
.IP \*(Pm
A multiple numeric parameter composed of any number of single numeric
parameters, separated by \*; character(s).
.IP \*(Pt
A text parameter composed of printable characters.
.SH
VT100 Mode
.ds RH VT100 Mode
.LP
Most of these control sequences are standard VT102 control sequences,
but there are some sequences here from later DEC VT terminals, too.
Major VT102 features not supported are smooth scrolling, double size
characters, blinking characters, and VT52 mode.
There are additional control sequences to provide
\fIxterm-\fPdependent functions, like the scrollbar or window size.
Where the function is specified by DEC or ISO 6429, the code assigned
to it is given in parentheses.  The escape codes to designate
character sets are specified by ISO 2022; see that document for a
discussion of character sets.
.St
.IP \\*(Be
Bell (Ctrl-G)
.IP \\*(Bs
Backspace (Ctrl-H)
.IP \\*(Ta
Horizontal Tab (HT) (Ctrl-I)
.IP \\*(Lf
Line Feed or New Line (NL) (Ctrl-J)
.IP \\*(Vt
Vertical Tab (Ctrl-K) same as LF
.IP \\*(Ff
Form Feed or New Page (NP) (Ctrl-L) same as LF
.IP \\*(Cr
Carriage Return (Ctrl-M)
.IP \\*(So
Shift Out (Ctrl-N) \(-> Switch to Alternate Character Set: invokes the
G1 character set.
.IP \\*(Si
Shift In (Ctrl-O) \(-> Switch to Standard Character Set: invokes the
G0 character set (the default).
.IP \\*(Es\\*#\\*8
DEC Screen Alignment Test (DECALN)
.IP \\*(Es\\*(((\\*(Cc
Designate G0 Character Set (ISO 2022)
  \*(Cc = \*0 \(-> DEC Special Character and Line Drawing Set
  \*(Cc = \*A \(-> United Kingdom (UK)
  \*(Cc = \*(cB \(-> United States (USASCII)
.IP \\*(Es\\*)\\*(Cc
Designate G1 Character Set (ISO 2022)
  \*(Cc = \*0 \(-> DEC Special Character and Line Drawing Set
  \*(Cc = \*A \(-> United Kingdom (UK)
  \*(Cc = \*(cB \(-> United States (USASCII)
.IP \\*(Es\\**\\*(Cc
Designate G2 Character Set (ISO 2022)
  \*(Cc = \*0 \(-> DEC Special Character and Line Drawing Set
  \*(Cc = \*A \(-> United Kingdom (UK)
  \*(Cc = \*(cB \(-> United States (USASCII)
.IP \\*(Es\\*+\\*(Cc
Designate G3 Character Set (ISO 2022)
  \*(Cc = \*0 \(-> DEC Special Character and Line Drawing Set
  \*(Cc = \*A \(-> United Kingdom (UK)
  \*(Cc = \*(cB \(-> United States (USASCII)
.IP \\*(Es\\*7
Save Cursor (DECSC)
.IP \\*(Es\\*8
Restore Cursor (DECRC)
.IP \\*(Es\\*=
Application Keypad (DECPAM)
.IP \\*(Es\\*>
Normal Keypad (DECPNM)
.IP \\*(Es\\*D
Index (IND)
.IP \\*(Es\\*E
Next Line (NEL)
.IP \\*(Es\\*H
Tab Set (HTS)
.IP \\*(Es\\*M
Reverse Index (RI)
.IP \\*(Es\\*N
Single Shift Select of G2 Character Set (SS2): affects next character only
.IP \\*(Es\\*O
Single Shift Select of G3 Character Set (SS3): affects next character only
.IP \\*(Es\\*P\\*(Pt\|\\*(Es\\*(bS
Device Control String (DCS)
  \fIxterm\fP implements no DCS functions; \*(Pt is ignored.
\*(Pt need not be printable characters.
.IP \\*(Es\\*Z
Return Terminal ID (DECID).  Obsolete form of \*(Es\*([[\*c (DA).
.IP \\*(Es\\*([[\\*(Ps\|\\*@
Insert \*(Ps (Blank) Character(s) (default = 1) (ICH)
.IP \\*(Es\\*([[\\*(Ps\|\\*A
Cursor Up \*(Ps Times (default = 1) (CUU)
.IP \\*(Es\\*([[\\*(Ps\|\\*(cB
Cursor Down \*(Ps Times (default = 1) (CUD)
.IP \\*(Es\\*([[\\*(Ps\|\\*C
Cursor Forward \*(Ps Times (default = 1) (CUF)
.IP \\*(Es\\*([[\\*(Ps\|\\*D
Cursor Backward \*(Ps Times (default = 1) (CUB)
.IP \\*(Es\\*([[\\*(Ps\|\\*;\\*(Ps\|\\*H
Cursor Position [row;column] (default = [1,1]) (CUP)
.IP \\*(Es\\*([[\\*(Ps\|\\*J
Erase in Display (ED)
  \*(Ps = \*0 \(-> Clear Below (default)
  \*(Ps = \*1 \(-> Clear Above
  \*(Ps = \*2 \(-> Clear All
.IP \\*(Es\\*([[\\*(Ps\|\\*K
Erase in Line (EL)
  \*(Ps = \*0 \(-> Clear to Right (default)
  \*(Ps = \*1 \(-> Clear to Left
  \*(Ps = \*2 \(-> Clear All
.IP \\*(Es\\*([[\\*(Ps\|\\*L
Insert \*(Ps Line(s) (default = 1) (IL)
.IP \\*(Es\\*([[\\*(Ps\|\\*M
Delete \*(Ps Line(s) (default = 1) (DL)
.IP \\*(Es\\*([[\\*(Ps\|\\*P
Delete \*(Ps Character(s) (default = 1) (DCH)
.IP \\*(Es\\*([[\\*(Ps\|\\*;\\*(Ps\|\\*;\\*(Ps\|\\*;\\*(Ps\|\\*;\\*(Ps\|\\*T
Initiate hilite mouse tracking.
Parameters are [func;startx;starty;firstrow;lastrow].
See the section \fBMouse Tracking\fP.
.IP \\*(Es\\*([[\\*(Ps\|\\*c
Send Device Attributes (DA)
  \*(Ps = \*0 or omitted \(-> request attributes from terminal
  \(-> \*(Es\*([[\*?\*1\*;\*2\*c (``I am a VT100 with Advanced
Video Option.'')
.IP \\*(Es\\*([[\\*(Ps\|\\*;\\*(Ps\|\\*f
Horizontal and Vertical Position [row;column] (default = [1,1]) (HVP)
.IP \\*(Es\\*([[\\*(Ps\|\\*g
Tab Clear (TBC)
  \*(Ps = \*0 \(-> Clear Current Column (default)
  \*(Ps = \*3 \(-> Clear All
.IP \\*(Es\\*([[\\*(Pm\|\\*h
Set Mode (SM)
  \*(Ps = \*4 \(-> Insert Mode (IRM)
  \*(Ps = \*2\*0 \(-> Automatic Newline (LNM)
.IP \\*(Es\\*([[\\*(Pm\|\\*l
Reset Mode (RM)
  \*(Ps = \*4 \(-> Replace Mode (IRM)
  \*(Ps = \*2\*0 \(-> Normal Linefeed (LNM)
.IP \\*(Es\\*([[\\*(Pm\|\\*m
Character Attributes (SGR)
  \*(Ps = \*0 \(-> Normal (default)
  \*(Ps = \*1 \(-> Bold
  \*(Ps = \*4 \(-> Underscore
  \*(Ps = \*5 \(-> Blink (appears as Bold)
  \*(Ps = \*7 \(-> Inverse
.IP \\*(Es\\*([[\\*(Ps\|\\*n
Device Status Report (DSR)
  \*(Ps = \*5 \(-> Status Report \*(Es\*([[\*0\*n (``OK'')
  \*(Ps = \*6 \(-> Report Cursor Position (CPR) [row;column] as
\*(Es\*([[\*(Ir\|\*;\*(Ic\|\*R
.IP \\*(Es\\*([[\\*(Ps\|\\*;\\*(Ps\|\\*r
Set Scrolling Region [top;bottom] (default = full size of window) (DECSTBM)
.IP \\*(Es\\*([[\\*(Ps\|\\*x
Request Terminal Parameters (DECREQTPARM)
.IP \\*(Es\\*([[\\*?\\*(Pm\|\\*h
DEC Private Mode Set (DECSET)
  \*(Ps = \*1 \(-> Application Cursor Keys (DECCKM)
  \*(Ps = \*2 \(-> Designate USASCII for character sets G0-G3.
(In the VT102, this selects VT52 mode (DECANM), which \fIxterm\fP
doesn't support.)
  \*(Ps = \*3 \(-> 132 Column Mode (DECCOLM)
  \*(Ps = \*4 \(-> Smooth (Slow) Scroll (DECSCLM)
  \*(Ps = \*5 \(-> Reverse Video (DECSCNM)
  \*(Ps = \*6 \(-> Origin Mode (DECOM)
  \*(Ps = \*7 \(-> Wraparound Mode (DECAWM)
  \*(Ps = \*8 \(-> Auto-repeat Keys (DECARM)
  \*(Ps = \*9 \(-> Send Mouse X & Y on button press.
See the section \fBMouse Tracking\fP.
  \*(Ps = \*3\*8 \(-> Enter Tektronix Mode (DECTEK)
  \*(Ps = \*4\*0 \(-> Allow 80 \z\(<-\(-> 132 Mode
  \*(Ps = \*4\*1 \(-> \fIcurses\fP(5) fix
  \*(Ps = \*4\*4 \(-> Turn On Margin Bell
  \*(Ps = \*4\*5 \(-> Reverse-wraparound Mode
  \*(Ps = \*4\*6 \(-> Start Logging
  \*(Ps = \*4\*7 \(-> Use Alternate Screen Buffer (unless
disabled by the \fBtiteInhibit\fP resource)
  \*(Ps = \*1\*0\*0\*0 \(-> Send Mouse X & Y on button press and release.
See the section \fBMouse Tracking\fP.
  \*(Ps = \*1\*0\*0\*1 \(-> Use Hilite Mouse Tracking.
See the section \fBMouse Tracking\fP.
.IP \\*(Es\\*([[\\*?\\*(Pm\|\\*l
DEC Private Mode Reset (DECRST)
  \*(Ps = \*1 \(-> Normal Cursor Keys (DECCKM)
  \*(Ps = \*3 \(-> 80 Column Mode (DECCOLM)
  \*(Ps = \*4 \(-> Jump (Fast) Scroll (DECSCLM)
  \*(Ps = \*5 \(-> Normal Video (DECSCNM)
  \*(Ps = \*6 \(-> Normal Cursor Mode (DECOM)
  \*(Ps = \*7 \(-> No Wraparound Mode (DECAWM)
  \*(Ps = \*8 \(-> No Auto-repeat Keys (DECARM)
  \*(Ps = \*9 \(-> Don't Send Mouse X & Y on button press
  \*(Ps = \*4\*0 \(-> Disallow 80 \z\(<-\(-> 132 Mode
  \*(Ps = \*4\*1 \(-> No \fIcurses\fP(5) fix
  \*(Ps = \*4\*4 \(-> Turn Off Margin Bell
  \*(Ps = \*4\*5 \(-> No Reverse-wraparound Mode
  \*(Ps = \*4\*6 \(-> Stop Logging
  \*(Ps = \*4\*7 \(-> Use Normal Screen Buffer
  \*(Ps = \*1\*0\*0\*0 \(-> Don't Send Mouse X & Y on button press and
release
  \*(Ps = \*1\*0\*0\*1 \(-> Don't Use Hilite Mouse Tracking
.IP \\*(Es\\*([[\\*?\\*(Pm\|\\*r
Restore DEC Private Mode Values.  The value of \*(Ps previously saved is
restored.  \*(Ps values are the same as for DECSET.
.IP \\*(Es\\*([[\\*?\\*(Pm\|\\*s
Save DEC Private Mode Values.  \*(Ps values are the same as for DECSET.
.IP \\*(Es\\*]\\*(Ps\|\\*;\\*(Pt\|\\*(Be
Set Text Parameters
  \*(Ps = \*0 \(-> Change Icon Name and Window Title to \*(Pt
  \*(Ps = \*1 \(-> Change Icon Name to \*(Pt
  \*(Ps = \*2 \(-> Change Window Title to \*(Pt
  \*(Ps = \*4\*6 \(-> Change Log File to \*(Pt (normally disabled by a
compile-time option)
  \*(Ps = \*5\*0 \(-> Set Font to \*(Pt
.IP \\*(Es\\*^\\*(Pt\|\\*(Es\\*(bS
Privacy Message (PM)
  \fIxterm\fP implements no PM functions; \*(Pt is ignored.
\*(Pt need not be printable characters.
.IP \\*(Es\\*_\\*(Pt\|\\*(Es\\*(bS
Application Program Command (APC)
  \fIxterm\fP implements no APC functions; \*(Pt is ignored.
\*(Pt need not be printable characters.
.IP \\*(Es\\*c
Full Reset (RIS)
.IP \\*(Es\\*n
Select the G2 Character Set (LS2)
.IP \\*(Es\\*o
Select the G3 Character Set (LS3)
.IP \\*(Es\\*|
Invoke the G3 Character Set as GR (LS3R).  Has no visible effect in \fIxterm\fP.
.IP \\*(Es\\*}
Invoke the G2 Character Set as GR (LS2R).  Has no visible effect in \fIxterm\fP.
.IP \\*(Es\\*(c~
Invoke the G1 Character Set as GR (LS1R).  Has no visible effect in \fIxterm\fP.
.Ed
.SH
Mouse Tracking
.LP
The VT widget can be set to send the mouse position and other
information on button presses.  These modes are typically used by
editors and other full-screen applications that want to make use of
the mouse.
.LP
There are three mutually exclusive modes, each enabled (or disabled)
by a different parameter in the DECSET (or DECRST) escape sequence.
Parameters for all mouse tracking escape sequences generated by
\fIxterm\fP encode numeric parameters in a single character as
\fIvalue\fP+040.  For example, \*! is 1.  The screen
coodinate system is 1-based.
.LP
X10 compatibility mode sends an escape sequence on button press
encoding the location and the mouse button pressed.
It is enabled by specifying parameter 9 to DECSET.
On button press, \fIxterm\fP sends
\*(Es\*([[\*M\*(Cb\*(Cx\*(Cy (6 characters).  \*(Cb is button\-1.
\*(Cx and \*(Cy are the x and y coordinates of the mouse when the
button was pressed.
.LP
Normal tracking mode sends an escape sequence on both button press and
release.  Modifier information is also sent.
It is enabled by specifying parameter 1000 to DECSET.
On button press or release, \fIxterm\fP sends
\*(Es\*([[\*M\*(Cb\*(Cx\*(Cy.
The low two bits of \*(Cb encode button information:
0=MB1 pressed, 1=MB2 pressed, 2=MB3 pressed, 3=release.
The upper bits encode what modifiers were down when the button was
pressed and are added together.  4=Shift, 8=Meta, 16=Control.
\*(Cx and \*(Cy are the x and y coordinates of the mouse event.  The
upper left corner is (1,1).
.LP
Mouse hilite tracking notifies a program of a button press, receives a
range of lines from the program, highlights the region covered by
the mouse within that range until button release, and then sends the
program the release coordinates.
It is enabled by specifying parameter 1001 to DECSET.
Warning: use of this mode requires a cooperating program or it will
hang \fIxterm.\fP
On button press, the same information as for normal tracking is
generated; \fIxterm\fP then waits for the program to send mouse
tracking information.
\fIAll X events are ignored until the proper escape sequence is
received from the pty:\fP 
\*(Es\*([[\*(Ps\|\*;\*(Ps\|\*;\*(Ps\|\*;\*(Ps\|\*;\*(Ps\|\*T.
The parameters are \fIfunc, startx, starty, firstrow,\fP and \fIlastrow.\fP
\fIfunc\fP is non-zero to initiate hilite tracking and zero to abort.
\fIstartx\fP and \fIstarty\fP give the starting x and y location for
the highlighted region.  The ending location tracks the mouse, but
will never be above row \fIfirstrow\fP and will always be above row
\fIlastrow.\fP  (The top of the screen is row 1.)
When the button is released, \fIxterm\fP reports the ending position
one of two ways: if the start and end coordinates are valid text
locations: \*(Es\*([[\*t\*(Cx\*(Cy.  If either coordinate is past
the end of the line:
\*(Es\*([[\*T\*(Cx\*(Cy\*(Cx\*(Cy\*(Cx\*(Cy.
The parameters are \fIstartx, starty, endx, endy, mousex,\fP and \fImousey.\fP
\fIstartx, starty, endx, \fPand\fI endy\fP give the starting and
ending character positions of the region. \fImousex\fP and \fImousey\fP
give the location of the mouse at button up, which may not be over a
character.
.SH
Tektronix 4014 Mode
.ds RH Tektronix 4014 Mode
.LP
Most of these sequences are standard Tektronix 4014 control sequences.
The major features missing are
the write-thru and defocused modes.
This document does not describe the commands used in the various
Tektronix plotting modes but does describe the commands to switch modes.
.St
.IP \\*(Be
Bell (Ctrl-G)
.IP \\*(Bs
Backspace (Ctrl-H)
.IP \\*(Ta
Horizontal Tab (Ctrl-I)
.IP \\*(Lf
Line Feed or New Line (Ctrl-J)
.IP \\*(Vt
Cursor up (Ctrl-K)
.IP \\*(Ff
Form Feed or New Page (Ctrl-L)
.IP \\*(Cr
Carriage Return (Ctrl-M)
.IP \\*(Es\\*(Et
Switch to VT100 Mode (ESC Ctrl-C)
.IP \\*(Es\\*(En
Return Terminal Status (ESC Ctrl-E)
.IP \\*(Es\\*(Lf
PAGE (Clear Screen)
.IP \\*(Es\\*(So
Begin 4015 APL mode (ignored by \fIxterm\fP) (ESC Ctrl-N)
.IP \\*(Es\\*(Si
End 4015 APL mode (ignored by \fIxterm\fP) (ESC Ctrl-O)
.IP \\*(Es\\*(Eb
COPY (Save Tektronix Codes to file COPYyy-mm-dd.hh:mm:ss) (ESC Ctrl-W)
.IP \\*(Es\\*(Ca
Bypass Condition (ESC Ctrl-X)
.IP \\*(Es\\*(Su
GIN mode (ESC Ctrl-Z)
.IP \\*(Es\\*(Fs
Special Point Plot Mode (ESC Ctrl-\e)
.IP \\*(Es\\*8
Select Large Character Set
.IP \\*(Es\\*9
Select #2 Character Set
.IP \\*(Es\\*:
Select #3 Character Set
.IP \\*(Es\\*;
Select Small Character Set
.IP \\*(Es\\*]\\*(Ps\|\\*;\\*(Pt\|\\*(Be
Set Text Parameters of VT window
  \*(Ps = \*0 \(-> Change Icon Name and Window Title to \*(Pt
  \*(Ps = \*1 \(-> Change Icon Name to \*(Pt
  \*(Ps = \*2 \(-> Change Window Title to \*(Pt
  \*(Ps = \*4\*6 \(-> Change Log File to \*(Pt
.IP \\*(Es\\*`
Normal Z Axis and Normal (solid) Vectors
.IP \\*(Es\\*a
Normal Z Axis and Dotted Line Vectors
.IP \\*(Es\\*b
Normal Z Axis and Dot-Dashed Vectors
.IP \\*(Es\\*c
Normal Z Axis and Short-Dashed Vectors
.IP \\*(Es\\*d
Normal Z Axis and Long-Dashed Vectors
.IP \\*(Es\\*h
Defocused Z Axis and Normal (solid) Vectors
.IP \\*(Es\\*i
Defocused Z Axis and Dotted Line Vectors
.IP \\*(Es\\*j
Defocused Z Axis and Dot-Dashed Vectors
.IP \\*(Es\\*k
Defocused Z Axis and Short-Dashed Vectors
.IP \\*(Es\\*l
Defocused Z Axis and Long-Dashed Vectors
.IP \\*(Es\\*p
Write-Thru Mode and Normal (solid) Vectors
.IP \\*(Es\\*q
Write-Thru Mode and Dotted Line Vectors
.IP \\*(Es\\*r
Write-Thru Mode and Dot-Dashed Vectors
.IP \\*(Es\\*s
Write-Thru Mode and Short-Dashed Vectors
.IP \\*(Es\\*t
Write-Thru Mode and Long-Dashed Vectors
.IP \\*(Fs
Point Plot Mode (Ctrl-\e)
.IP \\*(Gs
Graph Mode (Ctrl-])
.IP \\*(Rs
Incremental Plot Mode (Ctrl-^)
.IP \\*(Us
Alpha Mode (Ctrl-_)
.Ed
