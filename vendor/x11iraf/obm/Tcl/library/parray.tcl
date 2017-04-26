# parray:
# Print the contents of a global array on stdout.
#
# $Header: /user6/ouster/tcl/library/RCS/parray.tcl,v 1.5 93/02/06 16:33:45 ouster Exp $ SPRITE (Berkeley)
#
# Copyright (c) 1991-1993 The Regents of the University of California.
# All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#

proc parray a {
    upvar 1 $a array
    if [catch {array size array}] {
	error "\"$a\" isn't an array"
    }
    set maxl 0
    foreach name [lsort [array names array]] {
	if {[string length $name] > $maxl} {
	    set maxl [string length $name]
	}
    }
    set maxl [expr {$maxl + [string length $a] + 2}]
    foreach name [lsort [array names array]] {
	set nameString [format %s(%s) $a $name]
	puts stdout [format "%-*s = %s" $maxl $nameString $array($name)]
    }
}
