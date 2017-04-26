/* 
 * tclGet.c --
 *
 *	This file contains procedures to convert strings into
 *	other forms, like integers or floating-point numbers or
 *	booleans, doing syntax checking along the way.
 *
 * Copyright (c) 1990-1993 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#ifndef lint
static char rcsid[] = "$Header: /user6/ouster/tcl/RCS/tclGet.c,v 1.14 93/08/18 16:07:24 ouster Exp $ SPRITE (Berkeley)";
#endif /* not lint */

#include "tclInt.h"

/*
 *----------------------------------------------------------------------
 *
 * Tcl_GetInt --
 *
 *	Given a string, produce the corresponding integer value.
 *
 * Results:
 *	The return value is normally TCL_OK;  in this case *intPtr
 *	will be set to the integer value equivalent to string.  If
 *	string is improperly formed then TCL_ERROR is returned and
 *	an error message will be left in interp->result.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_GetInt(interp, string, intPtr)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting. */
    char *string;		/* String containing a (possibly signed)
				 * integer in a form acceptable to strtol. */
    int *intPtr;		/* Place to store converted result. */
{
    char *end, *p;
    int i;

    /*
     * Note: use strtoul instead of strtol for integer conversions
     * to allow full-size unsigned numbers, but don't depend on strtoul
     * to handle sign characters;  it won't in some implementations.
     */

    for (p = string; isspace(UCHAR(*p)); p++) {
	/* Empty loop body. */
    }
    if (*p == '-') {
	i = -strtoul(p+1, &end, 0);
    } else if (*p == '+') {
	i = strtoul(p+1, &end, 0);
    } else {
	i = strtoul(p, &end, 0);
    }
    while ((*end != '\0') && isspace(UCHAR(*end))) {
	end++;
    }
    if ((end == string) || (*end != 0)) {
	Tcl_AppendResult(interp, "expected integer but got \"", string,
		"\"", (char *) NULL);
	return TCL_ERROR;
    }
    *intPtr = i;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_GetDouble --
 *
 *	Given a string, produce the corresponding double-precision
 *	floating-point value.
 *
 * Results:
 *	The return value is normally TCL_OK;  in this case *doublePtr
 *	will be set to the double-precision value equivalent to string.
 *	If string is improperly formed then TCL_ERROR is returned and
 *	an error message will be left in interp->result.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_GetDouble(interp, string, doublePtr)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting. */
    char *string;		/* String containing a floating-point number
				 * in a form acceptable to strtod. */
    double *doublePtr;		/* Place to store converted result. */
{
    char *end;
    double d;

    d = strtod(string, &end);
    while ((*end != '\0') && isspace(UCHAR(*end))) {
	end++;
    }
    if ((end == string) || (*end != 0)) {
	Tcl_AppendResult(interp, "expected floating-point number but got \"",
		string, "\"", (char *) NULL);
	return TCL_ERROR;
    }
    *doublePtr = d;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_GetBoolean --
 *
 *	Given a string, return a 0/1 boolean value corresponding
 *	to the string.
 *
 * Results:
 *	The return value is normally TCL_OK;  in this case *boolPtr
 *	will be set to the 0/1 value equivalent to string.  If
 *	string is improperly formed then TCL_ERROR is returned and
 *	an error message will be left in interp->result.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_GetBoolean(interp, string, boolPtr)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting. */
    char *string;		/* String containing a boolean number
				 * specified either as 1/0 or true/false or
				 * yes/no. */
    int *boolPtr;		/* Place to store converted result, which
				 * will be 0 or 1. */
{
    char c;
    char lowerCase[10];
    int i, length;

    /*
     * Convert the input string to all lower-case.
     */

    for (i = 0; i < 9; i++) {
	c = string[i];
	if (c == 0) {
	    break;
	}
	if ((c >= 'A') && (c <= 'Z')) {
	    c += 'a' - 'A';
	}
	lowerCase[i] = c;
    }
    lowerCase[i] = 0;

    length = strlen(lowerCase);
    c = lowerCase[0];
    if ((c == '0') && (lowerCase[1] == '\0')) {
	*boolPtr = 0;
    } else if ((c == '1') && (lowerCase[1] == '\0')) {
	*boolPtr = 1;
    } else if ((c == 'y') && (strncmp(lowerCase, "yes", length) == 0)) {
	*boolPtr = 1;
    } else if ((c == 'n') && (strncmp(lowerCase, "no", length) == 0)) {
	*boolPtr = 0;
    } else if ((c == 't') && (strncmp(lowerCase, "true", length) == 0)) {
	*boolPtr = 1;
    } else if ((c == 'f') && (strncmp(lowerCase, "false", length) == 0)) {
	*boolPtr = 0;
    } else if ((c == 'o') && (length >= 2)) {
	if (strncmp(lowerCase, "on", length) == 0) {
	    *boolPtr = 1;
	} else if (strncmp(lowerCase, "off", length) == 0) {
	    *boolPtr = 0;
	}
    } else {
	Tcl_AppendResult(interp, "expected boolean value but got \"",
		string, "\"", (char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}
