/* 
 * tclCmdMZ.c --
 *
 *	This file contains the top-level command routines for most of
 *	the Tcl built-in commands whose names begin with the letters
 *	M to Z.  It contains only commands in the generic core (i.e.
 *	those that don't depend much upon UNIX facilities).
 *
 * Copyright (c) 1987-1993 The Regents of the University of California.
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
static char rcsid[] = "$Header: /user6/ouster/tcl/RCS/tclCmdMZ.c,v 1.44 93/10/15 11:41:16 ouster Exp $ SPRITE (Berkeley)";
#endif

#include "tclInt.h"

/*
 * Structure used to hold information about variable traces:
 */

typedef struct {
    int flags;			/* Operations for which Tcl command is
				 * to be invoked. */
    char *errMsg;		/* Error message returned from Tcl command,
				 * or NULL.  Malloc'ed. */
    int length;			/* Number of non-NULL chars. in command. */
    char command[4];		/* Space for Tcl command to invoke.  Actual
				 * size will be as large as necessary to
				 * hold command.  This field must be the
				 * last in the structure, so that it can
				 * be larger than 4 bytes. */
} TraceVarInfo;

/*
 * Forward declarations for procedures defined in this file:
 */

static char *		TraceVarProc _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, char *name1, char *name2,
			    int flags));

/*
 *----------------------------------------------------------------------
 *
 * Tcl_RegexpCmd --
 *
 *	This procedure is invoked to process the "regexp" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
int
Tcl_RegexpCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int noCase = 0;
    int indices = 0;
    regexp *regexpPtr;
    char **argPtr, *string, *pattern;
    int match = 0;			/* Initialization needed only to
					 * prevent compiler warning. */
    int i;
    Tcl_DString stringDString, patternDString;

    if (argc < 3) {
	wrongNumArgs:
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" ?switches? exp string ?matchVar? ?subMatchVar ",
		"subMatchVar ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    argPtr = argv+1;
    argc--;
    while ((argc > 0) && (argPtr[0][0] == '-')) {
	if (strcmp(argPtr[0], "-indices") == 0) {
	    indices = 1;
	} else if (strcmp(argPtr[0], "-nocase") == 0) {
	    noCase = 1;
	} else if (strcmp(argPtr[0], "--") == 0) {
	    argPtr++;
	    argc--;
	    break;
	} else {
	    Tcl_AppendResult(interp, "bad switch \"", argPtr[0],
		    "\": must be -indices, -nocase, or --", (char *) NULL);
	    return TCL_ERROR;
	}
	argPtr++;
	argc--;
    }
    if (argc < 2) {
	goto wrongNumArgs;
    }

    /*
     * Convert the string and pattern to lower case, if desired, and
     * perform the matching operation.
     */

    if (noCase) {
	register char *p;

	Tcl_DStringInit(&patternDString);
	Tcl_DStringAppend(&patternDString, argPtr[0], -1);
	pattern = Tcl_DStringValue(&patternDString);
	for (p = pattern; *p != 0; p++) {
	    if (isupper(UCHAR(*p))) {
		*p = tolower(*p);
	    }
	}
	Tcl_DStringInit(&stringDString);
	Tcl_DStringAppend(&stringDString, argPtr[1], -1);
	string = Tcl_DStringValue(&stringDString);
	for (p = string; *p != 0; p++) {
	    if (isupper(UCHAR(*p))) {
		*p = tolower(*p);
	    }
	}
    } else {
	pattern = argPtr[0];
	string = argPtr[1];
    }
    regexpPtr = TclCompileRegexp(interp, pattern);
    if (regexpPtr != NULL) {
	tclRegexpError = NULL;
	match = TclRegExec(regexpPtr, string, string);
    }
    if (noCase) {
	Tcl_DStringFree(&stringDString);
	Tcl_DStringFree(&patternDString);
    }
    if (regexpPtr == NULL) {
	return TCL_ERROR;
    }
    if (tclRegexpError != NULL) {
	Tcl_AppendResult(interp, "error while matching pattern: ",
		tclRegexpError, (char *) NULL);
	return TCL_ERROR;
    }
    if (!match) {
	interp->result = "0";
	return TCL_OK;
    }

    /*
     * If additional variable names have been specified, return
     * index information in those variables.
     */

    argc -= 2;
    if (argc > NSUBEXP) {
	interp->result = "too many substring variables";
	return TCL_ERROR;
    }
    for (i = 0; i < argc; i++) {
	char *result, info[50];

	if (regexpPtr->startp[i] == NULL) {
	    if (indices) {
		result = Tcl_SetVar(interp, argPtr[i+2], "-1 -1", 0);
	    } else {
		result = Tcl_SetVar(interp, argPtr[i+2], "", 0);
	    }
	} else {
	    if (indices) {
		sprintf(info, "%d %d", regexpPtr->startp[i] - string,
			regexpPtr->endp[i] - string - 1);
		result = Tcl_SetVar(interp, argPtr[i+2], info, 0);
	    } else {
		char savedChar, *first, *last;

		first = argPtr[1] + (regexpPtr->startp[i] - string);
		last = argPtr[1] + (regexpPtr->endp[i] - string);
		savedChar = *last;
		*last = 0;
		result = Tcl_SetVar(interp, argPtr[i+2], first, 0);
		*last = savedChar;
	    }
	}
	if (result == NULL) {
	    Tcl_AppendResult(interp, "couldn't set variable \"",
		    argPtr[i+2], "\"", (char *) NULL);
	    return TCL_ERROR;
	}
    }
    interp->result = "1";
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_RegsubCmd --
 *
 *	This procedure is invoked to process the "regsub" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
int
Tcl_RegsubCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int noCase = 0, all = 0;
    regexp *regexpPtr;
    char *string, *pattern, *p, *firstChar, *newValue, **argPtr;
    int match, flags, code, anyMatches;
    register char *src, c;
    Tcl_DString stringDString, patternDString;

    if (argc < 5) {
	wrongNumArgs:
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" ?switches? exp string subSpec varName\"", (char *) NULL);
	return TCL_ERROR;
    }
    argPtr = argv+1;
    argc--;
    while (argPtr[0][0] == '-') {
	if (strcmp(argPtr[0], "-nocase") == 0) {
	    noCase = 1;
	} else if (strcmp(argPtr[0], "-all") == 0) {
	    all = 1;
	} else if (strcmp(argPtr[0], "--") == 0) {
	    argPtr++;
	    argc--;
	    break;
	} else {
	    Tcl_AppendResult(interp, "bad switch \"", argPtr[0],
		    "\": must be -all, -nocase, or --", (char *) NULL);
	    return TCL_ERROR;
	}
	argPtr++;
	argc--;
    }
    if (argc != 4) {
	goto wrongNumArgs;
    }

    /*
     * Convert the string and pattern to lower case, if desired.
     */

    if (noCase) {
	Tcl_DStringInit(&patternDString);
	Tcl_DStringAppend(&patternDString, argPtr[0], -1);
	pattern = Tcl_DStringValue(&patternDString);
	for (p = pattern; *p != 0; p++) {
	    if (isupper(UCHAR(*p))) {
		*p = tolower(*p);
	    }
	}
	Tcl_DStringInit(&stringDString);
	Tcl_DStringAppend(&stringDString, argPtr[1], -1);
	string = Tcl_DStringValue(&stringDString);
	for (p = string; *p != 0; p++) {
	    if (isupper(UCHAR(*p))) {
		*p = tolower(*p);
	    }
	}
    } else {
	pattern = argPtr[0];
	string = argPtr[1];
    }
    regexpPtr = TclCompileRegexp(interp, pattern);
    if (regexpPtr == NULL) {
	code = TCL_ERROR;
	goto done;
    }

    /*
     * The following loop is to handle multiple matches within the
     * same source string;  each iteration handles one match and its
     * corresponding substitution.  If "-all" hasn't been specified
     * then the loop body only gets executed once.
     */

    flags = 0;
    anyMatches = 0;
    for (p = string; *p != 0; ) {
	tclRegexpError = NULL;
	match = TclRegExec(regexpPtr, p, string);
	if (tclRegexpError != NULL) {
	    Tcl_AppendResult(interp, "error while matching pattern: ",
		    tclRegexpError, (char *) NULL);
	    code = TCL_ERROR;
	    goto done;
	}
	if (!match) {
	    break;
	}
	anyMatches = 1;

	/*
	 * Copy the portion of the source string before the match to the
	 * result variable.
	 */
    
	src = argPtr[1] + (regexpPtr->startp[0] - string);
	c = *src;
	*src = 0;
	newValue = Tcl_SetVar(interp, argPtr[3], argPtr[1] + (p - string),
		flags);
	*src = c;
	flags = TCL_APPEND_VALUE;
	if (newValue == NULL) {
	    cantSet:
	    Tcl_AppendResult(interp, "couldn't set variable \"",
		    argPtr[3], "\"", (char *) NULL);
	    code = TCL_ERROR;
	    goto done;
	}
    
	/*
	 * Append the subSpec argument to the variable, making appropriate
	 * substitutions.  This code is a bit hairy because of the backslash
	 * conventions and because the code saves up ranges of characters in
	 * subSpec to reduce the number of calls to Tcl_SetVar.
	 */
    
	for (src = firstChar = argPtr[2], c = *src; c != 0; src++, c = *src) {
	    int index;
    
	    if (c == '&') {
		index = 0;
	    } else if (c == '\\') {
		c = src[1];
		if ((c >= '0') && (c <= '9')) {
		    index = c - '0';
		} else if ((c == '\\') || (c == '&')) {
		    *src = c;
		    src[1] = 0;
		    newValue = Tcl_SetVar(interp, argPtr[3], firstChar,
			    TCL_APPEND_VALUE);
		    *src = '\\';
		    src[1] = c;
		    if (newValue == NULL) {
			goto cantSet;
		    }
		    firstChar = src+2;
		    src++;
		    continue;
		} else {
		    continue;
		}
	    } else {
		continue;
	    }
	    if (firstChar != src) {
		c = *src;
		*src = 0;
		newValue = Tcl_SetVar(interp, argPtr[3], firstChar,
			TCL_APPEND_VALUE);
		*src = c;
		if (newValue == NULL) {
		    goto cantSet;
		}
	    }
	    if ((index < NSUBEXP) && (regexpPtr->startp[index] != NULL)
		    && (regexpPtr->endp[index] != NULL)) {
		char *first, *last, saved;
    
		first = argPtr[1] + (regexpPtr->startp[index] - string);
		last = argPtr[1] + (regexpPtr->endp[index] - string);
		saved = *last;
		*last = 0;
		newValue = Tcl_SetVar(interp, argPtr[3], first,
			TCL_APPEND_VALUE);
		*last = saved;
		if (newValue == NULL) {
		    goto cantSet;
		}
	    }
	    if (*src == '\\') {
		src++;
	    }
	    firstChar = src+1;
	}
	if (firstChar != src) {
	    if (Tcl_SetVar(interp, argPtr[3], firstChar,
		    TCL_APPEND_VALUE) == NULL) {
		goto cantSet;
	    }
	}
	if (regexpPtr->endp[0] == p) {
	    char tmp[2];

	    /*
	     * Always consume at least one character of the input string
	     * in order to prevent infinite loops.
	     */

	    tmp[0] = argPtr[1][p - string];
	    tmp[1] = 0;
	    newValue = Tcl_SetVar(interp, argPtr[3], tmp, flags);
	    if (newValue == NULL) {
		goto cantSet;
	    }
	    p = regexpPtr->endp[0] + 1;
	} else {
	    p = regexpPtr->endp[0];
	}
	if (!all) {
	    break;
	}
    }

    /*
     * Copy the portion of the source string after the last match to the
     * result variable.
     */

    if ((*p != 0) || !anyMatches) {
	if (Tcl_SetVar(interp, argPtr[3], argPtr[1] + (p - string), 
		flags) == NULL) {
	    goto cantSet;
	}
    }
    if (anyMatches) {
	interp->result = "1";
    } else {
	interp->result = "0";
    }
    code = TCL_OK;

    done:
    if (noCase) {
	Tcl_DStringFree(&stringDString);
	Tcl_DStringFree(&patternDString);
    }
    return code;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_RenameCmd --
 *
 *	This procedure is invoked to process the "rename" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
int
Tcl_RenameCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    register Command *cmdPtr;
    Interp *iPtr = (Interp *) interp;
    Tcl_HashEntry *hPtr;
    int new;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" oldName newName\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (argv[2][0] == '\0') {
	if (Tcl_DeleteCommand(interp, argv[1]) != 0) {
	    Tcl_AppendResult(interp, "can't delete \"", argv[1],
		    "\": command doesn't exist", (char *) NULL);
	    return TCL_ERROR;
	}
	return TCL_OK;
    }
    hPtr = Tcl_FindHashEntry(&iPtr->commandTable, argv[2]);
    if (hPtr != NULL) {
	Tcl_AppendResult(interp, "can't rename to \"", argv[2],
		"\": command already exists", (char *) NULL);
	return TCL_ERROR;
    }
    hPtr = Tcl_FindHashEntry(&iPtr->commandTable, argv[1]);
    if (hPtr == NULL) {
	Tcl_AppendResult(interp, "can't rename \"", argv[1],
		"\":  command doesn't exist", (char *) NULL);
	return TCL_ERROR;
    }
    cmdPtr = (Command *) Tcl_GetHashValue(hPtr);
    Tcl_DeleteHashEntry(hPtr);
    hPtr = Tcl_CreateHashEntry(&iPtr->commandTable, argv[2], &new);
    Tcl_SetHashValue(hPtr, cmdPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_ReturnCmd --
 *
 *	This procedure is invoked to process the "return" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
int
Tcl_ReturnCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    Interp *iPtr = (Interp *) interp;
    int c, code;

    if (iPtr->errorInfo != NULL) {
	ckfree(iPtr->errorInfo);
	iPtr->errorInfo = NULL;
    }
    if (iPtr->errorCode != NULL) {
	ckfree(iPtr->errorCode);
	iPtr->errorCode = NULL;
    }
    code = TCL_OK;
    for (argv++, argc--; argc > 1; argv += 2, argc -= 2) {
	if (strcmp(argv[0], "-code") == 0) {
	    c = argv[1][0];
	    if ((c == 'o') && (strcmp(argv[1], "ok") == 0)) {
		code = TCL_OK;
	    } else if ((c == 'e') && (strcmp(argv[1], "error") == 0)) {
		code = TCL_ERROR;
	    } else if ((c == 'r') && (strcmp(argv[1], "return") == 0)) {
		code = TCL_RETURN;
	    } else if ((c == 'b') && (strcmp(argv[1], "break") == 0)) {
		code = TCL_BREAK;
	    } else if ((c == 'c') && (strcmp(argv[1], "continue") == 0)) {
		code = TCL_CONTINUE;
	    } else if (Tcl_GetInt(interp, argv[1], &code) != TCL_OK) {
		Tcl_ResetResult(interp);
		Tcl_AppendResult(interp, "bad completion code \"",
			argv[1], "\": must be ok, error, return, break, ",
			"continue, or an integer", (char *) NULL);
		return TCL_ERROR;
	    }
	} else if (strcmp(argv[0], "-errorinfo") == 0) {
	    iPtr->errorInfo = ckalloc((unsigned) (strlen(argv[1]) + 1));
	    strcpy(iPtr->errorInfo, argv[1]);
	} else if (strcmp(argv[0], "-errorcode") == 0) {
	    iPtr->errorCode = ckalloc((unsigned) (strlen(argv[1]) + 1));
	    strcpy(iPtr->errorCode, argv[1]);
	} else {
	    Tcl_AppendResult(interp, "bad option \"", argv[0],
		    ": must be -code, -errorcode, or -errorinfo",
		    (char *) NULL);
	    return TCL_ERROR;
	}
    }
    if (argc == 1) {
	Tcl_SetResult(interp, argv[0], TCL_VOLATILE);
    }
    iPtr->returnCode = code;
    return TCL_RETURN;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_ScanCmd --
 *
 *	This procedure is invoked to process the "scan" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
int
Tcl_ScanCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
#   define MAX_FIELDS 20
    typedef struct {
	char fmt;			/* Format for field. */
	int size;			/* How many bytes to allow for
					 * field. */
	char *location;			/* Where field will be stored. */
    } Field;
    Field fields[MAX_FIELDS];		/* Info about all the fields in the
					 * format string. */
    register Field *curField;
    int numFields = 0;			/* Number of fields actually
					 * specified. */
    int suppress;			/* Current field is assignment-
					 * suppressed. */
    int totalSize = 0;			/* Number of bytes needed to store
					 * all results combined. */
    char *results;			/* Where scanned output goes.
					 * Malloced; NULL means not allocated
					 * yet. */
    int numScanned;			/* sscanf's result. */
    register char *fmt;
    int i, widthSpecified, length, code;

    /*
     * The variables below are used to hold a copy of the format
     * string, so that we can replace format specifiers like "%f"
     * and "%F" with specifiers like "%lf"
     */

#   define STATIC_SIZE 5
    char copyBuf[STATIC_SIZE], *fmtCopy;
    register char *dst;

    if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" string format ?varName varName ...?\"", (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * This procedure operates in four stages:
     * 1. Scan the format string, collecting information about each field.
     * 2. Allocate an array to hold all of the scanned fields.
     * 3. Call sscanf to do all the dirty work, and have it store the
     *    parsed fields in the array.
     * 4. Pick off the fields from the array and assign them to variables.
     */

    code = TCL_OK;
    results = NULL;
    length = strlen(argv[2]) * 2 + 1;
    if (length < STATIC_SIZE) {
	fmtCopy = copyBuf;
    } else {
	fmtCopy = ckalloc((unsigned) length);
    }
    dst = fmtCopy;
    for (fmt = argv[2]; *fmt != 0; fmt++) {
	*dst = *fmt;
	dst++;
	if (*fmt != '%') {
	    continue;
	}
	fmt++;
	if (*fmt == '%') {
	    *dst = *fmt;
	    dst++;
	    continue;
	}
	if (*fmt == '*') {
	    suppress = 1;
	    *dst = *fmt;
	    dst++;
	    fmt++;
	} else {
	    suppress = 0;
	}
	widthSpecified = 0;
	while (isdigit(UCHAR(*fmt))) {
	    widthSpecified = 1;
	    *dst = *fmt;
	    dst++;
	    fmt++;
	}
	if ((*fmt == 'l') || (*fmt == 'h') || (*fmt == 'L')) {
	    fmt++;
	}
	*dst = *fmt;
	dst++;
	if (suppress) {
	    continue;
	}
	if (numFields == MAX_FIELDS) {
	    interp->result = "too many fields to scan";
	    code = TCL_ERROR;
	    goto done;
	}
	curField = &fields[numFields];
	numFields++;
	switch (*fmt) {
	    case 'd':
	    case 'i':
	    case 'o':
	    case 'x':
		curField->fmt = 'd';
		curField->size = sizeof(int);
		break;

	    case 'u':
		curField->fmt = 'u';
		curField->size = sizeof(int);
		break;

	    case 's':
		curField->fmt = 's';
		curField->size = strlen(argv[1]) + 1;
		break;

	    case 'c':
                if (widthSpecified) {
                    interp->result = 
                         "field width may not be specified in %c conversion";
		    code = TCL_ERROR;
		    goto done;
                }
		curField->fmt = 'c';
		curField->size = sizeof(int);
		break;

	    case 'e':
	    case 'f':
	    case 'g':
		dst[-1] = 'l';
		dst[0] = 'f';
		dst++;
		curField->fmt = 'f';
		curField->size = sizeof(double);
		break;

	    case '[':
		curField->fmt = 's';
		curField->size = strlen(argv[1]) + 1;
		do {
		    fmt++;
		    *dst = *fmt;
		    dst++;
		} while (*fmt != ']');
		break;

	    default:
		sprintf(interp->result, "bad scan conversion character \"%c\"",
			*fmt);
		code = TCL_ERROR;
		goto done;
	}
	curField->size = TCL_ALIGN(curField->size);
	totalSize += curField->size;
    }
    *dst = 0;

    if (numFields != (argc-3)) {
	interp->result =
		"different numbers of variable names and field specifiers";
	code = TCL_ERROR;
	goto done;
    }

    /*
     * Step 2:
     */

    results = (char *) ckalloc((unsigned) totalSize);
    for (i = 0, totalSize = 0, curField = fields;
	    i < numFields; i++, curField++) {
	curField->location = results + totalSize;
	totalSize += curField->size;
    }

    /*
     * Fill in the remaining fields with NULL;  the only purpose of
     * this is to keep some memory analyzers, like Purify, from
     * complaining.
     */

    for ( ; i < MAX_FIELDS; i++, curField++) {
	curField->location = NULL;
    }

    /*
     * Step 3:
     */

    numScanned = sscanf(argv[1], fmtCopy,
	    fields[0].location, fields[1].location, fields[2].location,
	    fields[3].location, fields[4].location, fields[5].location,
	    fields[6].location, fields[7].location, fields[8].location,
	    fields[9].location, fields[10].location, fields[11].location,
	    fields[12].location, fields[13].location, fields[14].location,
	    fields[15].location, fields[16].location, fields[17].location,
	    fields[18].location, fields[19].location);

    /*
     * Step 4:
     */

    if (numScanned < numFields) {
	numFields = numScanned;
    }
    for (i = 0, curField = fields; i < numFields; i++, curField++) {
	switch (curField->fmt) {
	    char string[TCL_DOUBLE_SPACE];

	    case 'd':
		sprintf(string, "%d", *((int *) curField->location));
		if (Tcl_SetVar(interp, argv[i+3], string, 0) == NULL) {
		    storeError:
		    Tcl_AppendResult(interp,
			    "couldn't set variable \"", argv[i+3], "\"",
			    (char *) NULL);
		    code = TCL_ERROR;
		    goto done;
		}
		break;

	    case 'u':
		sprintf(string, "%u", *((int *) curField->location));
		if (Tcl_SetVar(interp, argv[i+3], string, 0) == NULL) {
		    goto storeError;
		}
		break;

	    case 'c':
		sprintf(string, "%d", *((char *) curField->location) & 0xff);
		if (Tcl_SetVar(interp, argv[i+3], string, 0) == NULL) {
		    goto storeError;
		}
		break;

	    case 's':
		if (Tcl_SetVar(interp, argv[i+3], curField->location, 0)
			== NULL) {
		    goto storeError;
		}
		break;

	    case 'f':
		Tcl_PrintDouble(interp, *((double *) curField->location),
			string);
		if (Tcl_SetVar(interp, argv[i+3], string, 0) == NULL) {
		    goto storeError;
		}
		break;
	}
    }
    sprintf(interp->result, "%d", numScanned);
    done:
    if (results != NULL) {
	ckfree(results);
    }
    if (fmtCopy != copyBuf) {
	ckfree(fmtCopy);
    }
    return code;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_SplitCmd --
 *
 *	This procedure is invoked to process the "split" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
int
Tcl_SplitCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    char *splitChars;
    register char *p, *p2;
    char *elementStart;

    if (argc == 2) {
	splitChars = " \n\t\r";
    } else if (argc == 3) {
	splitChars = argv[2];
    } else {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" string ?splitChars?\"", (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Handle the special case of splitting on every character.
     */

    if (*splitChars == 0) {
	char string[2];
	string[1] = 0;
	for (p = argv[1]; *p != 0; p++) {
	    string[0] = *p;
	    Tcl_AppendElement(interp, string);
	}
	return TCL_OK;
    }

    /*
     * Normal case: split on any of a given set of characters.
     * Discard instances of the split characters.
     */

    for (p = elementStart = argv[1]; *p != 0; p++) {
	char c = *p;
	for (p2 = splitChars; *p2 != 0; p2++) {
	    if (*p2 == c) {
		*p = 0;
		Tcl_AppendElement(interp, elementStart);
		*p = c;
		elementStart = p+1;
		break;
	    }
	}
    }
    if (p != argv[1]) {
	Tcl_AppendElement(interp, elementStart);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_StringCmd --
 *
 *	This procedure is invoked to process the "string" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
int
Tcl_StringCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int length;
    register char *p, c;
    int match;
    int first;
    int left = 0, right = 0;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" option arg ?arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);
    if ((c == 'c') && (strncmp(argv[1], "compare", length) == 0)) {
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " compare string1 string2\"", (char *) NULL);
	    return TCL_ERROR;
	}
	match = strcmp(argv[2], argv[3]);
	if (match > 0) {
	    interp->result = "1";
	} else if (match < 0) {
	    interp->result = "-1";
	} else {
	    interp->result = "0";
	}
	return TCL_OK;
    } else if ((c == 'f') && (strncmp(argv[1], "first", length) == 0)) {
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " first string1 string2\"", (char *) NULL);
	    return TCL_ERROR;
	}
	first = 1;

	firstLast:
	match = -1;
	c = *argv[2];
	length = strlen(argv[2]);
	for (p = argv[3]; *p != 0; p++) {
	    if (*p != c) {
		continue;
	    }
	    if (strncmp(argv[2], p, length) == 0) {
		match = p-argv[3];
		if (first) {
		    break;
		}
	    }
	}
	sprintf(interp->result, "%d", match);
	return TCL_OK;
    } else if ((c == 'i') && (strncmp(argv[1], "index", length) == 0)) {
	int index;

	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " index string charIndex\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (Tcl_GetInt(interp, argv[3], &index) != TCL_OK) {
	    return TCL_ERROR;
	}
	if ((index >= 0) && (index < (int) strlen(argv[2]))) {
	    interp->result[0] = argv[2][index];
	    interp->result[1] = 0;
	}
	return TCL_OK;
    } else if ((c == 'l') && (strncmp(argv[1], "last", length) == 0)
	    && (length >= 2)) {
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " last string1 string2\"", (char *) NULL);
	    return TCL_ERROR;
	}
	first = 0;
	goto firstLast;
    } else if ((c == 'l') && (strncmp(argv[1], "length", length) == 0)
	    && (length >= 2)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " length string\"", (char *) NULL);
	    return TCL_ERROR;
	}
	sprintf(interp->result, "%d", strlen(argv[2]));
	return TCL_OK;
    } else if ((c == 'm') && (strncmp(argv[1], "match", length) == 0)) {
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " match pattern string\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (Tcl_StringMatch(argv[3], argv[2]) != 0) {
	    interp->result = "1";
	} else {
	    interp->result = "0";
	}
	return TCL_OK;
    } else if ((c == 'r') && (strncmp(argv[1], "range", length) == 0)) {
	int first, last, stringLength;

	if (argc != 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " range string first last\"", (char *) NULL);
	    return TCL_ERROR;
	}
	stringLength = strlen(argv[2]);
	if (Tcl_GetInt(interp, argv[3], &first) != TCL_OK) {
	    return TCL_ERROR;
	}
	if ((*argv[4] == 'e')
		&& (strncmp(argv[4], "end", strlen(argv[4])) == 0)) {
	    last = stringLength-1;
	} else {
	    if (Tcl_GetInt(interp, argv[4], &last) != TCL_OK) {
		Tcl_ResetResult(interp);
		Tcl_AppendResult(interp,
			"expected integer or \"end\" but got \"",
			argv[4], "\"", (char *) NULL);
		return TCL_ERROR;
	    }
	}
	if (first < 0) {
	    first = 0;
	}
	if (last >= stringLength) {
	    last = stringLength-1;
	}
	if (last >= first) {
	    char saved, *p;

	    p = argv[2] + last + 1;
	    saved = *p;
	    *p = 0;
	    Tcl_SetResult(interp, argv[2] + first, TCL_VOLATILE);
	    *p = saved;
	}
	return TCL_OK;
    } else if ((c == 't') && (strncmp(argv[1], "tolower", length) == 0)
	    && (length >= 3)) {
	register char *p;

	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " tolower string\"", (char *) NULL);
	    return TCL_ERROR;
	}
	Tcl_SetResult(interp, argv[2], TCL_VOLATILE);
	for (p = interp->result; *p != 0; p++) {
	    if (isupper(UCHAR(*p))) {
		*p = tolower(*p);
	    }
	}
	return TCL_OK;
    } else if ((c == 't') && (strncmp(argv[1], "toupper", length) == 0)
	    && (length >= 3)) {
	register char *p;

	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " toupper string\"", (char *) NULL);
	    return TCL_ERROR;
	}
	Tcl_SetResult(interp, argv[2], TCL_VOLATILE);
	for (p = interp->result; *p != 0; p++) {
	    if (islower(UCHAR(*p))) {
		*p = toupper(*p);
	    }
	}
	return TCL_OK;
    } else if ((c == 't') && (strncmp(argv[1], "trim", length) == 0)
	    && (length == 4)) {
	char *trimChars;
	register char *p, *checkPtr;

	left = right = 1;

	trim:
	if (argc == 4) {
	    trimChars = argv[3];
	} else if (argc == 3) {
	    trimChars = " \t\n\r";
	} else {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " ", argv[1], " string ?chars?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	p = argv[2];
	if (left) {
	    for (c = *p; c != 0; p++, c = *p) {
		for (checkPtr = trimChars; *checkPtr != c; checkPtr++) {
		    if (*checkPtr == 0) {
			goto doneLeft;
		    }
		}
	    }
	}
	doneLeft:
	Tcl_SetResult(interp, p, TCL_VOLATILE);
	if (right) {
	    char *donePtr;

	    p = interp->result + strlen(interp->result) - 1;
	    donePtr = &interp->result[-1];
	    for (c = *p; p != donePtr; p--, c = *p) {
		for (checkPtr = trimChars; *checkPtr != c; checkPtr++) {
		    if (*checkPtr == 0) {
			goto doneRight;
		    }
		}
	    }
	    doneRight:
	    p[1] = 0;
	}
	return TCL_OK;
    } else if ((c == 't') && (strncmp(argv[1], "trimleft", length) == 0)
	    && (length > 4)) {
	left = 1;
	argv[1] = "trimleft";
	goto trim;
    } else if ((c == 't') && (strncmp(argv[1], "trimright", length) == 0)
	    && (length > 4)) {
	right = 1;
	argv[1] = "trimright";
	goto trim;
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
		"\": should be compare, first, index, last, length, match, ",
		"range, tolower, toupper, trim, trimleft, or trimright",
		(char *) NULL);
	return TCL_ERROR;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_SwitchCmd --
 *
 *	This procedure is invoked to process the "switch" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
int
Tcl_SwitchCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
#define EXACT	0
#define GLOB	1
#define REGEXP	2
    int i, code, mode, matched;
    int body;
    char *string;
    int switchArgc, splitArgs;
    char **switchArgv;

    switchArgc = argc-1;
    switchArgv = argv+1;
    mode = EXACT;
    while ((switchArgc > 0) && (*switchArgv[0] == '-')) {
	if (strcmp(*switchArgv, "-exact") == 0) {
	    mode = EXACT;
	} else if (strcmp(*switchArgv, "-glob") == 0) {
	    mode = GLOB;
	} else if (strcmp(*switchArgv, "-regexp") == 0) {
	    mode = REGEXP;
	} else if (strcmp(*switchArgv, "--") == 0) {
	    switchArgc--;
	    switchArgv++;
	    break;
	} else {
	    Tcl_AppendResult(interp, "bad option \"", switchArgv[0],
		    "\": should be -exact, -glob, -regexp, or --",
		    (char *) NULL);
	    return TCL_ERROR;
	}
	switchArgc--;
	switchArgv++;
    }
    if (switchArgc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " ?switches? string pattern body ... ?default body?\"",
		(char *) NULL);
	return TCL_ERROR;
    }
    string = *switchArgv;
    switchArgc--;
    switchArgv++;

    /*
     * If all of the pattern/command pairs are lumped into a single
     * argument, split them out again.
     */

    splitArgs = 0;
    if (switchArgc == 1) {
	code = Tcl_SplitList(interp, switchArgv[0], &switchArgc, &switchArgv);
	if (code != TCL_OK) {
	    return code;
	}
	splitArgs = 1;
    }

    for (i = 0; i < switchArgc; i += 2) {
	if (i == (switchArgc-1)) {
	    interp->result = "extra switch pattern with no body";
	    code = TCL_ERROR;
	    goto cleanup;
	}

	/*
	 * See if the pattern matches the string.
	 */

	matched = 0;
	if ((*switchArgv[i] == 'd') && (i == switchArgc-2)
		&& (strcmp(switchArgv[i], "default") == 0)) {
	    matched = 1;
	} else {
	    switch (mode) {
		case EXACT:
		    matched = (strcmp(string, switchArgv[i]) == 0);
		    break;
		case GLOB:
		    matched = Tcl_StringMatch(string, switchArgv[i]);
		    break;
		case REGEXP:
		    matched = Tcl_RegExpMatch(interp, string, switchArgv[i]);
		    if (matched < 0) {
			code = TCL_ERROR;
			goto cleanup;
		    }
		    break;
	    }
	}
	if (!matched) {
	    continue;
	}

	/*
	 * We've got a match.  Find a body to execute, skipping bodies
	 * that are "-".
	 */

	for (body = i+1; ; body += 2) {
	    if (body >= switchArgc) {
		Tcl_AppendResult(interp, "no body specified for pattern \"",
			switchArgv[i], "\"", (char *) NULL);
		code = TCL_ERROR;
		goto cleanup;
	    }
	    if ((switchArgv[body][0] != '-') || (switchArgv[body][1] != 0)) {
		break;
	    }
	}
	code = Tcl_Eval(interp, switchArgv[body]);
	if (code == TCL_ERROR) {
	    char msg[100];
	    sprintf(msg, "\n    (\"%.50s\" arm line %d)", switchArgv[i],
		    interp->errorLine);
	    Tcl_AddErrorInfo(interp, msg);
	}
	goto cleanup;
    }

    /*
     * Nothing matched:  return nothing.
     */

    code = TCL_OK;

    cleanup:
    if (splitArgs) {
	ckfree((char *) switchArgv);
    }
    return code;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_TraceCmd --
 *
 *	This procedure is invoked to process the "trace" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
int
Tcl_TraceCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    char c;
    int length;

    if (argc < 2) {
	Tcl_AppendResult(interp, "too few args: should be \"",
		argv[0], " option [arg arg ...]\"", (char *) NULL);
	return TCL_ERROR;
    }
    c = argv[1][1];
    length = strlen(argv[1]);
    if ((c == 'a') && (strncmp(argv[1], "variable", length) == 0)
	    && (length >= 2)) {
	char *p;
	int flags, length;
	TraceVarInfo *tvarPtr;

	if (argc != 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " variable name ops command\"", (char *) NULL);
	    return TCL_ERROR;
	}

	flags = 0;
	for (p = argv[3] ; *p != 0; p++) {
	    if (*p == 'r') {
		flags |= TCL_TRACE_READS;
	    } else if (*p == 'w') {
		flags |= TCL_TRACE_WRITES;
	    } else if (*p == 'u') {
		flags |= TCL_TRACE_UNSETS;
	    } else {
		goto badOps;
	    }
	}
	if (flags == 0) {
	    goto badOps;
	}

	length = strlen(argv[4]);
	tvarPtr = (TraceVarInfo *) ckalloc((unsigned)
		(sizeof(TraceVarInfo) - sizeof(tvarPtr->command) + length + 1));
	tvarPtr->flags = flags;
	tvarPtr->errMsg = NULL;
	tvarPtr->length = length;
	flags |= TCL_TRACE_UNSETS;
	strcpy(tvarPtr->command, argv[4]);
	if (Tcl_TraceVar(interp, argv[2], flags, TraceVarProc,
		(ClientData) tvarPtr) != TCL_OK) {
	    ckfree((char *) tvarPtr);
	    return TCL_ERROR;
	}
    } else if ((c == 'd') && (strncmp(argv[1], "vdelete", length)
	    && (length >= 2)) == 0) {
	char *p;
	int flags, length;
	TraceVarInfo *tvarPtr;
	ClientData clientData;

	if (argc != 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " vdelete name ops command\"", (char *) NULL);
	    return TCL_ERROR;
	}

	flags = 0;
	for (p = argv[3] ; *p != 0; p++) {
	    if (*p == 'r') {
		flags |= TCL_TRACE_READS;
	    } else if (*p == 'w') {
		flags |= TCL_TRACE_WRITES;
	    } else if (*p == 'u') {
		flags |= TCL_TRACE_UNSETS;
	    } else {
		goto badOps;
	    }
	}
	if (flags == 0) {
	    goto badOps;
	}

	/*
	 * Search through all of our traces on this variable to
	 * see if there's one with the given command.  If so, then
	 * delete the first one that matches.
	 */

	length = strlen(argv[4]);
	clientData = 0;
	while ((clientData = Tcl_VarTraceInfo(interp, argv[2], 0,
		TraceVarProc, clientData)) != 0) {
	    tvarPtr = (TraceVarInfo *) clientData;
	    if ((tvarPtr->length == length) && (tvarPtr->flags == flags)
		    && (strncmp(argv[4], tvarPtr->command, length) == 0)) {
		Tcl_UntraceVar(interp, argv[2], flags | TCL_TRACE_UNSETS,
			TraceVarProc, clientData);
		if (tvarPtr->errMsg != NULL) {
		    ckfree(tvarPtr->errMsg);
		}
		ckfree((char *) tvarPtr);
		break;
	    }
	}
    } else if ((c == 'i') && (strncmp(argv[1], "vinfo", length) == 0)
	    && (length >= 2)) {
	ClientData clientData;
	char ops[4], *p;
	char *prefix = "{";

	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " vinfo name\"", (char *) NULL);
	    return TCL_ERROR;
	}
	clientData = 0;
	while ((clientData = Tcl_VarTraceInfo(interp, argv[2], 0,
		TraceVarProc, clientData)) != 0) {
	    TraceVarInfo *tvarPtr = (TraceVarInfo *) clientData;
	    p = ops;
	    if (tvarPtr->flags & TCL_TRACE_READS) {
		*p = 'r';
		p++;
	    }
	    if (tvarPtr->flags & TCL_TRACE_WRITES) {
		*p = 'w';
		p++;
	    }
	    if (tvarPtr->flags & TCL_TRACE_UNSETS) {
		*p = 'u';
		p++;
	    }
	    *p = '\0';
	    Tcl_AppendResult(interp, prefix, (char *) NULL);
	    Tcl_AppendElement(interp, ops);
	    Tcl_AppendElement(interp, tvarPtr->command);
	    Tcl_AppendResult(interp, "}", (char *) NULL);
	    prefix = " {";
	}
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
		"\": should be variable, vdelete, or vinfo",
		(char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;

    badOps:
    Tcl_AppendResult(interp, "bad operations \"", argv[3],
	    "\": should be one or more of rwu", (char *) NULL);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * TraceVarProc --
 *
 *	This procedure is called to handle variable accesses that have
 *	been traced using the "trace" command.
 *
 * Results:
 *	Normally returns NULL.  If the trace command returns an error,
 *	then this procedure returns an error string.
 *
 * Side effects:
 *	Depends on the command associated with the trace.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
static char *
TraceVarProc(clientData, interp, name1, name2, flags)
    ClientData clientData;	/* Information about the variable trace. */
    Tcl_Interp *interp;		/* Interpreter containing variable. */
    char *name1;		/* Name of variable or array. */
    char *name2;		/* Name of element within array;  NULL means
				 * scalar variable is being referenced. */
    int flags;			/* OR-ed bits giving operation and other
				 * information. */
{
    TraceVarInfo *tvarPtr = (TraceVarInfo *) clientData;
    char *result;
    int code;
    Interp dummy;
    Tcl_DString cmd;

    result = NULL;
    if (tvarPtr->errMsg != NULL) {
	ckfree(tvarPtr->errMsg);
	tvarPtr->errMsg = NULL;
    }
    if ((tvarPtr->flags & flags) && !(flags & TCL_INTERP_DESTROYED)) {

	/*
	 * Generate a command to execute by appending list elements
	 * for the two variable names and the operation.  The five
	 * extra characters are for three space, the opcode character,
	 * and the terminating null.
	 */

	if (name2 == NULL) {
	    name2 = "";
	}
	Tcl_DStringInit(&cmd);
	Tcl_DStringAppend(&cmd, tvarPtr->command, tvarPtr->length);
	Tcl_DStringAppendElement(&cmd, name1);
	Tcl_DStringAppendElement(&cmd, name2);
	if (flags & TCL_TRACE_READS) {
	    Tcl_DStringAppend(&cmd, " r", 2);
	} else if (flags & TCL_TRACE_WRITES) {
	    Tcl_DStringAppend(&cmd, " w", 2);
	} else if (flags & TCL_TRACE_UNSETS) {
	    Tcl_DStringAppend(&cmd, " u", 2);
	}

	/*
	 * Execute the command.  Be careful to save and restore the
	 * result from the interpreter used for the command.
	 */

	if (interp->freeProc == 0) {
	    dummy.freeProc = (Tcl_FreeProc *) 0;
	    dummy.result = "";
	    Tcl_SetResult((Tcl_Interp *) &dummy, interp->result, TCL_VOLATILE);
	} else {
	    dummy.freeProc = interp->freeProc;
	    dummy.result = interp->result;
	    interp->freeProc = (Tcl_FreeProc *) 0;
	}
	code = Tcl_Eval(interp, Tcl_DStringValue(&cmd));
	Tcl_DStringFree(&cmd);
	if (code != TCL_OK) {
	    tvarPtr->errMsg = ckalloc((unsigned) (strlen(interp->result) + 1));
	    strcpy(tvarPtr->errMsg, interp->result);
	    result = tvarPtr->errMsg;
	    Tcl_ResetResult(interp);		/* Must clear error state. */
	}
	Tcl_SetResult(interp, dummy.result,
		(dummy.freeProc == 0) ? TCL_VOLATILE : dummy.freeProc);
    }
    if (flags & TCL_TRACE_DESTROYED) {
	result = NULL;
	if (tvarPtr->errMsg != NULL) {
	    ckfree(tvarPtr->errMsg);
	}
	ckfree((char *) tvarPtr);
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_WhileCmd --
 *
 *	This procedure is invoked to process the "while" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
int
Tcl_WhileCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int result, value;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " test command\"", (char *) NULL);
	return TCL_ERROR;
    }

    while (1) {
	result = Tcl_ExprBoolean(interp, argv[1], &value);
	if (result != TCL_OK) {
	    return result;
	}
	if (!value) {
	    break;
	}
	result = Tcl_Eval(interp, argv[2]);
	if ((result != TCL_OK) && (result != TCL_CONTINUE)) {
	    if (result == TCL_ERROR) {
		char msg[60];
		sprintf(msg, "\n    (\"while\" body line %d)",
			interp->errorLine);
		Tcl_AddErrorInfo(interp, msg);
	    }
	    break;
	}
    }
    if (result == TCL_BREAK) {
	result = TCL_OK;
    }
    if (result == TCL_OK) {
	Tcl_ResetResult(interp);
    }
    return result;
}
