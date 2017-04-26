/* 
 * tclCmdAH.c --
 *
 *	This file contains the top-level command routines for most of
 *	the Tcl built-in commands whose names begin with the letters
 *	A to H.
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
static char rcsid[] = "$Header: /user6/ouster/tcl/RCS/tclCmdAH.c,v 1.93 93/10/28 16:19:20 ouster Exp $ SPRITE (Berkeley)";
#endif

#include "tclInt.h"


/*
 *----------------------------------------------------------------------
 *
 * Tcl_BreakCmd --
 *
 *	This procedure is invoked to process the "break" Tcl command.
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
Tcl_BreakCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    if (argc != 1) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], "\"", (char *) NULL);
	return TCL_ERROR;
    }
    return TCL_BREAK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_CaseCmd --
 *
 *	This procedure is invoked to process the "case" Tcl command.
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
Tcl_CaseCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int i, result;
    int body;
    char *string;
    int caseArgc, splitArgs;
    char **caseArgv;

    if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " string ?in? patList body ... ?default body?\"",
		(char *) NULL);
	return TCL_ERROR;
    }
    string = argv[1];
    body = -1;
    if (strcmp(argv[2], "in") == 0) {
	i = 3;
    } else {
	i = 2;
    }
    caseArgc = argc - i;
    caseArgv = argv + i;

    /*
     * If all of the pattern/command pairs are lumped into a single
     * argument, split them out again.
     */

    splitArgs = 0;
    if (caseArgc == 1) {
	result = Tcl_SplitList(interp, caseArgv[0], &caseArgc, &caseArgv);
	if (result != TCL_OK) {
	    return result;
	}
	splitArgs = 1;
    }

    for (i = 0; i < caseArgc; i += 2) {
	int patArgc, j;
	char **patArgv;
	register char *p;

	if (i == (caseArgc-1)) {
	    interp->result = "extra case pattern with no body";
	    result = TCL_ERROR;
	    goto cleanup;
	}

	/*
	 * Check for special case of single pattern (no list) with
	 * no backslash sequences.
	 */

	for (p = caseArgv[i]; *p != 0; p++) {
	    if (isspace(UCHAR(*p)) || (*p == '\\')) {
		break;
	    }
	}
	if (*p == 0) {
	    if ((*caseArgv[i] == 'd')
		    && (strcmp(caseArgv[i], "default") == 0)) {
		body = i+1;
	    }
	    if (Tcl_StringMatch(string, caseArgv[i])) {
		body = i+1;
		goto match;
	    }
	    continue;
	}

	/*
	 * Break up pattern lists, then check each of the patterns
	 * in the list.
	 */

	result = Tcl_SplitList(interp, caseArgv[i], &patArgc, &patArgv);
	if (result != TCL_OK) {
	    goto cleanup;
	}
	for (j = 0; j < patArgc; j++) {
	    if (Tcl_StringMatch(string, patArgv[j])) {
		body = i+1;
		break;
	    }
	}
	ckfree((char *) patArgv);
	if (j < patArgc) {
	    break;
	}
    }

    match:
    if (body != -1) {
	result = Tcl_Eval(interp, caseArgv[body]);
	if (result == TCL_ERROR) {
	    char msg[100];
	    sprintf(msg, "\n    (\"%.50s\" arm line %d)", caseArgv[body-1],
		    interp->errorLine);
	    Tcl_AddErrorInfo(interp, msg);
	}
	goto cleanup;
    }

    /*
     * Nothing matched:  return nothing.
     */

    result = TCL_OK;

    cleanup:
    if (splitArgs) {
	ckfree((char *) caseArgv);
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_CatchCmd --
 *
 *	This procedure is invoked to process the "catch" Tcl command.
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
Tcl_CatchCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int result;

    if ((argc != 2) && (argc != 3)) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " command ?varName?\"", (char *) NULL);
	return TCL_ERROR;
    }
    result = Tcl_Eval(interp, argv[1]);
    if (argc == 3) {
	if (Tcl_SetVar(interp, argv[2], interp->result, 0) == NULL) {
	    Tcl_SetResult(interp, "couldn't save command result in variable",
		    TCL_STATIC);
	    return TCL_ERROR;
	}
    }
    Tcl_ResetResult(interp);
    sprintf(interp->result, "%d", result);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_ConcatCmd --
 *
 *	This procedure is invoked to process the "concat" Tcl command.
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
Tcl_ConcatCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    if (argc >= 2) {
	interp->result = Tcl_Concat(argc-1, argv+1);
	interp->freeProc = (Tcl_FreeProc *) free;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_ContinueCmd --
 *
 *	This procedure is invoked to process the "continue" Tcl command.
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
Tcl_ContinueCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    if (argc != 1) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		"\"", (char *) NULL);
	return TCL_ERROR;
    }
    return TCL_CONTINUE;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_ErrorCmd --
 *
 *	This procedure is invoked to process the "error" Tcl command.
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
Tcl_ErrorCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    Interp *iPtr = (Interp *) interp;

    if ((argc < 2) || (argc > 4)) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" message ?errorInfo? ?errorCode?\"", (char *) NULL);
	return TCL_ERROR;
    }
    if ((argc >= 3) && (argv[2][0] != 0)) {
	Tcl_AddErrorInfo(interp, argv[2]);
	iPtr->flags |= ERR_ALREADY_LOGGED;
    }
    if (argc == 4) {
	Tcl_SetVar2(interp, "errorCode", (char *) NULL, argv[3],
		TCL_GLOBAL_ONLY);
	iPtr->flags |= ERROR_CODE_SET;
    }
    Tcl_SetResult(interp, argv[1], TCL_VOLATILE);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_EvalCmd --
 *
 *	This procedure is invoked to process the "eval" Tcl command.
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
Tcl_EvalCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int result;
    char *cmd;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" arg ?arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (argc == 2) {
	result = Tcl_Eval(interp, argv[1]);
    } else {
    
	/*
	 * More than one argument:  concatenate them together with spaces
	 * between, then evaluate the result.
	 */
    
	cmd = Tcl_Concat(argc-1, argv+1);
	result = Tcl_Eval(interp, cmd);
	ckfree(cmd);
    }
    if (result == TCL_ERROR) {
	char msg[60];
	sprintf(msg, "\n    (\"eval\" body line %d)", interp->errorLine);
	Tcl_AddErrorInfo(interp, msg);
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_ExprCmd --
 *
 *	This procedure is invoked to process the "expr" Tcl command.
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
Tcl_ExprCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    Tcl_DString buffer;
    int i, result;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" arg ?arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (argc == 2) {
	return Tcl_ExprString(interp, argv[1]);
    }
    Tcl_DStringInit(&buffer);
    Tcl_DStringAppend(&buffer, argv[1], -1);
    for (i = 2; i < argc; i++) {
	Tcl_DStringAppend(&buffer, " ", 1);
	Tcl_DStringAppend(&buffer, argv[i], -1);
    }
    result = Tcl_ExprString(interp, buffer.string);
    Tcl_DStringFree(&buffer);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_ForCmd --
 *
 *	This procedure is invoked to process the "for" Tcl command.
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
Tcl_ForCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int result, value;

    if (argc != 5) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" start test next command\"", (char *) NULL);
	return TCL_ERROR;
    }

    result = Tcl_Eval(interp, argv[1]);
    if (result != TCL_OK) {
	if (result == TCL_ERROR) {
	    Tcl_AddErrorInfo(interp, "\n    (\"for\" initial command)");
	}
	return result;
    }
    while (1) {
	result = Tcl_ExprBoolean(interp, argv[2], &value);
	if (result != TCL_OK) {
	    return result;
	}
	if (!value) {
	    break;
	}
	result = Tcl_Eval(interp, argv[4]);
	if ((result != TCL_OK) && (result != TCL_CONTINUE)) {
	    if (result == TCL_ERROR) {
		char msg[60];
		sprintf(msg, "\n    (\"for\" body line %d)", interp->errorLine);
		Tcl_AddErrorInfo(interp, msg);
	    }
	    break;
	}
	result = Tcl_Eval(interp, argv[3]);
	if (result == TCL_BREAK) {
	    break;
	} else if (result != TCL_OK) {
	    if (result == TCL_ERROR) {
		Tcl_AddErrorInfo(interp, "\n    (\"for\" loop-end command)");
	    }
	    return result;
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

/*
 *----------------------------------------------------------------------
 *
 * Tcl_ForeachCmd --
 *
 *	This procedure is invoked to process the "foreach" Tcl command.
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
Tcl_ForeachCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int listArgc, i, result;
    char **listArgv;

    if (argc != 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" varName list command\"", (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Break the list up into elements, and execute the command once
     * for each value of the element.
     */

    result = Tcl_SplitList(interp, argv[2], &listArgc, &listArgv);
    if (result != TCL_OK) {
	return result;
    }
    for (i = 0; i < listArgc; i++) {
	if (Tcl_SetVar(interp, argv[1], listArgv[i], 0) == NULL) {
	    Tcl_SetResult(interp, "couldn't set loop variable", TCL_STATIC);
	    result = TCL_ERROR;
	    break;
	}

	result = Tcl_Eval(interp, argv[3]);
	if (result != TCL_OK) {
	    if (result == TCL_CONTINUE) {
		result = TCL_OK;
	    } else if (result == TCL_BREAK) {
		result = TCL_OK;
		break;
	    } else if (result == TCL_ERROR) {
		char msg[100];
		sprintf(msg, "\n    (\"foreach\" body line %d)",
			interp->errorLine);
		Tcl_AddErrorInfo(interp, msg);
		break;
	    } else {
		break;
	    }
	}
    }
    ckfree((char *) listArgv);
    if (result == TCL_OK) {
	Tcl_ResetResult(interp);
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_FormatCmd --
 *
 *	This procedure is invoked to process the "format" Tcl command.
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
Tcl_FormatCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    register char *format;	/* Used to read characters from the format
				 * string. */
    char newFormat[40];		/* A new format specifier is generated here. */
    int width;			/* Field width from field specifier, or 0 if
				 * no width given. */
    int precision;		/* Field precision from field specifier, or 0
				 * if no precision given. */
    int size;			/* Number of bytes needed for result of
				 * conversion, based on type of conversion
				 * ("e", "s", etc.) and width from above. */
    char *oneWordValue = NULL;	/* Used to hold value to pass to sprintf, if
				 * it's a one-word value. */
    double twoWordValue;	/* Used to hold value to pass to sprintf if
				 * it's a two-word value. */
    int useTwoWords;		/* 0 means use oneWordValue, 1 means use
				 * twoWordValue. */
    char *dst = interp->result;	/* Where result is stored.  Starts off at
				 * interp->resultSpace, but may get dynamically
				 * re-allocated if this isn't enough. */
    int dstSize = 0;		/* Number of non-null characters currently
				 * stored at dst. */
    int dstSpace = TCL_RESULT_SIZE;
				/* Total amount of storage space available
				 * in dst (not including null terminator. */
    int noPercent;		/* Special case for speed:  indicates there's
				 * no field specifier, just a string to copy. */
    int argIndex;		/* Index of argument to substitute next. */
    int gotXpg = 0;		/* Non-zero means that an XPG3 %n$-style
				 * specifier has been seen. */
    int gotSequential = 0;	/* Non-zero means that a regular sequential
				 * (non-XPG3) conversion specifier has been
				 * seen. */
    int useShort;		/* Value to be printed is short (half word). */
    char *end;			/* Used to locate end of numerical fields. */

    /*
     * This procedure is a bit nasty.  The goal is to use sprintf to
     * do most of the dirty work.  There are several problems:
     * 1. this procedure can't trust its arguments.
     * 2. we must be able to provide a large enough result area to hold
     *    whatever's generated.  This is hard to estimate.
     * 2. there's no way to move the arguments from argv to the call
     *    to sprintf in a reasonable way.  This is particularly nasty
     *    because some of the arguments may be two-word values (doubles).
     * So, what happens here is to scan the format string one % group
     * at a time, making many individual calls to sprintf.
     */

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" formatString ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    argIndex = 2;
    for (format = argv[1]; *format != 0; ) {
	register char *newPtr = newFormat;

	width = precision = useTwoWords = noPercent = useShort = 0;

	/*
	 * Get rid of any characters before the next field specifier.
	 */

	if (*format != '%') {
	    register char *p;

	    oneWordValue = p = format;
	    while ((*format != '%') && (*format != 0)) {
		*p = *format;
		p++;
		format++;
	    }
	    size = p - oneWordValue;
	    noPercent = 1;
	    goto doField;
	}

	if (format[1] == '%') {
	    oneWordValue = format;
	    size = 1;
	    noPercent = 1;
	    format += 2;
	    goto doField;
	}

	/*
	 * Parse off a field specifier, compute how many characters
	 * will be needed to store the result, and substitute for
	 * "*" size specifiers.
	 */

	*newPtr = '%';
	newPtr++;
	format++;
	if (isdigit(UCHAR(*format))) {
	    int tmp;

	    /*
	     * Check for an XPG3-style %n$ specification.  Note: there
	     * must not be a mixture of XPG3 specs and non-XPG3 specs
	     * in the same format string.
	     */

	    tmp = strtoul(format, &end, 10);
	    if (*end != '$') {
		goto notXpg;
	    }
	    format = end+1;
	    gotXpg = 1;
	    if (gotSequential) {
		goto mixedXPG;
	    }
	    argIndex = tmp+1;
	    if ((argIndex < 2) || (argIndex >= argc)) {
		goto badIndex;
	    }
	    goto xpgCheckDone;
	}

	notXpg:
	gotSequential = 1;
	if (gotXpg) {
	    goto mixedXPG;
	}

	xpgCheckDone:
	while ((*format == '-') || (*format == '#') || (*format == '0')
		|| (*format == ' ') || (*format == '+')) {
	    *newPtr = *format;
	    newPtr++;
	    format++;
	}
	if (isdigit(UCHAR(*format))) {
	    width = strtoul(format, &end, 10);
	    format = end;
	} else if (*format == '*') {
	    if (argIndex >= argc) {
		goto badIndex;
	    }
	    if (Tcl_GetInt(interp, argv[argIndex], &width) != TCL_OK) {
		goto fmtError;
	    }
	    argIndex++;
	    format++;
	}
	if (width != 0) {
	    sprintf(newPtr, "%d", width);
	    while (*newPtr != 0) {
		newPtr++;
	    }
	}
	if (*format == '.') {
	    *newPtr = '.';
	    newPtr++;
	    format++;
	}
	if (isdigit(UCHAR(*format))) {
	    precision = strtoul(format, &end, 10);
	    format = end;
	} else if (*format == '*') {
	    if (argIndex >= argc) {
		goto badIndex;
	    }
	    if (Tcl_GetInt(interp, argv[argIndex], &precision) != TCL_OK) {
		goto fmtError;
	    }
	    argIndex++;
	    format++;
	}
	if (precision != 0) {
	    sprintf(newPtr, "%d", precision);
	    while (*newPtr != 0) {
		newPtr++;
	    }
	}
	if (*format == 'l') {
	    format++;
	} else if (*format == 'h') {
	    useShort = 1;
	    *newPtr = 'h';
	    newPtr++;
	    format++;
	}
	*newPtr = *format;
	newPtr++;
	*newPtr = 0;
	if (argIndex >= argc) {
	    goto badIndex;
	}
	switch (*format) {
	    case 'i':
		newPtr[-1] = 'd';
	    case 'd':
	    case 'o':
	    case 'u':
	    case 'x':
	    case 'X':
		if (Tcl_GetInt(interp, argv[argIndex], (int *) &oneWordValue)
			!= TCL_OK) {
		    goto fmtError;
		}
		size = 40;
		break;
	    case 's':
		oneWordValue = argv[argIndex];
		size = strlen(argv[argIndex]);
		break;
	    case 'c':
		if (Tcl_GetInt(interp, argv[argIndex], (int *) &oneWordValue)
			!= TCL_OK) {
		    goto fmtError;
		}
		size = 1;
		break;
	    case 'e':
	    case 'E':
	    case 'f':
	    case 'g':
	    case 'G':
		if (Tcl_GetDouble(interp, argv[argIndex], &twoWordValue)
			!= TCL_OK) {
		    goto fmtError;
		}
		useTwoWords = 1;
		size = 320;
		if (precision > 10) {
		    size += precision;
		}
		break;
	    case 0:
		interp->result =
			"format string ended in middle of field specifier";
		goto fmtError;
	    default:
		sprintf(interp->result, "bad field specifier \"%c\"", *format);
		goto fmtError;
	}
	argIndex++;
	format++;

	/*
	 * Make sure that there's enough space to hold the formatted
	 * result, then format it.
	 */

	doField:
	if (width > size) {
	    size = width;
	}
	if ((dstSize + size) > dstSpace) {
	    char *newDst;
	    int newSpace;

	    newSpace = 2*(dstSize + size);
	    newDst = (char *) ckalloc((unsigned) newSpace+1);
	    if (dstSize != 0) {
		memcpy((VOID *) newDst, (VOID *) dst, dstSize);
	    }
	    if (dstSpace != TCL_RESULT_SIZE) {
		ckfree(dst);
	    }
	    dst = newDst;
	    dstSpace = newSpace;
	}
	if (noPercent) {
	    memcpy((VOID *) (dst+dstSize), (VOID *) oneWordValue, size);
	    dstSize += size;
	    dst[dstSize] = 0;
	} else {
	    if (useTwoWords) {
		sprintf(dst+dstSize, newFormat, twoWordValue);
	    } else if (useShort) {
		/*
		 * The double cast below is needed for a few machines
		 * (e.g. Pyramids as of 1/93) that don't like casts
		 * directly from pointers to shorts.
		 */

		sprintf(dst+dstSize, newFormat, (short) (int) oneWordValue);
	    } else {
		sprintf(dst+dstSize, newFormat, (char *) oneWordValue);
	    }
	    dstSize += strlen(dst+dstSize);
	}
    }

    interp->result = dst;
    if (dstSpace != TCL_RESULT_SIZE) {
	interp->freeProc = (Tcl_FreeProc *) free;
    } else {
	interp->freeProc = 0;
    }
    return TCL_OK;

    mixedXPG:
    interp->result = "cannot mix \"%\" and \"%n$\" conversion specifiers";
    goto fmtError;

    badIndex:
    if (gotXpg) {
	interp->result = "\"%n$\" argument index out of range";
    } else {
	interp->result = "not enough arguments for all format specifiers";
    }

    fmtError:
    if (dstSpace != TCL_RESULT_SIZE) {
	ckfree(dst);
    }
    return TCL_ERROR;
}
