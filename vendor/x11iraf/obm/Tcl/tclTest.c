/* 
 * tclTest.c --
 *
 *	This file contains C command procedures for a bunch of additional
 *	Tcl commands that are used for testing out Tcl's C interfaces.
 *	These commands are not normally included in Tcl applications;
 *	they're only used for testing.
 *
 * Copyright (c) 1993 The Regents of the University of California.
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
static char rcsid[] = "$Header: /user6/ouster/tcl/RCS/tclTest.c,v 1.15 93/09/09 16:46:52 ouster Exp $ SPRITE (Berkeley)";
#endif /* not lint */

#include "tclInt.h"
#include "tclUnix.h"

/*
 * The following variable is a special hack that allows applications
 * to be linked using the procedure "main" from the Tcl library.  The
 * variable generates a reference to "main", which causes main to
 * be brought in from the library (and all of Tcl with it).
 */

extern int main();
int *tclDummyMainPtr = (int *) main;

/*
 * Dynamic string shared by TestdcallCmd and DelCallbackProc;  used
 * to collect the results of the various deletion callbacks.
 */

static Tcl_DString delString;
static Tcl_Interp *delInterp;

/*
 * One of the following structures exists for each asynchronous
 * handler created by the "testasync" command".
 */

typedef struct TestAsyncHandler {
    int id;				/* Identifier for this handler. */
    Tcl_AsyncHandler handler;		/* Tcl's token for the handler. */
    char *command;			/* Command to invoke when the
					 * handler is invoked. */
    struct TestAsyncHandler *nextPtr;	/* Next is list of handlers. */
} TestAsyncHandler;

static TestAsyncHandler *firstHandler = NULL;

/*
 * The variable below is a token for an asynchronous handler for
 * interrupt signals, or NULL if none exists.
 */

static Tcl_AsyncHandler intHandler;

/*
 * The dynamic string below is used by the "testdstring" command
 * to test the dynamic string facilities.
 */

static Tcl_DString dstring;

/*
 * Forward declarations for procedures defined later in this file:
 */

static int		AsyncHandlerProc _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int code));
static void		CmdDelProc1 _ANSI_ARGS_((ClientData clientData));
static void		CmdDelProc2 _ANSI_ARGS_((ClientData clientData));
static int		CmdProc1 _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
static int		CmdProc2 _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
static void		DelCallbackProc _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp));
static int		IntHandlerProc _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int code));
static void		IntProc();
static int		TestasyncCmd _ANSI_ARGS_((ClientData dummy,
			    Tcl_Interp *interp, int argc, char **argv));
static int		TestcmdinfoCmd _ANSI_ARGS_((ClientData dummy,
			    Tcl_Interp *interp, int argc, char **argv));
static int		TestdcallCmd _ANSI_ARGS_((ClientData dummy,
			    Tcl_Interp *interp, int argc, char **argv));
static int		TestdstringCmd _ANSI_ARGS_((ClientData dummy,
			    Tcl_Interp *interp, int argc, char **argv));
static int		TestlinkCmd _ANSI_ARGS_((ClientData dummy,
			    Tcl_Interp *interp, int argc, char **argv));
static int		TestMathFunc _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, Tcl_Value *args,
			    Tcl_Value *resultPtr));

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	Depends on the startup script.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_AppInit(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
    /*
     * Call the init procedures for included packages.  Each call should
     * look like this:
     *
     * if (Mod_Init(interp) == TCL_ERROR) {
     *     return TCL_ERROR;
     * }
     *
     * where "Mod" is the name of the module.
     */

    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    /*
     * Create additional commands and math functions for testing Tcl.
     */

    Tcl_CreateCommand(interp, "testasync", TestasyncCmd, (ClientData) 0,
	    (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "testcmdinfo", TestcmdinfoCmd, (ClientData) 0,
	    (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "testdcall", TestdcallCmd, (ClientData) 0,
	    (Tcl_CmdDeleteProc *) NULL);
    Tcl_DStringInit(&dstring);
    Tcl_CreateCommand(interp, "testdstring", TestdstringCmd, (ClientData) 0,
	    (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "testlink", TestlinkCmd, (ClientData) 0,
	    (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateMathFunc(interp, "T1", 0, (Tcl_ValueType *) NULL, TestMathFunc,
	    (ClientData) 123);
    Tcl_CreateMathFunc(interp, "T2", 0, (Tcl_ValueType *) NULL, TestMathFunc,
	    (ClientData) 345);

    /*
     * Specify a user-specific startup file to invoke if the application
     * is run interactively.  If this line is deleted then no user-specific
     * startup file will be run under any conditions.
     */

    tcl_RcFileName = "~/.tclshrc";
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TestasyncCmd --
 *
 *	This procedure implements the "testasync" command.  It is used
 *	to test the asynchronous handler facilities of Tcl.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Creates, deletes, and invokes handlers.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
static int
TestasyncCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    TestAsyncHandler *asyncPtr, *prevPtr;
    int id, code;
    static int nextId = 1;

    if (argc < 2) {
	wrongNumArgs:
	interp->result = "wrong # args";
	return TCL_ERROR;
    }
    if (strcmp(argv[1], "create") == 0) {
	if (argc != 3) {
	    goto wrongNumArgs;
	}
	asyncPtr = (TestAsyncHandler *) ckalloc(sizeof(TestAsyncHandler));
	asyncPtr->id = nextId;
	nextId++;
	asyncPtr->handler = Tcl_AsyncCreate(AsyncHandlerProc,
		(ClientData) asyncPtr);
	asyncPtr->command = ckalloc((unsigned) (strlen(argv[2]) + 1));
	strcpy(asyncPtr->command, argv[2]);
	asyncPtr->nextPtr = firstHandler;
	firstHandler = asyncPtr;
	sprintf(interp->result, "%d", asyncPtr->id);
    } else if (strcmp(argv[1], "delete") == 0) {
	if (argc == 2) {
	    while (firstHandler != NULL) {
		asyncPtr = firstHandler;
		firstHandler = asyncPtr->nextPtr;
		Tcl_AsyncDelete(asyncPtr->handler);
		ckfree(asyncPtr->command);
		ckfree((char *) asyncPtr);
	    }
	    return TCL_OK;
	}
	if (argc != 3) {
	    goto wrongNumArgs;
	}
	if (Tcl_GetInt(interp, argv[2], &id) != TCL_OK) {
	    return TCL_ERROR;
	}
	for (prevPtr = NULL, asyncPtr = firstHandler; asyncPtr != NULL;
		prevPtr = asyncPtr, asyncPtr = asyncPtr->nextPtr) {
	    if (asyncPtr->id != id) {
		continue;
	    }
	    if (prevPtr == NULL) {
		firstHandler = asyncPtr->nextPtr;
	    } else {
		prevPtr->nextPtr = asyncPtr->nextPtr;
	    }
	    Tcl_AsyncDelete(asyncPtr->handler);
	    ckfree(asyncPtr->command);
	    ckfree((char *) asyncPtr);
	    break;
	}
    } else if (strcmp(argv[1], "int") == 0) {
	if (argc != 2) {
	    goto wrongNumArgs;
	}
	intHandler = Tcl_AsyncCreate(IntHandlerProc, (ClientData) interp);
	signal(SIGINT, IntProc);
    } else if (strcmp(argv[1], "mark") == 0) {
	if (argc != 5) {
	    goto wrongNumArgs;
	}
	if ((Tcl_GetInt(interp, argv[2], &id) != TCL_OK)
		|| (Tcl_GetInt(interp, argv[4], &code) != TCL_OK)) {
	    return TCL_ERROR;
	}
	for (asyncPtr = firstHandler; asyncPtr != NULL;
		asyncPtr = asyncPtr->nextPtr) {
	    if (asyncPtr->id == id) {
		Tcl_AsyncMark(asyncPtr->handler);
		break;
	    }
	}
	Tcl_SetResult(interp, argv[3], TCL_VOLATILE);
	return code;
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
		"\": must be create, delete, int, or mark",
		(char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

static int
AsyncHandlerProc(clientData, interp, code)
    ClientData clientData;	/* Pointer to TestAsyncHandler structure. */
    Tcl_Interp *interp;		/* Interpreter in which command was
				 * executed, or NULL. */
    int code;			/* Current return code from command. */
{
    TestAsyncHandler *asyncPtr = (TestAsyncHandler *) clientData;
    char *listArgv[4];
    char string[20], *cmd;

    sprintf(string, "%d", code);
    listArgv[0] = asyncPtr->command;
    listArgv[1] = interp->result;
    listArgv[2] = string;
    listArgv[3] = NULL;
    cmd = Tcl_Merge(3, listArgv);
    code = Tcl_Eval(interp, cmd);
    ckfree(cmd);
    return code;
}

static void
IntProc()
{
    Tcl_AsyncMark(intHandler);
}

static int
IntHandlerProc(clientData, interp, code)
    ClientData clientData;	/* Interpreter in which to invoke command. */
    Tcl_Interp *interp;		/* Interpreter in which command was
				 * executed, or NULL. */
    int code;			/* Current return code from command. */
{
    char *listArgv[4];
    char string[20], *cmd;

    interp = (Tcl_Interp *) clientData;
    listArgv[0] = Tcl_GetVar(interp, "sigIntCmd", TCL_GLOBAL_ONLY);
    if (listArgv[0] == NULL) {
	return code;
    }
    listArgv[1] = interp->result;
    sprintf(string, "%d", code);
    listArgv[2] = string;
    listArgv[3] = NULL;
    cmd = Tcl_Merge(3, listArgv);
    code = Tcl_Eval(interp, cmd);
    ckfree(cmd);
    return code;
}

/*
 *----------------------------------------------------------------------
 *
 * TestdcallCmd --
 *
 *	This procedure implements the "testdcall" command.  It is used
 *	to test Tcl_CallWhenDeleted.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Creates and deletes interpreters.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
static int
TestdcallCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int i, id;

    delInterp = Tcl_CreateInterp();
    Tcl_DStringInit(&delString);
    for (i = 1; i < argc; i++) {
	if (Tcl_GetInt(interp, argv[i], &id) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (id < 0) {
	    Tcl_DontCallWhenDeleted(delInterp, DelCallbackProc,
		    (ClientData) (-id));
	} else {
	    Tcl_CallWhenDeleted(delInterp, DelCallbackProc,
		    (ClientData) id);
	}
    }
    Tcl_DeleteInterp(delInterp);
    Tcl_DStringResult(interp, &delString);
    return TCL_OK;
}

/*
 * The deletion callback used by TestdcallCmd:
 */

static void
DelCallbackProc(clientData, interp)
    ClientData clientData;		/* Numerical value to append to
					 * delString. */
    Tcl_Interp *interp;			/* Interpreter being deleted. */
{
    int id = (int) clientData;
    char buffer[10];

    sprintf(buffer, "%d", id);
    Tcl_DStringAppendElement(&delString, buffer);
    if (interp != delInterp) {
	Tcl_DStringAppendElement(&delString, "bogus interpreter argument!");
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TestcmdinfoCmd --
 *
 *	This procedure implements the "testcmdinfo" command.  It is used
 *	to test Tcl_GetCmdInfo, Tcl_SetCmdInfo, and command creation
 *	and deletion.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Creates and deletes various commands and modifies their data.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
static int
TestcmdinfoCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    Tcl_CmdInfo info;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" option cmdName\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (strcmp(argv[1], "create") == 0) {
	Tcl_CreateCommand(interp, argv[2], CmdProc1, (ClientData) "original",
		CmdDelProc1);
    } else if (strcmp(argv[1], "delete") == 0) {
	Tcl_DStringInit(&delString);
	Tcl_DeleteCommand(interp, argv[2]);
	Tcl_DStringResult(interp, &delString);
    } else if (strcmp(argv[1], "get") == 0) {
	if (Tcl_GetCommandInfo(interp, argv[2], &info) ==0) {
	    interp->result = "??";
	    return TCL_OK;
	}
	if (info.proc == CmdProc1) {
	    Tcl_AppendResult(interp, "CmdProc1", " ",
		    (char *) info.clientData, (char *) NULL);
	} else if (info.proc == CmdProc2) {
	    Tcl_AppendResult(interp, "CmdProc2", " ",
		    (char *) info.clientData, (char *) NULL);
	} else {
	    Tcl_AppendResult(interp, "unknown", (char *) NULL);
	}
	if (info.deleteProc == CmdDelProc1) {
	    Tcl_AppendResult(interp, " CmdDelProc1", " ",
		    (char *) info.deleteData, (char *) NULL);
	} else if (info.deleteProc == CmdDelProc2) {
	    Tcl_AppendResult(interp, " CmdDelProc2", " ",
		    (char *) info.deleteData, (char *) NULL);
	} else {
	    Tcl_AppendResult(interp, " unknown", (char *) NULL);
	}
    } else if (strcmp(argv[1], "modify") == 0) {
	info.proc = CmdProc2;
	info.clientData = (ClientData) "new_command_data";
	info.deleteProc = CmdDelProc2;
	info.deleteData = (ClientData) "new_delete_data";
	if (Tcl_SetCommandInfo(interp, argv[2], &info) == 0) {
	    interp->result = "0";
	} else {
	    interp->result = "1";
	}
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
		"\": must be create, delete, get, or modify",
		(char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

	/*ARGSUSED*/
static int
CmdProc1(clientData, interp, argc, argv)
    ClientData clientData;		/* String to return. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    Tcl_AppendResult(interp, "CmdProc1 ", (char *) clientData,
	    (char *) NULL);
    return TCL_OK;
}

	/*ARGSUSED*/
static int
CmdProc2(clientData, interp, argc, argv)
    ClientData clientData;		/* String to return. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    Tcl_AppendResult(interp, "CmdProc2 ", (char *) clientData,
	    (char *) NULL);
    return TCL_OK;
}

static void
CmdDelProc1(clientData)
    ClientData clientData;		/* String to save. */
{
    Tcl_DStringInit(&delString);
    Tcl_DStringAppend(&delString, "CmdDelProc1 ", -1);
    Tcl_DStringAppend(&delString, (char *) clientData, -1);
}

static void
CmdDelProc2(clientData)
    ClientData clientData;		/* String to save. */
{
    Tcl_DStringInit(&delString);
    Tcl_DStringAppend(&delString, "CmdDelProc2 ", -1);
    Tcl_DStringAppend(&delString, (char *) clientData, -1);
}

/*
 *----------------------------------------------------------------------
 *
 * TestdstringCmd --
 *
 *	This procedure implements the "testdstring" command.  It is used
 *	to test the dynamic string facilities of Tcl.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Creates, deletes, and invokes handlers.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
static int
TestdstringCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int count;

    if (argc < 2) {
	wrongNumArgs:
	interp->result = "wrong # args";
	return TCL_ERROR;
    }
    if (strcmp(argv[1], "append") == 0) {
	if (argc != 4) {
	    goto wrongNumArgs;
	}
	if (Tcl_GetInt(interp, argv[3], &count) != TCL_OK) {
	    return TCL_ERROR;
	}
	Tcl_DStringAppend(&dstring, argv[2], count);
    } else if (strcmp(argv[1], "element") == 0) {
	if (argc != 3) {
	    goto wrongNumArgs;
	}
	Tcl_DStringAppendElement(&dstring, argv[2]);
    } else if (strcmp(argv[1], "end") == 0) {
	if (argc != 2) {
	    goto wrongNumArgs;
	}
	Tcl_DStringEndSublist(&dstring);
    } else if (strcmp(argv[1], "free") == 0) {
	if (argc != 2) {
	    goto wrongNumArgs;
	}
	Tcl_DStringFree(&dstring);
    } else if (strcmp(argv[1], "get") == 0) {
	if (argc != 2) {
	    goto wrongNumArgs;
	}
	interp->result = Tcl_DStringValue(&dstring);
    } else if (strcmp(argv[1], "length") == 0) {
	if (argc != 2) {
	    goto wrongNumArgs;
	}
	sprintf(interp->result, "%d", Tcl_DStringLength(&dstring));
    } else if (strcmp(argv[1], "result") == 0) {
	if (argc != 2) {
	    goto wrongNumArgs;
	}
	Tcl_DStringResult(interp, &dstring);
    } else if (strcmp(argv[1], "trunc") == 0) {
	if (argc != 3) {
	    goto wrongNumArgs;
	}
	if (Tcl_GetInt(interp, argv[2], &count) != TCL_OK) {
	    return TCL_ERROR;
	}
	Tcl_DStringTrunc(&dstring, count);
    } else if (strcmp(argv[1], "start") == 0) {
	if (argc != 2) {
	    goto wrongNumArgs;
	}
	Tcl_DStringStartSublist(&dstring);
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
		"\": must be append, element, end, free, get, length, ",
		"result, trunc, or start", (char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TestlinkCmd --
 *
 *	This procedure implements the "testlink" command.  It is used
 *	to test Tcl_LinkVar and related library procedures.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Creates and deletes various variable links, plus returns
 *	values of the linked variables.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
static int
TestlinkCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    static int intVar = 43;
    static int boolVar = 4;
    static double realVar = 1.23;
    static char *stringVar = NULL;
    char buffer[TCL_DOUBLE_SPACE];
    int writable, flag;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" option ?arg arg arg?\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (strcmp(argv[1], "create") == 0) {
	if (Tcl_GetBoolean(interp, argv[2], &writable) != TCL_OK) {
	    return TCL_ERROR;
	}
	flag = (writable != 0) ? 0 : TCL_LINK_READ_ONLY;
	if (Tcl_LinkVar(interp, "int", (char *) &intVar,
		TCL_LINK_INT | flag) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (Tcl_GetBoolean(interp, argv[3], &writable) != TCL_OK) {
	    return TCL_ERROR;
	}
	flag = (writable != 0) ? 0 : TCL_LINK_READ_ONLY;
	if (Tcl_LinkVar(interp, "real", (char *) &realVar,
		TCL_LINK_DOUBLE | flag) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (Tcl_GetBoolean(interp, argv[4], &writable) != TCL_OK) {
	    return TCL_ERROR;
	}
	flag = (writable != 0) ? 0 : TCL_LINK_READ_ONLY;
	if (Tcl_LinkVar(interp, "bool", (char *) &boolVar,
		TCL_LINK_BOOLEAN | flag) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (Tcl_GetBoolean(interp, argv[5], &writable) != TCL_OK) {
	    return TCL_ERROR;
	}
	flag = (writable != 0) ? 0 : TCL_LINK_READ_ONLY;
	if (Tcl_LinkVar(interp, "string", (char *) &stringVar,
		TCL_LINK_STRING | flag) != TCL_OK) {
	    return TCL_ERROR;
	}
    } else if (strcmp(argv[1], "delete") == 0) {
	Tcl_UnlinkVar(interp, "int");
	Tcl_UnlinkVar(interp, "real");
	Tcl_UnlinkVar(interp, "bool");
	Tcl_UnlinkVar(interp, "string");
    } else if (strcmp(argv[1], "get") == 0) {
	sprintf(buffer, "%d", intVar);
	Tcl_AppendElement(interp, buffer);
	Tcl_PrintDouble(interp, realVar, buffer);
	Tcl_AppendElement(interp, buffer);
	sprintf(buffer, "%d", boolVar);
	Tcl_AppendElement(interp, buffer);
	Tcl_AppendElement(interp, (stringVar == NULL) ? "-" : stringVar);
    } else if (strcmp(argv[1], "set") == 0) {
	if (argc != 6) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " ", argv[1],
		"intValue realValue boolValue stringValue\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (argv[2][0] != 0) {
	    if (Tcl_GetInt(interp, argv[2], &intVar) != TCL_OK) {
		return TCL_ERROR;
	    }
	}
	if (argv[3][0] != 0) {
	    if (Tcl_GetDouble(interp, argv[3], &realVar) != TCL_OK) {
		return TCL_ERROR;
	    }
	}
	if (argv[4][0] != 0) {
	    if (Tcl_GetInt(interp, argv[4], &boolVar) != TCL_OK) {
		return TCL_ERROR;
	    }
	}
	if (argv[5][0] != 0) {
	    if (stringVar != NULL) {
		ckfree(stringVar);
	    }
	    if (strcmp(argv[5], "-") == 0) {
		stringVar = NULL;
	    } else {
		stringVar = ckalloc((unsigned) (strlen(argv[5]) + 1));
		strcpy(stringVar, argv[5]);
	    }
	}
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
		"\": should be create, delete, get, or set",
		(char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TestMathFunc --
 *
 *	This is a user-defined math procedure to test out math procedures
 *	with no arguments.
 *
 * Results:
 *	A normal Tcl completion code.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
static int
TestMathFunc(clientData, interp, args, resultPtr)
    ClientData clientData;		/* Integer value to return. */
    Tcl_Interp *interp;			/* Not used. */
    Tcl_Value *args;			/* Not used. */
    Tcl_Value *resultPtr;		/* Where to store result. */
{
    resultPtr->type = TCL_INT;
    resultPtr->intValue = (int) clientData;
    return TCL_OK;
}
