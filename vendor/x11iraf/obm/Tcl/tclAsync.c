/* 
 * tclAsync.c --
 *
 *	This file provides low-level support needed to invoke signal
 *	handlers in a safe way.  The code here doesn't actually handle
 *	signals, though.  This code is based on proposals made by
 *	Mark Diekhans and Don Libes.
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
static char rcsid[] = "$Header: /user6/ouster/tcl/RCS/tclAsync.c,v 1.3 93/09/02 16:02:42 ouster Exp $ SPRITE (Berkeley)";
#endif /* not lint */

#include "tclInt.h"

/*
 * One of the following structures exists for each asynchronous
 * handler:
 */

typedef struct AsyncHandler {
    int ready;				/* Non-zero means this handler should
					 * be invoked in the next call to
					 * Tcl_AsyncInvoke. */
    struct AsyncHandler *nextPtr;	/* Next in list of all handlers for
					 * the process. */
    Tcl_AsyncProc *proc;		/* Procedure to call when handler
					 * is invoked. */
    ClientData clientData;		/* Value to pass to handler when it
					 * is invoked. */
} AsyncHandler;

/*
 * The variables below maintain a list of all existing handlers.
 */

static AsyncHandler *firstHandler;	/* First handler defined for process,
					 * or NULL if none. */
static AsyncHandler *lastHandler;	/* Last handler or NULL. */

/*
 * The variable below is set to 1 whenever a handler becomes ready and
 * it is cleared to zero whenever Tcl_AsyncInvoke is called.  It can be
 * checked elsewhere in the application to see if Tcl_AsyncInvoke
 * should be invoked.
 */

int tcl_AsyncReady = 0;

/*
 * The variable below indicates whether Tcl_AsyncInvoke is currently
 * working.  If so then we won't set tcl_AsyncReady again until
 * Tcl_AsyncInvoke returns.
 */

static int asyncActive = 0;

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AsyncCreate --
 *
 *	This procedure creates the data structures for an asynchronous
 *	handler, so that no memory has to be allocated when the handler
 *	is activated.
 *
 * Results:
 *	The return value is a token for the handler, which can be used
 *	to activate it later on.
 *
 * Side effects:
 *	Information about the handler is recorded.
 *
 *----------------------------------------------------------------------
 */

Tcl_AsyncHandler
Tcl_AsyncCreate(proc, clientData)
    Tcl_AsyncProc *proc;		/* Procedure to call when handler
					 * is invoked. */
    ClientData clientData;		/* Argument to pass to handler. */
{
    AsyncHandler *asyncPtr;

    asyncPtr = (AsyncHandler *) ckalloc(sizeof(AsyncHandler));
    asyncPtr->ready = 0;
    asyncPtr->nextPtr = NULL;
    asyncPtr->proc = proc;
    asyncPtr->clientData = clientData;
    if (firstHandler == NULL) {
	firstHandler = asyncPtr;
    } else {
	lastHandler->nextPtr = asyncPtr;
    }
    lastHandler = asyncPtr;
    return (Tcl_AsyncHandler) asyncPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AsyncMark --
 *
 *	This procedure is called to request that an asynchronous handler
 *	be invoked as soon as possible.  It's typically called from
 *	an interrupt handler, where it isn't safe to do anything that
 *	depends on or modifies application state.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The handler gets marked for invocation later.
 *
 *----------------------------------------------------------------------
 */

void
Tcl_AsyncMark(async)
    Tcl_AsyncHandler async;		/* Token for handler. */
{
    ((AsyncHandler *) async)->ready = 1;
    if (!asyncActive) {
	tcl_AsyncReady = 1;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AsyncInvoke --
 *
 *	This procedure is called at a "safe" time at background level
 *	to invoke any active asynchronous handlers.
 *
 * Results:
 *	The return value is a normal Tcl result, which is intended to
 *	replace the code argument as the current completion code for
 *	interp.
 *
 * Side effects:
 *	Depends on the handlers that are active.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_AsyncInvoke(interp, code)
    Tcl_Interp *interp;			/* If invoked from Tcl_Eval just after
					 * completing a command, points to
					 * interpreter.  Otherwise it is
					 * NULL. */
    int code; 				/* If interp is non-NULL, this gives
					 * completion code from command that
					 * just completed. */
{
    AsyncHandler *asyncPtr;

    if (tcl_AsyncReady == 0) {
	return code;
    }
    tcl_AsyncReady = 0;
    asyncActive = 1;
    if (interp == NULL) {
	code = 0;
    }

    /*
     * Make one or more passes over the list of handlers, invoking
     * at most one handler in each pass.  After invoking a handler,
     * go back to the start of the list again so that (a) if a new
     * higher-priority handler gets marked while executing a lower
     * priority handler, we execute the higher-priority handler
     * next, and (b) if a handler gets deleted during the execution
     * of a handler, then the list structure may change so it isn't
     * safe to continue down the list anyway.
     */

    while (1) {
	for (asyncPtr = firstHandler; asyncPtr != NULL;
		asyncPtr = asyncPtr->nextPtr) {
	    if (asyncPtr->ready) {
		break;
	    }
	}
	if (asyncPtr == NULL) {
	    break;
	}
	asyncPtr->ready = 0;
	code = (*asyncPtr->proc)(asyncPtr->clientData, interp, code);
    }
    asyncActive = 0;
    return code;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AsyncDelete --
 *
 *	Frees up all the state for an asynchronous handler.  The handler
 *	should never be used again.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The state associated with the handler is deleted.
 *
 *----------------------------------------------------------------------
 */

void
Tcl_AsyncDelete(async)
    Tcl_AsyncHandler async;		/* Token for handler to delete. */
{
    AsyncHandler *asyncPtr = (AsyncHandler *) async;
    AsyncHandler *prevPtr;

    if (firstHandler == asyncPtr) {
	firstHandler = asyncPtr->nextPtr;
	if (firstHandler == NULL) {
	    lastHandler = NULL;
	}
    } else {
	prevPtr = firstHandler;
	while (prevPtr->nextPtr != asyncPtr) {
	    prevPtr = prevPtr->nextPtr;
	}
	prevPtr->nextPtr = asyncPtr->nextPtr;
	if (lastHandler == asyncPtr) {
	    lastHandler = prevPtr;
	}
    }
    ckfree((char *) asyncPtr);
}
