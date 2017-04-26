/* 
 * tclUnixUtil.c --
 *
 *	This file contains a collection of utility procedures that
 *	are present in the Tcl's UNIX core but not in the generic
 *	core.  For example, they do file manipulation and process
 *	manipulation.
 *
 *	Parts of this file are based on code contributed by Karl
 *	Lehenbauer, Mark Diekhans and Peter da Silva.
 *
 * Copyright (c) 1991-1993 The Regents of the University of California.
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
static char rcsid[] = "$Header: /user6/ouster/tcl/RCS/tclUnixUtil.c,v 1.45 93/10/23 14:52:10 ouster Exp $ SPRITE (Berkeley)";
#endif /* not lint */

#include "tclInt.h"
#include "tclUnix.h"

/*
 * A linked list of the following structures is used to keep track
 * of child processes that have been detached but haven't exited
 * yet, so we can make sure that they're properly "reaped" (officially
 * waited for) and don't lie around as zombies cluttering the
 * system.
 */

typedef struct Detached {
    int pid;				/* Id of process that's been detached
					 * but isn't known to have exited. */
    struct Detached *nextPtr;		/* Next in list of all detached
					 * processes. */
} Detached;

static Detached *detList = NULL;	/* List of all detached proceses. */

/*
 * The following variables are used to keep track of all the open files
 * in the process.  These files can be shared across interpreters, so the
 * information can't be put in the Interp structure.
 */

int tclNumFiles = 0;		/* Number of entries in tclOpenFiles below.
				 * 0 means array hasn't been created yet. */
OpenFile **tclOpenFiles;	/* Pointer to malloc-ed array of pointers
				 * to information about open files.  Entry
				 * N corresponds to the file with fileno N.
				 * If an entry is NULL then the corresponding
				 * file isn't open.  If tclOpenFiles is NULL
				 * it means no files have been used, so even
				 * stdin/stdout/stderr entries haven't been
				 * setup yet. */

/*
 * Declarations for local procedures defined in this file:
 */

static int		FileForRedirect _ANSI_ARGS_((Tcl_Interp *interp,
			    char *spec, int atOk, char *arg, int flags,
			    char *nextArg, int *skipPtr, int *closePtr));
static void		MakeFileTable _ANSI_ARGS_((Interp *iPtr, int index));
static void		RestoreSignals _ANSI_ARGS_((void));

/*
 *----------------------------------------------------------------------
 *
 * Tcl_EvalFile --
 *
 *	Read in a file and process the entire file as one gigantic
 *	Tcl command.
 *
 * Results:
 *	A standard Tcl result, which is either the result of executing
 *	the file or an error indicating why the file couldn't be read.
 *
 * Side effects:
 *	Depends on the commands in the file.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_EvalFile(interp, fileName)
    Tcl_Interp *interp;		/* Interpreter in which to process file. */
    char *fileName;		/* Name of file to process.  Tilde-substitution
				 * will be performed on this name. */
{
    int fileId, result;
    struct stat statBuf;
    char *cmdBuffer, *oldScriptFile;
    Interp *iPtr = (Interp *) interp;
    Tcl_DString buffer;

    oldScriptFile = iPtr->scriptFile;
    iPtr->scriptFile = fileName;
    fileName = Tcl_TildeSubst(interp, fileName, &buffer);
    if (fileName == NULL) {
	goto error;
    }
    fileId = open(fileName, O_RDONLY, 0);
    if (fileId < 0) {
	Tcl_AppendResult(interp, "couldn't read file \"", fileName,
		"\": ", Tcl_PosixError(interp), (char *) NULL);
	goto error;
    }
    if (fstat(fileId, &statBuf) == -1) {
	Tcl_AppendResult(interp, "couldn't stat file \"", fileName,
		"\": ", Tcl_PosixError(interp), (char *) NULL);
	close(fileId);
	goto error;
    }
    cmdBuffer = (char *) ckalloc((unsigned) statBuf.st_size+1);
    if (read(fileId, cmdBuffer, (size_t) statBuf.st_size) != statBuf.st_size) {
	Tcl_AppendResult(interp, "error in reading file \"", fileName,
		"\": ", Tcl_PosixError(interp), (char *) NULL);
	close(fileId);
	ckfree(cmdBuffer);
	goto error;
    }
    if (close(fileId) != 0) {
	Tcl_AppendResult(interp, "error closing file \"", fileName,
		"\": ", Tcl_PosixError(interp), (char *) NULL);
	ckfree(cmdBuffer);
	goto error;
    }
    cmdBuffer[statBuf.st_size] = 0;
    result = Tcl_Eval(interp, cmdBuffer);
    if (result == TCL_RETURN) {
	result = TCL_OK;
    }
    if (result == TCL_ERROR) {
	char msg[200];

	/*
	 * Record information telling where the error occurred.
	 */

	sprintf(msg, "\n    (file \"%.150s\" line %d)", fileName,
		interp->errorLine);
	Tcl_AddErrorInfo(interp, msg);
    }
    ckfree(cmdBuffer);
    iPtr->scriptFile = oldScriptFile;
    Tcl_DStringFree(&buffer);
    return result;

    error:
    iPtr->scriptFile = oldScriptFile;
    Tcl_DStringFree(&buffer);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_DetachPids --
 *
 *	This procedure is called to indicate that one or more child
 *	processes have been placed in background and will never be
 *	waited for;  they should eventually be reaped by
 *	Tcl_ReapDetachedProcs.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

void
Tcl_DetachPids(numPids, pidPtr)
    int numPids;		/* Number of pids to detach:  gives size
				 * of array pointed to by pidPtr. */
    int *pidPtr;		/* Array of pids to detach. */
{
    register Detached *detPtr;
    int i;

    for (i = 0; i < numPids; i++) {
	detPtr = (Detached *) ckalloc(sizeof(Detached));
	detPtr->pid = pidPtr[i];
	detPtr->nextPtr = detList;
	detList = detPtr;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_ReapDetachedProcs --
 *
 *	This procedure checks to see if any detached processes have
 *	exited and, if so, it "reaps" them by officially waiting on
 *	them.  It should be called "occasionally" to make sure that
 *	all detached processes are eventually reaped.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Processes are waited on, so that they can be reaped by the
 *	system.
 *
 *----------------------------------------------------------------------
 */

void
Tcl_ReapDetachedProcs()
{
    register Detached *detPtr;
    Detached *nextPtr, *prevPtr;
    int status, result;

    for (detPtr = detList, prevPtr = NULL; detPtr != NULL; ) {
	result = waitpid(detPtr->pid, &status, WNOHANG);
	if ((result == 0) || ((result == -1) && (errno != ECHILD))) {
	    prevPtr = detPtr;
	    detPtr = detPtr->nextPtr;
	    continue;
	}
	nextPtr = detPtr->nextPtr;
	if (prevPtr == NULL) {
	    detList = detPtr->nextPtr;
	} else {
	    prevPtr->nextPtr = detPtr->nextPtr;
	}
	ckfree((char *) detPtr);
	detPtr = nextPtr;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_CreatePipeline --
 *
 *	Given an argc/argv array, instantiate a pipeline of processes
 *	as described by the argv.
 *
 * Results:
 *	The return value is a count of the number of new processes
 *	created, or -1 if an error occurred while creating the pipeline.
 *	*pidArrayPtr is filled in with the address of a dynamically
 *	allocated array giving the ids of all of the processes.  It
 *	is up to the caller to free this array when it isn't needed
 *	anymore.  If inPipePtr is non-NULL, *inPipePtr is filled in
 *	with the file id for the input pipe for the pipeline (if any):
 *	the caller must eventually close this file.  If outPipePtr
 *	isn't NULL, then *outPipePtr is filled in with the file id
 *	for the output pipe from the pipeline:  the caller must close
 *	this file.  If errFilePtr isn't NULL, then *errFilePtr is filled
 *	with a file id that may be used to read error output after the
 *	pipeline completes.
 *
 * Side effects:
 *	Processes and pipes are created.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_CreatePipeline(interp, argc, argv, pidArrayPtr, inPipePtr,
	outPipePtr, errFilePtr)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting. */
    int argc;			/* Number of entries in argv. */
    char **argv;		/* Array of strings describing commands in
				 * pipeline plus I/O redirection with <,
				 * <<,  >, etc.  Argv[argc] must be NULL. */
    int **pidArrayPtr;		/* Word at *pidArrayPtr gets filled in with
				 * address of array of pids for processes
				 * in pipeline (first pid is first process
				 * in pipeline). */
    int *inPipePtr;		/* If non-NULL, input to the pipeline comes
				 * from a pipe (unless overridden by
				 * redirection in the command).  The file
				 * id with which to write to this pipe is
				 * stored at *inPipePtr.  -1 means command
				 * specified its own input source. */
    int *outPipePtr;		/* If non-NULL, output to the pipeline goes
				 * to a pipe, unless overriden by redirection
				 * in the command.  The file id with which to
				 * read frome this pipe is stored at
				 * *outPipePtr.  -1 means command specified
				 * its own output sink. */
    int *errFilePtr;		/* If non-NULL, all stderr output from the
				 * pipeline will go to a temporary file
				 * created here, and a descriptor to read
				 * the file will be left at *errFilePtr.
				 * The file will be removed already, so
				 * closing this descriptor will be the end
				 * of the file.  If this is NULL, then
				 * all stderr output goes to our stderr.
				 * If the pipeline specifies redirection
				 * then the fill will still be created
				 * but it will never get any data. */
{
    int *pidPtr = NULL;		/* Points to malloc-ed array holding all
				 * the pids of child processes. */
    int numPids = 0;		/* Actual number of processes that exist
				 * at *pidPtr right now. */
    int cmdCount;		/* Count of number of distinct commands
				 * found in argc/argv. */
    char *input = NULL;		/* If non-null, then this points to a
				 * string containing input data (specified
				 * via <<) to be piped to the first process
				 * in the pipeline. */
    int inputId = -1;		/* If >= 0, gives file id to use as input for
				 * first process in pipeline (specified via
				 * < or <@). */
    int closeInput = 0;		/* If non-zero, then must close inputId
				 * when cleaning up (zero means the file needs
				 * to stay open for some other reason). */
    int outputId = -1;		/* Writable file id for output from last
				 * command in pipeline (could be file or pipe).
				 * -1 means use stdout. */
    int closeOutput = 0;	/* Non-zero means must close outputId when
				 * cleaning up (similar to closeInput). */
    int errorId = -1;		/* Writable file id for error output from
				 * all commands in pipeline. -1 means use
				 * stderr. */
    int closeError = 0;		/* Non-zero means must close errorId when
				 * cleaning up. */
    int pipeIds[2];		/* File ids for pipe that's being created. */
    int firstArg, lastArg;	/* Indexes of first and last arguments in
				 * current command. */
    int skip;			/* Number of arguments to skip (because they
				 * specify redirection). */
    int maxFd;			/* Highest known file descriptor (used to
				 * close off extraneous file descriptors in
				 * child process). */
    int lastBar;
    char *execName;
    int i, j, pid;
    char *p;
    Tcl_DString buffer;

    if (inPipePtr != NULL) {
	*inPipePtr = -1;
    }
    if (outPipePtr != NULL) {
	*outPipePtr = -1;
    }
    if (errFilePtr != NULL) {
	*errFilePtr = -1;
    }
    pipeIds[0] = pipeIds[1] = -1;

    /*
     * First, scan through all the arguments to figure out the structure
     * of the pipeline.  Process all of the input and output redirection
     * arguments and remove them from the argument list in the pipeline.
     * Count the number of distinct processes (it's the number of "|"
     * arguments plus one) but don't remove the "|" arguments.
     */

    cmdCount = 1;
    lastBar = -1;
    for (i = 0; i < argc; i++) {
	if ((argv[i][0] == '|') && (((argv[i][1] == 0))
		|| ((argv[i][1] == '&') && (argv[i][2] == 0)))) {
	    if ((i == (lastBar+1)) || (i == (argc-1))) {
		interp->result = "illegal use of | or |& in command";
		return -1;
	    }
	    lastBar = i;
	    cmdCount++;
	    continue;
	} else if (argv[i][0] == '<') {
	    if ((inputId >= 0) && closeInput) {
		close(inputId);
	    }
	    inputId = -1;
	    skip = 1;
	    if (argv[i][1] == '<') {
		input = argv[i]+2;
		if (*input == 0) {
		    input = argv[i+1];
		    if (input == 0) {
			Tcl_AppendResult(interp, "can't specify \"", argv[i],
				"\" as last word in command", (char *) NULL);
			goto error;
		    }
		    skip = 2;
		}
	    } else {
		input = 0;
		inputId = FileForRedirect(interp, argv[i]+1, 1, argv[i],
			O_RDONLY, argv[i+1], &skip, &closeInput);
		if (inputId < 0) {
		    goto error;
		}
	    }
	} else if (argv[i][0] == '>') {
	    int append, useForStdErr, useForStdOut, mustClose, fd, atOk, flags;

	    skip = atOk = 1;
	    append = useForStdErr = 0;
	    useForStdOut = 1;
	    if (argv[i][1] == '>') {
		p = argv[i] + 2;
		append = 1;
		atOk = 0;
		flags = O_WRONLY|O_CREAT;
	    } else {
		p = argv[i] + 1;
		flags = O_WRONLY|O_CREAT|O_TRUNC;
	    }
	    if (*p == '&') {
		useForStdErr = 1;
		p++;
	    }
	    fd = FileForRedirect(interp, p, atOk, argv[i], flags, argv[i+1],
		    &skip, &mustClose);
	    if (fd < 0) {
		goto error;
	    }
	    if (append) {
		lseek(fd, 0L, 2);
	    }

	    /*
	     * Got the file descriptor.  Now use it for standard output,
	     * standard error, or both, depending on the redirection.
	     */

	    if (useForStdOut) {
		if ((outputId > 0) && closeOutput) {
		    close(outputId);
		}
		outputId = fd;
		closeOutput = mustClose;
	    }
	    if (useForStdErr) {
		if ((errorId > 0) && closeError) {
		    close(errorId);
		}
		errorId = fd;
		closeError = (useForStdOut) ? 0 : mustClose;
	    }
	} else if ((argv[i][0] == '2') && (argv[i][1] == '>')) {
	    int append, atOk, flags;

	    if ((errorId > 0) && closeError) {
		close(errorId);
	    }
	    skip = 1;
	    p = argv[i] + 2;
	    if (*p == '>') {
		p++;
		append = 1;
		atOk = 0;
		flags = O_WRONLY|O_CREAT;
	    } else {
		append = 0;
		atOk = 1;
		flags = O_WRONLY|O_CREAT|O_TRUNC;
	    }
	    errorId = FileForRedirect(interp, p, atOk, argv[i], flags,
		    argv[i+1], &skip, &closeError);
	    if (errorId < 0) {
		goto error;
	    }
	    if (append) {
		lseek(errorId, 0L, 2);
	    }
	} else {
	    continue;
	}
	for (j = i+skip; j < argc; j++) {
	    argv[j-skip] = argv[j];
	}
	argc -= skip;
	i -= 1;			/* Process next arg from same position. */
    }
    if (argc == 0) {
	interp->result =  "didn't specify command to execute";
	return -1;
    }

    if (inputId < 0) {
	if (input != NULL) {
	    char inName[L_tmpnam];
	    int length;

	    /*
	     * The input for the first process is immediate data coming from
	     * Tcl.  Create a temporary file for it and put the data into the
	     * file.
	     */

#ifdef linux
	    mkstemp(inName);
#else
	    tmpnam(inName);
#endif
	    inputId = open(inName, O_RDWR|O_CREAT|O_TRUNC, 0600);
	    closeInput = 1;
	    if (inputId < 0) {
		Tcl_AppendResult(interp,
			"couldn't create input file for command: ",
			Tcl_PosixError(interp), (char *) NULL);
		goto error;
	    }
	    length = strlen(input);
	    if (write(inputId, input, (size_t) length) != length) {
		Tcl_AppendResult(interp,
			"couldn't write file input for command: ",
			Tcl_PosixError(interp), (char *) NULL);
		goto error;
	    }
	    if ((lseek(inputId, 0L, 0) == -1) || (unlink(inName) == -1)) {
		Tcl_AppendResult(interp,
			"couldn't reset or remove input file for command: ",
			Tcl_PosixError(interp), (char *) NULL);
		goto error;
	    }
	} else if (inPipePtr != NULL) {
	    /*
	     * The input for the first process in the pipeline is to
	     * come from a pipe that can be written from this end.
	     */

	    if (pipe(pipeIds) != 0) {
		Tcl_AppendResult(interp,
			"couldn't create input pipe for command: ",
			Tcl_PosixError(interp), (char *) NULL);
		goto error;
	    }
	    inputId = pipeIds[0];
	    closeInput = 1;
	    *inPipePtr = pipeIds[1];
	    pipeIds[0] = pipeIds[1] = -1;
	}
    }

    /*
     * Set up a pipe to receive output from the pipeline, if no other
     * output sink has been specified.
     */

    if ((outputId < 0) && (outPipePtr != NULL)) {
	if (pipe(pipeIds) != 0) {
	    Tcl_AppendResult(interp,
		    "couldn't create output pipe: ",
		    Tcl_PosixError(interp), (char *) NULL);
	    goto error;
	}
	outputId = pipeIds[1];
	closeOutput = 1;
	*outPipePtr = pipeIds[0];
	pipeIds[0] = pipeIds[1] = -1;
    }

    /*
     * Set up the standard error output sink for the pipeline, if
     * requested.  Use a temporary file which is opened, then deleted.
     * Could potentially just use pipe, but if it filled up it could
     * cause the pipeline to deadlock:  we'd be waiting for processes
     * to complete before reading stderr, and processes couldn't complete
     * because stderr was backed up.
     */

    if (errFilePtr != NULL) {
	char errName[L_tmpnam];

#ifdef linux
	mkstemp(errName);
#else
	tmpnam(errName);
#endif
	*errFilePtr = open(errName, O_RDONLY|O_CREAT|O_TRUNC, 0600);
	if (*errFilePtr < 0) {
	    errFileError:
	    Tcl_AppendResult(interp,
		    "couldn't create error file for command: ",
		    Tcl_PosixError(interp), (char *) NULL);
	    goto error;
	}
	if (errorId < 0) {
	    errorId = open(errName, O_WRONLY|O_CREAT|O_TRUNC, 0600);
	    if (errorId < 0) {
		goto errFileError;
	    }
	    closeError = 1;
	}
	if (unlink(errName) == -1) {
	    Tcl_AppendResult(interp,
		    "couldn't remove error file for command: ",
		    Tcl_PosixError(interp), (char *) NULL);
	    goto error;
	}
    }

    /*
     * Find the largest file descriptor used so far, so that we can
     * clean up all the extraneous file descriptors in the child
     * processes we create.
     */

    maxFd = inputId;
    if (outputId > maxFd) {
	maxFd = outputId;
    }
    if (errorId > maxFd) {
	maxFd = errorId;
    }
    if ((inPipePtr != NULL) && (*inPipePtr > maxFd)) {
	maxFd = *inPipePtr;
    }
    if ((outPipePtr != NULL) && (*outPipePtr > maxFd)) {
	maxFd = *outPipePtr;
    }
    if ((errFilePtr != NULL) && (*errFilePtr > maxFd)) {
	maxFd = *errFilePtr;
    }

    /*
     * Scan through the argc array, forking off a process for each
     * group of arguments between "|" arguments.
     */

    pidPtr = (int *) ckalloc((unsigned) (cmdCount * sizeof(int)));
    for (i = 0; i < numPids; i++) {
	pidPtr[i] = -1;
    }
    Tcl_ReapDetachedProcs();
    for (firstArg = 0; firstArg < argc; numPids++, firstArg = lastArg+1) {
	int joinThisError;
	int curOutputId;

	joinThisError = 0;
	for (lastArg = firstArg; lastArg < argc; lastArg++) {
	    if (argv[lastArg][0] == '|') {
		if (argv[lastArg][1] == 0) {
		    break;
		}
		if ((argv[lastArg][1] == '&') && (argv[lastArg][2] == 0)) {
		    joinThisError = 1;
		    break;
		}
	    }
	}
	argv[lastArg] = NULL;
	if (lastArg == argc) {
	    curOutputId = outputId;
	} else {
	    if (pipe(pipeIds) != 0) {
		Tcl_AppendResult(interp, "couldn't create pipe: ",
			Tcl_PosixError(interp), (char *) NULL);
		goto error;
	    }
	    curOutputId = pipeIds[1];
	    if (pipeIds[0] > maxFd) {
		maxFd = pipeIds[0];
	    }
	    if (pipeIds[1] > maxFd) {
		maxFd = pipeIds[1];
	    }
	}
	execName = Tcl_TildeSubst(interp, argv[firstArg], &buffer);
	pid = fork();
	if (pid == 0) {
	    char errSpace[200];

	    if (((inputId != -1) && (dup2(inputId, 0) == -1))
		    || ((curOutputId != -1) && (dup2(curOutputId, 1) == -1))
		    || (joinThisError && (dup2(1, 2) == -1))
		    || (!joinThisError && (errorId != -1)
			    && (dup2(errorId, 2) == -1))) {
		char *err;
		err = "forked process couldn't set up input/output\n";
		write(errorId < 0 ? 2 : errorId, err, (size_t) strlen(err));
		_exit(1);
	    }
	    for (i = 3; i <= maxFd; i++) {
		close(i);
	    }
	    RestoreSignals();
	    execvp(execName, &argv[firstArg]);
	    sprintf(errSpace, "couldn't find \"%.150s\" to execute\n",
		    argv[firstArg]);
	    write(2, errSpace, (size_t) strlen(errSpace));
	    _exit(1);
	}
	Tcl_DStringFree(&buffer);
	if (pid == -1) {
	    Tcl_AppendResult(interp, "couldn't fork child process: ",
		    Tcl_PosixError(interp), (char *) NULL);
	    goto error;
	}
	pidPtr[numPids] = pid;

	/*
	 * Close off our copies of file descriptors that were set up for
	 * this child, then set up the input for the next child.
	 */

	if ((inputId != -1) && closeInput) {
	    close(inputId);
	}
	if ((curOutputId != -1) && (curOutputId != outputId)) {
	    close(curOutputId);
	}
	inputId = pipeIds[0];
	closeInput = 1;
	pipeIds[0] = pipeIds[1] = -1;
    }
    *pidArrayPtr = pidPtr;

    /*
     * All done.  Cleanup open files lying around and then return.
     */

cleanup:
    if ((inputId != -1) && closeInput) {
	close(inputId);
    }
    if ((outputId != -1) && closeOutput) {
	close(outputId);
    }
    if ((errorId != -1) && closeError) {
	close(errorId);
    }
    return numPids;

    /*
     * An error occurred.  There could have been extra files open, such
     * as pipes between children.  Clean them all up.  Detach any child
     * processes that have been created.
     */

    error:
    if ((inPipePtr != NULL) && (*inPipePtr != -1)) {
	close(*inPipePtr);
	*inPipePtr = -1;
    }
    if ((outPipePtr != NULL) && (*outPipePtr != -1)) {
	close(*outPipePtr);
	*outPipePtr = -1;
    }
    if ((errFilePtr != NULL) && (*errFilePtr != -1)) {
	close(*errFilePtr);
	*errFilePtr = -1;
    }
    if (pipeIds[0] != -1) {
	close(pipeIds[0]);
    }
    if (pipeIds[1] != -1) {
	close(pipeIds[1]);
    }
    if (pidPtr != NULL) {
	for (i = 0; i < numPids; i++) {
	    if (pidPtr[i] != -1) {
		Tcl_DetachPids(1, &pidPtr[i]);
	    }
	}
	ckfree((char *) pidPtr);
    }
    numPids = -1;
    goto cleanup;
}

/*
 *----------------------------------------------------------------------
 *
 * FileForRedirect --
 *
 *	This procedure does much of the work of parsing redirection
 *	operators.  It handles "@" if specified and allowed, and a file
 *	name, and opens the file if necessary.
 *
 * Results:
 *	The return value is the descriptor number for the file.  If an
 *	error occurs then -1 is returned and an error message is left
 *	in interp->result.  Several arguments are side-effected; see
 *	the argument list below for details.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
FileForRedirect(interp, spec, atOk, arg, flags, nextArg, skipPtr, closePtr)
    Tcl_Interp *interp;			/* Intepreter to use for error
					 * reporting. */
    register char *spec;			/* Points to character just after
					 * redirection character. */
    int atOk;				/* Non-zero means '@' notation is
					 * OK, zero means it isn't. */
    char *arg;				/* Pointer to entire argument
					 * containing spec:  used for error
					 * reporting. */
    int flags;				/* Flags to use for opening file. */
    char *nextArg;			/* Next argument in argc/argv
					 * array, if needed for file name.
					 * May be NULL. */
    int *skipPtr;			/* This value is incremented if
					 * nextArg is used for redirection
					 * spec. */
    int *closePtr;			/* This value is set to 1 if the file
					 * that's returned must be closed, 0
					 * if it was specified with "@" so
					 * it must be left open. */
{
    int writing = (flags & O_WRONLY);
    FILE *f;
    int fd;

    if (atOk && (*spec == '@')) {
	spec++;
	if (*spec == 0) {
	    spec = nextArg;
	    if (spec == NULL) {
		goto badLastArg;
	    }
	    *skipPtr += 1;
	}
	if (Tcl_GetOpenFile(interp, spec, writing, 1, &f) != TCL_OK) {
	    return -1;
	}
	*closePtr = 0;
	fd = fileno(f);
    } else {
	if (*spec == 0) {
	    spec = nextArg;
	    if (spec == NULL) {
		goto badLastArg;
	    }
	    *skipPtr += 1;
	}
	fd = open(spec, flags, 0666);
	if (fd < 0) {
	    Tcl_AppendResult(interp, "couldn't ",
		    (writing) ? "write" : "read", " file \"", spec, "\": ",
		    Tcl_PosixError(interp), (char *) NULL);
	    return -1;
	}
	*closePtr = 1;
    }
    return fd;

    badLastArg:
    Tcl_AppendResult(interp, "can't specify \"", arg,
	    "\" as last word in command", (char *) NULL);
    return -1;
}

/*
 *----------------------------------------------------------------------
 *
 * RestoreSignals --
 *
 *	This procedure is invoked in a forked child process just before
 *	exec-ing a new program to restore all signals to their default
 *	settings.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Signal settings get changed.
 *
 *----------------------------------------------------------------------
 */

static void
RestoreSignals()
{
#ifdef SIGABRT
    signal(SIGABRT, SIG_DFL);
#endif
#ifdef SIGALRM
    signal(SIGALRM, SIG_DFL);
#endif
#ifdef SIGFPE
    signal(SIGFPE, SIG_DFL);
#endif
#ifdef SIGHUP
    signal(SIGHUP, SIG_DFL);
#endif
#ifdef SIGILL
    signal(SIGILL, SIG_DFL);
#endif
#ifdef SIGINT
    signal(SIGINT, SIG_DFL);
#endif
#ifdef SIGPIPE
    signal(SIGPIPE, SIG_DFL);
#endif
#ifdef SIGQUIT
    signal(SIGQUIT, SIG_DFL);
#endif
#ifdef SIGSEGV
    signal(SIGSEGV, SIG_DFL);
#endif
#ifdef SIGTERM
    signal(SIGTERM, SIG_DFL);
#endif
#ifdef SIGUSR1
    signal(SIGUSR1, SIG_DFL);
#endif
#ifdef SIGUSR2
    signal(SIGUSR2, SIG_DFL);
#endif
#ifdef SIGCHLD
    signal(SIGCHLD, SIG_DFL);
#endif
#ifdef SIGCONT
    signal(SIGCONT, SIG_DFL);
#endif
#ifdef SIGTSTP
    signal(SIGTSTP, SIG_DFL);
#endif
#ifdef SIGTTIN
    signal(SIGTTIN, SIG_DFL);
#endif
#ifdef SIGTTOU
    signal(SIGTTOU, SIG_DFL);
#endif
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_PosixError --
 *
 *	This procedure is typically called after UNIX kernel calls
 *	return errors.  It stores machine-readable information about
 *	the error in $errorCode returns an information string for
 *	the caller's use.
 *
 * Results:
 *	The return value is a human-readable string describing the
 *	error, as returned by strerror.
 *
 * Side effects:
 *	The global variable $errorCode is reset.
 *
 *----------------------------------------------------------------------
 */

char *
Tcl_PosixError(interp)
    Tcl_Interp *interp;		/* Interpreter whose $errorCode variable
				 * is to be changed. */
{
    char *id, *msg;

    id = Tcl_ErrnoId();
    msg = strerror(errno);
    Tcl_SetErrorCode(interp, "POSIX", id, msg, (char *) NULL);
    return msg;
}

/*
 *----------------------------------------------------------------------
 *
 * MakeFileTable --
 *
 *	Create or enlarge the file table for the interpreter, so that
 *	there is room for a given index.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The file table for iPtr will be created if it doesn't exist
 *	(and entries will be added for stdin, stdout, and stderr).
 *	If it already exists, then it will be grown if necessary.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
static void
MakeFileTable(iPtr, index)
    Interp *iPtr;		/* Interpreter whose table of files is
				 * to be manipulated. */
    int index;			/* Make sure table is large enough to
				 * hold at least this index. */
{
    /*
     * If the table doesn't even exist, then create it and initialize
     * entries for standard files.
     */

    if (tclNumFiles == 0) {
	OpenFile *oFilePtr;
	int i;

	if (index < 2) {
	    tclNumFiles = 3;
	} else {
	    tclNumFiles = index+1;
	}
	tclOpenFiles = (OpenFile **) ckalloc((unsigned)
		((tclNumFiles)*sizeof(OpenFile *)));
	for (i = tclNumFiles-1; i >= 0; i--) {
	    tclOpenFiles[i] = NULL;
	}

	oFilePtr = (OpenFile *) ckalloc(sizeof(OpenFile));
	oFilePtr->f = stdin;
	oFilePtr->f2 = NULL;
	oFilePtr->permissions = TCL_FILE_READABLE;
	oFilePtr->numPids = 0;
	oFilePtr->pidPtr = NULL;
	oFilePtr->errorId = -1;
	tclOpenFiles[0] = oFilePtr;

	oFilePtr = (OpenFile *) ckalloc(sizeof(OpenFile));
	oFilePtr->f = stdout;
	oFilePtr->f2 = NULL;
	oFilePtr->permissions = TCL_FILE_WRITABLE;
	oFilePtr->numPids = 0;
	oFilePtr->pidPtr = NULL;
	oFilePtr->errorId = -1;
	tclOpenFiles[1] = oFilePtr;

	oFilePtr = (OpenFile *) ckalloc(sizeof(OpenFile));
	oFilePtr->f = stderr;
	oFilePtr->f2 = NULL;
	oFilePtr->permissions = TCL_FILE_WRITABLE;
	oFilePtr->numPids = 0;
	oFilePtr->pidPtr = NULL;
	oFilePtr->errorId = -1;
	tclOpenFiles[2] = oFilePtr;
    } else if (index >= tclNumFiles) {
	int newSize;
	OpenFile **newPtrArray;
	int i;

	newSize = index+1;
	newPtrArray = (OpenFile **) ckalloc((unsigned)
		((newSize)*sizeof(OpenFile *)));
	memcpy((VOID *) newPtrArray, (VOID *) tclOpenFiles,
		tclNumFiles*sizeof(OpenFile *));
	for (i = tclNumFiles; i < newSize; i++) {
	    newPtrArray[i] = NULL;
	}
	ckfree((char *) tclOpenFiles);
	tclNumFiles = newSize;
	tclOpenFiles = newPtrArray;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_EnterFile --
 *
 *	This procedure is used to enter an already-open file into the
 *	file table for an interpreter so that the file can be read
 *	and written with Tcl commands.
 *
 * Results:
 *	There is no return value, but interp->result is set to
 *	hold Tcl's id for the open file, such as "file4".
 *
 * Side effects:
 *	"File" is added to the files accessible from interp.
 *
 *----------------------------------------------------------------------
 */

void
Tcl_EnterFile(interp, file, permissions)
    Tcl_Interp *interp;		/* Interpreter in which to make file
				 * available. */
    FILE *file;			/* File to make available in interp. */
    int permissions;		/* Ops that may be done on file:  OR-ed
				 * combinination of TCL_FILE_READABLE and
				 * TCL_FILE_WRITABLE. */
{
    Interp *iPtr = (Interp *) interp;
    int fd;
    register OpenFile *oFilePtr;

    fd = fileno(file);
    if (fd >= tclNumFiles) {
	MakeFileTable(iPtr, fd);
    }
    oFilePtr = tclOpenFiles[fd];

    /*
     * It's possible that there already appears to be a file open in
     * the slot.  This could happen, for example, if the application
     * closes a file behind our back so that we don't have a chance
     * to clean up.  This is probably a bad idea, but if it happens
     * just discard the information in the old record (hopefully the
     * application is smart enough to have really cleaned everything
     * up right).
     */

    if (oFilePtr == NULL) {
	oFilePtr = (OpenFile *) ckalloc(sizeof(OpenFile));
	tclOpenFiles[fd] = oFilePtr;
    }
    oFilePtr->f = file;
    oFilePtr->f2 = NULL;
    oFilePtr->permissions = permissions;
    oFilePtr->numPids = 0;
    oFilePtr->pidPtr = NULL;
    oFilePtr->errorId = -1;
    if (fd <= 2) {
	if (fd == 0) {
	    interp->result = "stdin";
	} else if (fd == 1) {
	    interp->result = "stdout";
	} else {
	    interp->result = "stderr";
	}
    } else {
	sprintf(interp->result, "file%d", fd);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_GetOpenFile --
 *
 *	Given a string identifier for an open file, find the corresponding
 *	open file structure, if there is one.
 *
 * Results:
 *	A standard Tcl return value.  If the open file is successfully
 *	located and meets any usage check requested by checkUsage, TCL_OK
 *	is returned and *filePtr is modified to hold a pointer to its
 *	FILE structure.  If an error occurs then TCL_ERROR is returned
 *	and interp->result contains an error message.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_GetOpenFile(interp, string, forWriting, checkUsage, filePtr)
    Tcl_Interp *interp;		/* Interpreter in which to find file. */
    char *string;		/* String that identifies file. */
    int forWriting;		/* 1 means the file is going to be used
				 * for writing, 0 means for reading. */
    int checkUsage;		/* 1 means verify that the file was opened
				 * in a mode that allows the access specified
				 * by "forWriting". */
    FILE **filePtr;		/* Store pointer to FILE structure here. */
{
    OpenFile *oFilePtr;
    int fd = 0;			/* Initial value needed only to stop compiler
				 * warnings. */
    Interp *iPtr = (Interp *) interp;

    if ((string[0] == 'f') && (string[1] == 'i') && (string[2] == 'l')
	    & (string[3] == 'e')) {
	char *end;

	fd = strtoul(string+4, &end, 10);
	if ((end == string+4) || (*end != 0)) {
	    goto badId;
	}
    } else if ((string[0] == 's') && (string[1] == 't')
	    && (string[2] == 'd')) {
	if (strcmp(string+3, "in") == 0) {
	    fd = 0;
	} else if (strcmp(string+3, "out") == 0) {
	    fd = 1;
	} else if (strcmp(string+3, "err") == 0) {
	    fd = 2;
	} else {
	    goto badId;
	}
    } else {
	badId:
	Tcl_AppendResult(interp, "bad file identifier \"", string,
		"\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (fd >= tclNumFiles) {
	if ((tclNumFiles == 0) && (fd <= 2)) {
	    MakeFileTable(iPtr, fd);
	} else {
	    notOpen:
	    Tcl_AppendResult(interp, "file \"", string, "\" isn't open",
		    (char *) NULL);
	    return TCL_ERROR;
	}
    }
    oFilePtr = tclOpenFiles[fd];
    if (oFilePtr == NULL) {
	goto notOpen;
    }
    if (forWriting) {
	if (checkUsage && !(oFilePtr->permissions & TCL_FILE_WRITABLE)) {
	    Tcl_AppendResult(interp, "\"", string,
		    "\" wasn't opened for writing", (char *) NULL);
	    return TCL_ERROR;
	}
	if (oFilePtr->f2 != NULL) {
	    *filePtr = oFilePtr->f2;
	} else {
	    *filePtr = oFilePtr->f;
	}
    } else {
	if (checkUsage && !(oFilePtr->permissions & TCL_FILE_READABLE)) {
	    Tcl_AppendResult(interp, "\"", string,
		    "\" wasn't opened for reading", (char *) NULL);
	    return TCL_ERROR;
	}
	*filePtr = oFilePtr->f;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_FilePermissions --
 *
 *	Given a FILE * pointer, return the read/write permissions
 *	associated with the open file.
 *
 * Results:
 *	If file is currently open, the return value is an OR-ed
 *	combination of TCL_FILE_READABLE and TCL_FILE_WRITABLE,
 *	which indicates the operations permitted on the open file.
 *	If the file isn't open then the return value is -1.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_FilePermissions(file)
    FILE *file;			/* File for which permissions are wanted. */
{
    register OpenFile *oFilePtr;
    int i, fd;

    /*
     * First try the entry in tclOpenFiles given by the file descriptor
     * for the file.  If that doesn't match then search all the entries
     * in tclOpenFiles.
     */

    if (file != NULL) {
	fd = fileno(file);
	if (fd < tclNumFiles) {
	    oFilePtr = tclOpenFiles[fd];
	    if ((oFilePtr != NULL) && (oFilePtr->f == file)) {
		return oFilePtr->permissions;
	    }
	}
    }
    for (i = 0; i < tclNumFiles; i++) {
	oFilePtr = tclOpenFiles[i];
	if (oFilePtr == NULL) {
	    continue;
	}
	if ((oFilePtr->f == file) || (oFilePtr->f2 == file)) {
	    return oFilePtr->permissions;
	}
    }
    return -1;
}

/*
 *----------------------------------------------------------------------
 *
 * TclOpen, etc. --
 *
 *	Below are a bunch of procedures that are used by Tcl instead
 *	of system calls.  Each of the procedures executes the
 *	corresponding system call and retries automatically
 *	if the system call was interrupted by a signal.
 *
 * Results:
 *	Whatever the system call would normally return.
 *
 * Side effects:
 *	Whatever the system call would normally do.
 *
 * NOTE:
 *	This should be the last page of this file, since it undefines
 *	the macros that redirect read etc. to the procedures below.
 *
 *----------------------------------------------------------------------
 */

#undef open
int
TclOpen(path, oflag, mode)
    char *path;
    int oflag;
    int mode;
{
    int result;
    while (1) {
	result = open(path, oflag, mode);
	if ((result != -1) || (errno != EINTR)) {
	    return result;
	}
    }
}

#undef read
int
TclRead(fd, buf, numBytes)
    int fd;
    VOID *buf;
    size_t numBytes;
{
    int result;
    while (1) {
	result = read(fd, buf, (size_t) numBytes);
	if ((result != -1) || (errno != EINTR)) {
	    return result;
	}
    }
}

#undef waitpid
extern pid_t waitpid _ANSI_ARGS_((pid_t pid, int *stat_loc, int options));

/*
 * Note:  the #ifdef below is needed to avoid compiler errors on systems
 * that have ANSI compilers and also define pid_t to be short.  The
 * problem is a complex one having to do with argument type promotion.
 */

#ifdef _USING_PROTOTYPES_
int
TclWaitpid _ANSI_ARGS_((pid_t pid, int *statPtr, int options))
#else
int
TclWaitpid(pid, statPtr, options)
    pid_t pid;
    int *statPtr;
    int options;
#endif /* _USING_PROTOTYPES_ */
{
    int result;
    while (1) {
	result = waitpid(pid, statPtr, options);
	if ((result != -1) || (errno != EINTR)) {
	    return result;
	}
    }
}

#undef write
int
TclWrite(fd, buf, numBytes)
    int fd;
    VOID *buf;
    size_t numBytes;
{
    int result;
    while (1) {
	result = write(fd, buf, (size_t) numBytes);
	if ((result != -1) || (errno != EINTR)) {
	    return result;
	}
    }
}
