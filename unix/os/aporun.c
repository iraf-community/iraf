/* RUN -- Send a command to UNIX and return the execution status to our
 * caller at the completion of the command.
 */
run (task, argv)
char	*task;
char	*argv[];
{
	int	waitpid, fork();
	char	*s, *t, path[50];

	s = path;
	t = "/usr/bin/";
	while (*t != EOS)
	    *s++ = *t++;
	for (t = task;  (*s++ = *t++) != EOS;  )
	    ;

	if ((waitpid = fork()) == 0) {
	    enbint (SIG_DFL);

	    execv (path+9, argv);	/* command */
	    execv (path+4, argv);	/* /bin/command */
	    execv (path  , argv);	/* /usr/bin/command */

	    fatalstr ("Cannot execute %s", path+9);
	}

	return (await (waitpid));
}


/*
 * Task execution and interrupt handling routines,
 * taken with minor modifications the F77 driver.
 */


/* SYS -- Execute a general UNIX command passed as a string.  The command may
 * contain i/o redirection metacharacters.
 */
sys (str)
char	*str;
{
	register char *s, *t;
	char	*argv[100], path[100];
	char	*inname, *outname;
	int	append;
	int	waitpid;
	int	argc;

	if (debug) {
	    fprintf (stderr, "debug: %s\n", str);
	    fflush (stderr);
	}

	inname  = NULL;
	outname = NULL;
	argv[0] = shellname;
	argc = 1;

	t = str;
	while (isspace (*t))
	    ++t;
	while (*t) {
	    if (*t == '<')
		inname = t+1;
	    else if (*t == '>') {
		if (t[1] == '>') {
		    append = YES;
		    outname = t+2;
		} else {
		    append = NO;
		    outname = t+1;
		}
	    } else
		argv[argc++] = t;
	    while ( !isspace (*t) && *t != '\0' )
		++t;
	    if (*t) {
		*t++ = '\0';
		while (isspace (*t))
		    ++t;
	    }
	}

	if (argc == 1)				/* no command */
	    return (-1);
	argv[argc] = 0;

	s = path;
	t = "/usr/bin/";
	while (*t)
	    *s++ = *t++;
	for (t = argv[1] ; *s++ = *t++ ; )
	    ;
	if ((waitpid = fork()) == 0) {
	    if (inname)
		freopen (inname, "r", stdin);
	    if (outname)
		freopen (outname, (append ? "a" : "w"), stdout);
	    enbint (SIG_DFL);

	    texec (path+9, argv);	/* command */
	    texec (path+4, argv);	/* /bin/command */
	    texec (path  , argv);	/* /usr/bin/command */

	    s = path;
	    t = "/usr/local/bin/";
	    while (*t)
		*s++ = *t++;
	    for (t = argv[1] ; *s++ = *t++ ; )
		;

	    texec (path  , argv);	/* /usr/local/bin/command */

	    fatalstr ("Cannot execute %s", path);
	}

	return (await (waitpid));
}


/* TEXEC -- Spawn a shell to execute with the given argument list.
 * Taken from the shell with minor modifications.
 */
texec (f, av)
char	*f;
char	**av;
{
	extern int errno;

	execv (f, av+1);

	if (errno == ENOEXEC) {
	    av[1] = f;
	    execv (shellname, av);
	    fatal ("No shell!");
	}
	if (errno == ENOMEM)
	    fatalstr ("%s: too large", f);
}


/* DONE -- Called at process shutdown to cleanup.  Primary action is to delete
 * the intermediate Fortran files, unless the -F flag was given on the command
 * line.
 */
done (k)
int	k;
{
	static	int recurs = NO;

	if (recurs == NO) {
	    recurs = YES;
	    if (!keepfort)
	        rmfiles();
	}

	ZZSTOP();
	exit (k);
}


/* ENBINT -- Post an exception handler function to be executed if any sort
 * of interrupt occurs.
 */
enbint (handler)
int	(*handler)();
{
	if (sig_int == 0)
	    signal (SIGINT, handler);
	if (sig_quit == 0)
	    signal (SIGQUIT, handler);
	if (sig_hup == 0)
	    signal (SIGHUP, handler);
	if (sig_term == 0)
	    signal (SIGTERM, handler);
}


/* INTERRUPT -- Exception handler, called if an interrupt is received
 * during compilation.
 */
interrupt()
{
	done (2);
}


/* AWAIT -- Wait for an asynchronous child process to terminate.
 */
await (waitpid)
int	waitpid;
{
	int	w, status;
	extern	interrupt();

	enbint (SIG_IGN);
	while ((w = wait (&status)) != waitpid)
	    if (w == -1)
		fatal ("bad wait code");
	enbint (interrupt);
	if (status & 0377) {
	    if (status != SIGINT) {
		fprintf (stderr, "Termination code %d", status);
		fflush (stderr);
	    }
	    done (2);
	}
	return (status>>8);
}


/* RMFILES -- Delete all of the ".f" intermediate Fortran files.
 */
rmfiles()
{
	int	i;

	for (i=0;  i < nxfiles;  i++) {
	    chdot (lxfiles[i], 'f');
	    unlink (lxfiles[i]);
	}
}


/* FATALSTR -- Fatal error with an sprintf format and one string argument.
 */
fatalstr (s1, s2)
char	*s1, *s2;
{
	char	out[100];

	sprintf (out, s1, s2);
	fatal (out);
}


/* FATAL -- A fatal error has occurred.  Print error message and terminate
 * process execution.
 */
fatal (s)
char	*s;
{
	fprintf (stderr, "Fatal compiler error: %s\n", s);
	fflush (stderr);
	done (1);
}


/* ERROR -- Print a warning message but do not terminate the process.
 */
error (s)
char	*s;
{
	fprintf (stderr, "Error: %s\n", s);
	fflush (stderr);
}

