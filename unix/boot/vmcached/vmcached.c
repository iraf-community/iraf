#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <ctype.h>
#include "vmcache.h"

#define NOKNET
#define	import_spp
#define	import_knames
#include <iraf.h>

/*
 * VMCACHED -- VMcache daemon.
 *
 * The VMcache daemon controls a virtual memory cache for optimizing file
 * storage in virtual memory on a single host computer.  Clients can connect
 * to the daemon to request that files be cached or uncached, query whether
 * a file is cached, modify cache parameters, or query the status of the
 * cache.
 */

#define	MAX_CLIENTS	256
#define MAX_ARGS	32
#define SZ_STATBUF	8192
#define SZ_CMDBUF	8192
#define	SZ_NAME		32
#define DEF_CACHESIZE   "50%"
#define DEF_PHYSPAGES   32768
#define DEF_PRIORITY    1
#define DEF_REFBASE     1
#define DEF_TOCK        600


/* Client connection. */
struct client {
	int fd;
	FILE *out;
	char name[SZ_NAME+1];
}; typedef struct client Client;

Client client[MAX_CLIENTS];
int nclients;
int maxclients;
int debug;
int running;
extern char *getenv();
void *vm;


/* MAIN -- VMCACHED main program.
 */
main (argc, argv)
int argc;
char **argv;
{
	char *argp, *op, *cachesize;
	int socket, lockpages, defpri, refbase, tock;
	int c_argc, fd, status, acmode, server, i;
	char *c_argv[MAX_ARGS];
	char initstr[SZ_FNAME];
	char osfn[SZ_FNAME];
	fd_set readfds;

	cachesize = DEF_CACHESIZE;
	socket = DEF_VMSOCK;
	defpri = DEF_PRIORITY;
	refbase = DEF_REFBASE;
	tock = DEF_TOCK;
	lockpages = 0;

	/* The socket to be used can be set in the environment. */
	if (argp = getenv (ENV_VMSOCK))
	    socket = atoi (argp);

	/* Parse argument list. */
	for (i=1;  i < argc, argp = argv[i];  i++) {
	    if (argname (argp, "-k", "-port")) {
		argp = (argv[++i]);
		socket = atoi (argp);
	    } else if (argname (argp, "-s", "-cachesize")) {
		argp = (argv[++i]);
		cachesize = argp;
	    } else if (argname (argp, "-p", "-defpri")) {
		argp = (argv[++i]);
		defpri = atoi (argp);
	    } else if (argname (argp, "-b", "-refbase")) {
		argp = (argv[++i]);
		refbase = atoi (argp);
	    } else if (argname (argp, "-t", "-tock")) {
		argp = (argv[++i]);
		tock = atoi (argp);
	    } else if (argname (argp, "-l", "-lockpages")) {
		lockpages++;
	    } else if (argname (argp, "-d", "-debug")) {
		debug++;
	    } else
		fprintf (stderr, "vmcached: unknown argument `%s'\n", argp);
	}

	/* Construct the initstr for VMcache. */
	op = initstr;
	sprintf (op, "cachesize=%s,defpri=%d,refbase=%d,tock=%d",
	    cachesize, defpri, refbase, tock);
	if (lockpages) {
	    op = initstr + strlen(initstr);
	    strcat (op, ",lockpages");
	}
	if (debug) {
	    op = initstr + strlen(initstr);
	    strcat (op, ",debug");
	}

	if (debug)
	    fprintf (stderr, "vmcached: init vmcache `%s'\n", initstr);

	/* Initialize the VM cache. */
	if (!(vm = vm_initcache (NULL, initstr))) {
	    fprintf (stderr, "vmcached: failed to open socket `%s'\n", osfn);
	    exit (1);
	}

	/* Open the server port for incoming connections.
	 */
	sprintf (osfn, "inet:%d::nonblock", socket);
	acmode = NEW_FILE;
	if (debug)
	    fprintf (stderr, "vmcached: open server socket `%s'\n", osfn);

	ZOPNND (osfn, &acmode, &server);
	if (server == XERR) {
	    fprintf (stderr, "vmcached: failed to open socket `%s'\n", osfn);
	    vm_closecache (vm);
	    exit (2);
	}

	if (debug)
	    fprintf (stderr, "vmcached: enter main server loop:\n");

	/* Loop indefinitely waiting for new connections or client
	 * requests.
	 */
	for (running=1;  running;  ) {
	    FD_ZERO (&readfds);
	    FD_SET (server, &readfds);
	    for (i=0;  i < maxclients;  i++)
		if (client[i].fd)
		    FD_SET (client[i].fd, &readfds);
	    if (select (MAX_CLIENTS, &readfds, NULL, NULL, NULL) <= 0)
		break;

	    /* Check for a new client connection. */
	    if (FD_ISSET (server, &readfds)) {
		char buf[SZ_CMDBUF];
		FILE *fdopen();
		int fd, n;

		if (debug)
		    fprintf (stderr, "vmcached: open new client connection: ");

		/* Accept the connection. */
		sprintf (osfn, "sock:%d", server);
		acmode = NEW_FILE;
		ZOPNND (osfn, &acmode, &fd);
		if (fd == XERR)
		    exit (1);

		for (i=0;  i < MAX_CLIENTS;  i++)
		    if (!client[i].fd)
			break;
		if (i >= MAX_CLIENTS) {
		    fprintf (stderr, "vmcached: too many clients\n");
		    ZCLSND (&fd, &status);
		    continue;
		}

		/* The client name is passed as data in an open. */
		if ((n = read (fd, buf, SZ_CMDBUF)) > 0) {
		    strncpy (client[i].name, buf, SZ_NAME);
		    client[i].name[n < SZ_NAME ? n : SZ_NAME] = '\0';
		}

		if (debug)
		    fprintf (stderr, "fd=%d (%s)\n", fd, client[i].name);

		client[i].fd = fd;
		client[i].out = fdopen (fd, "w");
		nclients++;
		if (i >= maxclients)
		    maxclients = i + 1;

		/* Send an acknowledge back to the client. */
		c_argc = 1;  c_argv[0] = client[i].name;
		putstati (client[i].out, c_argc, c_argv, 0);
	    }

	    /* Check for command input from clients.  Any command data
	     * must be sent as a complete command block.  The block must
	     * be syntatically complete, by may contain multiple 
	     * concatenated commands.  If a command references any data
	     * not passed as part of the command, the data can be read
	     * from the client input stream during execution of the command.
	     */
	    for (i=0;  i < MAX_CLIENTS;  i++) {
		Client *cx = &client[i];
		if (!cx->fd)
		    continue;

		if (FD_ISSET (cx->fd, &readfds)) {
		    int status, buflen;
		    char buf[SZ_CMDBUF];
		    char *ip, *itop;

		    if (debug) fprintf (stderr,
			"vmcached: client input on fd=%d: ", cx->fd);

		    if ((buflen = read (cx->fd, buf, SZ_CMDBUF)) <= 0) {
			if (debug)
			    fputs ("[EOF (disconnected)]\n", stderr);
			goto disconnect;
		    }
		    if (debug) {
			buf[buflen] = '\0';
			fputs (buf, stderr);
		    }

		    ip = buf;
		    itop = buf + buflen;

		    while (getcmd (&ip, itop, &c_argc, c_argv) > 0)
			if (execute (cx, c_argc, c_argv) > 0) {
disconnect:		    fclose (cx->out);
			    ZCLSND (&cx->fd, &status);
			    cx->fd = 0;
			    cx->out = NULL;
			    nclients--;
			    if (maxclients == i+1)
				maxclients--;
			    break;
			}

		    if (cx->out)
			fflush (cx->out);
		}
	    }
	}

	if (debug)
	    fprintf (stderr, "vmcached: shutdown\n");

	/* Close all client connections. */
	for (i=0;  i < maxclients;  i++) {
	    Client *cx = &client[i];
	    if (cx->fd) {
		fclose (cx->out);
		close (cx->fd);
		cx->fd = 0;
	    }
	}

	ZCLSND (&server, &status);
	vm_closecache (vm);
	exit (0);
}


/* EXECUTE -- Execute a vmcached directive.
 *
 * Directives are simple newline or semicolon delimited commands, with the
 * arguments delimited by whitespace or quotes, e.g., :
 *
 *	access /d1/iraf/h1904b.fits rw
 *
 * Multiple commands can be concatenated (with command delimiters) and sent
 * as a batch if desired.  They will be executed in sequence.  Most commands
 * result in a response to the client.  These have the form
 *
 *	<status> '=' <command> <args>
 *
 * for example,
 *
 *	1 = access /d1/iraf/h1904b.fits rw
 *
 * This form makes the status value easy to parse for simple commands.
 * The command is echoed so that the status value can be matched to the
 * command it is for, e.g., if multiple commands were issued.
 */
execute (cx, argc, argv)
Client *cx;
int argc;
char *argv[];
{
	char *cmd = argv[0];
	int execstat = 0;
	int i, status = 0;

	if (!cmd)
	    return (-1);

	if (debug) {
	    fprintf (stderr, "vmcached: execute \"%s (", cmd);
	    for (i=1;  i < argc;  i++) {
		if (i > 1)
		    fprintf (stderr, ", ");
		fprintf (stderr, "%s", argv[i]);
	    }
	    fprintf (stderr, ")\"\n");
	}

	if (strcmp (cmd, "bye") == 0) {
	    /* Usage: bye
	     * Close a client connection.
	     */
	    execstat = 1;

	} else if (strcmp (cmd, "quit") == 0) {
	    /* Usage: quit
	     * Shutdown vmcached and exit.
	     */
	    running = 0;

	} else if (strcmp (cmd, "access") == 0) {
	    /* Usage: access <fname> [<mode>]
	     *
	     * Determine whether the named file should be accessed via the
	     * VMcache (via virtual memory / normal i/o) or via direct i/o,
	     * bypassing VM.  In the simplest scenario we just check whether
	     * the named file is already in the cache, perhaps loaded via
	     * the cache directive by a control process.  More complex
	     * strategies are possible, e.g., every access could be set up
	     * to automatically cache the referenced file; caching could be
	     * decided on a per-process basic depending upon access history,
	     * etc.  A client about to access a file should issue an access
	     * directive to the cache to determine whether or not to use VM
	     * (e.g., normal file i/o) to access the file.
	     */
	    char *fname = argv[1];
	    char *mode = (argc > 2) ? argv[2] : "r";

	    if (!fname)
		status = -1;
	    else
		status = vm_access (vm, fname, mode, 0);
	    putstati (cx->out, argc, argv, status);

	} else if (strcmp (cmd, "cache") == 0) {
	    /* Usage: cache <fname>
	     * 
	     * Cache the named file.  The file is asynchronously loaded 
	     * into the VM cache.
	     */
	    char *fname = argv[1];
	    
	    if (!fname)
		status = -1;
	    else
		status = vm_cachefile (vm, fname, 0);
	    putstati (cx->out, argc, argv, status);

	} else if (strcmp (cmd, "uncache") == 0) {
	    /* Usage: uncache <fname>
	     *
	     * If the named file is present in the cache the space it is
	     * marked as ready for reuse.  Any VM space used by the file is
	     * not immediately reused.  The actual disk file is not affected.
	     */
	    char *fname = argv[1];
	    
	    if (!fname)
		status = -1;
	    else
		status = vm_uncachefile (vm, fname, 0);
	    putstati (cx->out, argc, argv, status);

	} else if (strcmp (cmd, "delete") == 0) {
	    /* Usage: delete <fname>
	     *
	     * If the named file is present in the cache it is removed from
	     * the cache, freeing the space to be used for other files.  The
	     * actual disk file is not affected.
	     */
	    char *fname = argv[1];
	    
	    if (!fname)
		status = -1;
	    else {
		status = vm_uncachefile (vm, fname,
		    VM_DESTROYREGION|VM_CANCELREFCNT);
	    }
	    putstati (cx->out, argc, argv, status);

	} else if (strcmp (cmd, "refresh") == 0) {
	    /* Usage: refresh <fname>
	     *
	     * If the named file is present in the cache it is moved to the
	     * head of the cache (most recently referenced), and any missing
	     * file pages are asynchronously loaded from disk.
	     */
	    char *fname = argv[1];
	    
	    if (!fname)
		status = -1;
	    else
		status = vm_refreshfile (vm, fname, 0);
	    putstati (cx->out, argc, argv, status);

	} else if (strcmp (cmd, "reserve") == 0) {
	    /* Usage: reserve <nbytes>
	     *
	     * The indicated amount of space is made available in the cache.
	     * The space goes on the VM free list, for use to buffer data
	     * without paging out other data.
	     */
	    long nbytes = (argv[1]) ? atol(argv[1]) : 0;

	    if (!nbytes)
		status = -1;
	    else
		status = vm_reservespace (vm, nbytes);
	    putstati (cx->out, argc, argv, status);

	} else if (strcmp (cmd, "status") == 0) {
	    /* Usage: status
	     *
	     * The status directive is used to query the status and contents
	     * of the VM cache.  A description of all parameters and cached
	     * files is returned in text form.
	     */
	    char statbuf[SZ_STATBUF];

	    status = vm_status (vm, statbuf, SZ_STATBUF, 0);
	    putstats (cx->out, argc, argv, status);
	    fputs (statbuf, cx->out);

	} else if (strcmp (cmd, "subscribe") == 0) {
	    /* Usage: subscribe */
	    fprintf (cx->out, "%s %d\n", cmd, status);

	} else if (strcmp (cmd, "unsubscribe") == 0) {
	    /* Usage: unsubscribe */
	    fprintf (cx->out, "%s %d\n", cmd, status);

	} else {
	    execstat = status = -1;
	    putstati (cx->out, argc, argv, status);
	}

	return (execstat);
}


/* PUTSTATI -- Return an integer valued command status to the client.
 */
putstati (fp, argc, argv, status)
FILE *fp;
int argc;
char **argv;
int status;
{
	register int i;

	fprintf (fp, "%d = %s", status, argv[0]);
	for (i=1;  i < argc && argv[i];  i++)
	    fprintf (fp, " %s", argv[i]);
	fprintf (fp, "\n");
	fflush (fp);

	if (debug)
	    fprintf (stderr, "vmcached: %s -> %d\n", argv[0], status);
}


/* PUTSTATS -- Return a string valued command status to the client.
 */
putstats (fp, argc, argv, status)
FILE *fp;
int argc;
char **argv;
char *status;
{
	register int i;

	fprintf (fp, "%s = %s", status, argv[0]);
	for (i=0;  i < argc && argv[i];  i++)
	    fprintf (fp, " %s", argv[i]);
	fprintf (fp, "\n");
	fflush (fp);
}


/* ARGNAME -- Test whether a string is one of the named arguments.
 */
argname (arg, name1, name2)
char *arg;
char *name1, *name2;
{
	int status = 0;

	if (name1)
	    status |= (strcmp (arg, name1) == 0);
	if (name2)
	    status |= (strcmp (arg, name2) == 0);

	return (status);
}


/* GETCMD -- Read a command from the input command block and parse it into
 * the command name and arguments.  The input pointer is left positioned
 * to the text following the command.  The command name is returned as
 * argv[0];
 */
getcmd (ipp, itop, argc, argv)
char **ipp;
char *itop;
int *argc;
char *argv[];
{
	register char *ip = *ipp;
	register char *argp;
	int i, nargs = 0;

	for (i=0;  i < MAX_ARGS;  i++)
	    argv[i] = NULL;

	while (ip < itop && (*ip == ' ' || *ip == '\t'))
	    ip++;

	/* Get command name and any arguments. */
	while (ip < itop && *ip != '\n' && *ip != ';') {
	    /* Get next argument. */
	    argp = ip;

	    /* Quoted strings may include whitespace.  The quote characters
	     * are omitted from the argument.
	     */
	    if (*ip == '\'') {
		for (argp = ++ip;  ip < itop && *ip != '\'';  )
		    ip++;
	    } else if (*ip == '"') {
		for (argp = ++ip;  ip < itop && *ip != '"';  )
		    ip++;
	    } else {
		while (ip < itop && !isspace(*ip)) {
		    if (*ip == '\\' && ip+1 < itop)
			ip++;
		    ip++;
		}
	    }

	    *ip++ = '\0';
	    if (argp[0])
		argv[nargs++] = argp;

	    /* Skip forward to next argument. */
	    while (ip < itop && (*ip == ' ' || *ip == '\t'))
		ip++;
	}

	/* Skip forward to next command line. */
	while (ip < itop && (isspace(*ip) || *ip == ';'))
	    ip++;

	*argc = nargs;
	*ipp = ip;

	return (nargs);
}
