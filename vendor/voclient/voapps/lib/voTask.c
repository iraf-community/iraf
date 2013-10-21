/**
 *  VOTASK.C -- Utilities to run a VOApps task as a connected subprocess.
 *
 *  @file       voTask.c
 *  @author     Mike Fitzpatrick
 *  @date       6/23/12
 *
 *  @brief      Utilities to run a VOApps task as a connected subprocess.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdarg.h>
#include <setjmp.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/select.h>
#include <sys/time.h>
#include <sys/types.h>
#include "voApps.h"
 

#define	PARENT_READ	rpipe[0]	/* read from parent		*/
#define	CHILD_WRITE	rpipe[1]	/* write to parent		*/
#define CHILD_READ	wpipe[0]	/* read from child		*/
#define PARENT_WRITE	wpipe[1]	/* write to parent		*/

#ifdef ERR
#undef ERR
#endif
#ifdef OK
#undef OK
#endif
#define ERR		1
#define OK		0

#define MAX_TASK_ARGS	64

    
static int  rpipe[2]	= {-1, -1};	/* subprocess read pipes	*/
static int  wpipe[2]	= {-1, -1};	/* subprocess write pipes	*/
static int  nr 		= 0;



/*  Internal procedures.
 */
static pid_t 	vo_taskExec (char *method, Task *apps, int argc, char *argv[]);
static int	vo_taskWait (pid_t pid, size_t *len);
static void    *vo_taskResult (pid_t pid, size_t len);
static void     vo_taskReaper (int sig, int *arg1, int *arg2);
static size_t   vo_taskRead (int fd, void *data, size_t len);
static size_t   vo_taskWrite (int fd, void *data, size_t len);

typedef void  (*SIGFUNC)();




/*****************************************************************************/
/**   			       Public procedures			    **/
/*****************************************************************************/


/**
 *  VO_RUNTASK -- Run a VOApps task as a connected subprocess.
 *
 *  @brief  Run a VOApps task as a connected subprocess.
 *  @fn     int vo_runTask (char *method, Task *apps, int argc, char *argv[], 
 *				size_t *len, void **result)
 *
 *  @param   method     task name to call
 *  @param   apps       application table
 *  @param   argc       argument count
 *  @param   argv       argument vector
 *  @param   len        length of result object
 *  @param   result     pointer to result object
 *  @return             status (0=OK, 1=ERR)
 */

static int	vo_signal_status = 0;
static int	vo_exit_status   = 0;
sigjmp_buf 	vo_env;

int
vo_runTask (char *method, Task *apps, int argc, char **argv, size_t *len, void **result)
{
    pid_t  pid;
    int    status = 0, retStatus = EXIT_SUCCESS;
    size_t resLen = 0;
    void  *res = (void *) NULL;
    static SIGFUNC  old_sigcld;
    char  err[128];


    memset (err, 0, 128);

    old_sigcld = (SIGFUNC) signal (SIGCHLD, (SIGFUNC) vo_taskReaper);
    vo_signal_status = 0;
    vo_exit_status   = 0;

    sigsetjmp (vo_env, 1);
    if (vo_signal_status || vo_exit_status) {
	if (vo_signal_status) 
	    sprintf (err, "Child exited with SIGNAL %d", vo_signal_status);
	else if (vo_exit_status > 1) 
	    sprintf (err, "Child exited with status %d", vo_exit_status);
	*len    = strlen (err);
	*result = (void *) strdup (err);

	return (EXIT_FAILURE);
    }

    if ((pid = vo_taskExec (method, apps, argc, argv)) > 0) {

        if ((status = vo_taskWait (pid, &resLen)) == EXIT_SUCCESS) {

            if (resLen && (res = vo_taskResult (pid, resLen)))
		*result = res; 		  /* save result from client   	*/
            else
		*result = (void *) NULL;  /* no result from client    	*/
	    *len = resLen;

        } else {
	    sprintf (err, "Child exited with status %d", status);
	    *len    = strlen (err);
	    *result = (void *) strdup (err);
	    return ( status ); 		/* child exits with error  	*/
	}

        close (CHILD_WRITE);		/* Close descriptors		*/
        close (CHILD_READ);

    } else {
	fprintf (stderr, "Error executing task\n");
	retStatus = EXIT_FAILURE;
    }

    signal (SIGCHLD, old_sigcld); 	/* reset the SIGCHLD handler  */
    return (retStatus);
}


/**
 *  VO_TASKTEST -- Execute a task as a unit test.
 *
 *  @brief  Execute a task as a unit test.
 *  @fn     int vo_taskTest (Task *self, char *arg, ...)
 *
 *  @param   self	task struct pointer
 *  @param   arg	first of variable arg list
 *  @return             status (0=OK, 1=ERR)
 */
int  
vo_taskTest (Task *self, char *arg, ...)
{
    int     i, status = OK, argc = 0;
    char   *argv[MAX_TASK_ARGS], *aval = NULL;
    size_t  reslen = 0;
    void   *result = (void *) NULL;
    va_list argp;

    extern  int optind;
#ifdef Darwin
    extern  int optreset;
#endif

    va_start (argp, arg);			/* initialize		*/
    memset (argv, 0, sizeof (char *) * MAX_TASK_ARGS);

    optind   = 0;                       /* required for unit test       */
#ifdef Darwin
    optreset = 1;                       /* required for unit test       */
#endif

    /*  Turn the varargs list into an argv[] we can pass to the task.
     */
    argc = 1;
    argv[0] = strdup (self->name);

    if (arg) {
        argv[1] = strdup (arg);
        for (i=2; (aval = (char *)va_arg(argp,char *)) != NULL; i++) {
	    if (aval)
	        argv[i] = strdup (aval);
        }
        argc = i;
        va_end (argp);
    }


    /*  Initialize the test output....
     */
    fprintf (stderr, "\n=======================================");
    fprintf (stderr, "======================================\n");
    fprintf (stderr, "== Test Cmd:  %s ", self->name);
    for (i=1; i < argc; i++)
        fprintf (stderr, "%s ", argv[i]);
    fprintf (stderr, "\n\n");


    /*  Execute the task, throw away any returned value.
     */
    if (!getenv ("VOAPP_CONNECTED"))
        status = vo_runTask (self->name, self, argc, argv, &reslen, &result);
    else
        status = (*(self->func)) (argc, argv, &reslen, &result);

    fprintf (stderr, "===================================");
    fprintf (stderr, "=============================  status = %2d\n", status);

vo_taskDbg();

    /*  Clean up.
     */
    for (i=0; i < argc; i++) {
	if (argv[i])
	    free (argv[i]);
    }

    if (status && result)            	/* print the error message 	*/
        fprintf (stderr, "%s\n", (char *) result);
    if (reslen && result)            	/* free result pointer     	*/
        free (result);

    self->ntests++;			/* update testing struct	*/
    if (status)
	self->nfail++;
    else
	self->npass++;

    return (status);
}


/**
 *  VO_TASKDBG -- Tasking debug breakpoint.
 *
 *  @brief  Tasking debug breakpoint.
 *  @fn     void vo_taskDbg (void)
 *
 *  @return             nothing
 */
void vo_taskDbg (void)
{
   static int i = 0;

   i++;
}


/**
 *  VO_TASKTESTREPORT -- Report from the task testing interface.
 *
 *  @brief  Report from the task testing interface.
 *  @fn     void vo_taskTestReport (self)
 *
 *  @param   self	task struct pointer
 *  @return             status (0=OK, 1=ERR)
 */
void
vo_taskTestReport (Task self)
{
    fprintf (stderr, "\n");
    fprintf (stderr, 
	"Task: %-12.12s  No. of Tests:  %d    Passed: %d     Failed: %d\n",
	self.name, self.ntests, self.npass, self.nfail); 

    fprintf (stderr, "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@");
    fprintf (stderr, "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n");
}


/**
 *  VO_TASKTESTFILE -- Create a small named test file.
 *
 *  @brief  Create a small named test file.
 *  @fn     void vo_taskTestFile (char *str, char *fname)
 *
 *  @param   str	test string
 *  @param   fname	test filename
 *  @return             status (0=OK, 1=ERR)
 */
void
vo_taskTestFile (char *str, char *fname)
{
    FILE *fd = (FILE *) NULL;

    if (access (fname, F_OK) == 0)
	unlink (fname);
    if ((fd = fopen (fname, "w+"))) {
        fprintf (fd, "%s", str);
        fclose (fd);
    }
}


/**
 *  VO_SETRESULTFROMFILE -- Set a result object to a file's contents.
 *
 *  @brief  Set a result object to a file's contents.
 *  @fn     int vo_setResultFromFile (char *fname, size_t *len, void **data)
 *
 *  @param   fname      file name to read
 *  @param   len        length of result object
 *  @param   data       pointer to result object
 *  @return             status (0=OK, 1=ERR)
 */
int
vo_setResultFromFile (char *fname, size_t *len, void **data)
{
    struct stat  st;
    int  fd = 0;


    if (stat (fname, &st) < 0)
	return (ERR);

    if ((fd = open (fname, O_RDONLY)) > 0) {
	*len = st.st_size;
	*data = calloc (1, st.st_size + 1);
	if (vo_taskRead (fd, *data, *len) < *len)
	    return (ERR);
	close (fd);
    } else
	return (ERR);

    return (OK);
}


/**
 *  VO_SETRESULTFROMINT -- Set a result object to an int value.
 *
 *  @brief  Set a result object to an int value.
 *  @fn     int vo_setResultFromInt (int value, size_t *len, void **data)
 *
 *  @param   value      value to set
 *  @param   len        length of result object
 *  @param   data       pointer to result object
 *  @return             status (0=OK, 1=ERR)
 */
int
vo_setResultFromInt (int value, size_t *len, void **data)
{
    char  buf[SZ_FNAME];

    memset (buf, 0, SZ_FNAME);
    sprintf (buf, "%d", value);

    *len = strlen (buf);
    *data = (char *) calloc (1, *len + 1);
    strcpy ((char *) data, buf);
    return (OK);
}


/**
 *  VO_SETRESULTFROMREAL -- Set a result object to a real value.
 *
 *  @brief  Set a result object to a real value.
 *  @fn     int vo_setResultFromReal (float value, size_t *len, void **data)
 *
 *  @param   value      value to set
 *  @param   len        length of result object
 *  @param   data       pointer to result object
 *  @return             status (0=OK, 1=ERR)
 */
int
vo_setResultFromReal (float value, size_t *len, void **data)
{
    char  buf[SZ_FNAME];

    memset (buf, 0, SZ_FNAME);
    sprintf (buf, "%g", value);

    *len = strlen (buf);
    *data = (char *) calloc (1, *len + 1);
    strcpy ((char *) data, buf);
    return (OK);
}


/**
 *  VO_SETRESULTFROMSTRING -- Set a result object to a string value.
 *
 *  @brief  Set a result object to a string value.
 *  @fn     int vo_setResultFromString (char *str, size_t *len, void **data)
 *
 *  @param   value      value to set
 *  @param   len        length of result object
 *  @param   data       pointer to result object
 *  @return             status (0=OK, 1=ERR)
 */
int
vo_setResultFromString (char *str, size_t *len, void **data)
{
    if (str) {
        *len = strlen (str);
        *data = (char *) calloc (1, *len + 1);
        strcpy ((char *) *data, str);
    } else {
        *len = 0;
        *data = NULL;
    }
    return (OK);
}


/**
 *  VO_APPENDRESULTFROMSTRING -- Append a result object to a string value.
 *
 *  @brief  Append a result object to a string value.
 *  @fn     int vo_appendResultFromString (char *str, size_t *len, 
 *			void **data, size_t *maxlen)
 *
 *  @param   value      value to set
 *  @param   len        length of result object
 *  @param   data       pointer to result object
 *  @param   maxlen     max length of result object
 *  @return             status (0=OK, 1=ERR)
 */
int
vo_appendResultFromString (char *str, size_t *len, void **data, size_t *maxlen)
{
    int  slen = strlen (str);

    if (str) {
        *len += slen;

	/*  Reallocate in case we overrun the data buffer.
	 */
	if ((*len) > (*maxlen)) {
	    char *new =  (char *) calloc (1, (*maxlen) + SZ_LINE);
            strcpy (new, *data);
	    free ((void *) *data);
	    *data = new;
	}

        strcat ((char *) *data, str);
    }
    return (OK);
}




/*****************************************************************************/
/**   			      Private procedures			    **/
/*****************************************************************************/


static void
vo_taskReaper (
  int     sig,                  /* signal which was trapped     */
  int     *arg1,                /* not used 			*/
  int     *arg2                 /* not used 			*/
)
{
    int status = 0, pid = 0;


    vo_signal_status = vo_exit_status  = 0;

    while ((pid = (int) waitpid ((pid_t) 0, &status, WNOHANG)) > 0)
	if (status) {
	    if (status & 0xFF)
		vo_signal_status = (status & 0xFF);
	    else if (status >> 8)
		vo_exit_status = (status >> 8);
	    siglongjmp (vo_env, 1);
	}
}


/**
 *  VO_TASKEXEC -- Execute a task as a connected subprocess.
 *
 *  @brief  Get the result object (if any) from the task.
 *  @fn     Task *vo_taskExec (char *method, Task *apps, int argc, char *argv[])
 *
 *  @param   method     task name to call
 *  @param   apps       task application table
 *  @param   argc       argument count
 *  @param   argv       argument vector
 *  @return             pid of child process
 */

#define	GO_MSG		"__go__"
#define GO_LEN		6
#define	E_NOTFOUND	"Task not found"

static pid_t
vo_taskExec (char *method, Task *apps, int argc, char **argv)
{
    Task  *app = (Task *) NULL;
    int	   status = 0;
    pid_t  cpid;
    size_t resLen = 0;
    void  *result = (void *) NULL;
    char   msg[128];

 
    /*  Create the pipes to/from the child process.
     */
    if (pipe (rpipe) < 0 || pipe (wpipe) < 0) {
        perror ("pipe failure");
        _exit (EXIT_FAILURE);
    }
 

    /*  Fork so the child does all the work.
     */
    if ( (cpid = fork ()) < 0 )
        return (cpid);
 
    if (cpid == 0) {            	/* Child 			*/

        close (CHILD_WRITE);		/* Close unused descriptors	*/
        close (CHILD_READ);
 
	/*  Wait for the '_go_' message.
	 */
	do {
	    memset (msg, 0, 128);
            nr = vo_taskRead (PARENT_READ, msg, GO_LEN);
	} while (strncmp (msg, GO_MSG, GO_LEN) != 0);


	/*  Loop through the application table.  If the names match, call
	 *  the entry-point function.
	 */
	for (app = apps; app->name; app++) {
	    if (strcmp (app->name, method) == 0) {
	        status = (*app->func)(argc, argv, &resLen, &result);

		/*  Send the parent the status reply,result length and
		 *  the result object, if there is one.
		 */
        	vo_taskWrite (PARENT_WRITE, &status, sizeof (int));
        	vo_taskWrite (PARENT_WRITE, &resLen, sizeof (size_t));
		if (resLen)
            	    vo_taskWrite (PARENT_WRITE, result, resLen);
		break;
	    }
	}
    	

	/*  If we get here, the task wasn't found.
	 */
	if (app->name == NULL) {
	    status = -1;
            vo_taskWrite (PARENT_WRITE, &status, sizeof (int));
            vo_taskWrite (PARENT_WRITE, E_NOTFOUND, strlen (E_NOTFOUND));
	}

	/*  Exit the child process.
	 */
        close (PARENT_READ);		/* Close descriptors		*/
        close (PARENT_WRITE);
        _exit (EXIT_SUCCESS);		/* child exits			*/
 
    } else {            		/* Parent 			*/
        close (PARENT_READ);		/* Close unneded descriptors	*/
        close (PARENT_WRITE);

        vo_taskWrite (CHILD_WRITE, GO_MSG, GO_LEN);

        return (cpid);
    }
}


/**
 *  VO_TASKWAIT -- Wait for a child process to complete.
 *
 *  @brief  Wait for a child process to complete.
 *  @fn     int vo_taskWait (pid_t pid, int *len)
 *
 *  @param   pid        child process pid
 *  @param   len        length of result object
 *  @return             exit status of child process
 */
static int
vo_taskWait (pid_t pid, size_t *len)
{
    int   status = 0;


    /*  Wait for status message.
     */
    vo_taskRead (CHILD_READ, &status, sizeof (int));
    vo_taskRead (CHILD_READ, len, sizeof (size_t));

    wait (NULL);                	/* Wait for child 		*/
    return (status);
}


/**
 *  VO_TASKRESULT -- Get the result object (if any) from the task.
 *
 *  @brief  Get the result object (if any) from the task.
 *  @fn     void *vo_taskResult (pid_t pid, size_t len)
 *
 *  @return             (allocated) pointer to the Task
 */
static void *
vo_taskResult (pid_t pid, size_t len)
{
    void   *result = (void *) calloc (1, len);

    vo_taskRead (CHILD_READ, result, len);

    return (result);
}


#define	SELWIDTH	32

/**
 *  VO_TASKREAD -- Read exactly "n" bytes from a task descriptor. 
 *
 *  @brief  Read exactly "n" bytes from a task descriptor. 
 *  @fn     size_t vo_taskRead (int fd, void *vptr, size_t nbytes)
 *
 *  @param  fd          file descriptor
 *  @param  vptr        data buffer to be written
 *  @param  nbytes      number of bytes to write
 *  @return             number of bytes written
 */
static size_t  
vo_taskRead (int fd, void *vptr, size_t nbytes)
{
    char    *ptr = vptr;
    int     nread = 0, nleft = nbytes, nb = 0, flags = 0;
    fd_set  allset, fds;
    struct  timeval tm = { 2, 0 };


    /*  Set non-blocking mode on the descriptor.
     */
    if ((flags = fcntl (fd, F_GETFL, 0)) == 0)
        if (fcntl (fd, F_SETFL, flags | O_NONBLOCK) < 0)
	    ; 				/* ignore error */

    FD_ZERO (&allset);
    FD_SET (fd, &allset);

    while (nleft > 0) {
      memcpy (&fds, &allset, sizeof(allset));
      if (select (SELWIDTH, &fds, NULL, NULL, &tm)) {
        if ( (nb = read (fd, ptr, nleft)) < 0) {
            if (errno == EINTR || errno == EAGAIN)
                nb = 0;             /* and call recv() again */
            else
                return (-1);
        } else if (nb == 0)
            break;                  /* EOF */
        nleft -= nb;
        ptr   += nb;
        nread += nb;
      }
    }

    return (nread);                 /* return no. of bytes read */
}


/**
 *  VO_TASKWRITE -- Write exactly "n" bytes to a task descriptor. 
 *
 *  @brief  Send exactly "n" bytes to a task descriptor. 
 *  @fn     size_t vo_taskWrite (int fd, void *vptr, size_t nbytes)
 *
 *  @param  fd          file descriptor
 *  @param  vptr        data buffer to be written
 *  @param  nbytes      number of bytes to write
 *  @return             number of bytes written
 */
static size_t  
vo_taskWrite (int fd, void *vptr, size_t nbytes)
{
    char    *ptr = vptr;
    int     nwritten = 0,  nleft = nbytes, nb = 0, flags = 0;
    fd_set  allset, fds;
    struct  timeval tm = { 2, 0 };


    /*  Set non-blocking mode on the descriptor.
     */
    if ((flags = fcntl (fd, F_GETFL, 0)) == 0)
        if (fcntl (fd, F_SETFL, flags | O_NONBLOCK) < 0)
	    ; 				/* ignore error */

    FD_ZERO (&allset);
    FD_SET (fd, &allset);

    while (nleft > 0) {
      memcpy (&fds, &allset, sizeof(allset));
      if (select (SELWIDTH, NULL, &fds, NULL, &tm)) {
        if ( (nb = write (fd, ptr, nleft)) <= 0) {
            if (errno == EINTR || errno == EAGAIN)
                nb = 0;             	/* and call write() again */
            else
                return (-1);
        }
        nleft    -= nb;
        ptr      += nb;
        nwritten += nb;
      }
    }

    return (nwritten);
}



#ifdef VO_UNIT_TESTS

/************************************************************************/
 *  Test Task declarations.
 */
int test_0 (int argc, char **argv, size_t *len, void **result);
int test_1 (int argc, char **argv, size_t *len, void **result);
int test_2 (int argc, char **argv, size_t *len, void **result);
int test_3 (int argc, char **argv, size_t *len, void **result);
int test_4 (int argc, char **argv, size_t *len, void **result);
int test_5 (int argc, char **argv, size_t *len, void **result);
int test_6 (int argc, char **argv, size_t *len, void **result);
int test_7 (int argc, char **argv, size_t *len, void **result);

Task testApps[] = {
   { "test0",		test_0 },	/* test applications 		*/
   { "test1",		test_1 },
   { "test2",		test_2 },
   { "test3",		test_3 },
   { "test4",		test_4 },
   { "test5",		test_5 },
   { "test6",		test_6 },
   { "test7",		test_7 },
   { NULL, 		NULL      },
};


/*  Child doesn't return a result.
 */
int test_0 (int argc, char **argv, size_t *len, void **result)
{
    return (0);
}

/*  Child returns an error code.
 */
int test_1 (int argc, char **argv, size_t *len, void **result)
{
    return (1);
}

/*  Child returns a result.
 */
int test_2 (int argc, char **argv, size_t *len, void **result)
{
    char  *str = strdup ("this is a test reply object");

    *len = strlen (str);
    if (*result == NULL)
	*result = calloc (1, strlen (str) + 1);
    strcpy ((char *) *result, str);

    free (str);
    return (0);
}

/*  Child exits with status code
 */
int test_3 (int argc, char **argv, size_t *len, void **result)
{
    _exit (5);
}

/*  Child exits w/ FPE
 */
int test_4 (int argc, char **argv, size_t *len, void **result)
{
    int i=1, j=0, k = 0;
    k = i / j;
}

/*  Child exits w/ segfault
 */
int test_5 (int argc, char **argv, size_t *len, void **result)
{
    char *foo = NULL;
    strcpy (foo, "bar");
}

/*  Child exits w/ SIGINT
 */
int test_6 (int argc, char **argv, size_t *len, void **result)
{
    raise (SIGINT);
}

/*  Multiple children....
 */
int test_7 (int argc, char **argv, size_t *len, void **result)
{
    pid_t  pid;

    if ((pid = fork()) < 0)
	perror ("fork fails");
    else if (pid == 0)
        raise (SIGILL);			/* child	*/
    else {
	sleep (1); wait (NULL); return (0);
    }
}


/**
 *  Program entry point.
 */
int
main (int argc, char *argv[])
{
    runTest ("test0", argc, argv);
    runTest ("test1", argc, argv);
    runTest ("test2", argc, argv);
    runTest ("test3", argc, argv);
    runTest ("test4", argc, argv);
    runTest ("test5", argc, argv);
    runTest ("test6", argc, argv);
    runTest ("test7", argc, argv);

    return (0);
}

int runTest (char *task, int argc, char **argv)
{
    int    status = 0, len = 0;
    void  *res = (void *) NULL;

    fprintf (stderr, "Running '%s' .... ", task);
    status = vo_runTask (task, argc, argv, &len, &res);
    fprintf (stderr, "status=%d  len=%d res='%s'\n", status, len, (char *)res);
    if (res)
	free (res);
}

#endif 	/* VO_UNIT_TESTS */
