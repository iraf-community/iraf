/**
 *  VOAPPS.h -- Task declarations for the VOClient Package applications.
 *
 *  @file       voApps.h
 *  @author     Mike Fitzpatrick
 *  @date       6/03/11
 *
 *  @brief      Task declarations for the VOClient Package applications.
 */


/*  Task structure.
 */
typedef struct {
   char	*taskName;				/* task name		      */
   int   (*func)(int argc, char **argv);      	/* task entry function        */
} Task;


/*  Task entry-point declarations.
 */
extern int  votcopy (int argc, char **argv);
extern int  votget (int argc, char **argv);
extern int  votinfo (int argc, char **argv);

extern int  vodata (int argc, char **argv);
extern int  vodirectory (int argc, char **argv);
extern int  vosesame (int argc, char **argv);



/*  Task application table.  Applications must be declared here to be found
 *  and used with the generic main().
 */
Task voApps[] = {
   { "votcopy",		votcopy     },		/* VOTable applications	      */
   { "votget", 		votget      },
   { "votinfo",		votinfo     },
   { "vosesame",	vosesame    },		/* VO-CLI applications        */
   { "vodata",		vodata 	    },		/* VO-CLI applications        */
   { "vodirectory",	vodirectory },		/* VO-CLI applications        */
   { NULL,		NULL	    }
};
