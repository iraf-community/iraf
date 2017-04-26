/**
 *  VOTASK.h -- Tasking interface declarations for the VOClient Package.
 *
 *  @file       voTask.h
 *  @author     Mike Fitzpatrick
 *  @date       9/24/12
 *
 *  @brief      Tasking interface declarations for the VOClient Package.
 */


#include <stdio.h>
#include <stdlib.h>


typedef int     handle_t;

#ifdef  OK
#undef  OK
#endif
#define OK      0

#ifdef  ERR
#undef  ERR
#endif
#define ERR     1

#ifdef  SZ_FORMAT
#undef  SZ_FORMAT
#endif
#define	SZ_FORMAT		32

#ifdef  SZ_FNAME
#undef  SZ_FNAME
#endif
#define SZ_FNAME		256

#ifdef  SZ_LINE
#undef  SZ_LINE
#endif
#define	SZ_LINE			4096


#define MAX_TASKS		128


/*  Debug and verbose flags.
 */
#define VOTASK_DEBUG (getenv("VOTASK_DBG")||access("/tmp/VOTASK_DBG",F_OK)==0)
#define VOTASK_VERB  (getenv("VOTASK_VERB")||access("/tmp/VOTASK_VERB",F_OK)==0)

/**
 *  Output parameter types.
 */
#define FORMATS "|int|rel|string|bool|raw|votable|xml|fits|html|"

#define TY_INT     	1               /* integer			*/
#define TY_REAL     	2               /* double-precision		*/
#define TY_STRING     	3               /* string			*/
#define TY_BOOL     	4               /* bool				*/
#define TY_RAW     	5               /* blob				*/

#define TY_VOTABLE	6               /* A VOTable                	*/
#define TY_XML     	7               /* A raw XML doc		*/
#define TY_FITS     	8               /* A FITS file			*/
#define TY_HTML     	9               /* A HTML doc			*/



/*  Parameter structure.
 */
typedef struct {
   char	   name[SZ_FNAME];		/* parameter name	      	*/
   char	   descr[SZ_FNAME];		/* parameter description      	*/

   int 	   type;			/* parameter type (int, etc)	*/
   int 	   encoding;			/* parameter encoding 		*/
   int     numValues;			/* number of parameter values	*/

   void   *defaultValue;		/* default value		*/
   void   *value;			/* ptr to value			*/
   size_t  valueLen;			/* size of value		*/

   void    *next;			/* next linked-list param	*/
} Param, *ParamP;

typedef struct {
    int    nParams;			/* number of params		*/
    Param *param;			/* parameter itself		*/
} PSet, *PSetP;



/*  Task structure.
 */
typedef struct {
   char	  name[SZ_FNAME];		/* package name		      	*/
   char	  descr[SZ_FNAME];		/* package description	      	*/

   Param *inParams;			/* input params			*/
   int    nInParams;			/* number of input params	*/
   Param *outParams;			/* output params		*/
   int    nOutParams;			/* number of output params	*/

   int    status;			/* task return status		*/
   char  *msg;				/* task return err message	*/

   int    ntasks;			/* number of tasks in list	*/
   void  *next;				/* next linked-list task	*/
} Task, *TaskP;



/*  Package structure.
 */
typedef struct {
   char	  name[SZ_FNAME];		/* package name		      	*/
   char	  descr[SZ_FNAME];		/* package description	      	*/
   char	  author[SZ_FNAME];		/* package author	      	*/
   char	  contact[SZ_FNAME];		/* package contact email      	*/
   char	  iconUrl[SZ_FNAME];		/* package icon URL	      	*/
   char	  version[SZ_FNAME];		/* package version string      	*/

   Task   tasks[MAX_TASKS];		/* task list			*/
   int    ntasks;			/* number of tasks in package	*/
   Param *params;			/* package params		*/
   int    nparams;			/* number of package params	*/

   char	  cwd[SZ_FNAME];		/* current working directory	*/

   int    npkgs;			/* number of packages in list	*/
   void  *next;				/* next linked-list pkg		*/
} Package, *PackageP;
