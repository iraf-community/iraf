/**
 *
 *  VOTASK.C -- Utilities to run a VOApps task as a connected subprocess.
 *
 *
 * Expanded Interface
 * --------------------------
 * 
 * This is an attempt to define the simplest possible interface which is still
 * general enough to support the package/task paradigm, providing a self
 * describing capability to allow generic/reusable software to be written to
 * manipulate and run tasks.  The main simplification is that a "package" is
 * implemented as an executable (a single self contained file) which may
 * contain multiple tasks and which can be queried to determine its
 * capabilities.  This limits "packages" to implementations which can be self
 * contained enough to be runnable from a single runtime file, which is
 * adequate for now to support the VOClient CLI tasks and which can be
 * generalized later if necessary.
 * 
 * A "package" is a precompiled executable with a defined API which can be
 * queried with standard CLI arguments to get package metadata as well as to
 * run the individual tasks.  Metadata queries include user help as in
 * "-help", plus programatic information such as the package name and
 * description, list of tasks, task interfaces, builtin help, etc.  "Package"
 * executables are installed in a standard directory which can be examined by
 * the tasking layer to determine what packages are available.  The file names
 * of package executables follow a simple naming convention so that they can
 * be easily referenced at runtime.
 * 
 *
 *  Client Interface:
 *  =================
 *  
 *  Parameter Handling: 
 *  -------------------
 *
 *  Directed parameter access:
 *
 *       int = vo_taskParamCount  (rp)
 *           vo_taskGetParamInfo  (rp, pnum, *name, *type, *encoding,
 * 					 *description)
 *   str = vo_taskGetStringParam  (rp, pname, pnum)  # pnum used if pname=null
 *    ival = vo_taskGetBoolParam  (rp, pname, pnum)
 *     ival = vo_taskGetIntParam  (rp, pname, pnum)
 *    dval = vo_taskGetRealParam  (rp, pname, pnum)
 *   void = vo_taskGetParamValue  (rp, pname, pnum,  size)
 * 
 *  Iterated parameter access:
 *
 *          pp = vo_taskGetParam  (rp, pname, pnum)       
 *	       len = vo_paramLen  (pp, [typ|enc])
 *	       pp = vo_paramNext  (pp, [typ|enc])
 *	      str = vo_paramAttr  (pp, attr)
 *
 *
 *
 *  @file       voParam.c
 *  @author     Mike Fitzpatrick
 *  @date       9/24/12
 *
 *  @brief      VOClient Tasking Interface.
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
#include "voTask.h"
 



/**************************************************************************/
/***	Package Introspection Methods					***/
/**************************************************************************/

/**
 *  VO_PKGLIST -- List packages available in the working directory..
 *
 *  @brief   List packages available in the working directory.
 *  @fn      xml = vo_pkgList (char *pattern)
 *
 *  @param   pattern   Package executable filename pattern (or NULL for all)
 *  @returns           XML description of the package list.
 */
char *
vo_pkgList (char *pattern)
{
    return (NULL);
}


/**
 *  VO_TASKLIST -- List tasks in package.
 *
 *  @brief   List tasks in package.
 *  @fn      handle = vo_taskList (char *pkgName)
 *
 *  @param   pkgName   Package name
 *  @returns           XML description of the task list
 */
char *
vo_taskList (char *pkgName)
{
    return (NULL);
}


/**
 *  VO_TASKPARAMS -- List parameters for a task.
 *
 *  @brief   List parameters for a task.
 *  @fn      xml = vo_taskParams (char *pkgName, char *taskName)
 *
 *  @param   pkgName   Package name
 *  @param   taskName  Task name
 *  @returns           XML description of the parameter list
 */
char *
vo_taskParams (char *pkgName, char *taskName)
{
    return (NULL);
}
 
                   
/**
 *  VO_SETPKGDIR -- Set the working package directory.
 *
 *  @brief   Set the working package directory.
 *  @fn      vo_setPkgDir (char *path)
 *
 *  @param   path      Path to current working package directory
 *  @returns           nothing
 */
void
vo_setPkgDir (char *path)
{
}


/**
 *  VO_GETPKGDIR -- Get the working package directory.
 *
 *  @brief   Get the working package directory.
 *  @fn      path = vo_getPkgDir (void)
 *
 *  @returns           path to current working package directory
 */
char *
vo_getPkgDir (void)
{
    return (NULL);
}



/**************************************************************************/
/***	Task Execution Methods						***/
/**************************************************************************/

/**
 *  VO_TASKINIT -- Prepare to run a task.
 *
 *  @brief   Prepare to run a task.
 *  @fn      handle = vo_taskInit (char *pkgName, char *taskName)
 *
 *  @param   pkgName   Package name
 *  @param   taskName  Task name
 *  @returns           handle to the task descriptor
 */
handle_t
vo_taskInit (char *pkgName, char *taskName) 
{
    return (0);
}

 
/**
 *  VO_TASKSETSTRINGPARAM -- Set a string-valued param.
 *
 *  @brief   Set a string-valued param.
 *  @fn      str = vo_taskSetStringParam (handle_t tp, char *paramName, 
 *			char *sval)
 *
 *  @param   tp        Task handle
 *  @param   paramName Parameter name
 *  @param   sval      Parameter value
 *  @returns           nothing
 */         
void
vo_taskSetStringParam (handle_t tp, char *paramName, char *sval)
{
}

           
/**
 *  VO_TASKSETBOOLPARAM -- Set a bool-valued param.
 *
 *  @brief   Set a bool-valued param.
 *  @fn      str = vo_taskSetBoolParam (handle_t tp, char *paramName, int bval)
 *
 *  @param   tp        Task handle
 *  @param   paramName Parameter name
 *  @param   bval      Parameter value (0=false, 1=true)
 *  @returns           nothing
 */         
void
vo_taskSetBoolParam (handle_t tp, char *paramName, int bval)
{
}

            
/**
 *  VO_TASKSETINTPARAM -- Set a int-valued param.
 *
 *  @brief   Set a int-valued param.
 *  @fn      str = vo_taskSetIntParam (handle_t tp, char *paramName, int ival)
 *
 *  @param   tp        Task handle
 *  @param   paramName Parameter name
 *  @param   ival      Parameter value
 *  @returns           nothing
 */         
void
vo_taskSetIntParam (handle_t tp, char *paramName, int ival)
{
}

           
/**
 *  VO_TASKSETREALPARAM -- Set a real-valued param.
 *
 *  @brief   Set a real-valued param.
 *  @fn      str = vo_taskSetRealParam (handle_t tp, char *paramName, 
 *			double rval)
 *
 *  @param   tp        Task handle
 *  @param   paramName Parameter name
 *  @param   dval      Parameter value
 *  @returns           nothing
 */         
void
vo_taskSetRealParam (handle_t tp, char *paramName, double dval)
{
}


#ifdef FOO
 
            vo_taskSetCallback (tp, &func)   # Set output param callback
      rp =  vo_taskExecuteSync (tp)      	# Execute synchronous; returns
 					#    output pset pointer
           vo_taskExecuteAsync (tp)      	# Execute async
              rp = vo_taskWait (tp)      	# Wait for task, get RP
     tatus = vo_taskCheckError (tp, *msg)	# Check for any error posted 
 						#    to TP or RP
#endif
 


/**************************************************************************/
/***	Output Parameters						***/
/**************************************************************************/

/**
 *  VO_TASKPARAMCOUNT -- Return the number of available output params.
 *
 *  @brief   Return the number of available output params.
 *  @fn      status = vo_taskParamCount (handle_t rp)
 *
 *  @param   rp        Return handle
 *  @returns           Number of available output params
 */
int
vo_taskParamCount (handle_t rp)      	
{
    return (0);
}


/**
 *  VO_TASKGETPARAMINFO -- Get information about an indexed  parameter.
 *
 *  @brief   Get information about an indexed parameter.
 *  @fn      status = vo_taskGetParamInfo (handle_t rp, int pnum,
 *			char *name, char *type, char *encoding, 
 *			char *description)
 *
 *  @param   rp        Return handle
 *  @param   pnum      Parameter index (0-based, used if paramName=NULL)
 *  @param   name      Parameter name
 *  @returns           pointer to parameter value
 */
void
vo_taskGetParamInfo (handle_t rp, int pnum, char *name, char *type, 
			char *encoding, char *description)
{
    return;
}
 

/**
 *  VO_TASKGETPARAMVALUE -- Get a value of the named/indexed output parameter.
 *
 *  @brief   Get a value of the named/indexed output parameter.
 *  @fn      status = vo_taskGetParamValue (handle_t rp, char *paramName,
 *				int pnum)
 *
 *  @param   rp        Return handle
 *  @param   paramName Parameter name
 *  @param   pnum      Parameter index (0-based, used if paramName=NULL)
 *  @returns           pointer to parameter value
 */
void *
vo_taskGetParamValue (handle_t rp, char *paramName, int pnum)       
{
    return ((void *) NULL);
}


/**
 *  VO_TASKGETSTRINGPARAM -- Get a string-valued output parameter.
 *
 *  @brief   Get a string-valued output parameter.
 *  @fn      str = vo_taskGetStringParam (handle_t rp, char *paramName, 
 *			int pnum)
 *
 *  @param   rp        Return handle
 *  @param   paramName Parameter name
 *  @param   pnum      Parameter index (0-based, used if paramName=NULL)
 *  @returns           parameter value as a character string
 */
char *
vo_taskGetStringParam (handle_t rp, char *paramName, int pnum)
{
    return ((char *) NULL);
}


/**
 *  VO_TASKGETBOOLPARAM -- Get a bool-valued output parameter.
 *
 *  @brief   Get a bool-valued output parameter.
 *  @fn      bval = vo_taskGetBoolParam (handle_t rp, char *paramName, int pnum)
 *
 *  @param   rp        Return handle
 *  @param   paramName Parameter name
 *  @param   pnum      Parameter index (0-based, used if paramName=NULL)
 *  @returns           parameter value as a bool (0=false, 1=true)
 */
int
vo_taskGetBoolParam (handle_t rp, char *paramName, int pnum)
{
    return (0);
}


/**
 *  VO_TASKGETINTPARAM -- Get a int-valued output parameter.
 *
 *  @brief   Get a int-valued output parameter.
 *  @fn      ival = vo_taskGetIntParam (handle_t rp, char *paramName, int pnum)
 *
 *  @param   rp        Return handle
 *  @param   paramName Parameter name
 *  @param   pnum      Parameter index (0-based, used if paramName=NULL)
 *  @returns           parameter value and an integer
 */
int
vo_taskGetIntParam (handle_t rp, char *paramName, int pnum)
{
    return (0);
}


/**
 *  VO_TASKGETREALPARAM -- Get a real-valued output parameter.
 *
 *  @brief   Get a real-valued output parameter.
 *  @fn      dval = vo_taskGetRealParam (handle_t rp, char *paramName, int pnum)
 *
 *  @param   rp        Return handle
 *  @param   paramName Parameter name
 *  @param   pnum      Parameter index (0-based, used if paramName=NULL)
 *  @returns           parameter value as a double-precision
 */
double
vo_taskGetRealParam (handle_t rp, char *paramName, int pnum)
{
    return ((double) 0.0);
}
 
                  
/**
 *  VO_TASKCLOSE -- Free task resources.
 *
 *  @brief   Free task resources.
 *  @fn      status = vo_taskClose (handle_t tp)
 *
 *  @param   tp        Task handle
 *  @returns           zero if OK, one on ERROR
 */
int
vo_taskClose (handle_t tp)
{
    return (OK);
}
