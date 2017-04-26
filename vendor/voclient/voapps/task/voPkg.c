/**
 *
 *  VOPKG.C -- Task and package management.
 *
 *  This file provides the introspection methods for a callable package.
 *  These routines are used to call a package binary to get the metadata
 *  for packages, tasks and parameters.
 *
 *  
 *  Package/Task Management:
 *  ------------------------
 *
 *                  vo_setPkgDir  (path)	      # set package dir
 *           path = vo_getPkgDir  ()	      	      # get package dir
 *
 *              pkg = vo_pkgList  (pattern)    	      # Get available packages
 *	         len = vo_pkgLen  (pkg)
 *	        pkg = vo_pkgNext  (pkg)
 *	        str = vo_pkgAttr  (pkg, attr)
 *
 *             pp = vo_pkgParams  (pkg)
 *            pp = vo_taskParams  (pkgName, taskName) # Input param defs
 *		( see parameter handling below )
 *	
 *            task = vo_taskList  (taskName) 	      # Get tasks in package
 *	        len = vo_taskLen  (task)
 *	       str = vo_taskNext  (task)
 *	      task = vo_taskAttr  (task, attr)
 *	
 *
 *           xml = vo_pkgListXML  (pattern)    	      # XML serializations (opt)
 *          xml = vo_taskListXML  (pkgName)
 *         xml = vo_paramListXML  (pkgName, taskName)
 *						
 *						
 *  @file       voPkg.c
 *  @author     Mike Fitzpatrick
 *  @date       9/24/12
 *
 *  @brief      Package and task management methods
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
}


/**************************************************************************/
/***	Package Management						***/
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
handle_t
vo_pkgList (char *pattern)
{
    return (NULL);
}


/**
 *  VO_PKGLIST -- List packages available in the working directory..
 *
 *  @brief   List packages available in the working directory.
 *  @fn      xml = vo_pkgList (char *pattern)
 *
 *  @param   pattern   Package executable filename pattern (or NULL for all)
 *  @returns           XML description of the package list.
 */
int
vo_pkgLen  (handle_t pkg)
{
    Package *pkgP = (Package *) task_H2P (pkg);

    return ( (pkgP ? pkgP->npkgs : 0) );
}


/**
 *  VO_PKGNEXT -- Get the next package in the list.
 *
 *  @brief   Get the next package in the list.
 *  @fn      pkg = vo_pkgNext (handle_t pkg)
 *
 *  @param   pkg       Handle to package
 *  @returns           Handle to next package in the list (0 at end of list)
 */
handle_t
vo_pkgNext  (handle_t pkg)
{
    Package *pkgP = (Package *) task_H2P (pkg);

    return ( (pkgP ? task_P2H (pkgP->next) : 0) );
}


/**
 *  VO_PKGATTR -- Get a package metadata by attribute name.
 *
 *  @brief   Get a package metadata by attribute name.
 *  @fn      pkg = vo_pkgAttr (handle_t pkg, char *attr)
 *
 *  @param   pattern   Package executable filename pattern (or NULL for all)
 *  @returns           Requested attribute string (or NULL if not found)
 */
char *
vo_pkgAttr  (handle_t pkg, char *attr)
{
    Package *pkgP = (Package *) task_H2P (pkg);

    return (NULL);
}
 

/**************************************************************************/
/***	Task Management 						***/
/**************************************************************************/

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
 *  VO_TASKLEN -- Get length of task list.
 *
 *  @brief   Get length of task list.
 *  @fn      len = vo_taskLen (handle_t task)
 *
 *  @param   task      Handle to task descriptor
 *  @returns           Number of tasks in the list.
 */
int
vo_taskLen  (handle_t task)
{
    Package *taskP = (Package *) task_H2P (task);

    return ( (taskP ? taskP->ntasks : 0) );
}


/**
 *  VO_TASKNEXT -- Get the next task in the list.
 *
 *  @brief   Get the next task in the list.
 *  @fn      task = vo_taskNext (handle_t task)
 *
 *  @param   task      Handle to task
 *  @returns           Handle to next task in the list (0 at end of list)
 */
handle_t
vo_taskNext  (handle_t task)
{
    Package *taskP = (Package *) task_H2P (task);

    return ( (taskP ? task_P2H (taskP->next) : 0) );
}


/**
 *  VO_TASKATTR -- Get a task metadata by attribute name.
 *
 *  @brief   Get a task metadata by attribute name.
 *  @fn      task = vo_taskAttr (handle_t task, char *attr)
 *
 *  @param   task      Handle to task
 *  @param   attr      Attribute to retrieve
 *  @returns           Requested attribute string (or NULL if not found)
 */
char *
vo_taskAttr  (handle_t task, char *attr)
{
    Package *taskP = (Package *) task_H2P (task);

    return (NULL);
}
 


/**************************************************************************/
/***	Task and Package Parameters					***/
/**************************************************************************/

/**
 *  VO_PKGPARAMS -- List parameters for a package.
 *
 *  @brief   List parameters for a package.
 *  @fn      xml = vo_pkgParams (handle_t pkg)
 *
 *  @param   pkg       Package handle
 *  @returns           XML description of the parameter list
 */
handle_t
vo_pkgParams (handle_t pkg)
{
    return (0);
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
handle_t
vo_taskParams (char *pkgName, char *taskName)
{
    return (0);
}



/**************************************************************************/
/***	XML Serializations						***/
/**************************************************************************/

/**
 *  VO_PKGLISTXML -- Get the package list as an XML document.
 *
 *  @brief   Get the package list as an XML document.
 *  @fn      xml = vo_pkgListXML (char *pattern)
 *
 *  @param   pattern   Package executable pattern
 *  @returns           XML description of the available packages.
 */
char *
vo_pkgListXML (char *pattern)
{
}


/**
 *  VO_TASKLISTXML -- Get the task list as an XML document.
 *
 *  @brief   Get the task list as an XML document.
 *  @fn      xml = vo_taskListXML (char *pkgName)
 *
 *  @param   pkgName   Package name
 *  @returns           XML description of the tasks in a package.
 */
char *
vo_taskListXML (char *pkgName)
{
}


/**
 *  VO_PARAMLISTXML -- Get the task parameter list as an XML document.
 *
 *  @brief   Get the task parameter list as an XML document.
 *  @fn      xml = vo_paramListXML (char *pkgName, char *taskName)
 *
 *  @param   pkgName   Package name
 *  @param   taskName  Task name
 *  @returns           XML description of the task parameters.
 */
char *
vo_paramListXML (char *pkgName, char *taskName)
{
}

