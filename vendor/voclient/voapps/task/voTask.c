/**
 *
 *  VOTASK.C -- Utilities to run a VOApps task as a connected subprocess.
 *
 *
 *  Client Interface:
 *  =================
 *  
 *  Task Execution:						
 *  ---------------
 *
 *              tp = vo_taskInit  (pkgName, taskName) # Initialize a task
 * 
 *         vo_taskSetStringParam  (tp, pname, sval)   # Set input params
 *           vo_taskSetBoolParam  (tp, pname, bval)
 *            vo_taskSetIntParam  (tp, pname, ival)
 *           vo_taskSetRealParam  (tp, pname, dval)
 *            vo_taskSetPtrParam  (tp, pname, ptr, size)
 * 
 *            vo_taskSetCallback  (tp, type, &func)   # Set param callback
 *       rp = vo_taskExecuteSync  (tp, status, msg)   # Execute sync
 *           vo_taskExecuteAsync  (tp)      	      # Execute async
 *              rp = vo_taskWait  (tp)      	      # Wait for task, get RP
 *    status = vo_taskCheckError  (tp, msg)	      # Check for any error
 * 
 *                  vo_taskClose  (tp)      	      # Free task resources
 * 
 *
 *  @file       voTask.c
 *  @author     Mike Fitzpatrick & Doug Tody
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
}

 
/**
 *  VO_TASKSETSTRINGPARAM -- Set a string-valued param.
 *
 *  @brief   Set a string-valued param.
 *  @fn      vo_taskSetStringParam (handle_t tp, char *paramName, char *sval)
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
 *  @fn      vo_taskSetBoolParam (handle_t tp, char *paramName, int bval)
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
 *  @fn      vo_taskSetIntParam (handle_t tp, char *paramName, int ival)
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
 *  @fn      vo_taskSetRealParam (handle_t tp, char *paramName, double rval)
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


/**
 *  VO_TASKSETRAWPARAM -- Set a raw-valued param.
 *
 *  @brief   Set a raw-valued param.
 *  @fn      vo_taskSetRawParam (handle_t tp, char *paramName, void *ptr,
 *			size_t size)
 *
 *  @param   tp        Task handle
 *  @param   paramName Parameter name
 *  @param   ptr       Pointer to raw data block
 *  @param   size      Size of data block
 *  @returns           nothing
 */         
void
vo_taskSetRawParam (handle_t tp, char *paramName, void *ptr, size_t size)
{
}


/**
 *  VO_TASKSETCALLBACK -- Set a callback for a parameter type.
 *
 *  @brief   Set a callback for a parameter type.
 *  @fn      vo_taskSetCallback (handle_t tp, int type, void *func)
 *
 *  @param   tp        Task handle
 *  @param   type      Parameter type
 *  @param   func      Callback function 
 */
void
vo_taskSetCallback (handle_t tp, int type, void *func)
{
}


/**
 *  VO_TASKEXECUTESYNC -- Execute a task synchronously.
 *
 *  @brief   Execute a task synchronously.
 *  @fn      rp = vo_taskExecuteSync (handle_t tp, int *status, char *msg)
 *
 *  @param   tp        Task handle
 *  @param   status    Task exit status
 *  @param   msg       Buffer for returned error message string
 *  @returns           Handle to output param list
 */         
handle_t
vo_taskExecuteSync (handle_t tp, int status, char *msg)
{
}

           
/**
 *  VO_TASKEXECUTEASYNC -- Execute a task asynchronously.
 *
 *  @brief   Execute a task asynchronously.
 *  @fn      rp = vo_taskExecuteAsync (handle_t tp)
 *
 *  @param   tp        Task handle
 *  @returns           Handle to output param list
 */         
void
vo_taskExecuteAsync (handle_t tp)
{
}


/**
 *  VO_TASKWAIT -- Wait for an asynchronous task to complete.
 *
 *  @brief   Wait for an asynchronous task to complete.
 *  @fn      rp = vo_taskWait (handle_t tp, int *status, char *msg)
 *
 *  @param   tp        Task handle
 *  @param   status    Task exit status
 *  @param   msg       Buffer for returned error message string
 *  @returns           Handle to output param list
 */         
handle_t
vo_taskWait (handle_t tp, int *status, char *msg)
{
}


/**
 *  VO_TASKCHECKERROR -- Get the task exit status and (any) error message.
 *
 *  @brief   Get the task exit status and (any) error message.
 *  @fn      status = vo_taskCheckError (handle_t tp, char *msg)
 *
 *  @param   tp        Task handle
 *  @param   msg       Buffer for returned error message string
 *  @returns           Task exit status
 */         
int
vo_taskCheckError (handle_t tp, char *msg)
{
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
