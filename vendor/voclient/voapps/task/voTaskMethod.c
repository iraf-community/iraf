/**
 *
 * 
 * Task Interface:                 	# Called by the task itself at runtime
 *
 *                  tp = vo_taskStart ()
 *        char *vo_taskGetPackageName (tp)
 *           char *vo_taskGetTaskName (tp)
 *      *char = vo_taskGetStringParam (tp, paramName)
 *         ival = vo_taskGetBoolParam (tp, paramName)
 *          ival = vo_taskGetIntParam (tp, paramName)
 *         dval = vo_taskGetBoolParam (tp, paramName)
 *
 *      status = vo_setIntOutputParam (tp, paramName, ival)
 *     status = vo_setRealOutputParam (tp, paramName, dval)
 *   status = vo_setStringOutputParam (tp, paramName, sval)
 *         status = vo_setOutputParam (tp, paramName, paramType, paramEncoding,
 *					*len, **void)
 *                    tp = vo_taskEnd (tp)
 *
 *
 *  Notes:
 *    1. Task should call vo_taskStart initially to get a runtime context,
 *       and vo_taskEnd when done to free resources, flush any output, and
 *       so forth.
 *    2. When a task is run in a "connected" (remote) fashion it is passed
 *       a keyword table of parameter values, via argc/argv or whatever.  
 *       True host level, CLI (argc/argv) mode can be provided as
 *       well in which case all the -help etc. generics can be provided by
 *       the task main (container).
 *       Defaulting can be performed task-side if provided.
 *    3. Input parameters can be retrieved by name (do we need to walk a
 *       list as well?  probably not in this case).
 *    4. Output parameters are flushed through to the client in each
 *       setOutputParam call, allowing interactive output at runtime.  A 
 *       single output parameter may be set multiple times, e.g.,
 *       for stdout, warning, status, etc. this could be normal.  The
 *       client tasking code accumulates output parameters in an output 
 *       param set during execution.  Whether or not parameters are passed
 *       on to a client callback is up to the client.
 *
 *  Whether or not a task executes directly in the client process or in a
 *  separate process is transparent to the client; normally tasks will execute
 *  in a subprocess.
 *
 */

#include "voTask.h"

