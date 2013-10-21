/**
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <Python.h>


typedef struct {
} voPackage;



/**
 *  VOP_TASKLIST -- List the tasks in the named package.
 */
static PyObject* 
vop_taskList (PyObject* self, PyObject* args)		/* pkg,pattern=None */
{
    const char *binpath;
    int status;

    if (!PyArg_ParseTuple (args, "s", &binpath))
        return NULL;


    sts = system(command);

    return Py_BuildValue("s", args);
}


/**
 *  VOP_PKGLIST -- List the packages in the search directory.
 */
static PyObject* 
vop_pkgList (PyObject* self, PyObject* args)		/* pattern=None     */
{
    char *s = "Hello from vop_pkgList";
    return Py_BuildValue("s", s);
}

static PyObject* 
vop_scan (PyObject* self, PyObject* args)
{
    char *s = "Hello from vop_scan";
    return Py_BuildValue("s", s);
}

static PyObject* 
vop_loadPackage (PyObject* self, PyObject* args)	/* name, file=None  */
{
    char *s = "Hello from vop_loadPackage";
    return Py_BuildValue("s", s);
}

/** ************************************************************************ */

/*
 * Bind Python function names to our C functions
 */
static PyMethodDef voPackage_methods[] = {
    { "vop_taskList",    vop_taskList,     METH_VARARGS },
    { "vop_pkgList",     vop_pkgList,      METH_VARARGS },
    { "vop_scan",        vop_scan,         METH_VARARGS },
    { "vop_loadPackage", vop_loadPackage,  METH_VARARGS },
    { NULL,				 NULL,             0			}
};


/**
 *  INITVOPACKAGE -- Python calls this to let us initialize our module
 */
void initvoPackage()
{
	(void) Py_InitModule("voPackage", voPackage_methods);
}
