/*
 * GTERMIO.H -- Public definitions for the gterm i/o protocol module.
 */
struct GT_function {
	char *name;		/* callback name */
	int (*func)();		/* callback function */
	XtPointer data;		/* callback data */
};
