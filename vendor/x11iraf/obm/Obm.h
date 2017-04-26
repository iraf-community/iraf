/* Copyright(c) 1993 Association of Universities for Research in Astronomy Inc.
 */

/*
 * OBM.H -- Public definitions for the object manager.
 */

#ifndef _Obm_Defined

#ifndef Obm_Private
typedef	struct { int dummy; } *ObmContext;
#endif

ObmContext ObmOpen (/* app_context, argc, argv */);
void ObmClose (/* obm */);
void ObmInitialize (/* obm */);
void ObmActivate (/* obm */);
void ObmDeactivate (/* obm, unmap */);
int ObmActivated (/* obm */);
int ObmStatus (/* obm, app_name, app_class */);
XtPointer ObmGetInterp (/* obm, object */);
int ObmDeliverMsg (/* obm, object, message */);
int ObmDeliverMsgFromFile (/* obm, object, filename */);
XtPointer ObmAddCallback (/* obm, fcn, callback_type, client_data */);
void ObmRemoveCallback (/* obm, callback_id */);

/* Callback type flags. */
#define	OBMCB_preserve			0000001 /* preserve over ObmInit */

#define	OBMCB_connect			0000010 /* callback types */
#define	OBMCB_activate			0000020
#define	OBMCB_deactivate		0000040
#define	OBMCB_setGterm			0000100
#define	OBMCB_clientOutput		0000200

#define	OBMUI_activate			0100000	/* internal */
#define	OBMUI_deactivate		0200000	/* internal */

/* ObmStatus states. */
#define	OBM_INITIALIZED			0
#define	OBM_ACTIVE			1
#define	OBM_IDLE			2

#define _Obm_Defined
#endif
