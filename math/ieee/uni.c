/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

float uni_(dummy)
float *dummy;
{
	return(rand()/ 2147483648.0);	/* return a real [0.0, 1.0) */
}
