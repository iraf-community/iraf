/* IMTOOL.H -- Global definitions for IMTOOL.
 */
#define	CURSOR_OFF	3		/* turn cursor off entirely	*/
#define	CURSOR_ON	4		/* turn it back on		*/

struct	fonttab {		/* Imtool font descriptor. */
	short	pointsize;
	char	ch_xsize, ch_ysize;
	short	win_xsize, win_ysize;
	struct	pixfont *pixfont;
	char	*path;
	char	*label;
};
