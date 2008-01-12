# GF_GFIND.COM -- Common block containing variables of gf_gfind

common	/ gfind /  gf_init, imhead, primary

int	gf_init			# have the variables been initialized?
pointer	imhead			# image header descriptor
char	primary[SZ_PATHNAME]	# name of image header

