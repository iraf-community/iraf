# Set the HSI architecture.

# Set the link 'as'.
# Domain/OS barfs on test -h; uses test -L instead.
if [ ver ]; then\
    if test -L as; then rm -rf as; fi;\
else\
    if test -h as; then rm -rf as; fi;\
fi
ln -s as.$MACH as

# Ditto for 'bin'.
if [ ver ]; then\
    if test -L bin; then rm -rf bin; fi;\
else\
    if test -h bin; then rm -rf bin; fi;\
fi
ln -s bin.$MACH bin
