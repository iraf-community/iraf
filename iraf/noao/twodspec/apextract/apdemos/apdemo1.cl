# Create demo data if needed.
artdata
mkexample ("multifiber", "apdemo", errors=no, verbose=yes, list=no)
bye
imdelete ("apdemo.ms.??h", verify=no)

# Set parameters.
verbose = yes
logfile = ""
plotfile = ""
unlearn apall

# Execute playback.
stty (playback="apdemos$apdemo1.dat", verify=no, delay=500)
