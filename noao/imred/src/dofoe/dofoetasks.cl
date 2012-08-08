#{ DOFOE tasks

task	dofoe		= "dofoe$dofoe.cl"
task	params		= "dofoe$params.par"

task	proc		= "dofoe$proc.cl"
task	response	= "dofoe$response.cl"
task	arcrefs		= "dofoe$arcrefs.cl"
task	doarcs		= "dofoe$doarcs.cl"
task	batch		= "dofoe$batch.cl"
task	listonly	= "dofoe$listonly.cl"

task	apscript	= "dofoe$x_apextract.e"

# Hide tasks from the user
hidetask apscript
hidetask params, proc, batch, arcrefs, doarcs, listonly, response

keep
