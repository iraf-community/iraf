for obj in m1 m31 ngc4525 
do
	vosesame -a $obj
        vosesame -d $obj
        vosesame -e $obj
        vosesame -n $obj
        vosesame -s $obj
        vosesame -t $obj
	vosesame -fa $obj
        vosesame -fd $obj
        vosesame -fe $obj
        vosesame -fs $obj
        vosesame -ft $obj
	vosesame -Afa $obj
        vosesame -Cfd $obj
        vosesame -Hfe $obj
        vosesame -Tfs $obj
        vosesame -Tfs -o sillyfile.txt $obj
	vosesame --all $obj
        vosesame --decimal $obj
        vosesame --errors $obj
        vosesame --name $obj
        vosesame --sex $obj
        vosesame --type $obj
	vosesame --force --all $obj
        vosesame --force --decimal $obj
        vosesame --force --error $obj
        vosesame --force --name $obj
        vosesame --force --sex $obj
        vosesame --force --type $obj
	vosesame --ascii --force --all $obj
        vosesame --comma --force --decimal $obj
        vosesame --header --force --errors $obj
        vosesame --tab --force --sex $obj
        vosesame --tab --force --sex --output=sillyfile.txt $obj
done
