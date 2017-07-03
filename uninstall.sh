#! /bin/bash

# This is the uninstall script for Stock_Manager. 

version=001

set -e

dest_conf_dir=$HOME/.stock_manager


echo "Stock_Manager uninstaller version" $version

procedure_operator_confirmation()
	{
	echo -n "proceed ? (y/n): "
	read key
	echo
	[ ! $key = "y" ] && 
		{
		echo "installation aborted by operator"
		exit 1
		}
	}

procedure_make()
	{
	cd src # change into source dir
	make uninstall # uninstall
	cd - # change back to origin dir
	}



[ -e $dest_conf_dir ] && 
	{
	echo "delete configuration directory" $dest_conf_dir
#	procedure_operator_confirmation
	rm -rf $dest_conf_dir
	}


#echo "uninstalling ..."
procedure_make
echo done
exit
