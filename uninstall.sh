#! /bin/bash

# This is the uninstall script for Stock_Manager. 

version=001

set -e

dest_conf_dir=$HOME/.stock_manager
dest_data_dir=$HOME/stock_manager

echo "Stock_Manager uninstaller version" $version

procedure_operator_confirmation()
	{
	echo -n "proceed ? (y/n): "
	read key
	echo
	if [ ! $key = "y" ] 
		then
			echo "aborted by operator"
			exit 1
	fi
	}

procedure_make()
	{
	cd src # change into source dir
	make uninstall # uninstall
	cd - # change back to origin dir
	}



[ -e $dest_conf_dir ] && 
	{
	echo "WARNING: configuration directory" $dest_conf_dir " will be deleted !"
	procedure_operator_confirmation
	rm -rf $dest_conf_dir
	}

[ -e $dest_data_dir ] && 
	{
	echo "WARNING: database directory" $dest_data_dir " will be deleted !"
	procedure_operator_confirmation
	rm -rf $dest_data_dir
	}

	
echo "uninstalling binaries ..."
procedure_make
echo done
exit
