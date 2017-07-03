#! /bin/bash

# This is the uninstall script for Stock_Manager. 

version=001

set -e
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


conf_directory=$HOME/.stock_manager
[ -e conf_directory ] && 
	{
	echo "delete configuration directory" $conf_directory
#	procedure_operator_confirmation
	rm -rf $conf_directory
	}


#echo "uninstalling ..."
procedure_make
echo done
exit
