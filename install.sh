#! /bin/bash

# This is the install script for Stock_Manager. 

version=001

set -e

#CS: check for ada compiler, gcc, make, ...


dest_conf_dir=$HOME/.stock_manager

echo "Stock_Manager installer version" $version

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
	make clean # clean up
	make # compile
	make install # install
	make clean # clean up
	cd - # change back to origin dir
	}

[ ! -e $dest_conf_dir ] && 
	{
	echo "creating hidden configuration directory" $dest_conf_dir "..."
	cp -R conf/stock_manager $dest_conf_dir
	}
#CS: ask user if configuration directory should be updated.


echo "compiling and installing ..."
set +e

procedure_make

echo done
exit
