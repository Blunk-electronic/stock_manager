#! /bin/bash

# This is the install script for Stock_Manager. 

version=001

set -e

#CS: check for ada compiler, gcc, make, ...


dest_conf_dir=$HOME/.stock_manager
dest_data_dir=$HOME/stock_manager

echo "Stock_Manager installer version" $version

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
	make clean # clean up
	make # compile
	make install # install
	make clean # clean up
	cd - # change back to origin dir
	}

# If configuration already exists, leave it as it is.
# Otherwise create it with a base configuration for evaluation.
[ ! -e $dest_conf_dir ] && 
	{
	echo "creating hidden configuration directory" $dest_conf_dir "..."
	cp -R conf/stock_manager $dest_conf_dir
	}
	#CS: ask user if configuration directory should be updated.

# If database directory already exists, leave it as it is.
# Otherwise create it with a dummy database for evaluation.
[ ! -e $dest_data_dir ] && 
	{
	echo "creating stock database directory" $dest_data_dir "..."
	cp -R example_database $dest_data_dir
	}


	
echo "compiling and installing ..."
set +e

procedure_make

echo "installation complete"
echo "now edit file paths in" $dest_conf_dir/stock_manager.conf
exit
