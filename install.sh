#! /bin/bash

# This is the install script for Stock_Manager. 

set -e

#CS: check for ada compiler, gcc, make, ...

dest_conf_dir=$HOME/.stock_manager
dest_data_dir=$HOME/stock_manager

echo "Stock_Manager installer"

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




	
echo "installing ..."

# Test if user wants to install the binary file or just the sample database:
if [ "$1" == "binary" ]; then
	echo "compiling ..."
	procedure_make

else

	# If configuration already exists, leave it as it is.
	# Otherwise create it with a base configuration for evaluation.
	[ ! -e $dest_conf_dir ] && 
		{
		echo "creating hidden configuration directory" $dest_conf_dir "..."
		cp -R conf/stock_manager $dest_conf_dir
		}
		#CS: ask user if configuration directory should be updated.

	# Overwrite existing database and subdirectories.
	echo "creating stock database directory" $dest_data_dir "..."
	mkdir -p $dest_data_dir
	cp -R example_database/* $dest_data_dir


	echo "A sample database has been created:" $dest_data_dir/stock_db.csv
	echo "Now edit file paths in" $dest_conf_dir/stock_manager.conf
	echo "For evaluation and training leave settings as they are."
fi

echo "Installation complete"
exit
