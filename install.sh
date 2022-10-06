#! /bin/bash

# This is the install script for Stock_Manager. 

echo "Stock Manager installer"

set -e

#CS: check for ada compiler, gcc, make, ...

prog_name=stock_manager

# These variables are defaults and will be
# overridden if specified by the user:
dest_conf_dir=$HOME/.$prog_name
dest_data_dir=$HOME/$prog_name

# default target directory for executable:
target_binary_dir=$HOME/bin





# Make sure there are arguments. There must be at least 1:
if [ "$#" -lt 1 ]; then
	echo "ERROR ! Missing arguments."
	exit 1
fi
	
	

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
	make install
	make clean # clean up
	cd - # change back to origin dir
	}

	
procedure_make_special()
	{
	cd src # change into source dir
	make clean # clean up
	make # compile
	make install DESTDIR=$target_binary_dir
	make clean # clean up
	cd - # change back to origin dir
	}



# Test if user wants to install the binary file.
# Example 1 for default installation: 
#  install.sh binary
# Example 2 for user specified installation: 
#  install.sh binary /user_name/bin

if [ "$1" == "binary" ]; then
	echo "compiling ..."

	if [ "$#" -eq 2 ]; then
		# overwrite target_dir if explicitely specified by user:
		target_binary_dir=$2
		echo "installing in user specified target directory:" $target_binary_dir
		
		if [ -e "$target_binary_dir" ]; then
			procedure_make_special
		else
			echo "ERROR: Target directory $target_binary_dir does not exist !"
			exit 1
		fi
		
	else
		echo "installing in default target directory:" $target_binary_dir
		procedure_make
	fi
fi
	
	
	
	
	
# Test if user wants to install the configuration:
# Example 1 for default configuration: 
#  install.sh configuration
# Example 2 for user specified configuration: 
#  install.sh configuration user_name

if [ "$1" == "configuration" ]; then
# dest_conf_dir=$HOME/.stock_manager

	echo "installing configuration ..."

	if [ "$#" -eq 2 ]; then
		# Overwrite configuration home directory if explicitely specified by user.
		# The user can specify the home directory only. The actual directory name is
		# fixed. The directory is hidden:
		dest_conf_dir=$2/.$prog_name
		echo "installing configuration in user specified target directory:" $dest_conf_dir
	fi

	
	# If configuration already exists, leave it as it is.
	# Otherwise create it with a base configuration for evaluation.
	if [ ! -e $dest_conf_dir ]; then
		echo "creating hidden configuration directory" $dest_conf_dir "..."
		cp -R conf/stock_manager $dest_conf_dir
		#CS: ask user if configuration directory should be updated.
	fi

	echo "Now edit file paths in" $dest_conf_dir/stock_manager.conf	
	echo "For evaluation and training leave settings as they are."
fi





# Test if user wants to install the example stock database:
# Example 1 for default database location: 
#  install.sh database
# Example 2 for user specified database location: 
#  install.sh database user_name

if [ "$1" == "database" ]; then
# dest_data_dir=$HOME/stock_manager

	echo "installing database ..."

	if [ "$#" -eq 2 ]; then
		# Overwrite database directory if explicitely specified by user.
		# The user can specify the home directory only. The actual directory name is
		# fixed:
		dest_data_dir=$2/$prog_name
		echo "installing database in user specified target directory:" $dest_data_dir
	fi

	
	# Overwrite existing database and subdirectories.
	echo "creating stock database directory" $dest_data_dir "..."
	mkdir -p $dest_data_dir
	cp -R example_database/* $dest_data_dir

	echo "A sample database has been created:" $dest_data_dir/stock_db.csv
	echo "For evaluation and training leave settings as they are."
fi


exit
