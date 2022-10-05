# Stock Manager
A simple ERP system to administrate an electronic component stock.

## Basics
The software consists of three parts:
- A sample database in your home directory in sub-directory stock_manager. 
  The database is a csv file named stock_db.csv.
  This directory also contains example files like order and withdrawal lists.
- The configuration in a hidden directory .stock_manager/conf/stock_manager.conf
  where you can edit the path to the database. There is nothing to do if you
  just want to play around with stock_manager or if you are evaluating the program.
- The executable (binary) file stock_manager itself.



## Installation of the Sample Database
### Default Installation
In order just to install the sample database in your own home 
directory run this command:
```sh
sh install.sh database
```
WARNING: The database already existing will be overwritten without warning!
The name of the directory containing the database is fixed to "stock_manager".

### Installation in an Explicitely Given Home Directory
To install the sample database in the home of another user run:
```sh
sh install.sh database user_name
```
WARNING: The database already existing will be overwritten without warning!
The name of the directory containing the database is fixed to "stock_manager".




## Installation of the Configuration Files
### Default Installation
In order just to install the default configuration in your own home 
directory run this command:
```sh
sh install.sh configuration
```
The name of the hidden directory containing the configuration is
fixed to ".stock_manager".
If a configuration already exists, then it will NOT be overwritten.
If required, edit the configuration file .stock_manager/conf/stock_manager.conf.

### Installation in an Explicitely Given Home Directory
To install the configuration files in a special location run this command:
```sh
sh install.sh configuration user_name
```
The name of the hidden directory containing the configuration is
fixed to ".stock_manager".
If a configuration already exists, then it will NOT be overwritten.
If required edit the configuration file .stock_manager/conf/stock_manager.conf.





## Installing the Executable (binary) File
### Default Installation
To install the binary file in your own home ($HOME/bin) run this command:
```sh
sh install.sh binary
```

### Installation in an Explicitely Given Directory
In order to make the stock_manager available for all users or
to install the binary file in a special location run this command:
```sh
sh install.sh binary /usr/local/bin
```
This of course requires root privileges.



# Uninstallation
Run the de-install script:
```sh
sh uninstall.sh
```
WARNING: The un-install-script is not complete yet and under construction !


# ToDo
- Here in this file: some examples on how to use stock_manager.

# Contact
Contact: http://www.blunk-electronic.de

Your feedback is highly welcome !

