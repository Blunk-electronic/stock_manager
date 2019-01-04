# stock_manager
A simple ERP System

# Installation
- The installation creates the executable stock_manager by
  default in /usr/local/bin . This requires root privileges.
  If this default location is not desired, modify the Makefile
  accordinly. 
- Another file gets created by the installer in your home directory
  in the hidden directory .stock_manager/conf/stock_manager.conf
  where you must set the path to the item database.
- A sample database is generated in your home directory in directory
  stock_manager. The database is a csv file named stock_db.csv.
  This directory also contains example files like order and withdrawal lists.

In order just to install the sample database run the install script:

```sh
sh install.sh
```

<!-- Edit the configuration file .stock_manager/conf/stock_manager.conf -->

To install only the binary file, run the install script with the option 'binary'.
This of course requires root privileges. The home directory of root remains untouched:

```sh
sh install.sh binary
```


## Uninstallation

Run the de-install script:

```sh
sh uninstall.sh
```

## ToDo
- Argument for the install and uninstall scripts to specifiy a dedicated location
  for the executable. This would enable the user to install in
  her home directory without being root.
- Here in this file: some examples on how to use stock_manager.

## Contact
Contact: http://www.blunk-electronic.de

Your feedback is highly welcome !

