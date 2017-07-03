------------------------------------------------------------------------------
--                                                                          --
--                             STOCK MANAGER                                --
--                                                                          --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--
-- to do:
-- command to output stock summary, statistics, ... : total value, parts, ...
-- file locking of stock_db_csv file
-- log roll_back action in log file
-- create log directory if not present
-- if storage places assigned, make sure the place is available (probably not a good idea)
-- parse for valid accessories in function parse_part_code

-- history of changes
-- v003:
-- improved make_bom function: 
--		- configuration file supports customer specific prefixes
--		- eagle bom file header is searched for column holding part_code_fac
--		  in order to allow reading eagle bom files of third parties.
--		- alternative facility names supported in configuration file
-- v004:
-- bugfix:
--		- import eagle bom
-- added prefixes BAT and Q
-- added special character /

-- v005:
-- improved make_bom function:
--		- warning is output if no valid order source available for a part
-- improved add function:
-- 		- if a part is to be added that already exists, the part id is displayed

-- v006
-- bugfix: quantity written in order list fixed

-- v007
-- prefix ACCESSORY added. function parse_part_code does not conduct any deeper checks on those parts.
-- 

-- v008
-- prefix FH (for fuse holders) added. no value check is done here

-- v009
-- columns in withdrawal list re-ordered
-- storage place is written in withdrawal list
-- bugfix: on checkout_bom a copy of the withdrawal list is made in log directory
-- show_by_code renamed to show_by_fac_code
-- show_by_order_code supported

-- v010
-- facility bom file now contains manufacturer names and part codes
-- default names for order, withdrawal lists removed
-- stale order or withdrawal lists will no longer be removed on start

-- v011
-- prefixes: WIRE, CABLE, PLUG, RECEPTACLE, TERMINALF, TERMINALM supported
-- execption fixed that occured on checkout_bom due to limited file name lenght (in function make_filename_by_date)

-- v012
-- supports scaling of facility bom file
-- supports merging bom files to one

-- v013
-- prefix DIS (for displays) supported
-- bugfix: parts without any vendor information are now written with price 0.00 in bom
-- on error OL410, message outputs that order list can not be written due to insufficient permissions on file
-- search function improved: show_by_order_code and show_by_fac_code supports wildcards '*'
-- If a manufacturer or distributor is deleted (by changing the name to "n/a" all other information belonging to it
-- is delete also.
-- On query_bom and checkout_bom the names of order and withdrawal list are fixed. No longer passed as arguments.
-- When assigning part codes (facility, manufacturer, distributor), a warning is issued if code already used.


with ada.text_io;				use ada.text_io;
with ada.integer_text_io;		use ada.integer_text_io;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.maps;
with ada.strings; 				use ada.strings;
with ada.numerics.elementary_functions;	use ada.numerics.elementary_functions;

with ada.strings.unbounded.text_io; use ada.strings.unbounded.text_io;
with ada.exceptions; 			use ada.exceptions;
 
with gnat.os_lib;   			use gnat.os_lib;
with ada.command_line;			use ada.command_line;
with ada.directories;			use ada.directories;
with ada.environment_variables;
 
with ada.calendar;				use ada.calendar;
with ada.calendar.formatting;	use ada.calendar.formatting;
with ada.calendar.time_zones;	use ada.calendar.time_zones;

with ada.containers.generic_array_sort;
with ada.containers.generic_constrained_array_sort;

with sm_string_processing;	use sm_string_processing;
with sm_csv;				use sm_csv;

procedure stock_manager is


	version			: String (1..3) := "014";

	part_count_max2			: natural;
	empty_lines_count		: natural := 0;
	part_count_pdb			: natural := 300;
	manufacturer_count_max 	: natural := 3;
	distributor_count_max 	: natural := 6;

	default_date	: constant string (1..19) := "0000-00-00 00:00:00";

	arg_ct			: natural;
	arg_pt			: natural := 1;
	arg_ct_pdb		: natural := 0;
	arg_ct_prj		: natural := 0;
	arg_ct_bom		: natural := 0;
	arg_ct_fac		: natural := 0;

	parts_db_file			: ada.text_io.file_type;
	stock_db_file			: ada.text_io.file_type;
	conf_file				: ada.text_io.file_type;
	log_file				: ada.text_io.file_type;
	eagle_bom_file			: ada.text_io.file_type;
	bom_output_file			: ada.text_io.file_type;
	facility_bom_file		: ada.text_io.file_type;
	facility_bom_file_scaled: ada.text_io.file_type; -- ins v012
	items_to_order_file		: ada.text_io.file_type;
	items_to_take_file		: ada.text_io.file_type;
	quantity_of_units		: natural := 1;

	simple_date_field_length	: natural := 10; -- 1974-12-21
	package simple_date_type is new generic_bounded_length(simple_date_field_length); use simple_date_type;

	prog_position		: string (1..5) := "-----";

	line				: unbounded_string;
	
	scratch_natural		: natural := 0;

	part_ct_db			: natural := 0;
	part_ct_proj		: natural := 0;
	show_mode 			: natural := 1; -- reduced mode , 0 -> full mode
	debug_mode			: natural := 0; -- default is no debug mode

	operator_confirmation_required : boolean := true; -- per default the operator is requested to confirm actions

	not_assigned_mark		: constant string (1..3) := "n/a";
	row_separator			: constant string (1..60) := "------------------------------------------------------------";
	row_separator_double	: constant string (1..60) := "============================================================";

	valid_letter 	: constant ada.strings.maps.character_set := ada.strings.maps.to_set( ranges =>  ( ('A','Z'),('a','z'),('0','9') ) );
	--valid_special 	:	ada.strings.maps.character_set := ada.strings.maps.to_set("_-+%"); -- rm v002
	--valid_special 	:	ada.strings.maps.character_set := ada.strings.maps.to_set("_-+%."); -- ins v002 -- rm v004
	valid_special 	: constant ada.strings.maps.character_set := ada.strings.maps.to_set("_-+%./"); -- ins v004
	valid_character	: constant ada.strings.maps.character_set := ada.strings.maps."or"(valid_letter,valid_special); -- compose set of valid characters

	--type part_prefix_type is (R, RN, C, L, K, IC, LED, D, T, F, X, J, S); -- rm v003
	--type part_prefix_type is (C, D, F, J, K, L, R, S, T, X, IC, RN, LED); -- ins v003 -- rm v004
	--type part_prefix_type is (C, D, F, J, K, L, Q, R, S, T, X, IC, RN, LED, BAT); -- ins v004 -- rm v006
	--type part_prefix_type is (C, D, F, J, K, L, Q, R, S, T, X, IC, RN, LED, BAT, ACCESSORY); -- -- ins v007 -- rm v008
	type part_prefix_type is (C, D, F, J, K, L, Q, R, S, T, X, IC, RN, LED, BAT, ACCESSORY, 
			FH, -- fuse holder -- ins v008
			-- ins v011 begin
			CABLE, -- cable -- ins v011
			WIRE, -- wire
			PLUG, -- plug
			RECEPTACLE, -- receptacle
			TERMINALF, -- terminal female
			TERMINALM, -- terminal male
			MODULE, -- module
			DIS -- display -- ins v013
			); 
			-- ins v011 end
	-- NOTE: adopt function verify_prefix after any change here !

	type part_code_keyword_type is ( PAC_S , PAC_T , VAL , TOL , VMAX , TK ); -- DO NOT CHANGE POSITIONS OF PAC_S AND PAC_T !!!
	type accessory_keyword_type is ( CABLE , JUMPER , CONNECTOR , HOLDER , SOCKET , SCREW , NUT , WASHER , BAG ); -- ins v007

	part_prefix_count		: constant natural := part_prefix_type'pos((part_prefix_type'last)); -- number of allowed prefixes
	part_code_keyword_count	: constant natural := part_code_keyword_type'pos((part_code_keyword_type'last)); -- number of allowed keywords in part_code_fac
	--valid_prefix	: part_prefix_type;
	ifs_in_part_code_fac	: constant character := '_';

	--type stock_operation_type is (default, show_by_id, show_by_code, add, edit, delete, query_bom, make_bom, checkout_bom, roll_back, log); -- rm v009
	--type stock_operation_type is (default, show_by_id, show_by_fac_code, show_by_order_code, add, edit, delete, query_bom, make_bom, checkout_bom, roll_back, log); -- ins v009 -- rm v011
	-- ins v012 begin
	type stock_operation_type is (default, show_by_id, show_by_fac_code, show_by_order_code, add, edit, delete,
			query_bom, 
			make_bom, 
			checkout_bom, 
			roll_back, 
			log,
			scale_bom,
			merge_bom,
			show_by_manu_code -- ins v013
			); 
	-- ins v012 end

	stock_operation	: stock_operation_type := default;

	type part_property_type is ( 

			-- editable
			qty_delta_stock,
			qty_delta_reserved,
			part_code_fac,
			storage_place,
			remarks,
			project,

			-- cs: do something more professional here, consider manufacturer_count_max & distributor_count_max
			manufacturer_1_name, manufacturer_1_part_code, manufacturer_1_status_production, manufacturer_1_datasheet_1, manufacturer_1_datasheet_2,
			manufacturer_2_name, manufacturer_2_part_code, manufacturer_2_status_production, manufacturer_2_datasheet_1, manufacturer_2_datasheet_2,
			manufacturer_3_name, manufacturer_3_part_code, manufacturer_3_status_production, manufacturer_3_datasheet_1, manufacturer_3_datasheet_2,
			distributor_1_name, distributor_1_order_code, distributor_1_qty_min, distributor_1_price_net,
			distributor_2_name, distributor_2_order_code, distributor_2_qty_min, distributor_2_price_net,
			distributor_3_name, distributor_3_order_code, distributor_3_qty_min, distributor_3_price_net,
			distributor_4_name, distributor_4_order_code, distributor_4_qty_min, distributor_4_price_net,
			distributor_5_name, distributor_5_order_code, distributor_5_qty_min, distributor_5_price_net,
			distributor_6_name, distributor_6_order_code, distributor_6_qty_min, distributor_6_price_net,

			-- not editable
			part_id, qty_available
			); 
	part_property : part_property_type;

	-- define editable part properties
	subtype part_property_editable_type is part_property_type range qty_delta_stock..distributor_6_price_net; -- cs: consider distributor_count_max
	--part_property_editable 	: part_property_editable_type;

	universal_string_length	: constant natural := 200; -- changed from 100 to 200 in v013
	package universal_string_type is new generic_bounded_length(universal_string_length); use universal_string_type;

	customized_prefix_count_max	: constant natural := 20; -- ins v003
	type customized_prefixes_type is array (natural range <>) of universal_string_type.bounded_string; -- ins v003
	subtype customized_prefixes_type_sized is customized_prefixes_type (1..customized_prefix_count_max); -- ins v003
	customized_prefixes	: customized_prefixes_type_sized; -- ins v003
	customized_prefixes_count	: natural := 0; -- ins v003

	facility_count_max		: constant natural := 3; -- ins v003
	facility_count			: natural; -- ins v003
	type facility_name_type is array (natural range <>) of string (1..3);
	subtype facility_name_type_sized is facility_name_type (1..facility_count_max);
	facility_names	: facility_name_type_sized;

	action_given		: universal_string_type.bounded_string;
	parts_db_csv		: universal_string_type.bounded_string;
	eagle_bom_csv  		: universal_string_type.bounded_string;
	part_code_given		: universal_string_type.bounded_string;
	stock_db_csv		: unbounded_string := to_unbounded_string("stock_db.csv");
	-- rm v010 begin
	--bom_file_csv		: universal_string_type.bounded_string := to_bounded_string("bom.csv");
	--items_to_order_csv	: universal_string_type.bounded_string := to_bounded_string("order.csv");
	--items_to_take_csv	: universal_string_type.bounded_string := to_bounded_string("take.csv");
	-- rm v010 end
	bom_file_csv		: universal_string_type.bounded_string; -- ins v010
	items_to_order_csv	: universal_string_type.bounded_string; -- ins v010
	items_to_take_csv	: universal_string_type.bounded_string; -- ins v010
	facility_bom_csv	: universal_string_type.bounded_string;
	facility_bom_csv_scaled	: universal_string_type.bounded_string; -- ins v012
	facility_bom_csv_1	: universal_string_type.bounded_string; -- ins v012
	facility_bom_csv_2	: universal_string_type.bounded_string; -- ins v012
	facility_bom_csv_12	: universal_string_type.bounded_string := to_bounded_string("collective_bom.csv"); -- ins v012
	columns_of_facility_bom	: natural := 7;

	
	type part_of_facility_bom is
		record
			position	: natural := 0;
			qty 		: natural := 0;
			name		: unbounded_string;
			part_code	: universal_string_type.bounded_string;
			part_id		: natural := 0;
		end record;
	type parts_of_facility_bom is array (natural range <>) of part_of_facility_bom;


	type money is delta 0.01 digits 14;
	type money_positive is new money range 0.00 .. money'last;

	type distributor_type is
		record
			name		: universal_string_type.bounded_string := to_bounded_string(not_assigned_mark);
			order_code	: universal_string_type.bounded_string := to_bounded_string(not_assigned_mark);
			date_edited	: string (1..19) := "0000-00-00 00:00:00";
			qty_min 	: natural := 1;
			price_net	: money_positive := 0.00;
			valid		: boolean := false;
			-- cs: currency
			-- cs: payment
		end record;
	type distributor_array_type is array (natural range <>) of distributor_type;
	subtype distributor_array_sized is distributor_array_type (1..distributor_count_max); 
	

	type status_production_type is ( active, obsolete, discontinued, preliminary, not_for_new_designs, unknown );

	type manufacturer_type is
		record
			name				: universal_string_type.bounded_string := to_bounded_string(not_assigned_mark);
			part_code			: universal_string_type.bounded_string := to_bounded_string(not_assigned_mark);
			date_edited			: string (1..19) := "0000-00-00 00:00:00";
			status_production 	: status_production_type := unknown;
			url_datasheet_1		: unbounded_string := to_unbounded_string(not_assigned_mark);
			url_datasheet_2		: unbounded_string := to_unbounded_string(not_assigned_mark);

		end record;
	type manufacturer_array_type is array (natural range <>) of manufacturer_type;
	subtype manufacturer_array_sized is manufacturer_array_type (1..manufacturer_count_max); 


	type part_stock_type is
		record
			part_id				: natural := 0;
			part_code_fac		: universal_string_type.bounded_string := to_bounded_string(not_assigned_mark);
			date_edited			: string (1..19) := "0000-00-00 00:00:00";
			qty_on_stock		: natural := 0;
			qty_reserved		: natural := 0;
			qty_available		: natural := 0;
			manufacturers		: manufacturer_array_sized;
			distributors		: distributor_array_sized;
			storage_place		: universal_string_type.bounded_string := to_bounded_string(not_assigned_mark);
			remarks				: universal_string_type.bounded_string := to_bounded_string(not_assigned_mark);
			project				: universal_string_type.bounded_string := to_bounded_string(not_assigned_mark);
		end record;
	type part_stock_array_type is array (natural range <>) of part_stock_type;
--	subtype part_stock_array_sized is part_stock_array_type (1..part_count_max);

	part_id_given		: natural;

	type part_to_order is
		record
			part_id				: natural := 0;
			quantity			: natural := 0;
			part_code_fac		: universal_string_type.bounded_string := to_bounded_string(not_assigned_mark);
			distributors		: distributor_array_sized;
		end record;
	type parts_to_order is array (natural range <>) of part_to_order;

	field_ct_per_stock_db_file_line	: natural := (6 + (manufacturer_count_max * 6) + (distributor_count_max * 5) + 3);
	line_of_stock_db_file 			: part_stock_type;

	-- these paramters are read from configuration file
	home_directory			: universal_string_type.bounded_string;
	conf_directory			: string (1..15) := ".stock_manager/";
	conf_file_name			: string (1..18) := "stock_manager.conf";
	help_file_name_german	: string (1..15) := "help_german.txt";
	help_file_name_english	: string (1..16) := "help_english.txt";
	log_file_txt			: unbounded_string := to_unbounded_string("stock_log.txt");
	directory_of_backup		: unbounded_string;
	directory_of_log		: unbounded_string;

	type language_type is (german, english);
	language 	: language_type := english;

	type currency_type is (EUR, USD);
	currency	: currency_type := EUR;

	column_of_part_code_facility_in_eagle_bom	: natural;

	--manufacturer_part_code_given	: universal_string_type.bounded_string; -- ins v013
	--property_string_length	: natural := 300; -- ins v013
	--package property_string_type is new generic_bounded_length(property_string_length); use property_string_type; -- ins v013
	property_string_given			: universal_string_type.bounded_string; -- ins v013
---------------------------------------------

	-- ins v013 begin
	procedure print_error_on_too_many_characters is
	begin
		set_output(standard_output);
		new_line;
		put_line("ERROR : Given property string too long !");
		put_line("        Max. count of characters is" & natural'image(universal_string_length) & " !");
	end print_error_on_too_many_characters;
	-- ins v013 end

	-- ins v009 begin
	procedure print_error_on_insufficient_rights
		(file_name : string) is
		previous_output	: Ada.Text_IO.File_Type renames current_output;
	begin
		new_line;
		--put_line("ERROR : Insufficient rights to create or access file '" & file_name & "' !"); -- rm v013
		put_line("ERROR : Insufficient rights to create, write or access file :"); -- ins v013
		put_line("        '" & file_name & "' !"); -- ins v013
		put_line("        Make sure file exists and access rights are correct.");
 		put_line("        Contact system administrator !");
		set_output(previous_output);
	end print_error_on_insufficient_rights;
	-- ins v009 end

	procedure print_error_on_invalid_character
		(part_code_fac	: string;
		 character_pos	: natural) is
		 previous_output	: Ada.Text_IO.File_Type renames current_output; -- ins v003
	begin
		set_output(standard_output); -- ins v003
		new_line;
		--put_line("ERROR : Part code '" & part_code_fac & "' contains invalid character at position" & natural'image(character_pos) & " !");
		put_line("ERROR : Part code contains invalid character at position" & natural'image(character_pos) & " !");
		new_line;
		set_output(previous_output); -- ins 003
	end print_error_on_invalid_character;


	procedure print_error_on_invalid_part_code_prefix
		(part_code_fac	: string) is
	begin
		new_line;
		put_line("ERROR : Part code must start with a valid prefix ! Valid prefixes are:");
		new_line;
		for p in 0..part_prefix_count
		loop
			put(part_prefix_type'image(part_prefix_type'val(p)) & "_ ");
			if p < part_prefix_count then put(" , "); end if;
		end loop;
		new_line(2);
	end print_error_on_invalid_part_code_prefix;

	procedure print_error_on_missing_ifs_in_part_code_fac
		(position	: natural) is
	begin
		new_line;
		put_line("ERROR : Field separator '" & ifs_in_part_code_fac & "' expected in part code at position" & natural'image(position) & " !");
		new_line;
	end print_error_on_missing_ifs_in_part_code_fac;

	procedure print_error_on_missing_keyword_pac
		(position	: natural) is
	begin
		new_line;
		put_line("ERROR : Keyword '" & part_code_keyword_type'image(part_code_keyword_type'val(0)) & "_' or '" &
			part_code_keyword_type'image(part_code_keyword_type'val(1)) & "_' expected in part code at position" &
			natural'image(position) & " !");
		new_line;
	end print_error_on_missing_keyword_pac;
	

	function parse_part_code
		(part_code_fac	: string;
		parse_depth		: natural := 0	
		) 
		return boolean is -- returns false if part code invalid
		prefix_valid		: boolean := false;
		part_is_accessory	: boolean := false; -- ins v007
		char_pt				: natural;
	begin
		-- check for forbidden characters
		--prog_position := "PA000"; -- rm v003
		--put_line(standard_output,"facility part code : " & part_code_fac); 
		for c in 1..part_code_fac'last -- check every character if it is part of set "valid_character"
		loop
			-- prog_position := "PA003"; -- rm v003
			--put_line(standard_output,"character : "); put(standard_output, part_code_fac'first); new_line;
			if not ada.strings.maps.is_in(part_code_fac(c),valid_character) then
				-- prog_position := "PA004"; -- rm v003
				print_error_on_invalid_character(part_code_fac,c);
				return false;
			end if;
		end loop;

		prog_position := "PA100";
		if parse_depth = 1 then

			-- check prefix
			for p in 0..part_prefix_count
			loop
				prog_position := "PA200";
				-- check for allowed prefix at the start of the part_code
				if index(part_code_fac, part_prefix_type'image(part_prefix_type'val(p)) & '_') = 1 then -- if valid prefix found
					prefix_valid := true; -- once a valid prefix found, set flag prefix_valid

					-- ins v007 begin
					if p = part_prefix_type'pos(ACCESSORY) then
						--part_is_accessory := true; -- rm v011
						-- CS: do some useful checks
						exit; -- ins v011
					end if;
					-- ins v007 end

					-- ins v011 begin
					if p = part_prefix_type'pos(MODULE) then
						-- CS: do some useful checks
						exit;
					end if;

					if p = part_prefix_type'pos(CABLE) then
						-- CS: do some useful checks
						exit;
					end if;

					if p = part_prefix_type'pos(WIRE) then
						-- CS: do some useful checks
						exit;
					end if;

					if p = part_prefix_type'pos(PLUG) then
						-- CS: do some useful checks
						exit;
					end if;

					if p = part_prefix_type'pos(RECEPTACLE) then
						-- CS: do some useful checks
						exit;
					end if;

					if p = part_prefix_type'pos(TERMINALF) 
					or p = part_prefix_type'pos(TERMINALM) 
					then
						-- CS: do some useful checks
						exit;
					end if;
					-- ins v011 end

					-- save position of last character of prefix ( +1 for trailing '_' )
					char_pt	:= part_prefix_type'image(part_prefix_type'val(p))'last + 1;

					-- ins v011 begin
					-- check package keyword after prefix
					prog_position := "PA210";
					char_pt := char_pt + 1;
					--put_line("char_pt: " & natural'image(char_pt));
					if 		index(part_code_fac, part_code_keyword_type'image(PAC_S) & '_') = char_pt then
							-- save position of last character of keyword ( +1 for trailing '_' )
							char_pt	:= char_pt + part_code_keyword_type'image(PAC_S)'last + 1;
					elsif	index(part_code_fac, part_code_keyword_type'image(PAC_T) & '_') = char_pt then
							-- save position of last character of keyword ( +1 for trailing '_' )
							char_pt	:= char_pt + part_code_keyword_type'image(PAC_T)'last + 1;
					else 	
						print_error_on_missing_keyword_pac(char_pt);
						return false;
					end if;
					-- ins v011 end

					--put_line("char_pt: " & natural'image(char_pt));
					exit; -- no further search requird
				end if;
			end loop;

			-- if prefix not valid after search, print error message
			prog_position := "PA300";
			if not prefix_valid then 
				print_error_on_invalid_part_code_prefix(part_code_fac);
				return false;
			end if;

			-- check ifs_in_part_code_fac after prefix
-- 			prog_position := "PA400";
-- 			char_pt := char_pt + 1;
-- 			if part_code_fac(char_pt) /= ifs_in_part_code_fac then
-- 				print_error_on_missing_ifs_in_part_code_fac(char_pt);
-- 				return false;
-- 			end if;

			-- check package keyword after prefix if part is non-accessory
-- rm v011 begin
-- 			if not part_is_accessory then -- ins v007
-- 				prog_position := "PA500";
-- 				char_pt := char_pt + 1;
-- 				--put_line("char_pt: " & natural'image(char_pt));
-- 				if 		index(part_code_fac, part_code_keyword_type'image(part_code_keyword_type'val(0)) & '_') = char_pt then
-- 						-- save position of last character of keyword ( +1 for trailing '_' )
-- 						char_pt	:= char_pt + part_code_keyword_type'image(part_code_keyword_type'val(0))'last + 1;
-- 				elsif	index(part_code_fac, part_code_keyword_type'image(part_code_keyword_type'val(1)) & '_') = char_pt then
-- 						-- save position of last character of keyword ( +1 for trailing '_' )
-- 						char_pt	:= char_pt + part_code_keyword_type'image(part_code_keyword_type'val(1))'last + 1;
-- 				else 	
-- 					print_error_on_missing_keyword_pac(char_pt);
-- 					return false;
-- 				end if;
-- 				--put_line("char_pt: " & natural'image(char_pt));
-- 			end if; -- ins v007
-- rm v011 end
			-- cs: parse for valid accessories
			-- cs: check for more keywords ?

		end if; -- if parse depth = 1
		prog_position := "PA900";
		return true;

		exception
			when constraint_error => 
				new_line;
				if prog_position = "PA400" then 
					print_error_on_missing_ifs_in_part_code_fac(char_pt);
				end if;
				return false;
			when others => return false;
	end parse_part_code;



	procedure check_environment is
		previous_input	: Ada.Text_IO.File_Type renames current_input;
		scratch_natural : natural; -- ins v003
	begin
		-- get home variable
		prog_position := "ENV00";
		if not ada.environment_variables.exists("HOME") then
			raise constraint_error;
		else
			-- compose home directory name
			home_directory := to_bounded_string(ada.environment_variables.value("HOME") & "/"); -- this is the absolute path of the home directory
			--put_line(to_string(home_directory));
		end if;

		-- check if conf file exists	
		prog_position := "ENV10";
		if not exists ( to_string(home_directory) & conf_directory & conf_file_name ) then 
			raise constraint_error;
		else
			-- read configuration file
			Open(
				file => conf_file,
				Mode => in_file,
				Name => ( to_string(home_directory) & conf_directory & conf_file_name )
				);
			set_input(conf_file);
			while not end_of_file
			loop
				line := get_line;

				-- get language
				if sm_string_processing.get_field(line,1,' ') = "language" then 
					prog_position := "ENV20";
					language := language_type'value(sm_string_processing.get_field(line,2,' '));
					if debug_mode = 1 then 
						put_line("language        : " & language_type'image(language));
					end if;
				end if;

				-- get currency
				if sm_string_processing.get_field(line,1,' ') = "currency" then 
					prog_position := "ENV30";
					currency := currency_type'value(sm_string_processing.get_field(line,2,' '));
					if debug_mode = 1 then 
						put_line("currency        : " & currency_type'image(currency));
					end if;
				end if;

				-- get location of log file
				if sm_string_processing.get_field(line,1,' ') = "directory_of_log" then 
					prog_position := "ENV40";
					if sm_string_processing.get_field(line,2,' ')(1) /= '/' then -- if no leading "/", take this as relative to home directory
						directory_of_log := to_unbounded_string(to_string(home_directory)) & 
							to_unbounded_string(sm_string_processing.get_field(line,2,' ')) & "/";
						log_file_txt := to_unbounded_string(to_string(home_directory)) & 
							to_unbounded_string(sm_string_processing.get_field(line,2,' ')) & "/" &
						simple_name(to_string(log_file_txt));
					else -- otherwise it is an absolute path
						directory_of_log := to_unbounded_string(sm_string_processing.get_field(line,2,' ')) & "/";
						log_file_txt := directory_of_log & simple_name(to_string(log_file_txt));
					end if;
					if debug_mode = 1 then 
						put_line("log file        : " & log_file_txt);
					end if;
				end if;

				-- get location of stock db file
				if sm_string_processing.get_field(line,1,' ') = "directory_of_stock_data_base" then 
					prog_position := "ENV50";
					if sm_string_processing.get_field(line,2,' ')(1) /= '/' then -- if no heading /, take this as relative to home directory
						prog_position := "ENV51";
						stock_db_csv := to_unbounded_string(to_string(home_directory)) &
							to_unbounded_string(sm_string_processing.get_field(line,2,' ')) & "/" &
							simple_name(to_string(stock_db_csv));
					else -- otherwise it is an absolute path
						prog_position := "ENV52";
						stock_db_csv := to_unbounded_string(sm_string_processing.get_field(line,2,' ')) & "/" &
							simple_name(to_string(stock_db_csv));
					end if;
					prog_position := "ENV53";
					if debug_mode = 1 then 
						put_line("stock db file   : " & stock_db_csv);
					end if;
				end if;

				-- get location of backup files
				if sm_string_processing.get_field(line,1,' ') = "directory_of_backup" then 
					prog_position := "ENV60";
					if sm_string_processing.get_field(line,2,' ')(1) /= '/' then -- if no heading /, take this as relative to home directory
						directory_of_backup := to_unbounded_string(to_string(home_directory)) &
							to_unbounded_string(sm_string_processing.get_field(line,2,' ')); -- & "/" & simple_name(to_string(directory_of_backup));
					else -- otherwise it is an absolute path
						directory_of_backup := to_unbounded_string(sm_string_processing.get_field(line,2,' '));
					end if;
					prog_position := "ENV61";
					if debug_mode = 1 then 
						put_line("backup dir.     : " & directory_of_backup);
					end if;
					if not exists(to_string(directory_of_backup)) then -- create backup directory if not there
						create_directory(to_string(directory_of_backup));
						if debug_mode = 1 then 
							put_line("... not preset, so it has been created anew.");
						end if;
					end if;
				end if;

				-- get facility name
				if sm_string_processing.get_field(line,1,' ') = "facility_name" then 
					prog_position := "ENV70";
					scratch_natural := sm_string_processing.get_field_count(line); -- get number of facility names given in conf file
					for f in 2..scratch_natural
					loop
						facility_names(f-1) := to_upper(sm_string_processing.get_field(line,f));
						facility_count	:= scratch_natural-1;
						--facility_name := to_upper(sm_string_processing.get_field(line,2,' ')); -- rm v003
						if debug_mode = 1 then 
							put("facilities      : " & facility_names(f-1)); new_line;
						end if;

					end loop;
				end if;

				-- ins v003 begin
				-- get customer specific item prefixes which may exist in the eagle bom file
				if sm_string_processing.get_field(line,1,' ') = "customer_prefixes" then 
					prog_position := "ENV80";
					scratch_natural := sm_string_processing.get_field_count(line); -- get number of prefixes given in conf file
					for f in 2..scratch_natural -- loop through prefixes 
					loop						-- and save them in customized_prefixes (1 based counting)
						customized_prefixes(f-1) := to_bounded_string(to_upper(sm_string_processing.get_field(line,f)));
						customized_prefixes_count := scratch_natural-1;
						if debug_mode = 1 then 
							put("customer_prefix : "); put(to_string(customized_prefixes(f-1))); new_line;
						end if;
					end loop;
					
				end if;
				-- ins v003 end


			end loop;
			close(conf_file);
		end if;

		-- check if help file exists	
		prog_position := "ENV90";
		case language is
			when german => 
				if not exists ( to_string(home_directory) & conf_directory & help_file_name_german ) then 
					put_line("ERROR : German help file missing !");
				end if;
			when english =>
				if not exists ( to_string(home_directory) & conf_directory & help_file_name_english ) then 
					put_line("ERROR : English help file missing !");
				end if;
			when others =>
				put_line("ERROR : Help file missing !");
		end case;

		if debug_mode = 1 then
			put_line(row_separator);
		end if;
		set_input(previous_input);
	end check_environment;


	procedure update_log is
		previous_output	: ada.text_io.file_type renames current_output;
	begin
		prog_position := "UL000";
		if exists (to_string(log_file_txt)) then -- if log file already there
			Open( -- just open it and append to it
				File => log_file,
				Mode => append_file,
				Name => to_string(log_file_txt)
				);
			set_output(log_file);
		else -- otherwise create new log file
			prog_position := "UL200";
			create( log_file, Name => to_string(log_file_txt)); Close(log_file);
			Open( -- and write into it
				File => log_file,
				Mode => out_file,
				Name => to_string(log_file_txt)
				);
			set_output(log_file);
			put_line("stock manager activities log");
			put_line(row_separator);
			put_line("DATE");		
			put_line("YYYY:MM:DD HH:MM:SS    | action");
			put_line(row_separator);
		end if;
		
		--put_line("arg. ct: " & natural'image(arg_ct));
		--put(image(clock, time_zone => UTC_Time_Offset(clock)) & "    | "); -- rm v002
		put(image(now, time_zone => UTC_Time_Offset(now)) & "    | "); -- ins v002
		--put_line(stock_operation_type'image(stock_operation));
		for a in 1..arg_ct
		loop
			put(argument(a) & "  ");
		end loop;
		new_line;
		--put_line(row_separator);
		prog_position := "UL100";
		close(log_file);
		--set_output(standard_output);
		prog_position := "UL101"; -- ins v009
		set_output(previous_output);
		prog_position := "UL102"; -- ins v009
	end update_log;


	function get_latest_backup_date
		return string is
		previous_input	: ada.text_io.file_type renames current_input;
		line			: unbounded_string;
		date   			: universal_string_type.bounded_string;
		time			: universal_string_type.bounded_string;
	begin
		open(
			file => log_file,
			mode => in_file,
			name => to_string(log_file_txt)
			);
		set_input(log_file);
		while not end_of_file
		loop
			line := get_line;
		end loop;
		-- we are interested in the last line. once the loop has finished line holds the last line of the file read
		-- the line holds for example: "2015-03-11 09:46:15    | edit  7  distributor_6_price_net  6"
		date := to_bounded_string(sm_csv.get_field(line,1,' ')); -- get date
		time := to_bounded_string(sm_csv.get_field(line,2,' ')); -- get time
		replace_element(time,3,'-'); replace_element(time,6,'-'); -- replace : by - to get 09-46-15 (HH-MM-SS)
		--put_line(to_string("BAK_" & date & "_" & time & "__" & simple_name(to_string(stock_db_csv)))); -- compose full file name
		set_input(previous_input);
		close(log_file);
		return(to_string("BAK_" & date & "_" & time & "__" & simple_name(to_string(stock_db_csv)))); -- compose and return full name
	end;

	procedure print_help_general is
		result	: natural;
	begin
		new_line;
--		put_line("STOCK MANAGER Version "& version);

		prog_position := "HLP00";
		case language is
			when german =>
				Spawn 
					(  
					Program_Name           => "/bin/cat",
					Args                   => 	(
												1=> new String'(to_string(home_directory) & conf_directory & help_file_name_german)
												),
					Output_File_Descriptor => Standout,
					Return_Code            => Result
					);

			when others =>
				Spawn 
					(  
					Program_Name           => "/bin/cat",
					Args                   => 	(
												1=> new String'(to_string(home_directory) & conf_directory & help_file_name_english)
												),
					Output_File_Descriptor => Standout,
					Return_Code            => Result
					);
		end case;
 		if 
 			Result /= 0 then raise constraint_error;
 		end if;
	end print_help_general;


	procedure print_log is
		result	: natural;
	begin
		new_line;
		prog_position := "PL000";
 		Spawn 
 			(  
 			Program_Name           => "/bin/cat",
 			Args                   => 	(
 										1=> new String'(to_string(log_file_txt))
 										),
 			Output_File_Descriptor => Standout,
 			Return_Code            => Result
 			);
 		if 
 			Result /= 0 then raise constraint_error;
 		end if;
	end print_log;




	procedure line_to_record(
		l	: string)	-- l is the line passed to 
		is
		char_pt			: natural := 1;				-- charcter pointer (points to character being processed inside the given line)
		char_current	: character;				-- holds current character being processed
		field_ct		: natural := 0;				-- holds number of fields found in given line
		field_content	: unbounded_string;
		line_length		: natural := l'last;
		ifs				: character := ';';
		ifs_pt_prev		: natural := l'first;

		procedure assign_field (text: string; f: natural) 
		is
		begin
--			put_line(text);
--			put_line(natural'image(f));
			prog_position := "LR100";
			case f is
				when 1 => line_of_stock_db_file.part_id := natural'value(text);
				when 2 => line_of_stock_db_file.part_code_fac := to_bounded_string(text);
				when 3 => line_of_stock_db_file.date_edited := text;
				when 4 => line_of_stock_db_file.qty_on_stock := natural'value(text);
				when 5 => line_of_stock_db_file.qty_reserved := natural'value(text);
				when 6 => line_of_stock_db_file.qty_available := natural'value(text);

				when 7 => line_of_stock_db_file.manufacturers(1).name := to_bounded_string(text);
				when 8 => line_of_stock_db_file.manufacturers(1).part_code := to_bounded_string(text);
				when 9 => line_of_stock_db_file.manufacturers(1).date_edited := text;
				when 10 => line_of_stock_db_file.manufacturers(1).status_production := status_production_type'value(text);
				when 11 => line_of_stock_db_file.manufacturers(1).url_datasheet_1 := to_unbounded_string(text);
				when 12 => line_of_stock_db_file.manufacturers(1).url_datasheet_2 := to_unbounded_string(text);

				when 13 => line_of_stock_db_file.manufacturers(2).name := to_bounded_string(text);
				when 14 => line_of_stock_db_file.manufacturers(2).part_code := to_bounded_string(text);
				when 15 => line_of_stock_db_file.manufacturers(2).date_edited := text;
				when 16 => line_of_stock_db_file.manufacturers(2).status_production := status_production_type'value(text);
				when 17 => line_of_stock_db_file.manufacturers(2).url_datasheet_1 := to_unbounded_string(text);
				when 18 => line_of_stock_db_file.manufacturers(2).url_datasheet_2 := to_unbounded_string(text);

				when 19 => line_of_stock_db_file.manufacturers(3).name := to_bounded_string(text);
				when 20 => line_of_stock_db_file.manufacturers(3).part_code := to_bounded_string(text);
				when 21 => line_of_stock_db_file.manufacturers(3).date_edited := text;
				when 22 => line_of_stock_db_file.manufacturers(3).status_production := status_production_type'value(text);
				when 23 => line_of_stock_db_file.manufacturers(3).url_datasheet_1 := to_unbounded_string(text);
				when 24 => line_of_stock_db_file.manufacturers(3).url_datasheet_2 := to_unbounded_string(text);

				when 25 => line_of_stock_db_file.distributors(1).name := to_bounded_string(text);
				when 26 => line_of_stock_db_file.distributors(1).order_code := to_bounded_string(text);
				when 27 => line_of_stock_db_file.distributors(1).date_edited := text;
				when 28 => line_of_stock_db_file.distributors(1).qty_min := natural'value(text);
				when 29 => line_of_stock_db_file.distributors(1).price_net := money_positive'value(text);

				when 30 => line_of_stock_db_file.distributors(2).name := to_bounded_string(text);
				when 31 => line_of_stock_db_file.distributors(2).order_code := to_bounded_string(text);
				when 32 => line_of_stock_db_file.distributors(2).date_edited := text;
				when 33 => line_of_stock_db_file.distributors(2).qty_min := natural'value(text);
				when 34 => line_of_stock_db_file.distributors(2).price_net := money_positive'value(text);

				when 35 => line_of_stock_db_file.distributors(3).name := to_bounded_string(text);
				when 36 => line_of_stock_db_file.distributors(3).order_code := to_bounded_string(text);
				when 37 => line_of_stock_db_file.distributors(3).date_edited := text;
				when 38 => line_of_stock_db_file.distributors(3).qty_min := natural'value(text);
				when 39 => line_of_stock_db_file.distributors(3).price_net := money_positive'value(text);

				when 40 => line_of_stock_db_file.distributors(4).name := to_bounded_string(text);
				when 41 => line_of_stock_db_file.distributors(4).order_code := to_bounded_string(text);
				when 42 => line_of_stock_db_file.distributors(4).date_edited := text;
				when 43 => line_of_stock_db_file.distributors(4).qty_min := natural'value(text);
				when 44 => line_of_stock_db_file.distributors(4).price_net := money_positive'value(text);

				when 45 => line_of_stock_db_file.distributors(5).name := to_bounded_string(text);
				when 46 => line_of_stock_db_file.distributors(5).order_code := to_bounded_string(text);
				when 47 => line_of_stock_db_file.distributors(5).date_edited := text;
				when 48 => line_of_stock_db_file.distributors(5).qty_min := natural'value(text);
				when 49 => line_of_stock_db_file.distributors(5).price_net := money_positive'value(text);

				when 50 => line_of_stock_db_file.distributors(6).name := to_bounded_string(text);
				when 51 => line_of_stock_db_file.distributors(6).order_code := to_bounded_string(text);
				when 52 => line_of_stock_db_file.distributors(6).date_edited := text;
				when 53 => line_of_stock_db_file.distributors(6).qty_min := natural'value(text);
				when 54 => line_of_stock_db_file.distributors(6).price_net := money_positive'value(text);

				when 55 => line_of_stock_db_file.storage_place := to_bounded_string(text);
				when 56 => line_of_stock_db_file.remarks := to_bounded_string(text);
				when 57 => line_of_stock_db_file.project := to_bounded_string(text);

				when others => null;
			end case;
		end assign_field;

	begin
		prog_position := "LR001";
		--put_line(l);
		while char_pt <= line_length -- process line until last character
		loop
			prog_position := "LR002";
			--put(l(char_pt));
			char_current:=l(char_pt); -- get character where char_pt points to
			if char_current /= ifs then -- if non-ifs found
				null;
			else -- if ifs found -> end of current field
				field_ct := field_ct + 1; -- count fields, field_ct points into current field
				prog_position := "LR004";
				assign_field(strip_text_delimiters(l(ifs_pt_prev+1..char_pt-1)),field_ct);
				prog_position := "LR005";
				ifs_pt_prev := char_pt; -- save position of current ifs in ifs_pt_prev (for next pass)
			end if;

--		relevant if line has no trailing ifs:
-- 			if char_pt = l'last then -- end of last field found
-- 				null;
-- 				put_line(l(ifs_pt_prev..char_pt));
-- 			end if;

		char_pt:=char_pt+1; -- advance character pointer for next pass
		end loop; --process line until last character
	end line_to_record;



	procedure write_stock_db_header is
	begin
		prog_position := "WH001";
		put_field(text => "MATERIAL ON STOCK"); put_lf;
 		put_field(text => "------------------------------"); put_lf;
 		put_field(text => "created by STOCK MANAGER"); put_field(text => "V" & version); put_lf;
 		--put_field(text => "date:"); put_field(text => image(clock, time_zone => UTC_Time_Offset(clock)) );
		put_field(text => "date:"); put_field(text => date_now); --image(clock, time_zone => UTC_Time_Offset(clock)) );
 		put_field(text => "(YYYY-MM-DD HH:MM:SS)"); put_lf(count => 2);

		put_field(text => "PART_ID");
		put_field(text => "PART_CODE_BEL");
		put_field(text => "DATE_EDITED");
		put_field(text => "QTY_ON_STOCK");
		put_field(text => "QTY_RESERVED");
		put_field(text => "QTY_AVAILABLE");

		for m in 1..manufacturer_count_max
		loop
			--put_field(text => "MANUFACTURER_NAME_" & trim(natural'image(m),left));
			put_field(text => "MANUFACTURER_" & trim(natural'image(m),left) & "_NAME");
			put_field(text => "PART_CODE");
			put_field(text => "DATE_EDITED");
			put_field(text => "STATUS_PRODUCTION");
			put_field(text => "URL_DATASHEET_1");
			put_field(text => "URL_DATASHEET_2");
		end loop;

		for d in 1..distributor_count_max
		loop
			--put_field(text => "DISTRIBUTOR_NAME_" & trim(natural'image(d),left));
			put_field(text => "DISTRIBUTOR_" & trim(natural'image(d),left) & "_NAME");
			put_field(text => "ORDER_CODE");
			put_field(text => "DATE_EDITED");
			put_field(text => "QTY_MIN");
			put_field(text => "PRICE_NET");
		end loop;

		put_field(text => "STORAGE_PLACE");
		put_field(text => "REMARKS");
		put_field(text => "PROJECT");
		put_lf;
	end write_stock_db_header;


	procedure print_error_on_unknown_part is
	begin
		set_output(standard_output);
		new_line;
		put_line("ERROR : Part not found on stock !");
		raise constraint_error;
	end print_error_on_unknown_part;

	procedure print_error_on_unknown_id is
	begin
		set_output(standard_output);
		new_line;
		--put_line("ERROR : Part ID outside range '1.." & trim(natural'image(part_count_max2),left) & "' !"); -- rm v013
		put_line("ERROR : Part ID not in use or outside range '1.." & trim(natural'image(part_count_max2),left) & "' !"); -- ins v013
		new_line;
		raise constraint_error;
	end print_error_on_unknown_id;

	procedure print_warning_on_multiple_occurences(ct:natural) is
	begin
		set_output(standard_output);
		new_line;
		put_line("WARNING :" & natural'image(ct) & " occurences found of given part_code or part_id !");
	end print_warning_on_multiple_occurences;

	procedure print_number_of_occurences(ct:natural) is
	begin
		set_output(standard_output);
		new_line;
		put_line("Number of parts found : " & natural'image(ct));
	end print_number_of_occurences;

	procedure print_supported_actions is
		ct	: natural := stock_operation_type'pos((stock_operation_type'last)); -- get pos of last supported action
	begin
		put_line("ERROR : Action not specified or unknown !");
		put_line("        Type action to do. For instance 'stock_manager " & to_lower(stock_operation_type'image(stock_operation_type'val(1)) & "'"));
		put_line("        Supported actions are: ");
		new_line;
		for s in 1..ct -- skip 'default' stock_operation
		loop
			put_line("- " & to_lower(stock_operation_type'image(stock_operation_type'val(s))));
		end loop;
	end print_supported_actions;

	procedure print_supported_properties is
		ct	: natural := part_property_editable_type'pos((part_property_editable_type'last)); -- get pos of last supported property
	begin
		put_line("ERROR : Property not specified or unknown !");
		put_line("        Type property to edit right after part id.");
		put_line("        For instance 'stock_manager edit 74 " & to_lower(part_property_editable_type'image(part_property_editable_type'val(1)) & "'"));
		put_line("        Supported properties are: ");
		new_line;
		for p in 0	..ct -- skip part_id. this property can not be edited
		loop
			put_line("- " & to_lower(part_property_editable_type'image(part_property_editable_type'val(p))));
		end loop;
	end print_supported_properties;

	procedure print_error_on_invalid_id is
	begin
		put_line("ERROR : Part ID not specified, invalid or outside range '1.." & trim(natural'image(part_count_max2),left) & "' !");
		put_line("        Specify part ID right after action.");
		put_line("        For instance 'stock_manager " & 
			to_lower(stock_operation_type'image(stock_operation)) &
			" 74' !");
	end;


	procedure print_error_on_missing_part_code_fac is
	begin
		put_line("ERROR : Part code not specified or invalid !");
		put_line("        Specify part code right after action.");
		put_line("        For instance 'stock_manager " & 
			to_lower(stock_operation_type'image(stock_operation)) & " 170 part_code_fac" &
			" IC_PAC_S_SO8_VAL_TL081'");
	end print_error_on_missing_part_code_fac;


	procedure print_error_on_invalid_part_code is
	begin
		put_line("ERROR : Part code not specified or invalid !");
		put_line("        Part code must not have special characters !");
		put_line("        Part code must not have more than" & natural'image(universal_string_length) & " characters !");
		put_line("        Specify part code right after action.");
		put_line("        For instance 'stock_manager " & 
			--to_lower(stock_operation_type'image(stock_operation_type'val(2))) & " IC_PAC_S_SO8_VAL_TL081' !");
			to_lower(stock_operation_type'image(stock_operation)) &
			" IC_PAC_S_SO8_VAL_TL081'");
	end print_error_on_invalid_part_code;

	procedure print_error_on_missing_eagle_bom_file is
	begin
		put_line("ERROR : Please specify BOM file output by EAGLE bom.ulp !");
		put_line("        For instance 'stock_manager " & 
			to_lower(stock_operation_type'image(stock_operation)) &
			" eagle_project_x.csv'");
	end print_error_on_missing_eagle_bom_file;

	procedure print_error_on_missing_order_list_file is
	begin
		put_line("ERROR : Please specify order list file !");
		put_line("        For instance 'stock_manager " & 
			to_lower(stock_operation_type'image(stock_operation)) &
			" project_x_bom.csv my_items_to_order.csv'");
	end print_error_on_missing_order_list_file;

	procedure print_error_on_missing_facility_bom_file is
	begin
		put_line("ERROR : Please specify BOM file to be generated for your facility !");
		put_line("        For instance 'stock_manager " & 
			to_lower(stock_operation_type'image(stock_operation)) &
			" eagle_project_x.csv project_x_bom.csv'");
	end print_error_on_missing_facility_bom_file;

	procedure print_error_on_missing_facility_bom_file_to_checkout is
	begin
		put_line("ERROR : Please specify facility BOM file to check out items from stock !");
		put_line("        For instance 'stock_manager " & 
			to_lower(stock_operation_type'image(stock_operation)) &
			" project_x_bom.csv'");
	end print_error_on_missing_facility_bom_file_to_checkout;

	procedure print_error_on_missing_facility_bom_unit_quantity_to_checkout is
	begin
		put_line("ERROR : Please specify quantity of units to check out items for !");
		put_line("        For instance 'stock_manager " & 
			to_lower(stock_operation_type'image(stock_operation)) &
			" project_x_bom.csv 100'");
	end print_error_on_missing_facility_bom_unit_quantity_to_checkout;

	procedure print_error_on_missing_project is
	begin
		put_line("ERROR : Project name expected !");
		put_line("        Type project name as shown below.");
		put_line("        For instance 'stock_manager edit 74 project X-15_avionic'");
	end;

	procedure print_error_on_missing_qty_delta is
	begin
		put_line("ERROR : Quantity added or taken from stock expected !");
		put_line("        Type quantity as shown below.");
		put_line("        Adding  to  stock : 'stock_manager edit 74 qty_delta 4'");
		put_line("        Taking from stock : 'stock_manager edit 74 qty_delta -4'");
	end;

	procedure print_error_on_invalid_qty_delta is
	begin
		put_line("ERROR : Quantity to be withdrawn from from stock exceedes number of avialable parts !");
		put_line("        You can't take more parts than those available !");
	end;

	procedure print_error_on_invalid_qty_reserved is
	begin
		put_line("ERROR : Quantity to be reserved exceedes number of parts on stock !");
		put_line("        You can't reserve more parts than those on stock !");
	end;

	procedure print_error_on_still_reserved_parts is
	begin
		put_line("ERROR : Quantity to be taken from stock exceedes number of reserved parts !");
		put_line("        You can't take parts from stock that are reserved !");
	end;

	procedure print_error_on_missing_manufacturer_name is
	begin
		put_line("ERROR : Manufacturer name missing ! Type the manufacturer name as shown below.");
		put_line("        For instance 'stock_manager edit 74 manufacturer_1_name TEXAS-INSTRUMENTS'");
	end;

	procedure print_error_on_missing_manufacturer_part_code is
	begin
		put_line("ERROR : Part code missing ! Type the part code as shown below.");
		put_line("        For instance 'stock_manager edit 74 manufacturer_1_part_code TL084D'");
	end;

	procedure print_error_on_missing_manufacturer_status_production is
		ct	: natural := status_production_type'pos((status_production_type'last)); -- get pos of last production status
	begin
		put_line("ERROR : Production status missing or invalid ! Type the production status as shown below.");
		put_line("        For instance 'stock_manager edit 74 manufacturer_1_status_production active'");
		new_line;
		put_line("        The status of production can be:");
		new_line;
		for s in 0..ct -- list available kinds of production status
		loop
			put_line("- " & to_lower(status_production_type'image(status_production_type'val(s))));
		end loop;
	end;

	procedure print_error_on_missing_manufacturer_datasheet is
	begin
		put_line("ERROR : URL to datasheet missing ! Type the datasheet URL as shown below.");
		put_line("        For instance 'stock_manager edit 74 manufacturer_1_datasheet_1 http://www.ti.com'");
	end;

	procedure print_error_on_missing_distributor_name is
	begin
		put_line("ERROR : Distributor name missing ! Type the name as shown below.");
		put_line("        For instance 'stock_manager edit 74 distributor_1_name AX'");
	end;

	procedure print_error_on_missing_distributor_order_code is
	begin
		put_line("ERROR : Distributor order code missing ! Type the code as shown below.");
		put_line("        For instance 'stock_manager edit 74 distributor_1_order_code 45338'");
	end;

	procedure print_error_on_missing_distributor_qty_min is
	begin
		put_line("ERROR : Minimal order quantity missing ! Type the quantity as shown below.");
		put_line("        For instance 'stock_manager edit 74 distributor_1_qty_min 10'");
	end;

	procedure print_error_on_missing_distributor_price_net is
	begin
		put_line("ERROR : Net price missing or invalid ! Type the price as shown below.");
		put_line("        For instance 'stock_manager edit 74 distributor_1_price_net 1.44'");
	end;

	procedure print_error_on_missing_storage_place is
	begin
		put_line("ERROR : Storage place missing ! Type the place as shown below.");
		put_line("        For instance 'stock_manager edit 74 storage_place shelf_7_box'");
	end;

	procedure print_error_on_missing_remarks is
	begin
		put_line("ERROR : Remarks missing ! Type remarks as shown below.");
		put_line("        For instance 'stock_manager edit 74 remarks ask_Mario_on_accessories'");
	end;

	procedure print_error_on_non_defined_manufacturer
		( m	: natural) is
	begin
		new_line;
		put_line("ERROR : Manufacturer number" & natural'image(m) & " not activated yet. Assign a name as shown below.");
		put_line("        For instance 'stock_manager edit 74 manufacturer_" & trim(natural'image(m),left) & "_name MOTOROLA'");
		new_line;
	end;

	procedure print_error_on_non_defined_distributor
		( d	: natural) is
	begin
		new_line;
		put_line("ERROR : Distributor number" & natural'image(d) & " not activated yet. Assign a name as shown below.");
		put_line("        For instance 'stock_manager edit 74 distributor_" & trim(natural'image(d),left) & "_name FARNELL'");
		new_line;
	end;

	procedure print_error_on_invalid_backup_file
		is
	begin
		new_line;
		put_line("ERROR : Backup file to restore from, does not exist !");
		put_line("        Rollback not possible !");
		new_line;
	end;

	procedure print_warning_on_zero_price 
		( part_id :	natural;
		  distributor : natural) is
		previous_output	: ada.text_io.file_type renames current_output;
	begin
		set_output(standard_output);
		put_line("WARNING : ZERO price found for part with ID" & natural'image(part_id) & " of distributor" & natural'image(distributor) & " !");
		set_output(previous_output);
	end;

	procedure print_warning_on_no_part_source
		( 	part_id : natural;
			part_code : string) is
		previous_output	: ada.text_io.file_type renames current_output;
	begin
		set_output(standard_output);
		new_line;
		put_line("WARNING : No valid order information available for part with ID" & natural'image(part_id));
		put_line("          and part code " & part_code & " !"); 
		new_line;
		put_line("          Make sure distributor name and order code are assigned !");
		put_line("          Type command 'stock_manager full show_by_id" & natural'image(part_id) & "' !");
		set_output(previous_output);
	end print_warning_on_no_part_source;


	procedure print_error_on_missing_facility_bom_file_to_query is
	begin
		put_line("ERROR : Please specify facility BOM file to query for !");
		put_line("        For instance 'stock_manager " & 
			to_lower(stock_operation_type'image(stock_operation)) &
			" project_x_bom.csv'");
	end print_error_on_missing_facility_bom_file_to_query;

	procedure print_error_on_missing_facility_bom_unit_quantity_to_query is
	begin
		put_line("ERROR : Please specify quantity of units to query for !");
		put_line("        For instance 'stock_manager " & 
			to_lower(stock_operation_type'image(stock_operation)) &
			" project_x_bom.csv 100'");
	end print_error_on_missing_facility_bom_unit_quantity_to_query;

	procedure print_error_on_missing_withdrawal_list is
	begin
		put_line("ERROR : Please specify withdrawal list file !");
		put_line("        For instance 'stock_manager " & 
			to_lower(stock_operation_type'image(stock_operation)) &
			" project_x_bom.csv items_to_order.csv items_to_take_from_stock'");
	end print_error_on_missing_withdrawal_list;

	function convert_data_base
		return boolean is
		part_section_entered 	: boolean := false;
		part_pointer 			: natural := 0;
		subtype part_stock_array_sized is part_stock_array_type (1..part_count_pdb); -- dyn
		part_stock_array 		: part_stock_array_sized;
		previous_input			: Ada.Text_IO.File_Type renames current_input;
		previous_output			: Ada.Text_IO.File_Type renames current_output;

	begin
		prog_position := "RD001";
		set_input(parts_db_file);
		-- this loop read the parts_db file in an array part_stock_array
		while not end_of_file
			loop
				line:=get_line;
					if sm_csv.get_field_count(line) > 0 then -- line must not be empty
						if part_section_entered then -- if part section entered
							--put_line(standard_output,line);
							part_pointer := part_pointer + 1;
							prog_position := "RD002";
							part_stock_array(part_pointer).part_id := part_pointer;
							part_stock_array(part_pointer).part_code_fac := to_bounded_string(sm_csv.get_field(line,1));

							if sm_csv.get_field(line,2) /= "" then
								part_stock_array(part_pointer).distributors(1).name := to_bounded_string("REICHELT");
								part_stock_array(part_pointer).distributors(1).order_code := to_bounded_string(sm_csv.get_field(line,2));
							end if;
							if sm_csv.get_field(line,3) /= "" then
								part_stock_array(part_pointer).distributors(2).name := to_bounded_string("AX");
								part_stock_array(part_pointer).distributors(2).order_code := to_bounded_string(sm_csv.get_field(line,3));
							end if;
							if sm_csv.get_field(line,4) /= "" then
								part_stock_array(part_pointer).distributors(3).name := to_bounded_string("FARNELL");
								part_stock_array(part_pointer).distributors(3).order_code := to_bounded_string(sm_csv.get_field(line,4));
							end if;
							if sm_csv.get_field(line,5) /= "" then
								part_stock_array(part_pointer).distributors(4).name := to_bounded_string("DIGI-KEY");
								part_stock_array(part_pointer).distributors(4).order_code := to_bounded_string(sm_csv.get_field(line,5));
							end if;
							if sm_csv.get_field(line,6) /= "" then
								part_stock_array(part_pointer).distributors(5).name := to_bounded_string("MOUSER");
								part_stock_array(part_pointer).distributors(5).order_code := to_bounded_string(sm_csv.get_field(line,6));
							end if;
							if sm_csv.get_field(line,8) /= "" then
								part_stock_array(part_pointer).distributors(6).name := to_bounded_string("RS");
								part_stock_array(part_pointer).distributors(6).order_code := to_bounded_string(sm_csv.get_field(line,8));
							end if;

							if sm_csv.get_field(line,9) /= "" then
								part_stock_array(part_pointer).manufacturers(1).part_code := to_bounded_string(sm_csv.get_field(line,9));
							end if;

 							if sm_csv.get_field(line,10) /= "" then
 								part_stock_array(part_pointer).manufacturers(1).url_datasheet_1 := to_unbounded_string(sm_csv.get_field(line,10));
 							end if;

						end if; -- if part section entered

						if sm_csv.get_field(line,1) = "PART_CODE_BEL" then	-- set part_section_entered flag upon passing the part_code_facility line
							part_section_entered := true;
						end if;

					end if;  -- line must not be empty
			end loop;
		prog_position := "RD100";
		set_input(previous_input);

		prog_position := "RD200";
		set_output(stock_db_file);
		prog_position := "RD300";
		write_stock_db_header;


		prog_position := "WR002";
		-- this loop writes the part_stock_array in the stock_db file
		for p in 1..part_pointer
		loop
			put_field(text => natural'image(part_stock_array(p).part_id));
			put_field(text => to_string(part_stock_array(p).part_code_fac));
			put_field(text => image(clock, time_zone => UTC_Time_Offset(clock)));
			for f in 1..3 loop put_field(text => "0"); end loop; -- fill qty fields with default 0

			-- data of first manuf. is known from the old parts_db file
			-- write data in stock_db
			put_field(text => "unknown"); -- fill manufacturer name 1
			put_field(text => to_string(part_stock_array(p).manufacturers(1).part_code));
			put_field(text => image(clock, time_zone => UTC_Time_Offset(clock)));
			put_field(text => "unknown"); -- fill status production
			put_field(text => to_string(part_stock_array(p).manufacturers(1).url_datasheet_1));
			put_field(text => not_assigned_mark); -- datasheet 2
			--for f in 1..13 loop put_field(text => not_assigned_mark); end loop; -- fill gap fields

			-- data of 2nd and 3rd manufacturer is to be filled with default data
			for m in 2..manufacturer_count_max
			loop
				put_field(text => not_assigned_mark); -- name
				put_field(text => not_assigned_mark); -- part code manuf
				put_field(text => default_date); -- date edited
				put_field(text => "unknown"); -- fill status production
				put_field(text => not_assigned_mark); -- datasheet 1
				put_field(text => not_assigned_mark); -- datasheet 2
			end loop;

			-- data of distributors
			for d in 1..distributor_count_max
			loop
				put_field(text => to_string(part_stock_array(p).distributors(d).name));
				put_field(text => to_string(part_stock_array(p).distributors(d).order_code));
				put_field(text => part_stock_array(p).distributors(d).date_edited);
				put_field(text => natural'image(part_stock_array(p).distributors(d).qty_min));
				put_field(text => money_positive'image(part_stock_array(p).distributors(d).price_net));
			end loop;

			-- storage place, remarks, project
			for f in 1..3 loop put_field(text => not_assigned_mark); end loop;
			put_lf;
		end loop;
		put_field(text => "END OF STOCK");
		set_output(previous_output);
		return true;
	end convert_data_base;


	-- here the actural stock operations like adding, deleting, showing parts take place
	function manage_stock (
			stock_operation 	: stock_operation_type;
			part_code_fac_given	: string;
			part_id_given		: natural
			)
		return boolean is
		part_on_stock 				: boolean := false;

		-- the part_stock_array is sized according to the pre-read action read_stock_data_base_part_count
		subtype part_stock_array_sized is part_stock_array_type (1..part_count_max2); -- dyn
		part_stock_array 			: part_stock_array_sized;

		-- the facility_bom_part_array is as many members large as the part_stock_array
		subtype parts_of_facility_bom_sized is parts_of_facility_bom (1..part_count_max2);
		facility_bom_part_array			: parts_of_facility_bom_sized;

		part_occured_on_stock		: natural := 0;
		--qty_on_stock_delta_given	: integer := 0; -- rm v013
		qty_on_stock_delta_scratch	: integer := 0; -- ins v013
		qty_on_stock_tmp			: natural := 0;
		--qty_reserved_given		: integer := 0; -- rm v013
		qty_reserved_scratch		: integer := 0; -- ins v013
		qty_reserved_tmp 			: natural := 0;
		qty_scratch					: natural := 0;
--		part_code_fac_given2					: universal_string_type.bounded_string; -- rm v013
		--manufacturer_name_given					: universal_string_type.bounded_string; -- rm v013
		--manufacturer_part_code_given			: universal_string_type.bounded_string; -- rm v013
		manufacturer_status_production_scratch 	: status_production_type;
--		manufacturer_datasheet_scratch			: g_type.bounded_string; -- ins v013
--		distributor_name_given					: universal_string_type.bounded_string; -- rm v013
--		distributor_order_code_given			: universal_string_type.bounded_string;
--		distributor_qty_min_given				: natural; -- rm v013
		distributor_qty_min_scratch				: natural; -- ins v013
--		distributor_price_net_given				: money_positive; -- rm v013
		distributor_price_net_scratch			: money_positive; -- ins v013
--		storage_place_given						: universal_string_type.bounded_string; -- rm v013
--		remarks_given							: universal_string_type.bounded_string;
--		project_given							: universal_string_type.bounded_string;
		part_ct_proj							: natural := 0;
		part_ct_proj_smd						: natural := 0;
		part_ct_proj_tht						: natural := 0;
		part_ct_proj_virtual					: natural := 0;
		parts_of_same_value						: unbounded_string;
		position								: natural := 0;
		part_occured_in_sdb						: boolean := false;
		id_master								: natural := 0;
		order_position 							: natural := 0;
		qty_required							: natural := 0;

		type part_of_eagle_bom is -- fields separated by ";" , text delimited by ",  as defined in bom.ulp
			record
				processed		: boolean := false;
				part			: universal_string_type.bounded_string;
				value			: universal_string_type.bounded_string;
				device			: universal_string_type.bounded_string;
				packge			: universal_string_type.bounded_string;
				description		: universal_string_type.bounded_string;
				bom				: boolean;
				commissioned	: simple_date_type.bounded_string;
				funct			: universal_string_type.bounded_string;
				part_code_fac	: universal_string_type.bounded_string;
				updated			: simple_date_type.bounded_string;
			end record;
		type parts_of_eagle_bom is array (natural range <>) of part_of_eagle_bom;



		function count_parts_in_eagle_bom
			return natural is
			line					: unbounded_string;
			part_section_entered 	: boolean := false;
		begin
			prog_position := "CP000";
			while not End_Of_File
				loop
					line:=get_line;
					if part_section_entered then
						if sm_csv.get_field(line,6) = "YES" then -- part must be registerd -- insist on capital letters
							--if sm_csv.get_field(line,9) /= "" then -- if field is empty, no part code present, abort -- rm v003

							-- now, for checking the facility part code, the column detetected earlier is to be used
							if sm_csv.get_field(line,column_of_part_code_facility_in_eagle_bom) /= "" then -- if field is empty, no part code present, abort -- ins v003
								part_ct_proj := part_ct_proj + 1; -- otherwise count parts

								-- count SMD and THT parts
								-- cut off characters starting at pos. 4 -- CS: unclear why the first character of this string is at pos. 2
								-- so that the prefix S_ or T_ remains from the package field.
								-- then test for S_ or T_ and count occurences
								if delete(sm_csv.get_field(line,4),4,sm_csv.get_field(line,4)'last) = "S_" then 
									part_ct_proj_smd := part_ct_proj_smd + 1;
								end if;

								if delete(sm_csv.get_field(line,4),4,sm_csv.get_field(line,4)'last) = "T_" then 
									part_ct_proj_tht := part_ct_proj_tht + 1;
								end if;

							else -- if facility part code is empty
								prog_position := "CP010";
								put_line(standard_output,"ERROR : No facility part code found for part '" & sm_csv.get_field(line,1) & "' !");
								raise constraint_error;
							end if;

						elsif sm_csv.get_field(line,6) = "NO" then -- we have a virtual device (like testpoints or fiducials)
							part_ct_proj_virtual := part_ct_proj_virtual + 1; -- count virtual parts
						else -- it is a non registered part -> abort
							prog_position := "CP020";
							put_line(standard_output,"ERROR : Part '" & sm_csv.get_field(line,1) & "' is not registered !");
							put_line(standard_output,"        Check your design !");
							raise constraint_error;
						end if;
					end if; -- if part_section_entered

					if not part_section_entered then 
						-- if table header found
						prog_position := "CP050";
						if sm_csv.get_field(line,1) = "Part" and sm_csv.get_field(line,2) = "Value" and
							sm_csv.get_field(line,3) = "Device" and sm_csv.get_field(line,4) = "Package" and
							sm_csv.get_field(line,5) = "Description" and sm_csv.get_field(line,6) = "BOM" and
							--sm_csv.get_field(line,7) = "COMMISSIONED" and sm_csv.get_field(line,8) = "FUNCTION" and -- rm v003

							-- ins v003 begin
							-- the column that holds the facility part code must be detected
							sm_csv.get_field(line,7) = "COMMISSIONED" and sm_csv.get_field(line,8) = "FUNCTION" then
								find_part_code_column:
								for f in 9..11 -- search in fields 9..11 for PART_CODE_FACILITY -- cs: search other fields ?
								loop
									for fa in 1..facility_count -- search for facility
									loop -- the primary facility code will be searched for first, then other facilities in 
										-- the order they appear in the conf file
										if sm_csv.get_field(line,f) = "PART_CODE_" & facility_names(fa) then
											prog_position := "CP055";
											if fa > 1 then -- if alternative facility code match
												prog_position := "CP056";
												new_line(standard_output);
												put_line(standard_output,"WARNING : PRIMARY FACILITY CODE '" & facility_names(1) & "' NOT FOUND IN EAGLE BOM !");
												put_line(standard_output,"          USING ALTERNATIVE CODE '" & facility_names(fa) & "'");
												new_line(standard_output);
												if request_user_confirmation
													(
													question_form => 1,
													show_confirmation_dialog => operator_confirmation_required
													) = false then
													raise constraint_error;
												end if;
											end if;
											column_of_part_code_facility_in_eagle_bom := f; -- save column for later usage
											part_section_entered := true; -- set part_section_entered flag
											exit find_part_code_column; -- do not search remaining columns
										end if;
									end loop;
								end loop find_part_code_column;
							-- ins v003 end

							--sm_csv.get_field(line,9) = "PART_CODE_" & facility_name then -- rm v003
							-- set part_section_entered flag
							--part_section_entered := true; -- rm v003
						end if;
					end if;
				end loop;

			prog_position := "CP060";

			-- abort program if no parts found
			if part_ct_proj = 0 then 
				prog_position := "CP100";
				put_line(standard_output,"ERROR : No parts with facility part code 'PART_CODE_" & facility_names(1) &
						"' found in file '" & to_string(eagle_bom_csv) & "' !");
				put_line(standard_output,"        Check facility code in file '" & conf_directory & conf_file_name & "' or header in file '" & to_string(eagle_bom_csv) & "' !");
				raise constraint_error; 
			end if;

--			new_line;
-- 			put_line("part statistic :");
-- 			put_line("SMD : " & natural'image(part_ct_proj_smd));
-- 			put_line("THT : " & natural'image(part_ct_proj_tht));
-- 			put_line("TOT : " & natural'image(part_ct_proj));
			--put_line("VRT : " & natural'image(part_ct_proj_virtual));

			return part_ct_proj; -- return total number of parts found in project file
		end count_parts_in_eagle_bom;



		function write_order_list
			(pos_ct	: natural) -- this is the number of positions of the facility bom file
			return boolean
			is
			part_found_on_stock		: boolean := false;
			position_pt				: natural := 0;
			order_sources_found		: natural := 0;
			order_sources_found_max	: natural := 0;

			-- number of pos. to order is equal or less than part_count_max2
			subtype parts_to_order_sized is parts_to_order (1..part_count_max2); 
			parts_to_order_array			: parts_to_order_sized;

		begin
			prog_position := "OL000";
			-- find part in stock data base and check availability
			for f in 1..pos_ct -- loop through facility_bom_part_array
			loop
				--put_line(standard_output,"f : " & natural'image(f));
				prog_position := "OL100";
				part_found_on_stock := false; -- reset this marker on every new position/part to be found
				for p in 1..part_count_max2 -- loop through part_stock_array
				loop
					if facility_bom_part_array(f).part_id = part_stock_array(p).part_id then -- part found in stock data base
						--put_line(standard_output,"part_id : " & natural'image(part_stock_array(p).part_id));
						if facility_bom_part_array(f).qty > part_stock_array(p).qty_available then -- if qty available on stock not sufficient
							position_pt := position_pt + 1; -- advance position_pt
							--put_line(standard_output,"part_id : " & natural'image(part_stock_array(p).part_id));
							parts_to_order_array(position_pt).part_id := part_stock_array(p).part_id; 
							parts_to_order_array(position_pt).part_code_fac := part_stock_array(p).part_code_fac;
							--parts_to_order_array(position_pt).quantity := facility_bom_part_array(f).qty; -- rm v006
							parts_to_order_array(position_pt).quantity := facility_bom_part_array(f).qty - part_stock_array(p).qty_available; -- ins v006
	
							-- collect distributor names and order codes
							prog_position := "OL200";
							order_sources_found := 0;
							for d in 1..distributor_count_max
							loop
								parts_to_order_array(position_pt).distributors(d).name := part_stock_array(p).distributors(d).name;
								parts_to_order_array(position_pt).distributors(d).order_code := part_stock_array(p).distributors(d).order_code;
								parts_to_order_array(position_pt).distributors(d).qty_min := part_stock_array(p).distributors(d).qty_min;
								parts_to_order_array(position_pt).distributors(d).price_net := part_stock_array(p).distributors(d).price_net;
					
								-- count valid order sources
								prog_position := "OL250";
								if to_string(part_stock_array(p).distributors(d).name) /= not_assigned_mark then -- distributor must be active
									if to_string(part_stock_array(p).distributors(d).order_code) /= not_assigned_mark then --order code must be there
										parts_to_order_array(position_pt).distributors(d).valid := true; -- mark distributor as valid source
										order_sources_found := order_sources_found + 1; -- count valid sources
									end if; -- order code must be there
								end if;  -- distributor must be active
							end loop;
							-- evaluate order_sources_found. if zero, print warning
							if order_sources_found = 0 then
								print_warning_on_no_part_source(part_stock_array(p).part_id, to_string(part_stock_array(p).part_code_fac));
							else -- otherwise update order_sources_found_max (will be used later when writing order information in order list)
								if order_sources_found > order_sources_found_max then
									order_sources_found_max := order_sources_found; 
								end if;
							end if;
						end if;
						part_found_on_stock := true;
						exit;
					end if;
				end loop;

				prog_position := "OL300";
				if part_found_on_stock = false then
					set_output(standard_output);
					new_line;
					put_line("ABORTED !");
					put_line("ERROR : Part with these properties not found in stock data base " & to_string(stock_db_csv) & ":");
					put_line("part_id      : " & natural'image(facility_bom_part_array(f).part_id));
					put_line("part_code_" & facility_names(1) & "  : " & to_string(facility_bom_part_array(f).part_code));
					raise constraint_error;
				end if;
			end loop;  -- loop through facility_bom_part_array

			--put_line(standard_output,"order : " & natural'image(parts_to_order));

			-- write items_to_order_file
			-- is case there is a shortage of parts, the items to order will be written here
			prog_position := "OL400";
			if position_pt > 0 then -- if at last one position needs to be ordered

				-- write order list header
				prog_position := "OL410";
				create( items_to_order_file, name => to_string(items_to_order_csv)); close(items_to_order_file);
				Open(
					File => items_to_order_file,
					Mode => out_file,
					Name => to_string(items_to_order_csv)
					);
				set_output(items_to_order_file);
				prog_position := "OL420";
				put_field(text => "STOCK MANAGER V" & version & " ORDER LIST"); put_lf;
				prog_position := "OL421";
				put_field(text => sm_csv.row_separator_2); put_lf;
				put_field(text => "BOARD:"); put_field(text => base_name(to_string(facility_bom_csv))); put_lf;
				put_field(text => "NUMBER OF UNITS: "); put_field(text => natural'image(quantity_of_units)); put_lf(count => 2);
				put_field(text => "DATE:"); put_field(text => date_now); put_field(text => "(YYYY:MM:DD HH:MM:SS)"); put_lf;
				put_field(text => sm_csv.row_separator_1); put_lf;
				put_field(text => "POS."); put_field(text => "PART_ID"); put_field(text => "PART_CODE_" & facility_names(1)); put_field(text => "QTY");

				-- write table header for ordering information
 				for os in 1..order_sources_found_max 
 				loop
					put_field(text => "DISTRIBUTOR NAME / ORDER_CODE / QTY_MIN / PRICE_NET");
 				end loop;
				
				put_lf(count => 2);

				prog_position := "OL430";
				for o in 1..position_pt
				loop
					put_field(items_to_order_file, text => natural'image(o));
					put_field(items_to_order_file, text => natural'image(parts_to_order_array(o).part_id));
					put_field(items_to_order_file, text => to_string(parts_to_order_array(o).part_code_fac));
					put_field(items_to_order_file, text => natural'image(parts_to_order_array(o).quantity));

					-- loop through distributors to find valid order information
 					for d in 1..distributor_count_max
 					loop
						if parts_to_order_array(o).distributors(d).valid then -- select only valid order sources
							put_field(text => to_string(parts_to_order_array(o).distributors(d).name) &
								" OC:  " & to_string(parts_to_order_array(o).distributors(d).order_code) &
								"  / QTY_MIN: " & natural'image(parts_to_order_array(o).distributors(d).qty_min) &
								"  / PRICE_NET: " & money_positive'image(parts_to_order_array(o).distributors(d).price_net)
							);
 						end if;  -- distributor valid
 					end loop;
					put_lf; -- put trailing line break
				end loop;

				put_field(text => "END OF ORDER LIST");
				close(items_to_order_file);
				prog_position := "OL440";
				set_output(standard_output);
				new_line;
				put_line("Parts need to be ordered. Please read file '" & to_string(items_to_order_csv) & "' !");
				new_line;
				if stock_operation /= query_bom then
					put_line("Withdrawal list not created.");
				end if;
				-- remove stale items_to_take file
				if exists(to_string(items_to_take_csv)) then
					delete_file(to_string(items_to_take_csv));
				end if;
				new_line;
				return false; -- parts need to be orderd, so return false
			end if; -- if parts_to_order > 0
			return true; -- if no parts are to order, return true
		end write_order_list;


		procedure write_items_to_take_from_stock_list
			( position_pt : natural)
			is
		begin
			prog_position := "IT000";
			create( items_to_take_file, name => to_string(items_to_take_csv)); close(items_to_take_file);
			open(
				file => items_to_take_file,
				mode => out_file,
				name => to_string(items_to_take_csv)
				);
			set_output(items_to_take_file);
			prog_position := "IT010";
			put_line("STOCK MANAGER V" & version & " WITHDRAWAL LIST");
			put_line(row_separator);
			put_field(text => "BOARD:"); put_field(text => base_name(to_string(facility_bom_csv))); put_lf;
			put_field(text => "NUMBER OF UNITS: "); put_field(text => natural'image(quantity_of_units)); put_lf(count => 2);
			--put_line("DATE: " & image(clock, time_zone => UTC_Time_Offset(clock)) & " (YYYY:MM:DD HH:MM:SS)");
			--put_line("DATE: " & date_now & " (YYYY:MM:DD HH:MM:SS)"); -- rm v010
			put_field(text => "DATE: " & date_now); put_field(text => "(YYYY:MM:DD HH:MM:SS)"); put_lf; -- ins v010
			put_line(row_separator);
			--put_field(text => "POS."); put_field(text => "PART_ID"); put_field(text => "PART_CODE_" & facility_names(1)); put_field(text => "QTY"); -- rm v009
			put_field(text => "POS."); put_field(text => "STORAGE_PLACE"); put_field(text => "PART_ID");  put_field(text => "QTY"); put_field(text => "PART_CODE_" & facility_names(1)); -- ins v009
			put_lf(count => 2);
			for t in 1..position_pt
			loop
				put_field(text => natural'image(t)); -- write position

				-- ins v009 begin
				for p in 1..part_count_max2 -- loop through part_stock_array to find storage_place of part
				loop
					if facility_bom_part_array(t).part_id = part_stock_array(p).part_id then -- on part_id match
						put_field(text => to_string(part_stock_array(p).storage_place)); -- ins v009
						exit; -- no need for further part search
					end if;
				end loop;
				-- ins v009 end

				put_field(text => natural'image(facility_bom_part_array(t).part_id)); -- ins v009
				put_field(text => natural'image(facility_bom_part_array(t).qty)); -- ins v009
				--put_field(text => natural'image(facility_bom_part_array(t).part_id)); -- rm v009
				put_field(text => to_string(facility_bom_part_array(t).part_code));
				--put_field(text => natural'image(facility_bom_part_array(t).qty)); -- rm v009
				put_lf;
			end loop;
			put_field(text => "END OF LIST"); put_lf;
			set_output(standard_output);
			close(items_to_take_file);
		end;



		function checkout_by_facility_bom
			return boolean -- return true if parts have been checked out
							-- return false if no parts have been checked out
			is
			position_pt						: natural := 0;
			line							: unbounded_string;
			part_section_entered			: boolean := false;

			positions_of_facility_bom_max	: natural := part_count_max2;
			previous_input					: ada.text_io.file_type renames current_input;
		begin
			-- read facility bom file in array
			prog_position := "CF000";
			Open(
				File => facility_bom_file,
				Mode => in_file,
				Name => to_string(facility_bom_csv)
				);			
			set_input(facility_bom_file);

			prog_position := "CF050";
			-- read facility_bom_file in array facility_bom_part_array
			while not end_of_file	-- we assume the facility_bom_file is ok, as it has been generated by stock_manager itself
			loop					-- so there will be no further syntax and type checks
				line := get_line;
				--put_line(standard_output, line);
				if part_section_entered then
					prog_position := "CF060";
					--put_line(standard_output, line);

					-- look for end of BOM mark in column 1
					if sm_csv.get_field(line,1) = sm_csv.row_separator_1 then 
						exit; -- stop reading the bom file
					end if;

					--put_line("field ct : " & natural'image(sm_csv.get_field_count(line)));
					if sm_csv.get_field(line,1) /= "" then -- if first position found, the first field is not empty
						--put_line(standard_output, line);
						prog_position := "CF070";
						position_pt := position_pt + 1; -- advance position pointer
						facility_bom_part_array(position_pt).position := natural'value(sm_csv.get_field(line,1));
						facility_bom_part_array(position_pt).qty := quantity_of_units * natural'value(sm_csv.get_field(line,2));
						facility_bom_part_array(position_pt).name := to_unbounded_string(sm_csv.get_field(line,3));
						facility_bom_part_array(position_pt).part_code := to_bounded_string(sm_csv.get_field(line,4));
						facility_bom_part_array(position_pt).part_id := natural'value(sm_csv.get_field(line,5));
					end if;
				else
					-- look for table header in column 1
					prog_position := "CF080";
					if sm_csv.get_field(line,1) = "POS." then 
						part_section_entered := true;
					end if;
				end if; -- if part_section_entered
			end loop;

			prog_position := "CF100";
			--set_input(standard_input);
			set_input(previous_input);
			close(facility_bom_file);

			prog_position := "CF200";
			if write_order_list(position_pt) then -- pass number of positions. 
				-- if result is true, all positions are available on stock, no order required
				-- ask operator if she really means it
				new_line(standard_output); 
				put_line(standard_output,"All positions of given BOM are available on stock.");
				new_line(standard_output);

				put_line("No order list created.");
				-- remove stale order file
				if exists(to_string(items_to_order_csv)) then
					delete_file(to_string(items_to_order_csv));
				end if;


				if stock_operation = checkout_bom then
					put_line(standard_output,"WARNING: All positions of given BOM will be checked out from stock !!!");
					put_line(standard_output,"WARNING: After checkout you must take all items from stock as listed in file '" & to_string(items_to_take_csv) & "' !");
					write_items_to_take_from_stock_list(position_pt);
					if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
						raise constraint_error;
					end if;

					put_line(standard_output,"Now withdraw items from stock as listed in file '" & to_string(items_to_take_csv) & "' !");
					new_line(standard_output);
					--put_line(standard_output,"ARE YOU DONE WITH TAKING ITEMS FROM STOCK ???");

					-- update part_stock_array
					prog_position := "CF200";
					for f in 1..position_pt -- loop through facility_bom_part_array
					loop
						for p in 1..part_count_max2 -- loop through part_stock_array
						loop
							if facility_bom_part_array(f).part_id = part_stock_array(p).part_id then -- on part_id match
								part_stock_array(p).qty_on_stock := part_stock_array(p).qty_on_stock - facility_bom_part_array(f).qty; -- update qty_on_stock
								part_stock_array(p).qty_available := part_stock_array(p).qty_on_stock - part_stock_array(p).qty_reserved;
								--part_stock_array(p).date_edited := image(clock, time_zone => UTC_Time_Offset(clock)); -- update time edited
								part_stock_array(p).date_edited := date_now; -- update time edited
								exit; -- no need for further part search
							end if;
						end loop;
					end loop;
				end if; -- if stock_operation = checkout_bom
				return true; -- if parts have been checked out, return true
			else -- parts are missing. write_order_list returned false

				-- if parts are to be ordered. in this case no checkout took place
				if stock_operation = checkout_bom then -- print message if checkout was required. on query no need for it.
					put_line(standard_output,"No parts checked out from stock.");
					new_line;
				end if;
				return false; -- if no parts have been checked out, return false

			end if; -- if write_order_list

		end checkout_by_facility_bom;



		procedure convert_eagle_bom_to_facility_bom is
			subtype parts_of_eagle_bom_sized is parts_of_eagle_bom (1..part_ct_proj);
			project_part			: parts_of_eagle_bom_sized;
			ct 						: natural := 0;
			part_section_entered 	: boolean := false;
			line					: unbounded_string;
			quantity				: natural;
			price_net_min			: money_positive;
			price_net_max			: money_positive;
			price_total_min			: money_positive := 0.00;
			price_total_max			: money_positive := 0.00;
			min_start_value_set 	: boolean := false;
			max_start_value_set 	: boolean := false;
			customized_prefix_length	: natural; -- ins v003
			valid_source_found		: boolean := false; -- ins v005

			function check_value	-- returns true if ok, raises constraint_error on occurence of forbidden character
				( 	value_test	: string) 
				return boolean is
			begin
				for i in 1..value_test'length
				loop
					case value_test(i) is
						when ' ' =>	return false;
						when character'val(181) => return false; -- micro character
						-- CS add more forbidden charaters here -- use "valid" ?
						when others => null;
					end case;
				end loop;
				return true;
			end check_value;


			function verify_prefix
				-- test if prefix is among allowed prefixes. returns given prefix if positive
				--return boolean is -- rm v003
				return string is -- ins v003
				prefix_valid	: boolean := false;
				prefix_to_return	: universal_string_type.bounded_string := to_bounded_string(""); -- defaults empty, --ins v003
			begin 
				-- test one-character standard prefixes C, D, F, J, K, L, Q, R, S, T, X
				if is_digit(to_string(project_part(ct).part)(2)) then -- test if 2nd character is a digit
					--case to_string(project_part(ct).part)(1) is -- test if 1st character is a valid prefix -- rm v003
					for p in 0..part_prefix_type'pos(X) -- NOTE: X must be last in one-character group type definition
					-- so this loop goes on until prefix X has been tested for
					loop -- on match of first character
						if to_string(project_part(ct).part)(1) = part_prefix_type'image(part_prefix_type'val(p))(1) then
							prefix_valid := true;
							-- save prefix_to_return
							prefix_to_return := to_bounded_string( slice(project_part(ct).part,1,1) ); -- ins v003
							exit;
						end if;
					end loop;
					--	when 'R' | 'C' | 'L' | 'F' | 'T' | 'D' | 'X' | 'J' | 'S' | 'K' => prefix_valid := true; -- rm v003
					--when others => null;	-- rm v003
					--end case;	-- rm v003
				end if;

				-- test two-character standard prefixes IC, RN
				--if index(project_part(ct).part,"IC") = 1 or index(project_part(ct).part,"RN") = 1 then -- rm v003
-- 				if index(project_part(ct).part, part_prefix_type'image(part_prefix_type'val(10)) ) = 1 -- this is an IC -- ins v003 -- rm v004
-- 				or index(project_part(ct).part, part_prefix_type'image(part_prefix_type'val(11)) ) = 1 then -- this is an RN -- ins v003 -- rm v004
				--if index(project_part(ct).part, part_prefix_type'image(part_prefix_type'val(11)) ) = 1 -- this is an IC -- ins v004 -- rm v011
				if index(project_part(ct).part, part_prefix_type'image(IC)) = 1 -- this is an IC -- ins v011
				--or index(project_part(ct).part, part_prefix_type'image(part_prefix_type'val(12)) ) = 1 then -- this is an RN -- ins v004 -- rm v011
				or index(project_part(ct).part, part_prefix_type'image(RN)) = 1 then -- this is an RN -- ins v011
					if is_digit(to_string(project_part(ct).part)(3)) then -- test if 3th character is a digit (i.e. IC3 or RN4)
						prefix_valid := true;
						-- save prefix_to_return
						prefix_to_return := to_bounded_string( slice(project_part(ct).part,1,2) ); -- ins v003
					end if;
				end if;

				-- test three-character standard prefixes LED
				--if index(project_part(ct).part,"LED") = 1 then -- rm v003
				--if index(project_part(ct).part, part_prefix_type'image(part_prefix_type'val(12)) ) = 1 then -- this is an LED -- ins v003 -- rm v004
				--if index(project_part(ct).part, part_prefix_type'image(part_prefix_type'val(13)) ) = 1 -- this is an LED -- ins v004 -- rm v011
				if index(project_part(ct).part, part_prefix_type'image(LED)) = 1 -- this is an LED -- ins v011
				or index(project_part(ct).part, part_prefix_type'image(DIS)) = 1 -- this is a display -- ins v013
				--or index(project_part(ct).part, part_prefix_type'image(part_prefix_type'val(14)) ) = 1 then -- this is a BAT -- ins v004 -- rm v011
				or index(project_part(ct).part, part_prefix_type'image(BAT)) = 1 then -- this is a BAT -- ins v011
					if is_digit(to_string(project_part(ct).part)(4)) then -- test if 4th character is a digit (i.e. LED3 or DIS7)
						prefix_valid := true;
						-- save prefix_to_return
						prefix_to_return := to_bounded_string( slice(project_part(ct).part,1,3) ); -- ins v003
					end if;
				end if;

				-- ins v007 begin
				--if index(project_part(ct).part, part_prefix_type'image(part_prefix_type'val(15)) ) = 1 then -- this is an accessory part
				if index(project_part(ct).part, part_prefix_type'image(ACCESSORY) ) = 1 then -- this is an accessory part
					prefix_valid := true;
					-- save prefix_to_return
					prefix_to_return := to_bounded_string( part_prefix_type'image(ACCESSORY) );
				end if;
				-- ins v007 end

				-- ins v008 begin
				if index(project_part(ct).part, part_prefix_type'image(FH) ) = 1 then -- this is a fuse holder
					prefix_valid := true;
					-- save prefix_to_return
					prefix_to_return := to_bounded_string( part_prefix_type'image(FH) );
				end if;
				-- ins v008 end

				-- ins v011 begin
				if index(project_part(ct).part, part_prefix_type'image(MODULE) ) = 1 then
					prefix_valid := true;
					-- save prefix_to_return
					prefix_to_return := to_bounded_string( part_prefix_type'image(MODULE) );
				end if;

				if index(project_part(ct).part, part_prefix_type'image(CABLE) ) = 1 then
					prefix_valid := true;
					-- save prefix_to_return
					prefix_to_return := to_bounded_string( part_prefix_type'image(CABLE) );
				end if;

				if index(project_part(ct).part, part_prefix_type'image(WIRE) ) = 1 then
					prefix_valid := true;
					-- save prefix_to_return
					prefix_to_return := to_bounded_string( part_prefix_type'image(WIRE) );
				end if;

				if index(project_part(ct).part, part_prefix_type'image(PLUG) ) = 1 then
					prefix_valid := true;
					-- save prefix_to_return
					prefix_to_return := to_bounded_string( part_prefix_type'image(PLUG) );
				end if;

				if index(project_part(ct).part, part_prefix_type'image(RECEPTACLE) ) = 1 then
					prefix_valid := true;
					-- save prefix_to_return
					prefix_to_return := to_bounded_string( part_prefix_type'image(RECEPTACLE) );
				end if;

				if index(project_part(ct).part, part_prefix_type'image(TERMINALF) ) = 1 then
					prefix_valid := true;
					-- save prefix_to_return
					prefix_to_return := to_bounded_string( part_prefix_type'image(TERMINALF) );
				end if;

				if index(project_part(ct).part, part_prefix_type'image(TERMINALM) ) = 1 then
					prefix_valid := true;
					-- save prefix_to_return
					prefix_to_return := to_bounded_string( part_prefix_type'image(TERMINALM) );
				end if;
				-- ins v011 end

				-- ins v003 begin
				-- test custom prefixes as specified in configuration file
				-- loop through custom prefixes
				for cp in 1..customized_prefixes_count 
				loop
					if index(project_part(ct).part,to_string(customized_prefixes(cp))) = 1 then -- on match
						customized_prefix_length := length(customized_prefixes(cp)); -- get length of prefix
						-- test if next character is a digit (i.e. XP3 or DT64)
						if is_digit(to_string(project_part(ct).part)(customized_prefix_length+1)) then 
							prefix_valid := true;
							-- save prefix_to_return
							prefix_to_return := customized_prefixes(cp); -- ins v003
							exit;
						end if;
					end if;
				end loop;
				-- ins v003 end

				-- after all those checks, if prefix is still invalid
				if prefix_valid = false then
					new_line(standard_output);					
					put_line(standard_output,"ERROR : Part " & to_string(project_part(ct).part) & " has invalid prefix !");	
					--put_line(standard_output,"        Prefixes allowed are: R, RN, C, L, F, T, D, X, J, S, IC, K"); -- rm v003
					put(standard_output,"        Standard prefixes are: "); -- ins v003
					--new_line(standard_output); -- rm v003

					-- ins v003 begin
					-- print standard prefixes
					for p in 0..part_prefix_count
					loop
						put(standard_output,part_prefix_type'image(part_prefix_type'val(p)));
						if p < part_prefix_count then put(standard_output,", "); end if;
					end loop;
					new_line(standard_output);

					-- print customer prefixes if specified
					if customized_prefixes_count > 0 then
						put(standard_output,"        Customer prefixes are: "); -- ins v003
						for cp in 1..customized_prefixes_count
						loop
							put(standard_output,to_string(customized_prefixes(cp)));
							if cp < customized_prefixes_count then put(standard_output,", "); end if;
						end loop;
						new_line(standard_output); 
					end if;
					new_line(standard_output); 
					-- ins v003 end

					--return false; -- rm v003
					-- prefix_to_return is still empty
					return to_string(prefix_to_return); -- ins v003
				end if;

				-- if prefix is valid
				--return true; -- rm v003
				-- prefix_to_return holds valid prefix
				return to_string(prefix_to_return); -- rm v003
			end verify_prefix;


			function verify_value_fields -- in project csv file
				return boolean is
			begin
				if Ada.Strings.Fixed.count(
					to_string(project_part(ct).part_code_fac),
					"_VAL_" & to_string(project_part(ct).value) -- verify _VAL_value_ against value
					) /= 1 then -- the _VAL_value field must occure only once
						new_line(standard_output);
						put_line(standard_output,"ERROR : Part Code of " & to_string(project_part(ct).part) & " does not contain a valid value field !");
						put_line(standard_output,"        Part Code found : " & to_string(project_part(ct).part_code_fac));
						put_line(standard_output,"        Value found     : " & to_string(project_part(ct).value));
						new_line(standard_output);
					return false;
				end if;
				return true;
			end verify_value_fields;

			function verify_package_fields -- in project csv file
				return boolean is
				begin
					if Ada.Strings.Fixed.count(
						to_upper(to_string(project_part(ct).part_code_fac)),
						"_PAC_" & to_string(project_part(ct).packge) -- verify _PAC_package against package
						) /= 1 then -- the _PAC_package field must occure only once
						new_line(standard_output);
						put_line(standard_output,"ERROR : Part Code of " & to_string(project_part(ct).part) & " does not contain a valid package field !");
						put_line(standard_output,"        Part Code found : " & to_string(project_part(ct).part_code_fac));
						put_line(standard_output,"        Package found   : " & to_string(project_part(ct).packge));
						new_line(standard_output);
						return false;
					end if;
				return true;
			end verify_package_fields;

			function verify_function_fields -- in project csv file
				return boolean is
				begin
					if project_part(ct).funct = "" then -- if function field empty
						new_line(standard_output);
						put_line(standard_output,"ERROR : Part " & to_string(project_part(ct).part) & " has not been assigned a function to !");
						new_line(standard_output);
						return false;
					end if;
				return true;
			end verify_function_fields;


		begin
		-- this loop reads the eagle bom file in sized array project_part
		prog_position := "RB000";
		while not end_of_file
			loop
				prog_position := "RB010";
				line:=get_line;
				prog_position := "RB020";
					if part_section_entered then
						if to_lower(sm_csv.get_field(line,6,';')) = "yes" then
							prog_position := "RB030";
							ct := ct + 1; -- count parts
							--put_line(natural'image(ct) & " " & line);
							-- fill project_part array
							project_part(ct).part	 		:= to_bounded_string(sm_csv.get_field(line,1,';'));
							project_part(ct).value	 		:= to_bounded_string(sm_csv.get_field(line,2,';'));

							-- make sure there is no forbidden character in the value field
							prog_position := "RB100";
							if check_value(to_string(project_part(ct).value)) = false then
								put_line(standard_output,"ERROR : Part " & to_string(project_part(ct).part) & " has forbidden characters in its value field !");
								put_line(standard_output,"        Found value '" & to_string(project_part(ct).value) & "'");
								raise constraint_error;
							end if;

							prog_position := "RB200";
							project_part(ct).device 		:= to_bounded_string(sm_csv.get_field(line,3,';'));
							project_part(ct).packge 		:= to_bounded_string(sm_csv.get_field(line,4,';'));
							project_part(ct).description	:= to_bounded_string(sm_csv.get_field(line,5,';'));
							project_part(ct).commissioned	:= to_bounded_string(check_date(sm_csv.get_field(line,7)));
							project_part(ct).funct	 		:= to_bounded_string(sm_csv.get_field(line,8,';'));

							-- ins v003 begin
							-- make sure, the facility part code is valid
							--prog_position := "RB210";
							--put_line(standard_output,"part_code_fac : " & sm_csv.get_field(line,9));
							prog_position := "RB211";
							if parse_part_code(trim(sm_csv.get_field(line,column_of_part_code_facility_in_eagle_bom),both)) = false then
								put_line(standard_output,"ERROR : Part " & to_string(project_part(ct).part) & " has forbidden characters in its facility part code !");
								new_line(standard_output); -- ins v003
								--put_line(standard_output,"        Found value '" & to_string(project_part(ct).value) & "'");
								prog_position := "RB212";
								raise constraint_error;
							else
								prog_position := "RB213";
								project_part(ct).part_code_fac	:= to_bounded_string(trim(sm_csv.get_field(line,column_of_part_code_facility_in_eagle_bom),both));
							end if;
							prog_position := "RB214";
							-- ins v003 end

							--project_part(ct).part_code_fac	:= to_bounded_string(trim(sm_csv.get_field(line,9),both)); -- rm v003

							-- verify if prefix is valid
							prog_position := "RB300"; -- rm v003
							--if verify_prefix = false then -- rm v003
							if verify_prefix = "" then -- ins v003
								raise constraint_error;
							end if;

							-- verify packages, values, function field of current part being processed
							prog_position := "RB400";
							-- if part prefix is R,C,L,F,T with a number following it (i.e. R505, C409)
							--if is_digit(to_string(project_part(ct).part)(2)) then -- test if 2nd character is a digit -- rm v003
							--	case to_string(project_part(ct).part)(1) is -- test if 1st character is a prefix -- rm v003

							-- rm v011 begin
							--if verify_prefix = "R" or verify_prefix = "C" or verify_prefix = "L" or verify_prefix = "F" or -- ins v003
							--verify_prefix = "D" or verify_prefix = "Q" or -- ins v004
							--verify_prefix = "T" or verify_prefix = "K" or verify_prefix = "RN" then -- ins v003
							-- rm v011 end

							-- ins v011 begin
							if verify_prefix = part_prefix_type'image(R) 
							or verify_prefix = part_prefix_type'image(C)
							or verify_prefix = part_prefix_type'image(L)
							or verify_prefix = part_prefix_type'image(F)
							or verify_prefix = part_prefix_type'image(D)
							or verify_prefix = part_prefix_type'image(Q)
							or verify_prefix = part_prefix_type'image(T)
							or verify_prefix = part_prefix_type'image(K)
							or verify_prefix = part_prefix_type'image(RN)
							then
							-- ins v011 end

								--when 'R' | 'C' | 'L' | 'F' | 'T' | 'K' => -- rm v003
								-- make sure part_code _VAL_ field matches actual value field
									if verify_value_fields = false then
										raise constraint_error;
									end if;
									-- make sure part_code _PAC_ field matches actual package field
									if verify_package_fields = false then
										raise constraint_error;
									end if;

							-- if part prefix is IC
							prog_position := "RB500";
							--if index(project_part(ct).part,"IC") = 1 then -- rm v003
							--	if is_digit(to_string(project_part(ct).part)(3)) then -- test if 3rd character is a digit (i.e. IC45) -- rm v003
							-- elsif verify_prefix = "IC" then -- ins v003 -- rm v011
							elsif verify_prefix = part_prefix_type'image(IC) then -- ins v011
								-- make sure part_code _VAL_ field matches actual value field
								if verify_value_fields = false then
									raise constraint_error;
								end if;
								-- make sure part_code _PAC_ field matches actual package field
								if verify_package_fields = false then
									raise constraint_error;
								end if;
								--end if; -- rm v003


							-- if part prefix is LED
							prog_position := "RB600";
							--if index(project_part(ct).part,"LED") = 1 then -- rm v003
							--	if is_digit(to_string(project_part(ct).part)(4)) then -- test if 4th character is a digit (i.e. LED3) -- rm v003
							--elsif verify_prefix = "LED" then -- ins v003 -- rm v011
							elsif verify_prefix = part_prefix_type'image(LED) -- ins v011
							or  verify_prefix = part_prefix_type'image(DIS) then -- ins v013
								-- make sure part_code _VAL_ field matches actual value field
								if verify_value_fields = false then
									raise constraint_error;
								end if;
								-- make sure part_code _PAC_ field matches actual package field
								if verify_package_fields = false then
									raise constraint_error;
								end if;
								-- check if function has been assigned
								if verify_function_fields = false then -- if no function assigned
									raise constraint_error;										
								end if;
								--end if; -- rm v003


-- rm v003 begin
-- 							-- if part prefix is RN
-- 							prog_position := "RB700";
-- 							if index(project_part(ct).part,"RN") = 1 then
-- 								if is_digit(to_string(project_part(ct).part)(3)) then -- test if 3rd character is a digit (i.e. RN405)
-- 									-- make sure part_code _VAL_ field matches actual value field
-- 									if verify_value_fields = false then
-- 										raise constraint_error;
-- 									end if;
-- 									-- make sure part_code _PAC_ field matches actual package field
-- 									if verify_package_fields = false then
-- 										raise constraint_error;
-- 									end if;
-- 								end if;
-- 							end if;
-- rm v003 end

							-- if part prefix is X,J,S with a number following it (i.e. X505, S409)
							prog_position := "RB800";
							--if is_digit(to_string(project_part(ct).part)(2)) then -- test if 2nd character is a digit -- rm v003
							--elsif verify_prefix = "X" or verify_prefix = "J" or verify_prefix = "S" then -- ins v003 -- rm v011

							-- ins v011 begin
							elsif verify_prefix = part_prefix_type'image(X)
							or verify_prefix = part_prefix_type'image(J)
							or verify_prefix = part_prefix_type'image(S)
							then
							-- ins v011 end

							--	prog_position := "RB830"; -- rm v003
							--	case to_string(project_part(ct).part)(1) is -- test if 1st character is a prefix -- rm v003
							--		when 'X' | 'J' | 'S' => -- rm v003
							--			prog_position := "RB840"; -- rm v003
								if verify_function_fields = false then -- if no function assigned to X,J,S
									prog_position := "RB850";
									raise constraint_error;										
								end if;
							--		when others => null; -- rm v003
							--	end case; -- rm v003

							-- ins v011 begin
							prog_position := "RBA00";
							elsif verify_prefix = part_prefix_type'image(RECEPTACLE)
							or verify_prefix = part_prefix_type'image(PLUG)
							or verify_prefix = part_prefix_type'image(TERMINALF)
							or verify_prefix = part_prefix_type'image(TERMINALM)
							or verify_prefix = part_prefix_type'image(CABLE)
							or verify_prefix = part_prefix_type'image(MODULE)
							or verify_prefix = part_prefix_type'image(WIRE)
							then
								prog_position := "RBA10";
								null;
							
							-- ins v011 end

							-- ins v003 begin
							-- for all other prefixes (incl. customer specific prefixes) applies:
							-- check function field in any case
							prog_position := "RB810";
							elsif verify_prefix /= "" then
								prog_position := "RB820";
								if verify_function_fields = false then -- if no function assigned to XP3, DT64, FH, ...
									prog_position := "RB870";
									raise constraint_error;										
								end if;
							end if;
							-- ins v003 end

						end if; -- if "yes" found
					end if; -- if part section entered

					prog_position := "RB890";
					if sm_csv.get_field(line,1,';') = "Part" then -- set part_section_entered flag upon passing the "Part" field -- ins v003
						prog_position := "RB900";
						part_section_entered := true;
					end if;
					prog_position := "RB892";
			end loop;
		

		-- write BOM file
		prog_position := "WB000";

		-- write BOM header
		set_output(bom_output_file);
		put_field(text => "BILL OF MATERIAL"); put_lf;
		put_field(text => "------------------------------"); put_lf;
		put_field(text => "board:"); put_field(text => base_name(to_string(eagle_bom_csv))); put_lf(count => 2);
		--put_field(text => "created by STOCK MANAGER"); put_field(text => "V" & version); put_lf; -- rm v010
		put_field(text => "created by"); put_field(text => "STOCK MANAGER"); put_field(text => "V" & version); put_lf; -- ins v010
		put_field(text => "contact:"); put_field(text => "www.blunk-electronic.de"); put_lf;
		put_field(text => "date:"); put_field(text => date_now);
		put_field(text => "(YYYY-MM-DD HH:MM:SS)"); put_lf(count => 2);
		put_field(text => "part count SMD:"); put_field(text => trim(natural'image(part_ct_proj_smd),left)); put_lf;
		put_field(text => "part count THT:"); put_field(text => trim(natural'image(part_ct_proj_tht),left)); put_lf;
		put_field(text => "part count TOTAL:"); put_field(text => trim(natural'image(part_ct_proj),left)); put_lf;
		new_line;
		put_field(text => "POS.");
		put_field(text => "QTY");
		put_field(text => "PART_NAME");
		put_field(text => "PART_CODE_" & facility_names(1));
		put_field(text => "PART_ID");
	
		-- ins v010 begin
		for m in 1..manufacturer_count_max
		loop
			put_field(text => "MANUFACTURER / PART_CODE");
		end loop;
		-- ins v010 end

		put_field(text => "PRICE_NET_MIN");
		put_field(text => "PRICE_NET_MAX");
		put_lf(count => 2);

		-- loop through array project_part, ct points to individual member being processed
		quantity := 0;
		ct := 0;
		while ct < part_ct_proj	
		loop
			ct := ct + 1; -- advance member pointer
			--put_line(standard_output,project_part(ct).part);
			if project_part(ct).processed = false then -- if part has not been processed yet
				--put_line(standard_output,project_part(ct).part);
				project_part(ct).processed := true; -- set processed flag
				id_master := ct; -- save member id in id_master
				quantity := 1;	-- so at least one part has been found
				parts_of_same_value := to_unbounded_string(to_string(project_part(id_master).part));
				-- CS: find part in data base by id_master
				prog_position := "WB010";

				-- do further looping in array project_part to find parts with same part_code_fac
				while ct < part_ct_proj 
				loop
					ct := ct + 1; -- advance member pointer
					if project_part(ct).processed = false then -- if part has not been processed yet
						--put_line(standard_output,to_string(project_part(ct).value));
						if project_part(ct).part_code_fac = project_part(id_master).part_code_fac then -- and if part code matches the part code of the part@master_id
							project_part(ct).processed := true;  -- set processed flag
							quantity := quantity + 1; -- count devices with same part code
							-- collect parts with same part_code_fac, they will be written into the bom later
							parts_of_same_value := parts_of_same_value & ", " & to_unbounded_string(to_string(project_part(ct).part));
							-- CS: find part in data base by ct
							--put_line(standard_output,parts_of_same_value);
						end if;
					end if;
				end loop;
				-- output result

				--put_line(standard_output,project_part(ct).part);

				-- write position, quantity, parts of same part code, value, package, placement [y/n], provided by customer [y/n]
				prog_position := "WB100";
				position := position + 1; -- increment position counter 
				put_field(text => trim(natural'image(position),left));
				put_field(text => trim(natural'image(quantity),left));
				put_field(text => to_string(parts_of_same_value));
				--put_field(text => to_string(project_part(id_master).value));
				--put_field(text => to_string(project_part(id_master).packge));
				put_field(text => to_string(project_part(id_master).part_code_fac));
				--put_field(text => "Y");  --CS: should depend on assembly variant but allows manual modification later
				--put_field(text => "Y");  -- provided by customer default : NO

				-- search for part in part_stock_array and write its part ID, price_net_min, price_net_max
				prog_position := "WB200";
				part_occured_in_sdb := false; -- ins v003
				for p in 1..part_count_max2
				loop
					-- if part found in stock data base, put its part_id in column E
					if part_stock_array(p).part_code_fac = project_part(id_master).part_code_fac then
						put_field(text => trim(natural'image(part_stock_array(p).part_id),left));

						-- ins v010 begin
						-- write manufacturer part codes
						for m in 1..manufacturer_count_max
						loop
							put_field(text => to_string(part_stock_array(p).manufacturers(m).name & " / " &
							part_stock_array(p).manufacturers(m).part_code));
						end loop;
						-- ins v010 end

						-- write min/max price of part in column F/G
						--price_net_min := minimum(dist_max => distributor_count_max, part_id => part_stock_array(p).part_id);
						min_start_value_set := false; -- reset flag that initiates reading the start value
						max_start_value_set := false; -- reset flag that initiates reading the start value
						valid_source_found := false; -- ins v005
						for d in 1..distributor_count_max
						loop
							if to_string(part_stock_array(p).distributors(d).name) /= not_assigned_mark then -- make sure distr. is active
								valid_source_found := true; -- if at least one distributor set -- ins v005

								-- check if price is zero and print a warning message
								if part_stock_array(p).distributors(d).price_net = 0.00 then
									print_warning_on_zero_price(
										part_id => part_stock_array(p).part_id,
										distributor => d
										);
								end if;


								-- find the lowest price
								-- if no start value set yet
								if not min_start_value_set then
									price_net_min := part_stock_array(p).distributors(d).price_net; -- take first price as start value
									--put_field(standard_output,text => money_positive'image(price_net_min));
									min_start_value_set := true;
								else 
									-- if start value set already
									--put_field(standard_output,text => money_positive'image(price_net_min));
									if part_stock_array(p).distributors(d).price_net < price_net_min then -- if price is lower than latest price_net_min
										price_net_min := part_stock_array(p).distributors(d).price_net; -- update price_net_min
									end if;
								end if; 




								-- find the highest price
								-- if no start value set yet
								if not max_start_value_set then
									price_net_max := part_stock_array(p).distributors(d).price_net; -- take first price as start value
									--put_field(standard_output,text => money_positive'image(price_net_min));
									max_start_value_set := true;
								else 
									-- if start value set already
									--put_field(standard_output,text => money_positive'image(price_net_min));
									if part_stock_array(p).distributors(d).price_net > price_net_max then -- if price is higher than latest price_net_max
										price_net_max := part_stock_array(p).distributors(d).price_net; -- update price_net_max
									end if;
								end if; 

							end if; -- if distributor is active

						end loop;

						-- ins v005 begin
						if not valid_source_found then
							print_warning_on_no_part_source
								(
								part_id => part_stock_array(p).part_id,
								part_code => to_string(part_stock_array(p).part_code_fac)
								);
							price_net_min := 0.00; -- ins v013
							price_net_max := 0.00; -- ins v013
						end if;
						-- ins v005 end

						put_field(text => money_positive'image(price_net_min * quantity));
						price_total_min := price_total_min + (price_net_min * quantity);
						put_field(text => money_positive'image(price_net_max * quantity));
						price_total_max := price_total_max + (price_net_max * quantity);
						put_lf; -- put trailing line break 

						--new_line(standard_output);

						part_occured_in_sdb := true;
						exit;
					end if;
				end loop;

				
	
				-- abort process if part not on stock
				prog_position := "WB300";
				if part_occured_in_sdb = false then 
					new_line;
					put_line("ABORTED ! BOM NOT COMPLETED !");
					set_output(standard_output);
					new_line;
					put_line("ERROR : Part with this part code not found in " & to_string(stock_db_csv) & ":");
					new_line;
					--put_line("value           : " & to_string(project_part(id_master).value));
					--put_line("package         : " & to_string(project_part(id_master).packge));
					put_line("part_code_" & facility_names(1) & "   : " & to_string(project_part(id_master).part_code_fac));
					put("parts affected  : ");

					-- the list of parts affected needs to be reduced to max. 5 positions
					if sm_csv.get_field_count(parts_of_same_value,',') > 5 then 
						for psv in 1..5
						loop
							put(sm_csv.get_field(parts_of_same_value,psv,','));
							put(",");
						end loop;
						put(" ...");
					--end if; -- rm v004
					-- ins v004 begin
					else -- otherwise put affected parts one by one
						for psv in 1..sm_csv.get_field_count(parts_of_same_value,',') -- process as many items as affected
						loop
							put(sm_csv.get_field(parts_of_same_value,psv,',')); -- put item
							if psv < sm_csv.get_field_count(parts_of_same_value,',') then -- do not put a comma after last item
								put(",");
							end if;
						end loop;
					end if;
					-- ins v004 end

					--put_line("parts affected  : " & to_string(parts_of_same_value));
					new_line;

					raise constraint_error;
				end if;

				ct := id_master; -- restore member pointer from master_id
			end if;

		end loop;

		-- write facility bom summary
		--for i in 1..columns_of_facility_bom -- rm v010
		for i in 1..columns_of_facility_bom + manufacturer_count_max -- ins v010
		loop
			put_field(text => sm_csv.row_separator_1);
		end loop;
		put_lf;

		for i in 1..5
		loop
			put_field;
		end loop;

		-- ins v010 begin
		-- insert as many empty fields as manufactuer_count_max
		for m in 1..manufacturer_count_max
		loop
			put_field;
		end loop;
		-- ins v010 end

		put_field(text => money_positive'image(price_total_min));
		put_field(text => money_positive'image(price_total_max)); 
		put_lf;
		put_field(text => "END OF BOM");
		-- summary done

		set_output(standard_output);
		new_line; put_line("BOM ready"); new_line;

		end convert_eagle_bom_to_facility_bom;


		--read stock data base in part_stock_array
		function read_stock_data_base
			return boolean is
			previous_input			: ada.text_io.file_type renames current_input;
			part_section_entered 	: boolean := false;
			part_pointer : natural 	:= 0;
		begin
			prog_position := "RS001";
			Open( 
				File => stock_db_file,
				Mode => in_file,
				Name => to_string(stock_db_csv)
				);
			set_input(stock_db_file);

			-- this loop read the stock_db file in array part_stock_array
			while not end_of_file
				loop
					line:=get_line;
						--if sm_csv.get_field_count(line) > 0 then -- line must not be empty

							--clear part_section_entered flag upon passing the end of stock mark
							if sm_csv.get_field(line,1) = "END OF STOCK" then	
								part_section_entered := false;
							end if;

							if part_section_entered then -- if part section entered
								part_pointer := part_pointer + 1;
								prog_position := "RS005";
								--put_line("part_pointer: " & natural'image(part_pointer));
								line_to_record(to_string(line));
								for field_pointer in 1..field_ct_per_stock_db_file_line -- process given fields per line
								loop
									prog_position := "RS010";
									--put_line(natural'image(field_pointer));
 									case field_pointer is
										when 1 => 	prog_position := "RSA10";
													part_stock_array(part_pointer).part_id := line_of_stock_db_file.part_id;
										when 2 => part_stock_array(part_pointer).part_code_fac := line_of_stock_db_file.part_code_fac;
										when 3 => part_stock_array(part_pointer).date_edited := line_of_stock_db_file.date_edited;
										when 4 => part_stock_array(part_pointer).qty_on_stock := line_of_stock_db_file.qty_on_stock;
										when 5 => part_stock_array(part_pointer).qty_reserved := line_of_stock_db_file.qty_reserved;
										when 6 => part_stock_array(part_pointer).qty_available := line_of_stock_db_file.qty_available;

 										when 7 => for m in 1..manufacturer_count_max 
											loop
												prog_position := "RS020";
												part_stock_array(part_pointer).manufacturers(m).name := line_of_stock_db_file.manufacturers(m).name;
												prog_position := "RS021";
												part_stock_array(part_pointer).manufacturers(m).part_code := line_of_stock_db_file.manufacturers(m).part_code;

												-- if date_edited n/a, default to default_date. otherwise copy date_edited as it is
												prog_position := "RS023";
												part_stock_array(part_pointer).manufacturers(m).date_edited := line_of_stock_db_file.manufacturers(m).date_edited;

												-- if status_production n/a, default to "unknown". otherwise copy status_production as it is
												prog_position := "RS024";
												part_stock_array(part_pointer).manufacturers(m).status_production := line_of_stock_db_file.manufacturers(m).status_production;

												part_stock_array(part_pointer).manufacturers(m).url_datasheet_1 := line_of_stock_db_file.manufacturers(m).url_datasheet_1;
												part_stock_array(part_pointer).manufacturers(m).url_datasheet_2 := line_of_stock_db_file.manufacturers(m).url_datasheet_2;
											end loop;

										when 25 => for d in 1..distributor_count_max 
											loop -- column Y
												prog_position := "RS030";
												part_stock_array(part_pointer).distributors(d).name := line_of_stock_db_file.distributors(d).name;
												part_stock_array(part_pointer).distributors(d).order_code := line_of_stock_db_file.distributors(d).order_code;
												part_stock_array(part_pointer).distributors(d).date_edited := line_of_stock_db_file.distributors(d).date_edited;
												part_stock_array(part_pointer).distributors(d).qty_min := line_of_stock_db_file.distributors(d).qty_min;
												part_stock_array(part_pointer).distributors(d).price_net := line_of_stock_db_file.distributors(d).price_net;
											end loop;

										when 55 =>	part_stock_array(part_pointer).storage_place := line_of_stock_db_file.storage_place;
										when 56 =>	part_stock_array(part_pointer).remarks := line_of_stock_db_file.remarks;
										when 57 =>	part_stock_array(part_pointer).project := line_of_stock_db_file.project;
											
										prog_position := "RS090";
										when others => prog_position := "RS011";
									end case;
									prog_position := "RS012";
								end loop;
							end if; -- if part section entered
							prog_position := "RS015";

							-- set part_section_entered flag upon passing the PART_ID line
							if sm_csv.get_field(line,1) = "PART_ID" then	
								part_section_entered := true;
							end if;

						--end if; -- line must not be empty
				end loop;
				prog_position := "RS095";
				close(stock_db_file);
				set_input(previous_input);
			return true;
		end read_stock_data_base;


		function update_stock_data_base 
			return boolean is
			previous_output	: ada.text_io.file_type renames current_output;
		begin
			prog_position := "UD000";
			-- backup stock_db
			--put_line(standard_output,make_filename_by_date(simple_name(to_string(stock_db_csv))));
			copy_file( 
				to_string(stock_db_csv),
				to_string(directory_of_backup) & "/" & 
					make_filename_by_date(prefix => "BAK_", file_name => simple_name(to_string(stock_db_csv)), date_now => now)
				);

			prog_position := "UD001";
			-- create a new stock_db
			Create( stock_db_file, Name => to_string(stock_db_csv)); Close(stock_db_file);
			Open( 
				file => stock_db_file,
				Mode => out_File,
				name => to_string(stock_db_csv)
				);
			set_output(stock_db_file);
			write_stock_db_header;
			for i in 1..part_stock_array'last
			loop
				put_field(text => natural'image(part_stock_array(i).part_id)); 
				put_field(text => to_string(part_stock_array(i).part_code_fac));
				put_field(text => part_stock_array(i).date_edited);
				put_field(text => natural'image(part_stock_array(i).qty_on_stock));    
				put_field(text => natural'image(part_stock_array(i).qty_reserved));    
				put_field(text => natural'image(part_stock_array(i).qty_available));
				for m in 1..manufacturer_count_max
				loop
					put_field(text => to_string(part_stock_array(i).manufacturers(m).name));
					put_field(text => to_string(part_stock_array(i).manufacturers(m).part_code));
					put_field(text => part_stock_array(i).manufacturers(m).date_edited);
					put_field(text => status_production_type'image(part_stock_array(i).manufacturers(m).status_production));
					put_field(text => to_string(part_stock_array(i).manufacturers(m).url_datasheet_1));
					put_field(text => to_string(part_stock_array(i).manufacturers(m).url_datasheet_2));
				end loop;
				for d in 1..distributor_count_max
				loop
					put_field(text => to_string(part_stock_array(i).distributors(d).name));
					put_field(text => to_string(part_stock_array(i).distributors(d).order_code));
					put_field(text => part_stock_array(i).distributors(d).date_edited);
					put_field(text => natural'image(part_stock_array(i).distributors(d).qty_min));
					put_field(text => money_positive'image(part_stock_array(i).distributors(d).price_net));
				end loop;
				put_field(text => to_string(part_stock_array(i).storage_place));
				put_field(text => to_string(part_stock_array(i).remarks));
				put_field(text => to_string(part_stock_array(i).project));
				put_lf;
			end loop;
			put_field(text => "END OF STOCK");
			close (stock_db_file);
			set_output(previous_output);
			return true;
		end update_stock_data_base;


		procedure show_part_properties
			( i	: natural) is
		begin
			new_line;
			put_line(row_separator);
			put_line("part id         :" & natural'image(part_stock_array(i).part_id));
			put_line("part code       : " & to_string(part_stock_array(i).part_code_fac));
			put_line("date edited     : " & part_stock_array(i).date_edited);
			put_line("qty on stock    :" & natural'image(part_stock_array(i).qty_on_stock));
			put_line("qty reserved    :" & natural'image(part_stock_array(i).qty_reserved));
			put_line("qty available   :" & natural'image(part_stock_array(i).qty_available));
			put_line("storage place   : " & to_string(part_stock_array(i).storage_place));
			put_line("project         : " & to_string(part_stock_array(i).project));
			put_line("remarks         : " & to_string(part_stock_array(i).remarks));

			if show_mode = 0 then
				for m in 1..manufacturer_count_max loop
					if to_string(part_stock_array(i).manufacturers(m).name) /= not_assigned_mark then
						put_line(row_separator);
						put_line("manufacturer" & natural'image(m) & "  : " & to_string(part_stock_array(i).manufacturers(m).name));
						put_line("date edited     : " & part_stock_array(i).manufacturers(m).date_edited);
						put_line("part code       : " & to_string(part_stock_array(i).manufacturers(m).part_code));
						put_line("url datasheet 1 : " & to_string(part_stock_array(i).manufacturers(m).url_datasheet_1));
						put_line("url datasheet 2 : " & to_string(part_stock_array(i).manufacturers(m).url_datasheet_2));
						put_line("status prod.    : " & status_production_type'image(part_stock_array(i).manufacturers(m).status_production));
					end if;
				end loop;

				for d in 1..distributor_count_max loop
					if to_string(part_stock_array(i).distributors(d).name) /= not_assigned_mark then
						put_line(row_separator);
						put_line("distributor" & natural'image(d) & "   : " & to_string(part_stock_array(i).distributors(d).name));
						put_line("order code      : " & to_string(part_stock_array(i).distributors(d).order_code));
						put_line("date edited     : " & part_stock_array(i).distributors(d).date_edited);
						put_line("qty min.        :" & natural'image(part_stock_array(i).distributors(d).qty_min));
						put_line("price net       :" & money_positive'image(part_stock_array(i).distributors(d).price_net));
					end if;
				end loop;
			end if;
			put_line(row_separator);
		end show_part_properties;


	begin -- manage_stock
		prog_position := "MS100";
		if stock_operation = show_by_id then
			set_output(standard_output);
			if read_stock_data_base then null;
			end if;
			prog_position := "MS110";
			for i in 1..part_count_max2 loop
				if part_id_given = part_stock_array(i).part_id then
					part_occured_on_stock := part_occured_on_stock + 1;
					show_part_properties(i);
					--exit;
				end if;
			end loop;
			if part_occured_on_stock > 1 then print_warning_on_multiple_occurences(part_occured_on_stock); end if;
			if part_occured_on_stock = 0 then print_error_on_unknown_id; end if;
		end if;

		prog_position := "MS200";
		if stock_operation = show_by_fac_code then
			set_output(standard_output);
			if read_stock_data_base then null;
			end if;
			prog_position := "MS210";
			for i in 1..part_count_max2 loop
				--if part_code_given = part_stock_array(i).part_code_fac then
				--if ada.strings.fixed.count(to_string(part_stock_array(i).part_code_fac), to_string(part_code_given)) > 0 then -- rm v013
				if search_pattern_in_text(to_string(part_stock_array(i).part_code_fac), to_string(part_code_given)) then -- ins v013
					prog_position := "MS220";
					part_occured_on_stock := part_occured_on_stock + 1;
					show_part_properties(i);
					--exit;
				end if;
			end loop;
			prog_position := "MS230";
			if part_occured_on_stock > 1 then print_number_of_occurences(part_occured_on_stock); end if;
			if part_occured_on_stock = 0 then print_error_on_unknown_part; end if;
		end if;

		-- ins v009 begin
		prog_position := "MS900";
		if stock_operation = show_by_order_code then
			set_output(standard_output);
			if read_stock_data_base then null;
			end if;
			prog_position := "MS910";
			--distributor_order_code_given := to_bounded_string(argument(arg_pt+1)); -- rm v013
			for i in 1..part_count_max2 loop
				for d in 1..distributor_count_max loop
					--if ada.strings.fixed.count(to_string(part_stock_array(i).distributors(d).order_code), to_string(distributor_order_code_given)) > 0 then -- rm v013
					if search_pattern_in_text(to_string(part_stock_array(i).distributors(d).order_code), to_string(property_string_given)) then -- ins v013
					--if search_pattern_in_text(to_string(part_stock_array(i).distributors(d).order_code), "*90909") then -- ins v013
						prog_position := "MS920";
						part_occured_on_stock := part_occured_on_stock + 1;
						show_part_properties(i);
						--exit;
					end if;
				end loop;
			end loop;
			prog_position := "MS930";
			if part_occured_on_stock > 1 then print_number_of_occurences(part_occured_on_stock); end if;
			if part_occured_on_stock = 0 then print_error_on_unknown_part; end if;
		end if;
		-- ins v009 end 

		-- ins v013 begin
		prog_position := "MS90A";
		if stock_operation = show_by_manu_code then
			set_output(standard_output);
			if read_stock_data_base then null;
			end if;
			prog_position := "MS92A";
			--manufacturer_part_code_given := to_bounded_string(argument(arg_pt+1));
			for i in 1..part_count_max2 loop
				for m in 1..manufacturer_count_max loop
					--if search_pattern_in_text(to_string(part_stock_array(i).manufacturers(m).part_code), to_string(manufacturer_part_code_given)) then -- rm v013
					if search_pattern_in_text(to_string(part_stock_array(i).manufacturers(m).part_code), to_string(property_string_given)) then -- ins v013
						prog_position := "MS94A";
						part_occured_on_stock := part_occured_on_stock + 1;
						show_part_properties(i);
						--exit;
					end if;
				end loop;
			end loop;
			prog_position := "MS96A";
			if part_occured_on_stock > 1 then print_number_of_occurences(part_occured_on_stock); end if;
			if part_occured_on_stock = 0 then print_error_on_unknown_part; end if;
		end if;
		-- ins v013 end

		prog_position := "MS300";
		if stock_operation = edit then
			if read_stock_data_base then null;
			end if;
			prog_position := "MS310";
			--put_line("part id to edit: " & natural'image(part_id_given));
			--put_line("part ct max    : " & natural'image(part_count_max));
			loop_through_part_stock_array:
			for i in 1..part_count_max2 loop
				--put_line("part id: " & natural'image(i));
				if part_id_given = part_stock_array(i).part_id then
					part_on_stock := true;
					--put_line("part found");
					if part_property = part_code_fac then
						prog_position := "MS315";
						put_line("part code old   : " & to_string(part_stock_array(i).part_code_fac));
						prog_position := "MS317";
						--part_code_fac_given2 := to_bounded_string(argument(arg_pt+3)); -- rm v013
						--put_line("part code new   : " & to_string(part_code_fac_given2)); -- rm v013
						put_line("part code new   : " & to_string(property_string_given)); -- ins v013
						prog_position := "MS319";
						--if parse_part_code(to_string(part_code_fac_given2),1) = false then -- rm v013
						if parse_part_code(to_string(property_string_given),1) = false then -- ins v013
							raise constraint_error;
						end if;
		
						-- ins v013 begin
						-- make sure the given part code does not exist already
						prog_position := "MS31B";
						for k in 1..part_count_max2 loop
							if part_stock_array(k).part_code_fac = property_string_given then
								new_line;
								put_line("ERROR : A part with the given part code already exists !");
								put_line("        ID of this part is :" & natural'image(part_stock_array(k).part_id));
								raise constraint_error;
							end if;
						end loop;
						-- ins v013 end

						if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
							raise constraint_error;
						end if;
						--part_stock_array(i).part_code_fac := part_code_fac_given2; -- rm v013
						part_stock_array(i).part_code_fac := property_string_given; -- ins v013
						--part_stock_array(i).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
						part_stock_array(i).date_edited := date_now;
						exit;
					end if;

					if part_property = qty_delta_stock then
						prog_position := "MS320";
						--qty_on_stock_delta_given := integer'value(argument(arg_pt+3)); -- rm v013
						qty_on_stock_delta_scratch := integer'value(to_string(property_string_given)); -- ins v013
						put_line("part code " & facility_names(1) & "   : " & to_string(part_stock_array(i).part_code_fac));
						put_line("storage place   : " & to_string(part_stock_array(i).storage_place)); -- ins v009
						put_line("qty reserved    :" & natural'image(part_stock_array(i).qty_reserved));
						put_line("qty on stock old:" & natural'image(part_stock_array(i).qty_on_stock));
						--if qty_on_stock_delta_given < 0 then -- rm v013
						if qty_on_stock_delta_scratch < 0 then -- ins v013
							--put_line("qty stock delta : " & trim(integer'image(qty_on_stock_delta_given),left)); -- rm v013
							put_line("qty stock delta : " & trim(integer'image(qty_on_stock_delta_scratch),left)); -- ins v013
						else
							--put_line("qty stock delta : +" & trim(integer'image(qty_on_stock_delta_given),left)); -- rm v013
							put_line("qty stock delta : +" & trim(integer'image(qty_on_stock_delta_scratch),left)); -- ins v013
						end if;
						prog_position := "MS322";
						--qty_on_stock_tmp := part_stock_array(i).qty_on_stock + qty_on_stock_delta_given; -- rm v013
						qty_on_stock_tmp := part_stock_array(i).qty_on_stock + qty_on_stock_delta_scratch; -- ins v013
						put_line("qty on stock new:" & natural'image(qty_on_stock_tmp));

						prog_position := "MS321"; -- here we test whether more parts are taken from stock than reserved
						qty_scratch := qty_on_stock_tmp - part_stock_array(i).qty_reserved;

						prog_position := "MS323";
						if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
							raise constraint_error;
						end if;
						part_stock_array(i).qty_on_stock := qty_on_stock_tmp;
						prog_position := "MS324";
						part_stock_array(i).qty_available := part_stock_array(i).qty_on_stock - part_stock_array(i).qty_reserved;
						--part_stock_array(i).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
						part_stock_array(i).date_edited := date_now;
						exit;
					end if;

					if part_property = qty_delta_reserved then
						prog_position := "MS330";
						--qty_reserved_given := integer'value(argument(arg_pt+3)); -- rm v013
						qty_reserved_scratch := integer'value(to_string(property_string_given)); -- ins v013
						put_line("part code " & facility_names(1) & "   : " & to_string(part_stock_array(i).part_code_fac));
						put_line("storage place   : " & to_string(part_stock_array(i).storage_place)); -- ins v009
						put_line("qty on stock    :" & natural'image(part_stock_array(i).qty_on_stock));
						put_line("qty reserved old:" & natural'image(part_stock_array(i).qty_reserved));
						--if qty_reserved_given < 0 then -- rm v013
						if qty_reserved_scratch < 0 then -- ins v013
							--put_line("qty rsvd. delta : " & trim(integer'image(qty_reserved_given),left)); -- rm v013
							put_line("qty rsvd. delta : " & trim(integer'image(qty_reserved_scratch),left)); -- ins v013
						else
							--put_line("qty rsvd. delta : +" & trim(integer'image(qty_reserved_given),left)); -- rm v013
							put_line("qty rsvd. delta : +" & trim(integer'image(qty_reserved_scratch),left)); -- ins v013
						end if;
						prog_position := "MS332";
						--if part_stock_array(i).qty_reserved + qty_reserved_given < 0 then  -- if qty_reserved already zero -- rm v013
						if part_stock_array(i).qty_reserved + qty_reserved_scratch < 0 then  -- if qty_reserved already zero -- ins v013
							part_stock_array(i).qty_reserved := 0; -- set the lower limit of reservations to zero
							new_line;
							--put_line("INFO : Number of reservations can not be less than zero.");
							put_line("INFO : Number of reservations is down to zero.");
							new_line;
						else	-- otherwise compute qty_reserved by
							--qty_reserved_tmp := part_stock_array(i).qty_reserved + qty_reserved_given; -- rm v013
							qty_reserved_tmp := part_stock_array(i).qty_reserved + qty_reserved_scratch; -- ins v013
						end if;
						put_line("qty reserved new:" & natural'image(qty_reserved_tmp));
						prog_position := "MS334";
						-- ins v013 begin
						if part_stock_array(i).qty_on_stock < qty_reserved_tmp then
							new_line;
							put_line("ERROR : You can not reserve more items than available on stock !");
							raise constraint_error;
						end if;
						-- ins v013 end
						if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
							raise constraint_error;
						end if;
						prog_position := "MS335";
						part_stock_array(i).qty_reserved := qty_reserved_tmp;
						part_stock_array(i).qty_available := part_stock_array(i).qty_on_stock - qty_reserved_tmp;
						part_stock_array(i).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
						exit;
					end if;

					for m in 1..manufacturer_count_max
					loop
						prog_position := "MS34X";
						if part_property = part_property_type'value("manufacturer_" & trim(natural'image(m),left) & "_name") then
							prog_position := "MS340";
							--manufacturer_name_given := to_bounded_string(argument(arg_pt+3)); -- rm v013
							--manufacturer_name_given := property_string_given; -- ins v013
							put_line("part code " & facility_names(1) & "   : " & to_string(part_stock_array(i).part_code_fac));
							put_line("manuf. name old : " & to_string(part_stock_array(i).manufacturers(m).name));
							--put_line("manuf. name new : " & to_string(manufacturer_name_given)); -- rm v013
							put_line("manuf. name new : " & to_string(property_string_given)); -- ins v013
							prog_position := "MS34H";
							if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
								raise constraint_error;
							end if;
							prog_position := "MS34A";
							--part_stock_array(i).manufacturers(m).name := manufacturer_name_given; -- rm v013
							part_stock_array(i).manufacturers(m).name := property_string_given; -- ins v013
							-- ins v013 begin
							-- If a manufacturer is deleted ( by assigning "n/a"), all order information is to be cleared
							if property_string_given = to_bounded_string(not_assigned_mark) then
								part_stock_array(i).manufacturers(m).part_code := to_bounded_string(not_assigned_mark);
								part_stock_array(i).manufacturers(m).url_datasheet_1 := to_unbounded_string(not_assigned_mark);
								part_stock_array(i).manufacturers(m).url_datasheet_2 := to_unbounded_string(not_assigned_mark);
								part_stock_array(i).manufacturers(m).status_production := unknown;
							end if;
							-- ins v013 end
							part_stock_array(i).manufacturers(m).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
							exit loop_through_part_stock_array;
						end if;

						if part_property = part_property_type'value("manufacturer_" & trim(natural'image(m),left) & "_part_code") then
							-- ensure a part code can be assigned only, if the particular manufacturer name has been assigned
							if to_string(part_stock_array(i).manufacturers(m).name) = not_assigned_mark then
								print_error_on_non_defined_manufacturer(m);
							else
								prog_position := "MS342";
								--manufacturer_part_code_given := to_bounded_string(argument(arg_pt+3)); -- rm v013
								--manufacturer_part_code_given := property_string_given; -- ins v013

								-- ins v013 begin
								-- make sure the given manufacturer part code does not exist already
								prog_position := "MS34M";
								for k in 1..part_count_max2 loop
									for l in 1..manufacturer_count_max loop
										if part_stock_array(k).manufacturers(l).part_code = property_string_given then
											new_line;
											put_line("WARNING : The given manufacturer part code is already ");
											put_line("          used by part with ID :" & natural'image(part_stock_array(k).part_id));
											--raise constraint_error;
											if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
												raise constraint_error;
											end if;
										end if;
									end loop;
								end loop;
								-- ins v013 end

								put_line("part code " & facility_names(1) & "   : " & to_string(part_stock_array(i).part_code_fac));
								put_line("manuf. name     : " & to_string(part_stock_array(i).manufacturers(m).name));
								put_line("part code old   : " & to_string(part_stock_array(i).manufacturers(m).part_code));
								--put_line("part code new   : " & to_string(manufacturer_part_code_given)); -- rm v013
								put_line("part code new   : " & to_string(property_string_given)); -- ins v013
								prog_position := "MS34B";
								if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
									raise constraint_error;
								end if;
								--part_stock_array(i).manufacturers(m).part_code := manufacturer_part_code_given; -- rm v013
								part_stock_array(i).manufacturers(m).part_code := property_string_given; -- ins v013
								--part_stock_array(i).manufacturers(m).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
								part_stock_array(i).manufacturers(m).date_edited := date_now;
							end if;
							exit loop_through_part_stock_array;
						end if;

						if part_property = part_property_type'value("manufacturer_" & trim(natural'image(m),left) & "_status_production") then
							-- ensure a production status can be assigned only, if the particular manufacturer name has been assigned
							if to_string(part_stock_array(i).manufacturers(m).name) = not_assigned_mark then
								print_error_on_non_defined_manufacturer(m);
							else
								prog_position := "MS343";
								--manufacturer_status_production_given := status_production_type'value(argument(arg_pt+3)); -- rm v013
								manufacturer_status_production_scratch := status_production_type'value(to_string(property_string_given)); -- in v013
								put_line("part code " & facility_names(1) & "   : " & to_string(part_stock_array(i).part_code_fac));
								put_line("manuf. name     : " & to_string(part_stock_array(i).manufacturers(m).name));
								put_line("sts. prod. old  : " & status_production_type'image(part_stock_array(i).manufacturers(m).status_production));
								put_line("sts. prod. new  : " & status_production_type'image(manufacturer_status_production_scratch));
								prog_position := "MS34C";
								if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
									raise constraint_error;
								end if;
								part_stock_array(i).manufacturers(m).status_production := manufacturer_status_production_scratch;
								--part_stock_array(i).manufacturers(m).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
								part_stock_array(i).manufacturers(m).date_edited := date_now;
							end if;
							exit loop_through_part_stock_array;
						end if;

						if part_property = part_property_type'value("manufacturer_" & trim(natural'image(m),left) & "_datasheet_1") then
							-- ensure the datasheet can be assigned only, if the particular manufacturer name has been assigned
							if to_string(part_stock_array(i).manufacturers(m).name) = not_assigned_mark then
								print_error_on_non_defined_manufacturer(m);
							else
								prog_position := "MS344";
								--manufacturer_datasheet_given := to_unbounded_string(argument(arg_pt+3)); -- rm v013
								--manufacturer_datasheet_scratch := property_string_given)); -- ins v013
								put_line("part code " & facility_names(1) & "   : " & to_string(part_stock_array(i).part_code_fac));
								put_line("manuf. name     : " & to_string(part_stock_array(i).manufacturers(m).name));
								put_line("datasheet 1 old : " & to_string(part_stock_array(i).manufacturers(m).url_datasheet_1));
								--put_line("datasheet 1 new : " & to_string(manufacturer_datasheet_given)); -- rm v013
								put_line("datasheet 1 new : " & to_string(property_string_given)); -- ins v013
								prog_position := "MS34D";
								if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
									raise constraint_error;
								end if;
								--part_stock_array(i).manufacturers(m).url_datasheet_1 := manufacturer_datasheet_given; -- rm v013
								part_stock_array(i).manufacturers(m).url_datasheet_1 := to_unbounded_string(to_string(property_string_given)); -- ins v013
								--part_stock_array(i).manufacturers(m).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
								part_stock_array(i).manufacturers(m).date_edited := date_now;
							end if;
							exit loop_through_part_stock_array;
						end if;

						if part_property = part_property_type'value("manufacturer_" & trim(natural'image(m),left) & "_datasheet_2") then
							-- ensure the datasheet can be assigned only, if the particular manufacturer name has been assigned
							if to_string(part_stock_array(i).manufacturers(m).name) = not_assigned_mark then
								print_error_on_non_defined_manufacturer(m);
							else
								prog_position := "MS345";
								--manufacturer_datasheet_given := to_unbounded_string(argument(arg_pt+3));
								--manufacturer_datasheet_scratch := to_unbounded_string(property_string_given); -- ins v013
								put_line("part code " & facility_names(1) & "   : " & to_string(part_stock_array(i).part_code_fac));
								put_line("manuf. name     : " & to_string(part_stock_array(i).manufacturers(m).name));
								put_line("datasheet 2 old : " & to_string(part_stock_array(i).manufacturers(m).url_datasheet_2));
								--put_line("datasheet 2 new : " & to_string(manufacturer_datasheet_given)); -- rm v013
								put_line("datasheet 2 new : " & to_string(property_string_given)); -- ins v013
								prog_position := "MS34E";
								if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
									raise constraint_error;
								end if;
								--part_stock_array(i).manufacturers(m).url_datasheet_2 := manufacturer_datasheet_given; -- rm v013
								part_stock_array(i).manufacturers(m).url_datasheet_2 := to_unbounded_string(to_string(property_string_given)); -- ins v013
								--part_stock_array(i).manufacturers(m).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
								part_stock_array(i).manufacturers(m).date_edited := date_now;
							end if;
							exit loop_through_part_stock_array;
						end if;
					end loop;

					for d in 1..distributor_count_max
					loop
						if part_property = part_property_type'value("distributor_" & trim(natural'image(d),left) & "_name") then
							prog_position := "MS346";
							--distributor_name_given := to_bounded_string(argument(arg_pt+3)); -- rm v013
							put_line("part code " & facility_names(1) & "   : " & to_string(part_stock_array(i).part_code_fac));
							put_line("dist. name old  : " & to_string(part_stock_array(i).distributors(d).name));
							--put_line("dist. name new  : " & to_string(distributor_name_given)); -- rm v013
							put_line("dist. name new  : " & to_string(property_string_given)); -- ins v013
							prog_position := "MS34F";
							if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
								raise constraint_error;
							end if;
							--part_stock_array(i).distributors(d).name := distributor_name_given; -- rm v013
							part_stock_array(i).distributors(d).name := property_string_given; -- ins v013

							-- ins v013 begin
							-- If a distributor is deleted ( by assigning "n/a"), all order information is to be cleared
							--if distributor_name_given = to_bounded_string(not_assigned_mark) then
							if property_string_given = to_bounded_string(not_assigned_mark) then
								part_stock_array(i).distributors(d).order_code := to_bounded_string(not_assigned_mark);
								part_stock_array(i).distributors(d).qty_min := 1;
								part_stock_array(i).distributors(d).price_net := 0.00;
							end if;
							-- ins v013 end
							--part_stock_array(i).distributors(d).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
							part_stock_array(i).distributors(d).date_edited := date_now;
							exit loop_through_part_stock_array;
						end if;

						--if part_property = distributor_x_order_code then
						if part_property = part_property_type'value("distributor_" & trim(natural'image(d),left) & "_order_code") then
							prog_position := "MS347";
							-- ensure the order code can be assigned only, if the particular distributor name has been assigned
							if to_string(part_stock_array(i).distributors(d).name) = not_assigned_mark then
								print_error_on_non_defined_distributor(d);
							else
								-- ins v013 begin
								-- make sure the given distributor order code does not exist already
								prog_position := "MS34N";
								for k in 1..part_count_max2 loop
									for l in 1..distributor_count_max loop
										if part_stock_array(k).distributors(l).order_code = property_string_given then
											new_line;
											put_line("WARNING : The given distributor order code is already ");
											put_line("          used by part with ID :" & natural'image(part_stock_array(k).part_id));
											if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
												raise constraint_error;
											end if;
										end if;
									end loop;
								end loop;
								-- ins v013 end

								--distributor_order_code_given := to_bounded_string(argument(arg_pt+3)); -- rm v013
								put_line("part code " & facility_names(1) & "   : " & to_string(part_stock_array(i).part_code_fac));
								put_line("distributor name: " & to_string(part_stock_array(i).distributors(d).name));
								put_line("order code old  : " & to_string(part_stock_array(i).distributors(d).order_code));
								--put_line("order code new  : " & to_string(distributor_order_code_given)); -- rm v013
								put_line("order code new  : " & to_string(property_string_given)); -- ins v013
								prog_position := "MS34G";
								if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
									raise constraint_error;
								end if;
								--part_stock_array(i).distributors(d).order_code := distributor_order_code_given; -- rm v013
								part_stock_array(i).distributors(d).order_code := property_string_given; -- ins v013
								--part_stock_array(i).distributors(d).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
								part_stock_array(i).distributors(d).date_edited := date_now;
							end if;
							exit loop_through_part_stock_array;
						end if;

						--if part_property = distributor_x_qty_min then
						if part_property = part_property_type'value("distributor_" & trim(natural'image(d),left) & "_qty_min") then
							prog_position := "MS348";
							-- ensure the qty min can be assigned only, if the particular distributor name has been assigned
							if to_string(part_stock_array(i).distributors(d).name) = not_assigned_mark then
								print_error_on_non_defined_distributor(d);
							else
								--distributor_qty_min_given := natural'value(argument(arg_pt+3)); -- rm v013
								distributor_qty_min_scratch := natural'value(to_string(property_string_given)); -- ins v013
								put_line("part code " & facility_names(1) & "   : " & to_string(part_stock_array(i).part_code_fac));
								put_line("distributor name: " & to_string(part_stock_array(i).distributors(d).name));
								put_line("dist. order code: " & to_string(part_stock_array(i).distributors(d).order_code));
								put_line("qty min old     :" & natural'image(part_stock_array(i).distributors(d).qty_min));
								--put_line("qty min new     :" & natural'image(distributor_qty_min_given)); -- rm v013
								put_line("qty min new     :" & natural'image(distributor_qty_min_scratch)); -- ins v013
								prog_position := "MS34H";
								if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
									raise constraint_error;
								end if;
								--part_stock_array(i).distributors(d).qty_min := distributor_qty_min_given; -- rm v013
								part_stock_array(i).distributors(d).qty_min := distributor_qty_min_scratch; -- ins v013
								--part_stock_array(i).distributors(d).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
								part_stock_array(i).distributors(d).date_edited := date_now;
							end if;
							exit loop_through_part_stock_array;
						end if;

						--if part_property = distributor_x_price_net then
						if part_property = part_property_type'value("distributor_" & trim(natural'image(d),left) & "_price_net") then
							prog_position := "MS349";
							-- ensure the order code can be assigned only, if the particular distributor name has been assigned
							if to_string(part_stock_array(i).distributors(d).name) = not_assigned_mark then
								print_error_on_non_defined_distributor(d);
							else
								-- distributor_price_net_given := money_positive'value(argument(arg_pt+3)); -- rm v013
								distributor_price_net_scratch := money_positive'value(to_string(property_string_given)); -- ins v013
								put_line("part code " & facility_names(1) & "   : " & to_string(part_stock_array(i).part_code_fac));
								put_line("distributor name: " & to_string(part_stock_array(i).distributors(d).name));
								put_line("price net old   :" & money_positive'image(part_stock_array(i).distributors(d).price_net));
								--put_line("price net new   :" & money_positive'image(distributor_price_net_given)); -- rm v013
								put_line("price net new   :" & money_positive'image(distributor_price_net_scratch)); -- ins v013
								prog_position := "MS34I";
								if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
									raise constraint_error;
								end if;
								--part_stock_array(i).distributors(d).price_net := distributor_price_net_given; -- rm v013
								part_stock_array(i).distributors(d).price_net := distributor_price_net_scratch; -- rm v013
								--part_stock_array(i).distributors(d).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
								part_stock_array(i).distributors(d).date_edited := date_now;
							end if;
							exit loop_through_part_stock_array;
						end if;
					end loop;

					if part_property = storage_place then
						prog_position := "MS360";
						--storage_place_given := to_bounded_string(argument(arg_pt+3)); -- rm v013
						put_line("part code " & facility_names(1) & "   : " & to_string(part_stock_array(i).part_code_fac));
						put_line("place old       : " & to_string(part_stock_array(i).storage_place));
						--put_line("place new       : " & to_string(storage_place_given)); -- rm v013
						put_line("place new       : " & to_string(property_string_given)); -- ins v013
						prog_position := "MS365";
						if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
							raise constraint_error;
						end if;
						--part_stock_array(i).storage_place := storage_place_given; -- rm v013
						part_stock_array(i).storage_place := property_string_given; -- ins v013
						--part_stock_array(i).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
						part_stock_array(i).date_edited := date_now;
						exit;
					end if;

					if part_property = remarks then
						prog_position := "MS370";
						--remarks_given := to_bounded_string(argument(arg_pt+3)); -- rm v013
						put_line("part code " & facility_names(1) & "   : " & to_string(part_stock_array(i).part_code_fac));
						put_line("remarks old     : " & to_string(part_stock_array(i).remarks));
						--put_line("remarks new     : " & to_string(remarks_given)); -- rm v013
						put_line("remarks new     : " & to_string(property_string_given)); -- ins v013
						prog_position := "MS375";
						if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
							raise constraint_error;
						end if;
						--part_stock_array(i).remarks := remarks_given; -- rm v013
						part_stock_array(i).remarks := property_string_given; -- ins v013
						--part_stock_array(i).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
						part_stock_array(i).date_edited := date_now; 
						exit;
					end if;

					if part_property = project then
						prog_position := "MS380";
						put_line("part code " & facility_names(1) & "   : " & to_string(part_stock_array(i).part_code_fac));
						put_line("project old     : " & to_string(part_stock_array(i).project));
						--project_given := to_bounded_string(argument(arg_pt+3)); -- rm v013
						--put_line("project new     : " & to_string(project_given)); -- rm v013
						put_line("project new     : " & to_string(property_string_given)); -- ins v013
						prog_position := "MS385";
						if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
							raise constraint_error;
						end if;
						--part_stock_array(i).project := project_given; -- rm v013
						part_stock_array(i).project := property_string_given; -- ins v013
						--part_stock_array(i).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
						part_stock_array(i).date_edited := date_now;
						exit;
					end if;
				end if;
			end loop loop_through_part_stock_array;

			-- if loop finished without a positive match
			prog_position := "MS311";
			if not part_on_stock then 
				print_error_on_unknown_part;
			else -- if part found
				if update_stock_data_base then null;
				end if;
			end if;
			return true; -- return true if edit was successful
		end if;

		prog_position := "MS400";
		if stock_operation = add then
			if read_stock_data_base then null;
			end if;
			prog_position := "MS410";
			-- make sure the given part code does not exist already
			for i in 1..part_count_max2 loop
				if part_stock_array(i).part_code_fac = to_bounded_string(part_code_fac_given) then
					new_line;
					put_line("ERROR : A part with the given part code already exists !");
					put_line("        ID of this part is :" & natural'image(part_stock_array(i).part_id)); -- ins v005
					raise constraint_error;
				end if;
			end loop;
			prog_position := "MS414";
			-- find a position where the id is zero
			for i in 1..part_count_max2 loop
				if part_stock_array(i).part_id = 0 then -- this is a free position to insert a new part
					if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
						raise constraint_error;
					end if;
					part_stock_array(i).part_id := i;
					part_stock_array(i).part_code_fac := to_bounded_string(part_code_fac_given);
					--part_stock_array(i).date_edited := image(clock, time_zone => UTC_Time_Offset(clock));
					part_stock_array(i).date_edited := date_now;
					scratch_natural := i; -- save assigned part_id for later usage
					exit;
				end if;
			end loop;
			if update_stock_data_base then null;
			end if;
			put_line(standard_output,"Part has been added to data base with part_id" & natural'image(scratch_natural));
			new_line(standard_output);
			return true; -- return true if add was successful
		end if;

		prog_position := "MS500";
		if stock_operation = delete then
			if read_stock_data_base then null;
			end if;
			prog_position := "MS510";
			-- make sure the given part code does exist already
			for i in 1..part_count_max2 loop
				if part_stock_array(i).part_code_fac = to_bounded_string(part_code_fac_given) then
					part_occured_on_stock := part_occured_on_stock + 1;
				end if;
			end loop;
			if part_occured_on_stock > 1 then 
				print_warning_on_multiple_occurences(part_occured_on_stock); -- cs: force data base check ?
			end if;
			if part_occured_on_stock = 0 then print_error_on_unknown_part; end if;

			prog_position := "MS414";
			-- find part by part code in array and reset all occurences
			for i in 1..part_count_max2 loop
				if part_stock_array(i).part_code_fac = to_bounded_string(part_code_fac_given) then
					new_line;
					put_line("WARNING : DELETE PART WITH");
					put_line("          ID        : " & trim(natural'image(part_stock_array(i).part_id),left));
					put_line("          PART_CODE : " & part_code_fac_given);
					if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
						raise constraint_error;
					end if;
					-- reset all fields
					part_stock_array(i).part_id := 0;
					part_stock_array(i).part_code_fac := to_bounded_string(not_assigned_mark);
					part_stock_array(i).date_edited := default_date;
					part_stock_array(i).qty_on_stock := 0;
					part_stock_array(i).qty_reserved := 0;
					part_stock_array(i).qty_available:= 0;
					for m in 1..manufacturer_count_max
					loop
						part_stock_array(i).manufacturers(m).name := to_bounded_string(not_assigned_mark);
						part_stock_array(i).manufacturers(m).part_code := to_bounded_string(not_assigned_mark);
						part_stock_array(i).manufacturers(m).date_edited := default_date;
						part_stock_array(i).manufacturers(m).status_production := unknown;
						part_stock_array(i).manufacturers(m).url_datasheet_1 := to_unbounded_string(not_assigned_mark);
						part_stock_array(i).manufacturers(m).url_datasheet_2 := to_unbounded_string(not_assigned_mark);
					end loop;

					for d in 1..distributor_count_max
					loop
						part_stock_array(i).distributors(d).name := to_bounded_string(not_assigned_mark);
						part_stock_array(i).distributors(d).order_code := to_bounded_string(not_assigned_mark);
						part_stock_array(i).distributors(d).date_edited := default_date;
						part_stock_array(i).distributors(d).qty_min := 1;
						part_stock_array(i).distributors(d).price_net := 0.00;
					end loop;
					part_stock_array(i).storage_place := to_bounded_string(not_assigned_mark);
				end if;
			end loop;
			if update_stock_data_base then null;
			end if;
			return true; -- return true if delete was successful
		end if;

		prog_position := "MS600";
		if stock_operation = make_bom then
			if read_stock_data_base then null;
			end if;
			prog_position := "MS610";
			Open(
				file => eagle_bom_file,
				Mode => in_file,
				Name => to_string(eagle_bom_csv) -- this is the bom output by eagle ulp bom.ulp
				);
			set_input(eagle_bom_file);

			-- get part count of project eagle bom file
			part_ct_proj := count_parts_in_eagle_bom;

			reset(eagle_bom_file);
		
			prog_position := "MS620";
			create( bom_output_file, name => to_string(bom_file_csv)); close(bom_output_file);
			open(
				file => bom_output_file,
				mode => out_file,
				name => to_string(bom_file_csv)	-- this is the facility bom to be created
				);
			prog_position := "MS630";
			set_output(bom_output_file);
			prog_position := "MS640";
			convert_eagle_bom_to_facility_bom;

			close(eagle_bom_file);
			close(bom_output_file);
		end if;

		prog_position := "MS700";
		if stock_operation = checkout_bom then
			if read_stock_data_base then null;
			end if;
			prog_position := "MS710";
			if checkout_by_facility_bom then -- true if parts have been checked out
				if update_stock_data_base then null;
				end if;
				return true; -- return true if checkout was successful
			end if;
		end if;

		-- this is basically the same as checkout_bom but no checkout will take place
		prog_position := "MS800";
		if stock_operation = query_bom then
			if read_stock_data_base then null;
			end if;
			prog_position := "MS810";
			if checkout_by_facility_bom then  -- stock_operation will be evaluated
				null;
			end if;
			if update_stock_data_base then null;
			end if;
		end if;

		return false;
	end manage_stock;


	--pre-read stock data base to get part_count_on_stock and empty_lines_count
	procedure read_stock_data_base_part_count is
		previous_input			: ada.text_io.file_type renames current_input;
		line					: unbounded_string;
		part_section_entered 	: boolean := false;
	begin
		prog_position := "SC001";
		Open( 
			File => stock_db_file,
			Mode => in_file,
			Name => to_string(stock_db_csv)
			);
		prog_position := "SC050";
		set_input(stock_db_file);

		prog_position := "SC100";
		while not end_of_file
			loop
				line := get_line;

 				-- count empty lines upon passing an id=0 field
 				if sm_csv.get_field(line,1) = "0" then	
 					empty_lines_count := empty_lines_count + 1;
					--put_line("empty_lines_count :" & natural'image(empty_lines_count));
 				end if;

				-- stop counting lines upon passing end of stock mark
				if sm_csv.get_field(line,1) = "END OF STOCK" then	
					part_section_entered := false;
				end if;


				if part_section_entered then
					--if sm_csv.get_field(line,1) /= "0" then
						part_count_max2 := part_count_max2 + 1;
					--end if;
				end if;

				prog_position := "SC150";
				-- set part_section_entered flag upon passing the PART_ID line
				if sm_csv.get_field(line,1) = "PART_ID" then	
					part_section_entered := true;
				end if;

			end loop;

		prog_position := "SC200";
		close(stock_db_file);
		set_input(previous_input);
	end read_stock_data_base_part_count;



-- ins v012 begin

	function make_facility_bom_array(
		in_bom : string
		) return parts_of_facility_bom is
		previous_input : ada.text_io.file_type renames current_input;
		subtype parts_of_facility_bom_sized is parts_of_facility_bom (1..part_count_max2);
		facility_bom_part_array	: parts_of_facility_bom_sized;
		line					: unbounded_string;
		position_pt				: natural := 0;
		part_section_entered	: boolean := false;
	begin
		prog_position := "MBA00";
		open(file => facility_bom_file,
			mode => in_file,
			name => in_bom
			);
		set_input(facility_bom_file);

		-- read facility_bom_file in array facility_bom_part_array
		while not end_of_file	-- we assume the facility_bom_file is ok, as it has been generated by stock_manager itself
		loop					-- so there will be no further syntax and type checks
			line := get_line;
			if part_section_entered then
				prog_position := "MBA10";

				-- look for end of BOM mark in column 1
				if sm_csv.get_field(line,1) = sm_csv.row_separator_1 then 
					exit; -- stop reading the bom file
				end if;

				if sm_csv.get_field(line,1) /= "" then -- if first position found, the first field is not empty
					prog_position := "MBA20";
					position_pt := position_pt + 1; -- advance position pointer
					prog_position := "MBA21";
					facility_bom_part_array(position_pt).position := natural'value(sm_csv.get_field(line,1));
 					prog_position := "MBA22";
					facility_bom_part_array(position_pt).qty := natural'value(sm_csv.get_field(line,2));
					prog_position := "MBA23";
					facility_bom_part_array(position_pt).name := to_unbounded_string(sm_csv.get_field(line,3));
					prog_position := "MBA24";
					facility_bom_part_array(position_pt).part_code := to_bounded_string(sm_csv.get_field(line,4));
					prog_position := "MBA25";
					facility_bom_part_array(position_pt).part_id := natural'value(sm_csv.get_field(line,5));
				end if;

			else
				-- look for table header in column 1
				prog_position := "MBA40";
				if sm_csv.get_field(line,1) = "POS." then 
					part_section_entered := true;
				end if;
			end if; -- if part_section_entered
		end loop;
		prog_position := "MBA50";
		close(facility_bom_file);
		prog_position := "MBA60";
		set_input(previous_input);
		return facility_bom_part_array;
	end make_facility_bom_array;


	procedure scale_facility_bom
		(in_bom			: string;
		out_bom			: string;
		scale_factor	: natural
		) is
		previous_input			: ada.text_io.file_type renames current_input;
		previous_output			: Ada.Text_IO.File_Type renames current_output;
		line					: unbounded_string;
		position_pt				: natural := 0;
		part_section_entered	: boolean := false;
		subtype parts_of_facility_bom_sized is parts_of_facility_bom (1..part_count_max2);
		facility_bom_part_array	: parts_of_facility_bom_sized;
	begin
		prog_position := "SB000";

		facility_bom_part_array := make_facility_bom_array(in_bom);

-- 		open(file => facility_bom_file,
-- 			mode => in_file,
-- 			name => in_bom
-- 			);
-- 		set_input(facility_bom_file);
-- 
-- 		prog_position := "SB010";
-- 		-- read facility_bom_file in array facility_bom_part_array
-- 		while not end_of_file	-- we assume the facility_bom_file is ok, as it has been generated by stock_manager itself
-- 		loop					-- so there will be no further syntax and type checks
-- 			line := get_line;
-- 			if part_section_entered then
-- 				prog_position := "SB020";
-- 
-- 				-- look for end of BOM mark in column 1
-- 				if sm_csv.get_field(line,1) = sm_csv.row_separator_1 then 
-- 					exit; -- stop reading the bom file
-- 				end if;
-- 
-- 				if sm_csv.get_field(line,1) /= "" then -- if first position found, the first field is not empty
-- 					prog_position := "SB030";
-- 					position_pt := position_pt + 1; -- advance position pointer
-- 					facility_bom_part_array(position_pt).position := natural'value(sm_csv.get_field(line,1));
-- 					facility_bom_part_array(position_pt).qty := natural'value(sm_csv.get_field(line,2));
-- 					facility_bom_part_array(position_pt).name := to_unbounded_string(sm_csv.get_field(line,3));
-- 					facility_bom_part_array(position_pt).part_code := to_bounded_string(sm_csv.get_field(line,4));
-- 					facility_bom_part_array(position_pt).part_id := natural'value(sm_csv.get_field(line,5));
-- 				end if;
-- 
-- 			else
-- 				-- look for table header in column 1
-- 				prog_position := "SB040";
-- 				if sm_csv.get_field(line,1) = "POS." then 
-- 					part_section_entered := true;
-- 				end if;
-- 			end if; -- if part_section_entered
-- 		end loop;
-- 		prog_position := "SB050";
-- 		close(facility_bom_file);
-- 		prog_position := "SB051";
-- 		set_input(previous_input);
		-- all information has been red from given facility bom file
		-- now we write it back in output file taking scale_factor into account:

		prog_position := "SB060";
		create( facility_bom_file_scaled, name => out_bom); close(facility_bom_file_scaled);
		open(file => facility_bom_file_scaled,
			mode => out_file,
			name => out_bom
			);
		set_output(facility_bom_file_scaled);

		prog_position := "SB070";
		put_field(text => "SCALED BILL OF MATERIAL"); put_lf;
		put_field(text => "FOR TEMPORARILY USE ONLY !"); put_lf;
		put_field(text => "------------------------------"); put_lf;
		put_field(text => "board:"); put_field(text => base_name(out_bom)); put_lf;
		put_field(text => "quantity:"); put_field(text => trim(natural'image(scale_factor),left)); put_lf(count => 2);
		put_field(text => "created by"); put_field(text => "STOCK MANAGER"); put_field(text => "V" & version); put_lf;
		put_field(text => "contact:"); put_field(text => "www.blunk-electronic.de"); put_lf;
		put_field(text => "date:"); put_field(text => date_now);
		put_field(text => "(YYYY-MM-DD HH:MM:SS)"); put_lf(count => 2);
-- 		put_field(text => "part count SMD:"); put_field(text => trim(natural'image(part_ct_proj_smd * scale_factor),left)); put_lf;
-- 		put_field(text => "part count THT:"); put_field(text => trim(natural'image(part_ct_proj_tht * scale_factor),left)); put_lf;
-- 		put_field(text => "part count TOTAL:"); put_field(text => trim(natural'image(part_ct_proj * scale_factor),left)); put_lf;
		put_field(text => "POS.");
		put_field(text => "QTY");
		put_field(text => "PART_NAME");
		put_field(text => "PART_CODE_" & facility_names(1));
		put_field(text => "PART_ID");
		put_lf(count => 2);
	
		-- write members of facility_bom_part_array in output file
		for p in 1..part_count_max2
		loop
			prog_position := "SB080";
			-- loop here until end of facility_bom_part_array found (where position of particual member is zero)
			if facility_bom_part_array(p).position = 0 then
				exit;
			end if;
			put_field(text => trim(natural'image(facility_bom_part_array(p).position),left));
			put_field(text => trim(natural'image(facility_bom_part_array(p).qty * scale_factor),left));
			put_field(text => to_string(facility_bom_part_array(p).name));
			put_field(text => to_string(facility_bom_part_array(p).part_code));
			put_field(text => trim(natural'image(facility_bom_part_array(p).part_id),left));
			put_lf;
		end loop;

		-- write summary
		for i in 1..5
		loop
			put_field(text => sm_csv.row_separator_1);
		end loop;
		put_lf;

		prog_position := "SB090";
		put_field(text => "END OF BOM");
		prog_position := "SB100";
		close(facility_bom_file_scaled);
		set_output(previous_output);
	end scale_facility_bom;


	procedure merge_facility_bom(
		in_bom1	: string;
		in_bom2 : string;
		out_bom : string) is
		--previous_input			: ada.text_io.file_type renames current_input;
		previous_output			: Ada.Text_IO.File_Type renames current_output;
		position_pt				: natural := 0;
		subtype parts_of_facility_bom_sized is parts_of_facility_bom (1..part_count_max2);
		facility_bom_part_array_in1	: parts_of_facility_bom_sized;
		facility_bom_part_array_in2	: parts_of_facility_bom_sized;
		facility_bom_part_array_out	: parts_of_facility_bom_sized;
	begin
		prog_position := "MF000";
		facility_bom_part_array_in1 := make_facility_bom_array(in_bom1);
		facility_bom_part_array_in2 := make_facility_bom_array(in_bom2);

		-- process members of array #1
		for p1 in 1..part_count_max2
		loop
			prog_position := "MF010";
			-- loop here until end of facility_bom_part_array found (where position of particual member is zero)
			if facility_bom_part_array_in1(p1).position = 0 then
				exit;
			end if;

			position_pt := position_pt + 1;
			facility_bom_part_array_out(position_pt).position := position_pt;
			-- get quantity of this member
			facility_bom_part_array_out(position_pt).qty := facility_bom_part_array_in1(p1).qty;
			facility_bom_part_array_out(position_pt).name := to_unbounded_string("parts of " & in_bom1);
			facility_bom_part_array_out(position_pt).part_code := facility_bom_part_array_in1(p1).part_code;
			facility_bom_part_array_out(position_pt).part_id := facility_bom_part_array_in1(p1).part_id;

			-- process members of array #2
			for p2 in 1..part_count_max2
			loop
				prog_position := "MF100";
				-- loop here until end of facility_bom_part_array found (where position of particual member is zero)
				if facility_bom_part_array_in2(p2).position = 0 then
					exit;
				end if;
				-- search for member that has the same part_id
				if facility_bom_part_array_in2(p2).part_id = facility_bom_part_array_in1(p1).part_id then
					--put_line(standard_output, natural'image(facility_bom_part_array_in2(p2).part_id));
					-- update quantity of member
					facility_bom_part_array_out(position_pt).qty := facility_bom_part_array_in1(p1).qty + facility_bom_part_array_in2(p2).qty;
					facility_bom_part_array_out(position_pt).name := facility_bom_part_array_out(position_pt).name & to_unbounded_string(" & " & in_bom2);
					facility_bom_part_array_in2(p2).part_id := 0; -- part_id zero means, member processed (required for processing array #2 later)
					exit; -- no further searching required
				end if;
			end loop;
		end loop;

		-- process remaining members of array #2
		for p1 in 1..part_count_max2
		loop
			prog_position := "MF200";
			-- loop here until end of facility_bom_part_array found (where position of particual member is zero)
			if facility_bom_part_array_in2(p1).position = 0 then
				exit;
			end if;
			-- search for member that has not been not processed yet, means where part_id is non-zero
			if facility_bom_part_array_in2(p1).part_id /= 0 then
				position_pt := position_pt + 1;
				facility_bom_part_array_out(position_pt).position := position_pt;
				facility_bom_part_array_out(position_pt).qty := facility_bom_part_array_in2(p1).qty;
				facility_bom_part_array_out(position_pt).name := to_unbounded_string("parts of " & in_bom2);
				facility_bom_part_array_out(position_pt).part_code := facility_bom_part_array_in2(p1).part_code;
				facility_bom_part_array_out(position_pt).part_id := facility_bom_part_array_in2(p1).part_id;
			end if;
		end loop;


		-- write facility_bom_part_array_out in output file
 		prog_position := "MF300";
 		create( bom_output_file, name => out_bom); close(bom_output_file);
		open(file => bom_output_file,
			mode => out_file,
			name => out_bom
			);
		set_output(bom_output_file);
 
 		prog_position := "MF400";
		put_field(text => "COMPOSITE BILL OF MATERIAL"); put_lf;
		put_field(text => "FOR TEMPORARILY USE ONLY !"); put_lf;
		put_field(text => "------------------------------"); put_lf;
--		put_field(text => "boards:"); put_field(text => base_name(in_bom1)); put_lf;
		put_field(text => "created by"); put_field(text => "STOCK MANAGER"); put_field(text => "V" & version); put_lf;
		put_field(text => "contact:"); put_field(text => "www.blunk-electronic.de"); put_lf;
		put_field(text => "date:"); put_field(text => date_now);
		put_field(text => "(YYYY-MM-DD HH:MM:SS)"); put_lf(count => 2);
-- 		put_field(text => "part count SMD:"); put_field(text => trim(natural'image(part_ct_proj_smd * scale_factor),left)); put_lf;
-- 		put_field(text => "part count THT:"); put_field(text => trim(natural'image(part_ct_proj_tht * scale_factor),left)); put_lf;
-- 		put_field(text => "part count TOTAL:"); put_field(text => trim(natural'image(part_ct_proj * scale_factor),left)); put_lf;
		put_field(text => "POS.");
		put_field(text => "QTY");
		put_field(text => "PART_NAME");
		put_field(text => "PART_CODE_" & facility_names(1));
		put_field(text => "PART_ID");
		put_lf(count => 2);

		for p in 1..part_count_max2
		loop
			prog_position := "MF450";
			-- loop here until end of facility_bom_part_array found (where position of particual member is zero)
			if facility_bom_part_array_out(p).position = 0 then
				exit;
			end if;
			put_field(text => trim(natural'image(facility_bom_part_array_out(p).position),left));
			put_field(text => trim(natural'image(facility_bom_part_array_out(p).qty),left));
			--put_field(text => to_string(facility_bom_part_array_out(p).name));
			put_field(text => "collected");
			put_field(text => to_string(facility_bom_part_array_out(p).part_code));
			put_field(text => trim(natural'image(facility_bom_part_array_out(p).part_id),left));
			put_lf;
		end loop;

		-- write summary
		for i in 1..4
		loop
			put_field(text => sm_csv.row_separator_1);
		end loop;
		put_lf;

		prog_position := "MF500";
		put_field(text => "END OF BOM");
		prog_position := "MF510";
		close(bom_output_file);
		set_output(previous_output);
	end merge_facility_bom;
-- ins v012 end

-------- MAIN PROGRAM ------------------------------------------------------------------------------------

begin

	new_line(3);
	put_line("STOCK MANAGER V"& version);
	put_line(row_separator_double);
	check_environment;

	prog_position := "IN000";
	arg_ct := argument_count;

	-- rm v010 begin
	--clean up stale order and take lists
-- 	if exists (to_string(items_to_order_csv)) then 
-- 		delete_file(to_string(items_to_order_csv));
-- 	end if;
-- 	if exists (to_string(items_to_take_csv)) then 
-- 		delete_file(to_string(items_to_take_csv));
-- 	end if;
	-- rm v010 end

	if arg_ct < 1 then
		prog_position := "IN101";
		print_supported_actions;
		--raise constraint_error;
	else
		prog_position := "AR001";


	while arg_pt <= arg_ct
		loop
			if argument(arg_pt) = "help" then -- print help
				prog_position := "HL001";
				print_help_general;
				raise constraint_error;
			end if;

			if argument(arg_pt) = "pdb" then -- this is a hidden option
				parts_db_csv:=to_bounded_string(Argument(arg_pt+1));
				put_line("parts data base : " & to_string(parts_db_csv));

				-- open old data base
				prog_position := "PD001";
				Open( 
					File => parts_db_file,
					Mode => in_file,
					Name => to_string(parts_db_csv)
					);
				prog_position := "PD002";
				--set_input(parts_db_file);

				-- open new data base
				prog_position := "PD004";
				Create( stock_db_file, Name => to_string(stock_db_csv)); Close(stock_db_file);
				put_line("stock data base : " & to_string(stock_db_csv));
				prog_position := "PD005";
				Open( 
					File => stock_db_file,
					Mode => out_File,
					Name => to_string(stock_db_csv)
					);
				prog_position := "PD007";
				--put_line("test 1");
				--set_output(stock_db_file);
				--put_line("test 2");

				if convert_data_base then null;
				end if;

				prog_position := "PD021";
				close (stock_db_file);
				prog_position := "PD041";
				close (parts_db_file);

				exit; -- no further evaluation of arguments required
			end if;

			if argument(arg_pt) = "debug" then -- set debug mode on
				debug_mode := 1; 
				put_line("debug mode      :" & natural'image(debug_mode));
				check_environment;
				arg_pt := arg_pt + 1;
				prog_position := "DG001";
			end if;

			if argument(arg_pt) = "full" then -- set full mode on
				show_mode := 0; 
				arg_pt := arg_pt + 1;
			end if;

			if argument(arg_pt) = "no_confirmation" then -- turn user confirmation off -- use with care
				operator_confirmation_required := false;
				arg_pt := arg_pt + 1;
			end if;

			if argument(arg_pt) = "sdb" then -- if stock_db file specified
				stock_db_csv := to_unbounded_string(argument(arg_pt+1));
			end if;
		
			if argument(arg_pt) = "show_by_id" then
				stock_operation := show_by_id;
				prog_position := "AR090";
				put_line("action          : " & stock_operation_type'image(stock_operation));
				prog_position := "AR094";
				part_id_given:= natural'value(Argument(arg_pt+1));
				--put_line("stock data base : " & to_string(stock_db_csv));
				prog_position := "AR096";
				put_line("part id         :" & natural'image(part_id_given));
				prog_position := "AR098";

				--put_line(stock_operation_type'image(stock_operation));
				--put_line(natural'image(part_id_given));
				read_stock_data_base_part_count; -- dyn
				--put_line("positions on stock :" & natural'image(part_count_max2));

				if manage_stock(stock_operation => stock_operation, part_id_given => part_id_given, part_code_fac_given => not_assigned_mark) then null;
				end if;
			end if;

			if argument(arg_pt) = "show_by_fac_code" then
				stock_operation := show_by_fac_code;
				prog_position := "AR080";
				put_line("action          : " & stock_operation_type'image(stock_operation));
				prog_position := "AR084";
				part_code_given:=to_bounded_string(Argument(arg_pt+1));
				--put_line("stock data base : " & to_string(stock_db_csv));
				put_line("part code       : " & to_string(part_code_given));
				prog_position := "AR086";
				read_stock_data_base_part_count; --dyn
				-- make sure the part code contains valid characters
				-- if parse_part_code(to_string(part_code_given)) then -- rm v013
				-- CS: a parse_part_code_allowing_asterisk should be allowed here instead
					prog_position := "AR088";
					if manage_stock(stock_operation, part_id_given => 0, part_code_fac_given => to_string(part_code_given)) then null;
					end if;
				-- end if; -- rm v013
			end if;

			-- ins v013 begin
			if argument(arg_pt) = "show_by_manu_code" then
				stock_operation := show_by_manu_code;
				prog_position := "AR120";
				put_line("action          : " & stock_operation_type'image(stock_operation));
				prog_position := "AR122";
				property_string_given := to_bounded_string(Argument(arg_pt+1));
				--put_line("manuf. part code: " & to_string(manufacturer_part_code_given)); -- rm v013
				put_line("manuf. part code: " & to_string(property_string_given)); -- ins v013
				prog_position := "AR124";
				read_stock_data_base_part_count; --dyn
				prog_position := "AR126";
				if manage_stock(stock_operation, part_id_given => 0, part_code_fac_given => to_string(part_code_given)) then null;
				end if;
			end if;
			-- ins v013 end

			-- ins v009 begin
			if argument(arg_pt) = "show_by_order_code" then
				stock_operation := show_by_order_code;
				prog_position := "AR110";
				put_line("action          : " & stock_operation_type'image(stock_operation));
				prog_position := "AR111";
				property_string_given := to_bounded_string(Argument(arg_pt+1)); -- ins v013
				--put_line("order code      : " & Argument(arg_pt+1)); -- rm v013
				put_line("order code      : " & to_string(property_string_given)); -- ins v013
				prog_position := "AR112";
				read_stock_data_base_part_count; --dyn
				-- make sure the part code contains valid characters
				prog_position := "AR113";
				if manage_stock(stock_operation, part_id_given => 0, part_code_fac_given => to_string(part_code_given)) then null;
				end if;
			end if;
			-- ins v009 end

			if argument(arg_pt) = "edit" then
				stock_operation := edit;
				prog_position := "AR010";
				put_line("action          : " & stock_operation_type'image(stock_operation));
				part_id_given:= natural'value(Argument(arg_pt+1));
				put_line("part id         :" & natural'image(part_id_given));
				prog_position := "AR011";
				part_property := part_property_type'value(argument(arg_pt+2));
				--put_line("stock data base : " & to_string(stock_db_csv));
				put_line("property to edit: " & part_property_type'image(part_property));
				prog_position := "AR012"; -- ins v013
				property_string_given := to_bounded_string(argument(arg_pt+3)); -- ins v013
				read_stock_data_base_part_count; -- dyn
				if manage_stock(stock_operation, part_id_given => part_id_given, part_code_fac_given => not_assigned_mark) then
					update_log;
				end if;
			end if;

			if argument(arg_pt) = "add" then
				stock_operation := add;
				prog_position := "AR020";
				put_line("action          : " & stock_operation_type'image(stock_operation));
				prog_position := "AR024";
				part_code_given:=to_bounded_string(Argument(arg_pt+1));
				--put_line("stock data base : " & to_string(stock_db_csv));
				put_line("part code       : " & to_string(part_code_given));

				read_stock_data_base_part_count;
				if empty_lines_count = 0 then -- if no empty line to fill new part in
					part_count_max2 := part_count_max2 + 1; -- increment part count (to instantiate part_stock_array with later)
				else
					null; -- leave part_count_max2 as it is -> no more additional elements need to be instantiated
				end if;
				-- make sure the part code contains valid characters, check syntax
				if parse_part_code(to_string(part_code_given), parse_depth => 1) then 
					if parse_part_code(to_string(part_code_given)) then
						if manage_stock(stock_operation, part_id_given => 0, part_code_fac_given => to_string(part_code_given)) then
							update_log;
						end if;
					end if;
				end if;
			end if;

			if argument(arg_pt) = "delete" then
				stock_operation := delete;
				prog_position := "AR030";
				put_line("action          : " & stock_operation_type'image(stock_operation));
				prog_position := "AR034";
				part_code_given:=to_bounded_string(Argument(arg_pt+1));
				--put_line("stock data base : " & to_string(stock_db_csv));
				put_line("part_code_fac   : " & to_string(part_code_given));
				-- make sure the part code contains valid characters
				read_stock_data_base_part_count; -- dyn
				if parse_part_code(to_string(part_code_given)) then
					if manage_stock(stock_operation, part_id_given => 0, part_code_fac_given => to_string(part_code_given)) then 
						update_log;
					end if;
				end if;
			end if;

			if argument(arg_pt) = "make_bom" then
				stock_operation := make_bom;
				prog_position := "AR040";
				put_line("action          : " & stock_operation_type'image(stock_operation));
				--put_line("stock data base : " & to_string(stock_db_csv));
				put_line("facility name   : " & facility_names(1));

				prog_position := "AR041";
				eagle_bom_csv:=to_bounded_string(Argument(arg_pt+1));
				put_line("input eagle bom : " & to_string(eagle_bom_csv));
	
				if not exists(to_string(eagle_bom_csv)) then
					prog_position := "AR042"; -- error on invalid file
					print_error_on_invalid_file(to_string(eagle_bom_csv));
					raise constraint_error;
				end if;

				prog_position := "AR045";
				if argument_count = arg_pt+1 then -- if argumment for facility bom file missing
					-- default to "eagle_bom_file_bom.csv
					bom_file_csv := to_bounded_string( base_name(to_string(eagle_bom_csv)) & "_bom.csv");
					bom_file_csv := to_bounded_string(containing_directory(to_string(eagle_bom_csv)) & "/" & to_string(bom_file_csv));
					new_line;
					put_line("IMPORTANT INFO  : Name of output BOM file missing after EAGLE BOM. -> Default name used.");
				else
					-- otherwise take what user gives
					bom_file_csv:=to_bounded_string(Argument(arg_pt+2));
				end if;
				--quantity_of_units:= natural'value(Argument(arg_pt+3));

				put_line("output bom      : " & to_string(bom_file_csv));
				-- notify user that file already exists
				prog_position := "AR046";
				if exists(to_string(bom_file_csv)) then
					if operator_confirmation_required then -- ins v003
						print_warning_on_already_existing_file(to_string(bom_file_csv));
					end if; -- ins v003
					if request_user_confirmation(
						question_form => 3,
						show_confirmation_dialog => operator_confirmation_required) 
						= false then
						raise constraint_error;
					end if;
				end if;
				--put_line("qty of units    :" & natural'image(quantity_of_units));
				read_stock_data_base_part_count; -- dyn
				if manage_stock(stock_operation, part_id_given => 0, part_code_fac_given => "") then null;
				end if;
			end if;

			-- ins v012 begin
			if argument(arg_pt) = "scale_bom" then
				stock_operation := scale_bom;
				prog_position := "AR300";
				put_line("action          : " & stock_operation_type'image(stock_operation));
				facility_bom_csv := to_bounded_string(Argument(arg_pt+1));
				prog_position := "AR310";
				put_line("input fac. bom  : " & to_string(facility_bom_csv));
				quantity_of_units:= natural'value(Argument(arg_pt+2));
				prog_position := "AR320";
				put_line("qty of units    :" & natural'image(quantity_of_units));
				facility_bom_csv_scaled := to_bounded_string(base_name(to_string(facility_bom_csv))) & "_x" & trim(natural'image(quantity_of_units),left) & ".csv";
				put_line("output fac. bom : " & to_string(facility_bom_csv_scaled));
				prog_position := "AR380";
				read_stock_data_base_part_count; -- dyn
				prog_position := "AR390";
				scale_facility_bom(
					in_bom => to_string(facility_bom_csv),
					out_bom => to_string(facility_bom_csv_scaled),
					scale_factor => quantity_of_units
					);
			end if;

			if argument(arg_pt) = "merge_bom" then
				stock_operation := merge_bom;
				prog_position := "AR400";
				put_line("action          : " & stock_operation_type'image(stock_operation));
				facility_bom_csv_1 := to_bounded_string(Argument(arg_pt+1));
				prog_position := "AR410";
				put_line("input fac. bom 1: " & to_string(facility_bom_csv_1));
				facility_bom_csv_2 := to_bounded_string(Argument(arg_pt+2));
				prog_position := "AR420";
				put_line("input fac. bom 2: " & to_string(facility_bom_csv_2));
				prog_position := "AR430";
				--facility_bom_csv_12 := to_bounded_string(base_name(to_string(facility_bom_csv_1)) & "_" & base_name(to_string(facility_bom_csv_2)) & ".csv");
				put_line("output fac. bom : " & to_string(facility_bom_csv_12));
				prog_position := "AR440";
				read_stock_data_base_part_count; -- dyn
				prog_position := "AR450";
				merge_facility_bom(
					in_bom1 => to_string(facility_bom_csv_1),
					in_bom2 => to_string(facility_bom_csv_2),
					out_bom => to_string(facility_bom_csv_12)
					);
				prog_position := "AR450";
			end if;
			-- ins v012 end

			if argument(arg_pt) = "checkout_bom" then
				stock_operation := checkout_bom;
				prog_position := "AR050";
				put_line("action          : " & stock_operation_type'image(stock_operation));
				facility_bom_csv := to_bounded_string(Argument(arg_pt+1));
				--put_line("stock data base : " & to_string(stock_db_csv));
				put_line("input fac. bom  : " & to_string(facility_bom_csv));

				prog_position := "AR052";
				-- items_to_order_csv := to_bounded_string(argument(arg_pt+2)); -- rm v013
				-- The name of the order list is no longer provided via arguments. Instead, it will be derived
				-- from the facility bom file name.
				items_to_order_csv := to_bounded_string(base_name(to_string(facility_bom_csv)) & "_order.csv"); -- ins v013
				put_line("order list      : " & to_string(items_to_order_csv));

				prog_position := "AR053";
				--items_to_take_csv := to_bounded_string(argument(arg_pt+3)); -- rm v013
				-- The name of the withdrawal list is no longer provided via arguments. Instead, it will be derived
				-- from the facility bom file name.
				items_to_take_csv := to_bounded_string(base_name(to_string(facility_bom_csv)) & "_withdrawal.csv"); -- ins v013
				put_line("withdrawal list : " & to_string(items_to_take_csv));

				prog_position := "AR054";
				--quantity_of_units:= natural'value(Argument(arg_pt+4)); -- rm v013
				-- The quantity argument is now to be found on position 2.
				quantity_of_units:= natural'value(Argument(arg_pt+2)); -- ins v013
				put_line("qty of units    :" & natural'image(quantity_of_units));

				read_stock_data_base_part_count; -- dyn
				if manage_stock(stock_operation, part_id_given => 0, part_code_fac_given => "") then
					update_log; 
					prog_position := "AR055"; -- ins v009
					--copy_file(to_string(facility_bom_csv), to_string(directory_of_log) &  -- rm v009

					-- ins v011 begin
					-- debug
-- 					put_line("items_to_take   : " & to_string(items_to_take_csv));
-- 					prog_position := "AR058";
-- 					put_line("directory_of_log: " & to_string(directory_of_log));
-- 					prog_position := "AR056";
-- 					--put_line("log file        : " & to_string(directory_of_log) & 
-- 					--make_filename_by_date(prefix => "LOG_withdrawal_list_", file_name => to_string(items_to_take_csv), date_now => now));
-- 					put_line("log file        : " & to_string(directory_of_log) & 
-- 					make_filename_by_date(prefix => "LOG_withdrawal_list_", file_name => to_string(items_to_take_csv), date_now => now));
-- 					prog_position := "AR057";
					-- debug
					-- ins v011 end

					copy_file(to_string(items_to_take_csv), to_string(directory_of_log) &  -- ins v009
					make_filename_by_date(prefix => "LOG_withdrawal_list_", file_name => to_string(items_to_take_csv), date_now => now)
					);
				end if;
			end if;

			if argument(arg_pt) = "query_bom" then
				stock_operation := query_bom;
				prog_position := "AR060";
				put_line("action          : " & stock_operation_type'image(stock_operation));
				prog_position := "AR062";
				facility_bom_csv := to_bounded_string(Argument(arg_pt+1));
				put_line("input fac. bom  : " & to_string(facility_bom_csv));
				prog_position := "AR061";
				--items_to_order_csv := to_bounded_string(argument(arg_pt+2)); -- rm v013
				-- The name of the order list is no longer provided via arguments. Instead, it will be derived
				-- from the facility bom file name.
				items_to_order_csv := to_bounded_string(base_name(to_string(facility_bom_csv)) & "_order.csv"); -- ins v013
				put_line("order list      : " & to_string(items_to_order_csv));
				prog_position := "AR064";
				--quantity_of_units:= natural'value(Argument(arg_pt+3)); -- rm v013
				-- The quantity argument is now to be found on position 2.
				quantity_of_units:= natural'value(Argument(arg_pt+2)); -- ins v013
				put_line("qty of units    :" & natural'image(quantity_of_units));
				read_stock_data_base_part_count; -- dyn
				if manage_stock(stock_operation, part_id_given => 0, part_code_fac_given => "") then null;
				end if;
			end if;

			if argument(arg_pt) = "roll_back" then
				stock_operation := roll_back;
				prog_position := "AR070";
				put_line("action          : " & stock_operation_type'image(stock_operation));
				put_line("stock data base : " & to_string(stock_db_csv));
				--update_log;
				prog_position := "AR075";
				put_line("restore from    : " & to_string(directory_of_backup) & "/" & get_latest_backup_date);
				prog_position := "AR077";
				if request_user_confirmation(show_confirmation_dialog => operator_confirmation_required) = false then
					raise constraint_error;
				end if;
				prog_position := "AR078";
				--put_line(to_string(directory_of_backup) & "/" & get_latest_backup_date);
 				copy_file(
 					source_name  => to_string(directory_of_backup) & "/" & get_latest_backup_date, 
 					target_name => to_string(stock_db_csv)
 				);
			end if;


			if argument(arg_pt) = "log" then
				stock_operation := log;
				prog_position := "AR100";
				put_line("action          : " & stock_operation_type'image(stock_operation));
				put_line("stock data base : " & to_string(stock_db_csv));
				print_log;
			end if;


			-- if unsupported action given, print help text
			if stock_operation = default then
				prog_position := "AR200";
				print_supported_actions;
			end if;

			arg_pt := arg_pt + 1;
		end loop;
	end if;

	
	prog_position := "LG000";

----exception handler------------------------------------------------------------------
	exception
		when constraint_error => 
			new_line;
			if prog_position = "IN001" then print_help_general; end if;
			if prog_position = "IN101" then print_help_general; end if;
			
			if prog_position = "AR094" then print_error_on_invalid_id; end if;
			if prog_position = "AR084" then print_error_on_invalid_part_code; end if;
			if prog_position = "AR024" then print_error_on_invalid_part_code; end if;
			if prog_position = "AR034" then print_error_on_invalid_part_code; end if;
			if prog_position = "AR010" then print_error_on_invalid_id; end if;
			if prog_position = "AR011" then print_supported_properties; end if;
			if prog_position = "AR041" then print_error_on_missing_eagle_bom_file; end if;
			if prog_position = "AR045" then print_error_on_missing_facility_bom_file; end if;
			if prog_position = "AR050" then print_error_on_missing_facility_bom_file_to_checkout; end if;
			if prog_position = "AR052" then print_error_on_missing_order_list_file; end if;
			if prog_position = "AR053" then print_error_on_missing_withdrawal_list; end if;
			if prog_position = "AR054" then print_error_on_missing_facility_bom_unit_quantity_to_checkout; end if;
			if prog_position = "AR062" then print_error_on_missing_facility_bom_file_to_query; end if;
			if prog_position = "AR061" then print_error_on_missing_order_list_file; end if;
			if prog_position = "AR064" then print_error_on_missing_facility_bom_unit_quantity_to_query; end if;

			if prog_position = "MS317" then print_error_on_missing_part_code_fac; end if;
			if prog_position = "MS320" then print_error_on_missing_qty_delta; end if;
			if prog_position = "MS322" then print_error_on_invalid_qty_delta; end if;
			if prog_position = "MS332" then print_error_on_invalid_qty_reserved; end if;
			if prog_position = "MS321" then print_error_on_still_reserved_parts; end if;
			if prog_position = "MS340" then print_error_on_missing_manufacturer_name; end if;
			if prog_position = "MS342" then print_error_on_missing_manufacturer_part_code; end if;
			if prog_position = "MS343" then print_error_on_missing_manufacturer_status_production; end if;
			if prog_position = "MS344" then print_error_on_missing_manufacturer_datasheet; end if;
			if prog_position = "MS345" then print_error_on_missing_manufacturer_datasheet; end if;
			if prog_position = "MS346" then print_error_on_missing_distributor_name; end if;
			if prog_position = "MS347" then print_error_on_missing_distributor_order_code; end if;
			if prog_position = "MS348" then print_error_on_missing_distributor_qty_min; end if;
			if prog_position = "MS349" then print_error_on_missing_distributor_price_net; end if;
			if prog_position = "MS360" then print_error_on_missing_storage_place; end if;
			if prog_position = "MS370" then print_error_on_missing_remarks; end if;
			if prog_position = "MS380" then print_error_on_missing_project; end if;

			new_line;
			put_line(standard_output,"program aborted at position " & prog_position);
			set_exit_status(1);

		when storage_error =>
			new_line;
			put("storage error !");
			--put_line(exception_name(event));
			--put_line(exception_message(event));
			put_line(standard_output," Program aborted at position " & prog_position);
			set_exit_status(2);

		when others =>
			if prog_position = "AR075" then print_error_on_invalid_backup_file; end if;
			if prog_position = "AR078" then print_error_on_invalid_backup_file; end if;
			if prog_position = "RS001" then 
				put_line("ERROR : stock data base '" & to_string(stock_db_csv) & "' not found !");
			end if;
			if prog_position = "MS620" then print_error_on_insufficient_rights(to_string(bom_file_csv)); end if; -- ins v009
			if prog_position = "OL410" then print_error_on_insufficient_rights(to_string(items_to_order_csv)); end if; -- ins v013
			if prog_position = "UD000" then print_error_on_insufficient_rights(to_string(stock_db_csv)); end if; -- ins v009
			if prog_position = "UD001" then print_error_on_insufficient_rights(to_string(stock_db_csv)); end if; -- ins v009
			if prog_position = "IT000" then print_error_on_insufficient_rights(to_string(items_to_take_csv)); end if; -- ins v009
			if prog_position = "CF000" then print_error_on_insufficient_rights(to_string(facility_bom_csv)); end if; -- ins v009
			if prog_position = "AR012" then print_error_on_too_many_characters; end if; -- ins v013
			--put("unexpected exception ! ");
			--put_line(exception_name(event));
			--put_line(exception_message(event));
			new_line; -- ins v009
			put_line(standard_output,"Program aborted at position " & prog_position);
			set_exit_status(2);

end stock_manager;
