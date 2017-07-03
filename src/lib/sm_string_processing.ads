------------------------------------------------------------------------------
--                                                                          --
--                         SYSTEM M-1 COMPONENTS                            --
--                                                                          --
--                                 M-1                                      --
--                                                                          --
--                               S p e c                                    --
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
--   history of changes:
--


with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.calendar.time_zones;	use ada.calendar.time_zones;
with ada.calendar;				use ada.calendar;
with ada.calendar.formatting;	use ada.calendar.formatting;

package sm_string_processing is

	now				: time := clock;
	date_now		: string (1..19) := image(now, time_zone => UTC_Time_Offset(now));

	function search_pattern_in_text
		-- version 001 / MBL
		(text    : string;
		 pattern : string)
		return boolean;

	procedure print_warning_on_already_existing_file
		-- version 001 / MBL
		(file_name : string := "");

	function request_user_confirmation
		-- version 001
		-- question_form given as natural determines kind of question addressed to the operator, default is 0
		-- show_confirmation_dialog given as boolean determines if operater request shall be done or not. default is true
		-- 
		-- returns true if user typed y, otherwise false
		( question_form : natural := 0;
		  show_confirmation_dialog : boolean := true)
		return boolean;

	procedure print_error_on_invalid_file
		(file_name	: string := "");


	function check_date	
		-- version 001 / MBL
		-- returns given string unchanged if ok, raises constraint_error on occurence of space or semicolon
		( date_given	: string) 
		return string;

	function make_filename_by_date
		-- renders a given text (mostly a file name) to BAK_YYYY-MM-DD_HH-MM-SS_file_name
		-- the given text must not exceed a length of 224 characters
		-- version 006 / MBL
		(
		prefix		: string := "BAK_"; -- ins v004
		file_name	: string;
		date_now	: time := clock -- ins v005
		) return string;


	function Is_Field	
		(
		-- version 1.0 / MBL
		Line	: unbounded_string;  	-- given line to examine
		Value 	: String ; 				-- given value to be tested for
		Field	: Natural				-- field number to expect value in
		) 
		return Boolean;

	function Get_Field	
		(
		-- version 2.0 / MBL
		Line	: unbounded_string;
		Field	: Natural;
		IFS		: character := ' '
		)
		return string;

	function Get_Field_Count 
		(
		-- version 1.0 / MBL
		-- returns number of fields separated by whitespace
		Line	: unbounded_string
		)
		return Natural;


end sm_string_processing;

