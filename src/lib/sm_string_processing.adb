------------------------------------------------------------------------------
--                                                                          --
--                         SYSTEM M-1 COMPONENTS                            --
--                                                                          --
--                                 M-1                                      --
--                                                                          --
--                               B o d y                                    --
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


with ada.text_io;				use ada.text_io;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.maps;	 		use ada.strings.maps;

package body sm_string_processing is

	number_type		: character_set := to_set("0123456789");

	function search_pattern_in_text
		-- version 001 / MBL
		(text    : string;
		 pattern : string)
		return boolean is
		scratch			: natural := 0;
		pattern_position: natural := 0;
		hard_start		: boolean := false; -- true if search string does not start with '*'
		hard_end		: boolean := false; -- true if search string does not end with '*'
		pattern_start	: natural := 0; -- points to first character of search string
										-- 1 if search string does not start with '*' , 2 if search string starts with '*'
		pattern_end		: natural := 0; -- points to last character of search string
										-- equal to length of search string if string does not end with '*'
										-- is equal to length of search string -1 if search string ends with with '*'
		length_text 	: natural := text'length; -- length of string to be searched in
		length_pattern  : natural := pattern'length; -- length of string to be seached for

		-- instantiate a bounded string type with length equal to search pattern length
		package pattern_string_type is new generic_bounded_length(length_pattern); use pattern_string_type;
		pattern_stripped : pattern_string_type.bounded_string;
		pattern_stripped_length : natural;
	begin
		-- elaborate kind of search pattern ( with or without '*' at the beginng or end)
		-- adjust pattern start/end position
		-- set or reset hard_start/hard_end flag

-- 		if pattern(pattern'first) = '*' then -- check pattern beginning
-- 			pattern_start := 2;
-- 			hard_start := false;
-- 			if pattern(pattern'last) = '*' then -- check pattern end
-- 				pattern_end := length_pattern - 1;
-- 				hard_end := false;
-- 			else
-- 				pattern_end := length_pattern;
-- 				hard_end := true;
-- 			end if;
-- 		end if;

		if pattern(pattern'first) = '*' then -- check pattern beginning
			pattern_start := 2;
			hard_start := false;
			--put_line("--01--");
		else
			pattern_start := 1;
			hard_start := true;
			--put_line("--02--");
		end if;

		if pattern(pattern'last) = '*' then -- check pattern end
			pattern_end := length_pattern - 1;
			hard_end := false;
			--put_line("--03--");
		else
			pattern_end := length_pattern;
			hard_end := true;
			--put_line("--04--");
		end if;

		-- strip '*' from beginning or end (if any)
		pattern_stripped := bounded_slice(to_bounded_string(pattern),pattern_start,pattern_end);
		-- get pattern length
		pattern_stripped_length := to_string(pattern_stripped)'length;
		--put_line("search pat.       : " & to_string(pattern_stripped));
		--put_line("search pat. length: " & natural'image(pattern_stripped_length));

		-- on exact match
		if text = pattern then 
			--put_line("--0--");
			return true; 
		end if;

		-- the pattern to be found, must be shorter than the actual text
		if pattern_stripped_length < length_text then
			-- depending on search pattern (hard_end, hard_start) perform pattern search
			-- and return true to calling program
			if hard_end then 
				if hard_start then -- pattern example: xyz
					-- the pattern is to match the the text 1:1.
					-- NOTE: This code is not reachable, as the 1:1 match is checked for earlier in this function.
					if text = to_string(pattern_stripped) then 
						--put_line("--1--");
						return true; 
					end if;
				else -- pattern example: *yxz
					-- Now we make a character pointer for pattern_stripped, that starts with 1 on the right end.
					-- In other words: what ada.strings.fixed.index returns, must be mirrored.
					scratch := text'length + 1;
					pattern_position := (ada.strings.fixed.index(text,to_string(pattern_stripped)) - scratch) * (-1);
					-- if the pattern is on the very right end the following test results true:
					if pattern_position = pattern_stripped_length then
						-- example: search *yxz in test_string_blaxyz is positive
						--put_line("--2--");
						return true;
					end if;
				end if;
			else
				if hard_start then -- pattern example: xyz*
					-- in this case the pattern must be found on the very left of the text (position 1)
					if ada.strings.fixed.index(text,to_string(pattern_stripped)) = 1 then 
						--put_line("--3--");
						return true; 
					end if; 
				else -- pattern example: *xyz*
					-- first condition: the pattern must not be on the very left. position greater 1.
					if ada.strings.fixed.index(text,to_string(pattern_stripped)) > 1 then
						-- second condition: the pattern must not be on the very right.
						-- Now we make a character pointer for pattern_stripped, that starts with 1 on the right end.
						-- In other words: what ada.strings.fixed.index returns, must be mirrored.
						scratch := text'length + 1;
						pattern_position := (ada.strings.fixed.index(text,to_string(pattern_stripped)) - scratch) * (-1);
						if pattern_position > pattern_stripped_length then
							-- example: search *yxz* in test_string_blaxyz will result false
							-- example: search *yxz* in test_string_blaxyzm will result true
							--put_line("--4--");
							return true; 
						end if;
					end if; 
				end if;
			end if;
		end if;

		-- if search negative, return false to calling program
		return false;
	end search_pattern_in_text;

	procedure print_warning_on_already_existing_file
		-- version 001 / MBL
		(file_name : string := "")
	is
	begin
		new_line;
		put_line("WARNING : Specified file '" & file_name & "' already exists !");
	end print_warning_on_already_existing_file;


	procedure print_error_on_invalid_file
		(file_name	: string := "")
		is
	begin
		new_line;
		put_line("ERROR : Specified file '" & file_name & "' does not exist !");
	end print_error_on_invalid_file;


	function request_user_confirmation
		-- version 002
		-- question_form given as natural determines kind of question addressed to the operator, default is 0
		-- show_confirmation_dialog given as boolean determines if operater request shall be done or not. default is true
		-- 
		-- returns true if user typed y, otherwise false
		( question_form : natural := 0;
		  show_confirmation_dialog : boolean := true)
		return boolean is
		type key_type is (y,n);
		key : key_type;
		c : string (1..1) := "n";
		Previous_Output	: File_Type renames Current_Output; -- ins v002
		Previous_Input	: File_Type renames Current_Input; -- ins v002
		--prog_position : string (1..5) := "RQ001";
		begin
			if show_confirmation_dialog = false then  -- exit with true if dialog is disabled
				set_output(previous_output); -- ins v002
				set_input(previous_input);  -- ins v002
				return true; 
			end if;

			set_output(standard_output);
			set_input(standard_input);
			new_line;
			case question_form is
				when 0 => put("ARE YOU SURE ? (y/n) :");
				when 1 => put("PROCEED ? (y/n) :");
				when 2 => put("EXECUTE ? (y/n) :");
				when 3 => put("OVERWRITE ? (y/n) :");
				when others => put("DO YOU REALLY MIND IT ? (y/n) :");
			end case;
			get(c);
			new_line;

			set_output(previous_output); -- ins v002
			set_input(previous_input);  -- ins v002

			key := key_type'value(c); -- do a type check (y/n)
			if key = y then return true; end if;
			return false;
		end request_user_confirmation;


	function check_space_semicolon
		-- version 001 / MBL
		-- returns given string unchanged if ok, raises constraint_error on occurence of space or semicolon
		( test_string	: string) 
		return string is
		begin
			if Ada.Strings.Fixed.count(test_string," ") > 0 then
				put_line("ERROR : Whitespace not allowed !");
				raise constraint_error;
			end if;

			if Ada.Strings.Fixed.count(test_string,";") > 0 then
				put_line("ERROR : Semikolons not allowed !");
				raise constraint_error;
			end if;
			return test_string;
		end check_space_semicolon;


	function check_date
		-- version 001 / MBL
		-- returns given string unchanged if ok, raises constraint_error on occurence of space or semicolon
		( date_given	: string) 
		return string is
		scratch	: string (1..date_given'length);

		procedure put_date_error is
			begin
				put_line("ERROR : Invalid date format. Required format is: YYYY-MM-DD");
			end put_date_error;

		begin
			scratch := check_space_semicolon(date_given);
			--put_line(scratch);

			-- check length of date
			if date_given'length /= 10 then
				raise constraint_error;
			end if;

			for i in 1..date_given'length
			loop
				case i is
					when 1 => 
						if scratch(i) /= '2' then
							put_date_error;
							raise constraint_error;
						end if;

					when 2 => 
						if scratch(i) /= '0' then
							put_date_error;
							raise constraint_error;
						end if;

					when 3|4| 6|7| 9|10 => 
						if not is_in(scratch(i),number_type) then
							put_date_error;
							raise constraint_error;
						end if;
					when others =>
						if scratch(i) /= '-' then
							put_date_error;
							raise constraint_error;
						end if;
				end case;
			end loop;
			return scratch;
		end check_date;


	function make_filename_by_date
		-- renders a given text (mostly a file name) to BAK_YYYY-MM-DD_HH-MM-SS_file_name
		-- the given text must not exceed a length of 224 characters
		-- version 006 / MBL
		(
		prefix		: string := "BAK_"; -- ins v004
		file_name	: string;
		date_now	: time := clock -- ins v005
		) return string is
		--now		: time := clock; -- rm v005
		now			: time := date_now; -- ins v005
		package file_date_type is new generic_bounded_length(24); use file_date_type; -- BAK_YYYY-MM-DD_HH-MM-SS_
		file_date	: file_date_type.bounded_string;
		--package scratch_type is new generic_bounded_length(40); use scratch_type; -- max length of the name of the bakup file is 40 characters
		--package scratch_type is new generic_bounded_length(60); use scratch_type; -- max length of the name of the bakup file is 60 characters -- rm v006
		package scratch_type is new generic_bounded_length(224); use scratch_type; -- max length of the name of the bakup file is 200 characters -- ins v006
		scratch		: scratch_type.bounded_string;
		begin
			--put_line("prefix    : " & prefix); -- debug ins v006
			--put_line("file_name : " & file_name); -- debug ins v006
			scratch := to_bounded_string( image(now, time_zone => UTC_Time_Offset(now) ) );
			--put_line("date_now  : " & to_string(scratch)); -- debug ins v006
			replace_element(scratch,11,'_');
			replace_element(scratch,14,'-');
			replace_element(scratch,17,'-');
			--scratch := "BAK_" & scratch & "__" & file_name; -- rm v004
			scratch := prefix & scratch & "__" & file_name; -- ins v004
			--put_line("date_now  : " & to_string(scratch)); -- debug ins v006
			return to_string(scratch);
		end make_filename_by_date;


	function Get_Field_Count 
						(
						-- version 1.0 / MBL
						Line	: unbounded_string
						)
						return Natural is

		line_length	:	Natural;					-- length of given line
		char_pt		:	Natural := 1;				-- charcter pointer (points to character being processed inside the given line)
		IFS1		: 	constant Character := ' '; 				-- field separator space
		IFS2		: 	constant Character := Character'Val(9); -- field separator tabulator
		field_ct	:	Natural := 0;				-- field counter (the first field found gets number 1 assigned)
		field_pt	:	Natural := 1;				-- field pointer (points to the charcter being processed inside the current field)
		inside_field:	Boolean := true;			-- true if char_pt points inside a field
		char_current:	Character;					-- holds current character being processed
		char_last	:	Character := ' ';			-- holds character processed previous to char_current

		begin
			--put ("line  : "& Line); new_line;
			--put ("field : "); put (Field); new_line;
			--put ("value : "& Value); new_line;
			line_length:=(Length(Line));
			while char_pt <= line_length
				loop
					--put (char_pt);
					char_current:=(To_String(Line)(char_pt)); 
					if char_current = IFS1 or char_current = IFS2 then
						inside_field := false;
					else
						inside_field := true;
					end if;
	
					-- count fields if character other than IFS found
					if ((char_last = IFS1 or char_last = IFS2) and (char_current /= IFS1 and char_current /= IFS2)) then
						field_ct:=field_ct+1;
					end if;

					-- save last character
					char_last:=char_current;

					-- advance character pointer by one
					char_pt:=char_pt+1; 

					--put (char_current); put (" --"); new_line;
				end loop;
			
			return field_ct;
		end get_field_count;


	function Is_Field	
		(
		-- version 1.0 / MBL
		Line	: unbounded_string;  	-- given line to examine
		Value 	: String ; 				-- given value to be tested for
		Field	: Natural				-- field number to expect value in
		) 
		return Boolean is 

		R			: 	Boolean := false; 			-- on match return true, else return false
		line_length	:	Natural;					-- length of given line
		char_pt		:	Natural := 1;				-- charcter pointer (points to character being processed inside the given line)
		value_length:	Natural;					-- length of given value
		IFS1		: 	constant Character := ' '; 				-- field separator space
		IFS2		: 	constant Character := Character'Val(9); -- field separator tabulator
		field_ct	:	Natural := 0;				-- field counter (the first field found gets number 1 assigned)
		field_pt	:	Natural := 1;				-- field pointer (points to the charcter being processed inside the current field)
		inside_field:	Boolean := true;			-- true if char_pt points inside a field
		char_current:	Character;					-- holds current character being processed
		char_last	:	Character := ' ';			-- holds character processed previous to char_current

		begin
			--put ("line  : "& Line); new_line;
			--put ("field : "); put (Field); new_line;
			--put ("value : "& Value); new_line;
			line_length:=(Length(Line));
			value_length:=(Length(To_Unbounded_String(Value)));
			while char_pt <= line_length
				loop
					--put (char_pt);
					char_current:=(To_String(Line)(char_pt)); 
					if char_current = IFS1 or char_current = IFS2 then
						inside_field := false;
					else
						inside_field := true;
					end if;
	
					-- count fields if character other than IFS found
					if ((char_last = IFS1 or char_last = IFS2) and (char_current /= IFS1 and char_current /= IFS2)) then
						field_ct:=field_ct+1;
					end if;

					if (Field = field_ct) then
						--put ("target field found"); new_line;
						if (inside_field = true) then -- if field entered
							--put ("target field entered"); 

							-- if Value is too short (to avoid constraint error at runtime)
							if field_pt > value_length then
								R := false;
								return R;
							end if;

							-- if character in value matches
							if Value(field_pt) = char_current then
								--put (field_pt); put (Value(field_pt)); new_line;
								field_pt:=field_pt+1;
							else
								-- on first mismatch exit
								--put ("mismatch"); new_line;
								R := false;
								return R;
							end if;

							-- in case the last field matches
							if char_pt = line_length then
								if (field_pt-1) = value_length then
									--put ("match at line end"); new_line;
									R := true;
									return R;
								end if;
							end if;

						else -- once field is left
							if (field_pt-1) = value_length then
								--put ("field left"); new_line;
								R := true;
								return R;
							end if;
						end if;
					end if;
						
					-- save last character
					char_last:=char_current;

					-- advance character pointer by one
					char_pt:=char_pt+1; 

					--put (char_current); put (" --"); new_line;
				end loop;

			R:=false;
			return R;
		end is_field;




	function Get_Field	(
						-- version 2.0 / MBL
						Line	: unbounded_string;
						Field	: Natural;
						IFS		: character := ' '
						)
						return string is

		Value		:	unbounded_string;			-- field content to return (NOTE: Value gets converted to string on return)
		line_length	:	Natural;					-- length of given line
		char_pt		:	Natural := 1;				-- charcter pointer (points to character being processed inside the given line)
-- 		IFS1		: 	constant Character := ' '; 				-- field separator space
-- 		IFS2		: 	constant Character := Character'Val(9); -- field separator tabulator
		IFS1		: 	Character; 					-- field separator space
		IFS2		: 	Character; 					-- field separator tabulator
		field_ct	:	Natural := 0;				-- field counter (the first field found gets number 1 assigned)
		field_pt	:	Natural := 1;				-- field pointer (points to the charcter being processed inside the current field)
		inside_field:	Boolean := true;			-- true if char_pt points inside a field
		char_current:	Character;					-- holds current character being processed
--		char_last	:	Character := ' ';			-- holds character processed previous to char_current
		char_last	:	Character;					-- holds character processed previous to char_current

		begin
			if IFS = ' ' then
				IFS1 := IFS;
				IFS2 := Character'Val(9); -- tabulator
				char_last := ' ';
			else
				IFS1 := IFS;
				IFS2 := IFS;
				char_last := IFS;
			end if;
--			put ("line  : "& Line); new_line;
--			put ("field : "); put (Field); new_line;
--			put ("value : "& Value); new_line;
			line_length:=(Length(Line));
			while char_pt <= line_length
				loop
					--put (char_pt);
					char_current:=(To_String(Line)(char_pt)); 
					if char_current = IFS1 or char_current = IFS2 then
						inside_field := false;
					else
						inside_field := true;
					end if;
	
					-- count fields if character other than IFS found
					if ((char_last = IFS1 or char_last = IFS2) and (char_current /= IFS1 and char_current /= IFS2)) then
						field_ct:=field_ct+1;
					end if;

					if (Field = field_ct) then
						--put ("target field found"); new_line;
						if (inside_field = true) then -- if field entered
							--put ("target field entered"); 
							
							--Value := Value & char_current; -- rm V1.1
							--skip LF -- CS: skip other control characters ?
							if char_current /= Character'Val(10) then Value := Value & char_current; end if; -- ins V1.1
							field_pt:=field_pt+1;
						end if;
					end if;

					if (field_ct > Field) then return to_string(Value); end if;

						
					-- save last character
					char_last:=char_current;

					-- advance character pointer by one
					char_pt:=char_pt+1; 

					--put (char_current); put (" --"); new_line;
				end loop;
			
			return to_string(Value);
		end get_field;


end sm_string_processing;

