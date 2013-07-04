--------------------------------------------------------------------------------
--                                                                            --
--                Simple wrapper functions to provide an API                  --
--                         to deal with GPIO ports                            --
--                                                                            --
-- Authors :                                                                  --
--   lancelot@lancelotsix.com                                                 --
--                                                                            --
-- Licence :                                                                  --
--   GPL V3                                                                   --
--                                                                            --

with Ada.Directories;
with Ada.Integer_Text_IO;
with Ada.Sequential_IO;
with Ada.Strings.Fixed;
with Ada.Strings;
with Ada.Text_IO;
with Interfaces.C;
with System;

package body GPIO is
   ----------------------------------------------------------------------------
   -- Helper functions used across the package                               --
   ----------------------------------------------------------------------------
   
   ----------------------------------------------------------------------------
   -- Get_Sysfs_File_Name
   function Get_Sysfs_File_Name (id : GPIO_ID) return String is
      package AS  renames Ada.Strings;
      package ASF renames Ada.Strings.Fixed;

   begin
      -- There should be a better way to use the 'Image func
      return "/sys/class/gpio/gpio" &
        ASF.Trim(GPIO_ID'Image(id), AS.Both);
   end Get_Sysfs_File_Name;

   ----------------------------------------------------------------------------
   -- Write_Command_To_Sysfs
   procedure Write_Command_To_Sysfs (File : String;
                                     Cmd  : String) is
      package IC renames Interfaces.C;

      function c_write (file_path : System.Address;
                        img       : System.Address)
      return Interfaces.C.int;
      Pragma Import (C, c_write, "_c_write_to_file");

      f_path : aliased constant IC.char_array :=
         IC.To_C (File, True);
      img    : aliased constant IC.char_array :=
         IC.To_C (Cmd, True);
      r      : Integer;
   begin
      r := Integer(c_write(f_path'Address, img'Address));

      case r is 
         when 0 =>
	    null; -- everithing OK
         when -1 =>
            raise NOT_AUTHORIZED_EXCEPTION with
              "Access denied to gpio sub system";
         when others =>
            raise GPIO_SUBSTYSTEM_ERROR;
      end case;
   end Write_Command_To_Sysfs;

   procedure Assert_Exported(gpio : GPIO_Type) is
      pragma Inline(Assert_Exported);
   begin
      if not (Is_Exported(gpio)) then
         raise INVALID_GPIO_STATE with
           "Unexported gpio";
      end if;
   end Assert_Exported;
   
   ----------------------------------------------------------------------------
   -- Implementation of interface                                            --
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   -- Is_Valid_GPIO_ID
   function Is_Valid_GPIO_ID (n : GPIO_ID) return Boolean is
      package AD renames Ada.Directories;

      procedure Determine_Chip_Range (Chip_Ctrl_Path : String;
                                      base : out Natural;
                                      last : out Natural) is
         package ATI renames Ada.Text_IO;
         package Id_IO is new Ada.Text_IO.Integer_IO (GPIO_ID);
         package Nat_IO is new Ada.Text_IO.Integer_IO (Natural);

         F      : ATI.File_Type;
         NbGpio : Natural;
      begin
         ATI.Open (F, ATI.In_File, Chip_Ctrl_Path & "/base");
         Nat_IO.Get (f, base);
         ATI.Close (F);
         ATI.Open (F, ATI.In_File, Chip_Ctrl_Path & "/ngpio");
         Nat_IO.Get (f, NbGpio);
         ATI.Close (F);
         Last := Base + NbGpio - 1;
      end Determine_Chip_Range;

      dir_search : AD.Search_Type;
      Filter     : constant AD.Filter_Type := (AD.Ordinary_File => False,
                                               AD.Special_File => False,
                                               AD.Directory => True);
      Dir_Entry  : AD.Directory_Entry_Type;
      Found_Chip : Boolean := False;
      Base       : Natural;
      Last       : Natural;
   begin
      AD.Start_Search ( Search    => dir_search,
                       Directory => "/sys/class/gpio",
                       Pattern   => "gpiochip*",
                       Filter    => Filter);

      while Not (Found_Chip ) and AD.More_Entries ( dir_search ) loop
         AD.Get_Next_Entry (dir_search, Dir_Entry);
         Determine_Chip_Range ( AD.Full_Name (Dir_Entry), Base, Last);
         Found_Chip := (Integer(n) >= Base and Integer(n) <= Last);
      end loop;
      AD.End_Search ( dir_search );
      return Found_Chip;
   end Is_Valid_GPIO_ID;

   ----------------------------------------------------------------------------
   -- Export
   procedure Export (gpio : out GPIO_Type;
                     id   : GPIO_ID) is
      package AS  renames Ada.Strings;
      package ASF renames Ada.Strings.Fixed;
      package IC  renames Interfaces.C;

   begin
      if not (Is_Valid_GPIO_ID(id)) then
         raise INVALID_GPIO_ID_ERROR with
           GPIO_ID'Image(id) & " is not a valid GPIO id on this system";
      end if;
      
      Write_Command_To_Sysfs (File =>"/sys/class/gpio/export",
                              Cmd  => ASF.Trim (GPIO_ID'Image(id), AS.Both));
      gpio.Id       := id;
      gpio.Exported := True;
   end Export;

   ----------------------------------------------------------------------------
   -- Is_Exported
   function Is_Exported (gpio : GPIO_Type) return Boolean is
   begin
      return gpio.Exported;
   end Is_Exported;

   ----------------------------------------------------------------------------
   -- Is_Exported
   function Is_Exported (id : GPIO_ID) return Boolean is
      package AD renames Ada.Directories;

   begin
      return AD.Exists (Get_Sysfs_File_Name (id));
   end Is_Exported;

   ----------------------------------------------------------------------------
   -- Un_Export
   procedure Un_Export (gpio : in out GPIO_Type) is
      package ASF renames Ada.Strings.Fixed;
      package AS  renames Ada.Strings;

   begin
      Write_Command_To_Sysfs ("/sys/class/gpio/unexport",
                              ASF.Trim (GPIO_ID'Image(gpio.id), AS.Both));
      gpio.Exported := False;
   end Un_Export;

   ----------------------------------------------------------------------------
   -- Set_Direction
   procedure Set_Direction (gpio : in out GPIO_Type;
                            Dir  : Gpio_Direction) is
      package ASF renames Ada.Strings.Fixed;
      package AS  renames Ada.Strings;

   begin
      Assert_Exported(gpio);
      
      case Dir is
         when gpio_in  =>
            Write_Command_To_Sysfs (gpio.Get_Sysfs_File_Name & "/direction",
                                    "in");
	 when gpio_out =>
            Write_Command_To_Sysfs (gpio.Get_Sysfs_File_Name & "/direction",
                                    "out");
      end case;
      gpio.Direction := Dir;
   end Set_Direction;

   ----------------------------------------------------------------------------
   -- Get_Direction
   function Get_Direction (gpio : GPIO_Type) return GPIO_Direction is
   begin
      Assert_Exported(gpio);
      
      return gpio.Direction;
   end Get_Direction;

   ----------------------------------------------------------------------------
   -- Set_Value
   procedure Set_Value (gpio  : GPIO_Type;
                        value : GPIO_Value) is
      package ASF renames Ada.Strings.Fixed;
      package AS  renames Ada.Strings;

   begin
      Assert_Exported(gpio);
      
      Write_Command_To_Sysfs (gpio.Get_Sysfs_File_Name & "/value",
                              ASF.Trim (Integer'Image(GPIO_Value'Pos (value)),
                                        AS.Both));
   end Set_Value;
   procedure Set_Value (gpio  : GPIO_Type;
                        value : Boolean) is
   begin
      gpio.Set_Value(if value then High else Low);
   end Set_Value;

   ----------------------------------------------------------------------------
   -- Get_Value
   function Get_Value (gpio : GPIO_Type) return GPIO_Value is
      package ATI renames Ada.Text_IO;
      package AIO renames Ada.Integer_Text_IO;

      fd      : ATI.File_Type;
      raw_val : Integer;
      ret     : GPIO_Value;

   begin
      Assert_Exported(gpio);
      
      ATI.Open (fd, ATI.In_File,
                gpio.Get_Sysfs_File_Name & "/value");
      AIO.Get (fd, raw_val);
      ret := GPIO_Value'Val (raw_val);
      ATI.Close (fd);

      return ret;
   end Get_Value;

   ----------------------------------------------------------------------------
   -- Finalize
   procedure Finalize (gpio : in out GPIO_TYPE) is
   begin
      if gpio.Is_Exported then
         gpio.Un_Export;
      end if;
   end Finalize;

   ----------------------------------------------------------------------------
   -- Get_Sysfs_File_Name
   function Get_Sysfs_File_Name (gpio : GPIO_TYPE) return String is
   begin
      return Get_Sysfs_File_Name (gpio.id);
   end Get_Sysfs_File_Name;

end GPIO;
