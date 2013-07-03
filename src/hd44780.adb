--------------------------------------------------------------------------------
--                                                                            --
--                                  HD447890                                  --
--                                                                            --
-- Authors :                                                                  --
--   lancelot@lancelotsix.com                                                 --
--                                                                            --
-- Licence :                                                                  --
--   GPL V3                                                                   --
--                                                                            --
--                                                                            --
-- Specs of the display are taken from http://fr.wikipedia.org/wiki/HD44780   --
--                                                                            --

with GPIO; use GPIO;
with Ada.Text_IO;

package body HD44780 is

   ----------------------------------------------------------------------------
   --                               Helpers                                  --
   ----------------------------------------------------------------------------
   procedure Send_Data(Data_Trame : Data_Trame_Type; Char_Mde : Boolean) is
      pragma Inline(Send_Data);
   begin
      -- This procedure is only called inside the module. The caller is in
      -- charge of aquiring the semaphore, no check is done here!

      -- Set RS
      if Char_Mde then
         Instance.Mapping(RS).Set_Value(High);
      else
         Instance.Mapping(RS).Set_Value(Low);
      end if;

      -- Set rw (write operation)
      Instance.Mapping(RW).Set_Value(Low);
      delay 4.0E-9;

      -- RS and RW read on high front of D
      Instance.Mapping(E).Set_Value(High);

      -- Specify the data_trame
      for i in D4..D7 loop
         Instance.Mapping(i).Set_Value(Data_Trame(i));
      end loop;

      delay 80.0E-9;
      Instance.Mapping(E).Set_Value(Low);

      -- Reset everything
      for i in Pin_Type'Range loop
         Instance.Mapping(i).Set_Value(Low);
      end loop;

      delay 500.0E-9; -- 500 nano s before E can change
   end Send_Data;

   ----------------------------------------------------------------------------
   --                   Implement the public interface                       --
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   -- Init
   procedure Init(Layout : HD44780_Pin_Layout_Type) is
   begin
      for i in Layout'Range loop
         Instance.Mapping(i).Export(GPIO_ID(Layout(i)));
         Instance.Mapping(i).Set_Direction(gpio_out);
      end loop;

      -- one
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => Low,
                                   D6 => Low,
                                   D5 => High,
                                   D4 => High),
                Char_Mde => False);
      delay 4.1E-3;

      -- two
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => Low,
                                   D6 => Low,
                                   D5 => High,
                                   D4 => High),
                Char_Mde => False);
      delay 100.0E-6;

      -- three
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => Low,
                                   D6 => Low,
                                   D5 => High,
                                   D4 => High),
                Char_Mde => False);

      -- Set 4 bits mode
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => Low,
                                   D6 => Low,
                                   D5 => High,
                                   D4 => Low),
                Char_Mde => False);

      -- chars are 5x8 pixels
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => Low,
                                   D6 => Low,
                                   D5 => High,
                                   D4 => Low),
                Char_Mde => False);
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => High,
                                   D6 => Low,
                                   D5 => Low,
                                   D4 => Low),
                Char_Mde => False);

      -- set cursor blinking
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => Low,
                                   D6 => Low,
                                   D5 => Low,
                                   D4 => Low),
                Char_Mde => False);
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => High,
                                   D6 => High,
                                   D5 => Low,
                                   D4 => Low),
                Char_Mde => False);

      -- Clear display
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => Low,
                                   D6 => Low,
                                   D5 => Low,
                                   D4 => Low),
                Char_Mde => False);
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => Low,
                                   D6 => Low,
                                   D5 => Low,
                                   D4 => High),
                Char_Mde => False);

      -- Set cursor at home (first char of first line)
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => Low,
                                   D6 => Low,
                                   D5 => Low,
                                   D4 => Low),
                Char_Mde => False);
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => Low,
                                   D6 => Low,
                                   D5 => High,
                                   D4 => Low),
                Char_Mde => False);

      Instance.Sem.V;
   end Init;

   ----------------------------------------------------------------------------
   -- Put
   procedure Put(Line : Screen_Height_Range;
                 Row  : Screen_Width_Range;
                 Char : Character) is
      add       : Int8;
      rep_add   : Int8_Rep := (others=> Low);
      rep_val   : Int8_Rep := (others=> Low);
   begin
      if Line = 1 then
         add := 16#00#;
      else
         add := 16#40#;
      end if;
      add := add + Int8(Row) - 1;
      ConvertInt8ToBites(add, rep_add);
      ConvertInt8ToBites(Char_Mapping(Char), rep_val);

      -- Critical section for concurency
      Instance.Sem.P;

      -- Set DDRAM address (where the char will be written)
      -- 16#00# is the first char of the first line
      -- 16#40# is the first of the second line
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => High,
                                   D6 => rep_add(6),
                                   D5 => rep_add(5),
                                   D4 => rep_add(4)),
                Char_Mde => False);
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => rep_add(3),
                                   D6 => rep_add(2),
                                   D5 => rep_add(1),
                                   D4 => rep_add(0)),
                Char_Mde => False);

      -- Send the code of the first char. 4 most significant bits first
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => rep_val(7),
                                   D6 => rep_val(6),
                                   D5 => rep_val(5),
                                   D4 => rep_val(4)),
                Char_Mde => True);
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => rep_val(3),
                                   D6 => rep_val(2),
                                   D5 => rep_val(1),
                                   D4 => rep_val(0)),
                Char_Mde => True);

      Instance.Sem.V;
   end Put;

   ----------------------------------------------------------------------------
   -- Put
   procedure Put(Line      : Screen_Height_Range;
                 Start_Row : Screen_Width_Range;
                 Msg       : String) is
      Idx : Natural := Natural(Start_Row);
   begin
      while Idx <= Natural(Screen_Width_Range'Last) and
        (Idx+Msg'First-1 <= Msg'Last) loop
         Put(Line, Screen_Width_Range(Idx), Msg(Idx+Msg'First-1));
         Idx := Idx + 1;
      end loop;

   end Put;

   ----------------------------------------------------------------------------
   -- Clear
   procedure Clear is
   begin
      Instance.Sem.P;
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => Low,
                                   D6 => Low,
                                   D5 => Low,
                                   D4 => Low),
                Char_Mde => False);
      Send_Data(Data_Trame =>
                  Data_Trame_Type'(D7 => Low,
                                   D6 => Low,
                                   D5 => Low,
                                   D4 => High),
                Char_Mde => False);
      Instance.Sem.V;
   end Clear;

   ----------------------------------------------------------------------------
   --               Private implementation for the package                   --
   ----------------------------------------------------------------------------
   protected body Semaphore is

      entry P when Val > 0 is
      begin
         Val := Val - 1;
      end P;

      procedure V is
      begin
         Val := Val + 1;
      end V;

   end Semaphore;

   ----------------------------------------------------------------------------
   -- ConvertInt8ToBites
   procedure ConvertInt8ToBites(val : Int8;
                                Rep : out Int8_Rep) is
      pragma Inline(ConvertInt8ToBites);
      tmp : Int8 := val;
   begin
      for i in 0..7 loop
         if (tmp mod 2) = 1 then
            Rep(i) := High;
         else
            Rep(i) := Low;
         end if;
         tmp := tmp / 2;
      end loop;
   end ConvertInt8ToBites;

   ----------------------------------------------------------------------------
   -- Finalize
   procedure Finalize(obj : in out HD44780_Controller_Type) is
   begin
      Clear;

      obj.Sem.P;
      for i in obj.Mapping'Range loop
         obj.Mapping(i).Un_Export;
      end loop;

      -- Since no one should be able to use the obj anymore, do not release
      -- the semaphore (which will not exist anymore anyway)
   end Finalize;

end HD44780;
