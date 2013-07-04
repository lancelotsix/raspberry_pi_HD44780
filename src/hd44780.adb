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

package body HD44780 is
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

      Send_4Bits(2#0011#, False); delay 4.1E-3;   -- one
      Send_4Bits(2#0011#, False); delay 100.0E-6; -- two
      Send_4Bits(2#0011#, False);                 -- three
      Send_4Bits(2#0010#, False);                 -- Set 4 bits mode
      Send_8Bits(2#0010_1000#, False);            -- chars are 5x8 pixels
      Send_8Bits(2#0000_1100#, False);            -- set Invisible cursor
      Send_8Bits(2#0000_0001#, False);            -- Clear display
      Send_8Bits(2#0000_0010#, False);            -- Set cursor at home
      Instance.Sem.V;
   end Init;

   ----------------------------------------------------------------------------
   -- Address_For_Cursor
   function Address_For_Cursor (Line : Screen_Height_Range;
                                Row  : Screen_Width_Range) return Int8 is
   begin
      -- Set DDRAM address (where the char will be written)
      -- 16#00# is the first char of the first line
      -- 16#40# is the first of the second line
      return (if Line = 1 then 16#00# else 16#40#) + Int8(Row)  - 1;
   end Address_For_Cursor;

   ----------------------------------------------------------------------------
   -- Put
   procedure Put(Line : Screen_Height_Range;
                 Row  : Screen_Width_Range;
                 Char : Character) is
   begin
      Instance.Sem.P;
      -- To set the cursor, send a command with msb at 1 and the 7 lsb
      -- representing the address
      Send_8Bits(Address_For_Cursor(Line, Row) or 2#1000_0000#, False);
      Send_8Bits(Char_Mapping(Char), True);
      Instance.Sem.V;
   end Put;

   ----------------------------------------------------------------------------
   -- Put
   procedure Put(Line      : Screen_Height_Range;
                 Start_Row : Screen_Width_Range;
                 Msg       : String) is
   begin
      Instance.Sem.P;
      -- Place the cursor;
      Send_8Bits(Address_For_Cursor(Line, Start_Row) or 2#1000_0000#, False);

      for i in Msg'First .. Natural'Min(Msg'Last,
                                        Msg'First + 17-Natural(Start_Row)) loop
         Send_8Bits(Char_Mapping(Msg(i)), True);
      end loop;

      Instance.Sem.V;
   end Put;

   ----------------------------------------------------------------------------
   -- Clear
   procedure Clear is
   begin
      Instance.Sem.P;
      Send_8Bits(2#0000_0001#, False);
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
   -- Send_4Bits
   procedure Send_4Bits(Dat : Int4; Is_Data : Boolean) is
   begin
      -- Register Select; High (True) => Data; Low (False) => Non data
      Instance.Mapping(RS).Set_Value(Is_Data);
      -- We send data, read write bite must be low
      Instance.Mapping(RW).Set_Value(Low);

      Instance.Mapping(D4).Set_Value((Dat and 2#0001#) /= 0); -- lsb
      Instance.Mapping(D5).Set_Value((Dat and 2#0010#) /= 0);
      Instance.Mapping(D6).Set_Value((Dat and 2#0100#) /= 0);
      Instance.Mapping(D7).Set_Value((Dat and 2#1000#) /= 0); -- msb

      delay  40.0E-9;
      Instance.Mapping(E).Set_Value(High);
      delay 230.0E-9;
      Instance.Mapping(E).Set_Value(Low);
      delay 230.0E-9; -- to make sure that at least 500ns elaps before another
                      -- command
   end Send_4Bits;

   ----------------------------------------------------------------------------
   -- Send_8Bits
   procedure Send_8Bits(Dat : Int8; Is_Data : Boolean) is
   begin
      -- Need to send the data into two parts: first the most significant bits
      Send_4Bits(Int4(Dat / 16), Is_Data);
      Send_4Bits(Int4(Dat mod 16), Is_Data);
   end Send_8Bits;

   ----------------------------------------------------------------------------
   -- Finalize
   procedure Finalize(obj : in out HD44780_Controller_Type) is
   begin
      Clear; -- Remove all text present on the screen
      obj.Sem.P;
      for i in obj.Mapping'Range loop
         obj.Mapping(i).Un_Export;
      end loop;
      -- Since no one should be able to use the obj anymore, do not release
      -- the semaphore (which will not exist anymore anyway)
   end Finalize;

end HD44780;
