--------------------------------------------------------------------------------
--                                                                            --
--                    HD44780 controlls a simple lcd display                  --
-- with 2 lines and 16 chars on each lines. Even if this package provides a   --
-- slow solution, it is functionnal !                                         --
--                                                                            --
-- Authors :                                                                  --
--   lancelot@lancelotsix.com                                                 --
--                                                                            --
-- Licence :                                                                  --
--   GPL V3                                                                   --
--                                                                            --

with GPIO;
with Ada.Finalization;
with Interfaces;

package HD44780 is
   type Pin_Type is (
                     RS,         -- Sync
                     RW,         -- Read/Write
                     E,          -- Execute
                     D4,D5,D6,D7 -- data bus (D4 is LSB, D7 is MDB)
                    );

   type HD44780_Pin_Layout_Type is array(Pin_Type) of Natural;
   type Screen_Height_Range is new Natural range 1..2;
   type Screen_Width_Range  is new Natural range 1..16;

   procedure Init(Layout : HD44780_Pin_Layout_Type);
   -- Initialize the screen.
   -- In the layout, each value must be a valid GPIO_Id. The corresponding
   -- pin must be mapped to the corresponding pin on the controller

   procedure Put(Line : Screen_Height_Range;
                 Row  : Screen_Width_Range;
                 Char : Character);
   -- Put a char at the specified position

   procedure Put(Line      : Screen_Height_Range;
                 Start_Row : Screen_Width_Range;
                 Msg       : String);
   -- Print a message on the line, starting at the desired line

   procedure Clear;
   -- remove all chars from the screen

private
   type HD44780_Pin_Mapping_Type is array(Pin_Type) of GPIO.GPIO_Type;
   type Int4 is mod 2**4;
   type Int8 is mod 2**8;

   protected type Semaphore(Init_Val : Natural) is
      entry P;
      procedure V;
   private
      Val : Natural := Init_Val;
   end Semaphore;

   -- To clear everithing at the end, use a controlled type which will be
   -- reclamed when the package goes out of scope
   type HD44780_Controller_Type is new Ada.Finalization.Limited_Controlled with
      record
         Sem     : Semaphore(0);
         Mapping : HD44780_Pin_Mapping_Type;
      end record;
   overriding
   procedure Finalize(obj : in out HD44780_Controller_Type);
   Instance : HD44780_Controller_Type;

   procedure Send_4Bits(Dat : Int4; Is_Data : Boolean);
   procedure Send_8Bits(Dat : Int8; Is_Data : Boolean);

   type Char_Rep_Map is array (Character) of Int8;
   Char_Mapping : constant Char_Rep_Map :=
     Char_Rep_Map'(
                   ' ' => 16#20#,
                   '!' => 16#21#,
                   '"' => 16#22#,
                   '#' => 16#23#,
                   '$' => 16#24#,
                   '%' => 16#25#,
                   '&' => 16#26#,
                   ''' => 16#27#,
                   '(' => 16#28#,
                   ')' => 16#29#,
                   '*' => 16#2A#,
                   '+' => 16#2B#,
                   ',' => 16#2C#,
                   '-' => 16#2D#,
                   '.' => 16#2E#,
                   '/' => 16#2F#,

                   '0' => 16#30#,
                   '1' => 16#31#,
                   '2' => 16#32#,
                   '3' => 16#33#,
                   '4' => 16#34#,
                   '5' => 16#35#,
                   '6' => 16#36#,
                   '7' => 16#37#,
                   '8' => 16#38#,
                   '9' => 16#39#,
                   ':' => 16#3A#,
                   ';' => 16#3B#,
                   '<' => 16#3C#,
                   '=' => 16#3D#,
                   '>' => 16#3E#,
                   '?' => 16#3F#,

                   '@' => 16#40#,
                   'A' => 16#41#,
                   'B' => 16#42#,
                   'C' => 16#43#,
                   'D' => 16#44#,
                   'E' => 16#45#,
                   'F' => 16#46#,
                   'G' => 16#47#,
                   'H' => 16#48#,
                   'I' => 16#49#,
                   'J' => 16#4A#,
                   'K' => 16#4B#,
                   'L' => 16#4C#,
                   'M' => 16#4D#,
                   'N' => 16#4E#,
                   'O' => 16#4F#,

                   'P' => 16#50#,
                   'Q' => 16#51#,
                   'R' => 16#52#,
                   'S' => 16#53#,
                   'T' => 16#54#,
                   'U' => 16#55#,
                   'V' => 16#56#,
                   'W' => 16#57#,
                   'X' => 16#58#,
                   'Y' => 16#59#,
                   'Z' => 16#5A#,
                   '[' => 16#5B#,

                   ']' => 16#5D#,
                   '^' => 16#5E#,
                   '_' => 16#5F#,

                   '`' => 16#60#,
                   'a' => 16#61#,
                   'b' => 16#62#,
                   'c' => 16#63#,
                   'd' => 16#64#,
                   'e' => 16#65#,
                   'f' => 16#66#,
                   'g' => 16#67#,
                   'h' => 16#68#,
                   'i' => 16#69#,
                   'j' => 16#6A#,
                   'k' => 16#6B#,
                   'l' => 16#6C#,
                   'm' => 16#6D#,
                   'n' => 16#6E#,
                   'o' => 16#6F#,

                   'p' => 16#70#,
                   'q' => 16#71#,
                   'r' => 16#72#,
                   's' => 16#73#,
                   't' => 16#74#,
                   'u' => 16#75#,
                   'v' => 16#76#,
                   'w' => 16#77#,
                   'x' => 16#78#,
                   'y' => 16#79#,
                   'z' => 16#7A#,

                   others => 16#20# -- unsupported chars are displayer widh ' '
                  );

end HD44780;
