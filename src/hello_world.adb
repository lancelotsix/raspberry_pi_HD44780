--------------------------------------------------------------------------------
--                                                                            --
--                            Hello world program                             --
--                                                                            --
-- Demonstrate the use of the HD44780 library                                 --
--                                                                            --
-- Authors :                                                                  --
--   lancelot@lancelotsix.com                                                 --
--                                                                            --
-- Licence :                                                                  --
--   GPL V3                                                                   --
--                                                                            --

with HD44780; use HD44780;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Calendar; use Ada.Calendar;

procedure Hello_World is
   Mapping : constant HD44780_Pin_Layout_Type :=
     HD44780_Pin_Layout_Type'(
                              RS =>25,
                              RW => 23,
                              E  => 24,
                              D4 => 22,
                              D5 => 18,
                              D6 => 17,
                              D7 => 4);

   ----------------------------------------------------------------------------
   -- We use two concurrent tasks to display messages on the screen.
   -- One (The Cound_Down task) tels approximatly how many time is remaning
   -- before we stop the application, the other (Show_My_Mail) show an email
   -- on the first line and activate a vertical scroll if it is too large for
   -- the 16 char display.
   -- THOSE ARE NOT STRONG REAL TIME ! Count_Down needs more than 30s to
   -- reach 0, and then it waits for Show_My_Mail to finish what it is
   -- currently doing and join the rendezvous
   --
   -- Furthemore, since we rely on sysfs to access GPIO (which is deadly slow
   -- be prepared for low performances !
   ----------------------------------------------------------------------------
   task Count_Down is
      entry Finished;
   end Count_Down;

   task Show_My_Mail is
      entry Start;
   end Show_My_Mail;

   ----------------------------------------------------------------------------
   task body Count_Down is
      Cpt : Natural := 30;
      Start_time : Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      while Cpt > 0 loop
            HD44780.Put(2, 1, "Quit in " &
                          Ada.Strings.Fixed.Trim(Natural'Image(Cpt),
                          Ada.Strings.Both)
                        & "s ");
         Cpt := Cpt - 1;
         delay Start_time + Duration(30-cpt) - Ada.Calendar.Clock;
      end loop;

      accept Finished do
         null;
      end Finished;

      HD44780.Clear;
      HD44780.Put(1, 1, "BOUUM !!");
      delay Start_time + Duration(35) - Ada.Calendar.Clock;
   end Count_Down;

   task body Show_My_Mail is
      mail        : constant String := "lancelot@lancelotsix.com";
      curr_fst    : Natural := mail'First;
      moving_left : Boolean := True;
      Stop        : Boolean := False;
   begin
      accept Start do
         null;
      end Start;

      while not(Stop) loop
         select
            Count_Down.Finished;
            Stop := True;
         or
            delay 0.25;

            -- Count down not finished, display mail
            HD44780.Put(1, 1, mail(curr_fst..curr_fst+15));
            if moving_left and
              -- to the left and still data to show after
              curr_fst+16 <= mail'Last then
               curr_fst := curr_fst + 1;
            elsif moving_left then
               -- reached the end, go in the other direction
               moving_left := false;
            elsif not (moving_left) and
              curr_fst > 1 then
               curr_fst := curr_fst - 1;
            elsif not (moving_left) then
               moving_left := true;
            end if;
         end select;
      end loop;
   end Show_My_Mail;

begin
   HD44780.Init(Layout => Mapping);

   HD44780.Put(1, 1, "Hello world ada!");
   delay 5.2;
   Show_My_Mail.Start;

end Hello_World;
