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
with Ada.Finalization;

package GPIO is

   -- GPIO are identified by regular naural number
   type GPIO_ID is new Natural;

   -- GPIO pins are used to send data out or get data in.
   type GPIO_Direction is (gpio_in, gpio_out);

   -- A GPIO pin can have one of the two values : High (1, which
   -- means non zero voltage) and Low (0 or 0 voltage)
   type GPIO_Value is (Low, High);
   for GPIO_Value'Size use 1;
   for GPIO_Value use (
      Low  => 0,
      High => 1
   );

   type GPIO_Type is new Ada.Finalization.Limited_Controlled with private;

   -- Some exception that could occur
   NOT_AUTHORIZED_EXCEPTION : Exception;
   GPIO_SUBSTYSTEM_ERROR    : Exception;
   INVALID_GPIO_ID_ERROR    : Exception;
   INVALID_GPIO_STATE       : Exception;

   -- Tels if the given GPIO_ID is present on the current
   -- system.
   function Is_Valid_GPIO_ID (n : GPIO_ID) return Boolean;

   -- Initialize the GPIO and ask access to it via the system
   procedure Export (gpio : out GPIO_Type;
                     id   : GPIO_ID);

   -- Tels weather we are granted by the system to access the
   -- gpio port
   function Is_Exported (gpio : GPIO_Type) return Boolean;
   function Is_Exported (id : GPIO_ID) return Boolean;

   -- Tell the system that the GPIO port is not in use anymore
   procedure Un_Export (gpio : in out GPIO_Type);
   --with Precondition => gpio.Is_Exported;

   -- Specify the direction of the port.
   procedure Set_Direction (gpio : in out GPIO_Type;
                            dir  : GPIO_Direction);

   -- Return the direction of the GPIO port
   function Get_Direction (gpio : GPIO_Type) return GPIO_Direction;

   procedure Set_Value (gpio  : GPIO_Type;
                        value : GPIO_Value);

   function Get_Value (gpio : GPIO_Type) return GPIO_Value;

   -- make sure to release the resources when removing object
   overriding
   procedure Finalize (gpio : in out GPIO_Type);

private
   type GPIO_Type is new Ada.Finalization.Limited_Controlled with
      record
         Id        : GPIO_ID;
         Exported  : Boolean := False;
         Direction : GPIO_Direction;
      end record;

   function Get_Sysfs_File_Name (gpio : GPIO_TYPE) return String;
   --with Precondition => gpio.Is_Exported;

end GPIO;
