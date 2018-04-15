------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2018, AdaCore                         --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

--  The file declares the main procedure for the demonstration.

with STM32.Device; use STM32.Device;
with STM32.GPIO;   use STM32.GPIO;
with STM32.CAN;    use STM32.CAN;
with HAL;
with Ada.Real_Time;

procedure Demo_CAN_Basic is

   Controller : CAN_Controller renames CAN_1;

   Green_LED  : GPIO_Point renames PD12;
   Orange_LED : GPIO_Point renames PD13;
   Red_LED    : GPIO_Point renames PD14;
   Blue_LED   : GPIO_Point renames PD15;

   Pattern : GPIO_Points := (Orange_LED, Red_LED, Blue_LED, Green_LED);

   procedure Initialize_LEDs;
   --  Enable the clock for the four LEDs and configure them as outputs. Note
   --  there is a procedure defined in STM32.Board to do this.

   procedure Initialize_CAN;
   --  Enable clock and configure the CAN module and the tx/rx pins.

   procedure Initialize_LEDs is
   begin
      Enable_Clock (Pattern);
      Configure_IO (Pattern, (Mode_Out, Floating, Push_Pull, Speed_100MHz));
   end Initialize_LEDs;

   procedure Initialize_CAN is
      Tx_Pin : GPIO_Point renames PD1;
      Rx_Pin : GPIO_Point renames PD0;

      --  Set to 125 kbit/s
      --  see http://www.bittiming.can-wiki.info/#bxCAN
      Bit_Timing : constant Bit_Timing_Config :=
         (Resynch_Jump_Width => 1,
          Time_Segment_1     => 13,
          Time_Segment_2     => 2,
          Quanta_Prescaler   => 21);

      Identifier : constant Filter_32 :=
         (Std_ID => 16#14#,
          Ext_ID => 0,
          Ide    => False,
          Rtr    => False);

      Mask : constant Filter_32 :=
         (Std_ID => Standard_Id'Last,
          Ext_ID => 0,
          Ide    => True,
          Rtr    => True);

      Bank_Config : constant CAN_Filter_Bank :=
         (Bank_Nr         => 0,
          Activated       => True,
          Fifo_Assignment => FIFO_0,
          Filters         => (Mask32, (Identifier, Mask)));
   begin
      Enable_Clock (Points => (Tx_Pin, Rx_Pin));
      Configure_IO (Rx_Pin, (Mode_AF, Pull_Up,  Push_Pull, Speed_50MHz, GPIO_AF_CAN1_9));
      Configure_IO (Tx_Pin, (Mode_AF, Floating, Push_Pull, Speed_50MHz, GPIO_AF_CAN1_9));

      Enable_Clock (Controller);

      Reset (Controller);

      Configure
         (This                => Controller,
          Mode                => Loopback,
          Time_Triggered      => False,
          Auto_Bus_Off        => False,
          Auto_Wakeup         => False,
          Auto_Retransmission => False,
          Rx_FIFO_Locked      => False,
          Tx_FIFO_Prio        => False,
          Timing_Config       => Bit_Timing);

      Set_Slave_Start_Bank (14);

      Configure_Filter
         (This        => Controller,
          Bank_Config => Bank_Config);
   end Initialize_CAN;

   use Ada.Real_Time;
   use type HAL.UInt8;

   Msg_Tx : CAN_Message :=
      (Std_ID => 16#14#, --  Same as required by filter
       Ext_ID => 2,
       Ide    => False,  --  Same as required by filter
       Rtr    => False,  --  Same as required by filter
       Dlc    => 4,
       Data   => (1, 0, 0, 0, others => 0));

   Msg_Rx : CAN_Message;

   Next_Release : Time := Clock;
   Success : Boolean;
begin

   Initialize_LEDs;
   Initialize_CAN;

   while True loop
      --  Request to send the message and wait until success
      Transmit_Message
         (This    => Controller,
          Message => Msg_Tx,
          Success => Success);

      if not Success then
         raise Program_Error with "Transmit failure";
      end if;

      --  Wait for a response
      Receive_Message
         (This    => Controller,
          Fifo    => FIFO_0,
          Message => Msg_Rx,
          Success => Success);

      if not Success then
         raise Program_Error with "Receive failure";
      end if;

      --  Turn on leds based on received data
      for Led in 0 .. 3 loop
         if Msg_Rx.Data (Led) > 0 then
            Pattern (Led + 1).Set;
         else
            Pattern (Led + 1).Clear;
         end if;
      end loop;

      --  Shift the received data and send it again...
      Msg_Tx.Data (0 .. 3) := Msg_Rx.Data (3) & Msg_Rx.Data (0 .. 2);

      Next_Release := Next_Release + Milliseconds (100);
      delay until Next_Release;
   end loop;

end Demo_CAN_Basic;
