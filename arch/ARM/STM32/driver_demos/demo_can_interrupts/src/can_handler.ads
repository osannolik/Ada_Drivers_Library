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

with Ada.Interrupts;
with STM32.CAN;
with System;

package CAN_Handler is
   use STM32.CAN;

   function Tx_Interrupt_Id (Device : not null access CAN_Controller)
                             return Ada.Interrupts.Interrupt_ID;
   function Rx0_Interrupt_Id (Device : not null access CAN_Controller)
                              return Ada.Interrupts.Interrupt_ID;
   function Rx1_Interrupt_Id (Device : not null access CAN_Controller)
                              return Ada.Interrupts.Interrupt_ID;
   function SCE_Interrupt_Id (Device : not null access CAN_Controller)
                              return Ada.Interrupts.Interrupt_ID;

   subtype Index is Natural range 0 .. 7;
   type Msg_Array is array (Index'Range) of CAN_Message;

   type Message_Buffer is record
      Buffer  : Msg_Array;
      Idx_New : Index := 0;
      Idx_Old : Index := 0;
   end record;

   protected type Controller (Device : not null access CAN_Controller) is

      pragma Interrupt_Priority (System.Interrupt_Priority'Last);

      procedure Transmit_Message
        (Message : in     CAN_Message;
         Success :    out Boolean);

      entry Receive_Message
        (Message : out CAN_Message);

      procedure Enable_Receiver
        (Fifo : in Fifo_Nr);

   private

      Tx_Buffer : Message_Buffer;

      Rx_Msg : CAN_Message;
      New_Message : Boolean := False;

      procedure Send;

      procedure IRQ_Handler;
      pragma Attach_Handler (IRQ_Handler, Tx_Interrupt_Id (Device));
      pragma Attach_Handler (IRQ_Handler, Rx0_Interrupt_Id (Device));
      pragma Attach_Handler (IRQ_Handler, Rx1_Interrupt_Id (Device));
      pragma Attach_Handler (IRQ_Handler, SCE_Interrupt_Id (Device));

   end Controller;

private

   Max_Nof_Messages : constant Natural := Index'Last;

   function Nof_Messages (This : Message_Buffer) return Natural;

   procedure Get_Next_Message
     (This    : in out Message_Buffer;
      Message :    out CAN_Message)
    with Pre => Nof_Messages (This) > 0;

   procedure Put_Message
     (This    : in out Message_Buffer;
      Message : in     CAN_Message)
    with Pre => Nof_Messages (This) < Max_Nof_Messages;

end CAN_Handler;
