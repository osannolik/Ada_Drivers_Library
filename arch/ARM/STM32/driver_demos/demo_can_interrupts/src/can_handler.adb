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

with HAL;
with STM32.Device;
with Ada.Interrupts.Names;

package body CAN_Handler is

   function Tx_Interrupt_Id
     (Device : not null access CAN_Controller)
      return Ada.Interrupts.Interrupt_ID
   is
   begin
      if Device = STM32.Device.CAN_1'Access then
         return Ada.Interrupts.Names.CAN1_TX_Interrupt;
      elsif Device = STM32.Device.CAN_2'Access then
         return Ada.Interrupts.Names.CAN2_TX_Interrupt;
      else
         raise Constraint_Error;
      end if;
   end Tx_Interrupt_Id;

   function Rx0_Interrupt_Id
     (Device : not null access CAN_Controller)
      return Ada.Interrupts.Interrupt_ID
   is
   begin
      if Device = STM32.Device.CAN_1'Access then
         return Ada.Interrupts.Names.CAN1_RX0_Interrupt;
      elsif Device = STM32.Device.CAN_2'Access then
         return Ada.Interrupts.Names.CAN2_RX0_Interrupt;
      else
         raise Constraint_Error;
      end if;
   end Rx0_Interrupt_Id;

   function Rx1_Interrupt_Id
     (Device : not null access CAN_Controller)
      return Ada.Interrupts.Interrupt_ID
   is
   begin
      if Device = STM32.Device.CAN_1'Access then
         return Ada.Interrupts.Names.CAN1_RX1_Interrupt;
      elsif Device = STM32.Device.CAN_2'Access then
         return Ada.Interrupts.Names.CAN2_RX1_Interrupt;
      else
         raise Constraint_Error;
      end if;
   end Rx1_Interrupt_Id;

   function SCE_Interrupt_Id
     (Device : not null access CAN_Controller)
      return Ada.Interrupts.Interrupt_ID
   is
   begin
      if Device = STM32.Device.CAN_1'Access then
         return Ada.Interrupts.Names.CAN1_SCE_Interrupt;
      elsif Device = STM32.Device.CAN_2'Access then
         return Ada.Interrupts.Names.CAN2_SCE_Interrupt;
      else
         raise Constraint_Error;
      end if;
   end SCE_Interrupt_Id;

   function Nof_Messages (This : Message_Buffer) return Natural
   is
   begin
      if This.Idx_New >= This.Idx_Old then
         return This.Idx_New - This.Idx_Old;
      else
         return Index'Last - This.Idx_Old + 1 + This.Idx_New - Index'First;
      end if;
   end Nof_Messages;

   procedure Get_Next_Message
     (This    : in out Message_Buffer;
      Message :    out CAN_Message)
   is
   begin
      Message := This.Buffer (This.Idx_Old);
      This.Idx_Old := (This.Idx_Old + 1) mod (Index'Last + 1);
   end Get_Next_Message;

   procedure Put_Message
     (This    : in out Message_Buffer;
      Message : in     CAN_Message)
   is
      Idx_Next : constant Index := (This.Idx_New + 1) mod (Index'Last + 1);
   begin
      This.Buffer (This.Idx_New) := Message;
      This.Idx_New := Idx_Next;
   end Put_Message;

   protected body Controller is

      procedure Transmit_Message
        (Message : in     CAN_Message;
         Success :    out Boolean)
      is
      begin
         Success := Nof_Messages (Tx_Buffer) < Max_Nof_Messages;
         if Success then
            Put_Message (Tx_Buffer, Message);
            Send;
         end if;
      end Transmit_Message;

      entry Receive_Message
        (Message : out CAN_Message) when New_Message
      is
      begin
         Message := Rx_Msg;
         New_Message := False;
      end Receive_Message;

      procedure Enable_Receiver
        (Fifo : in Fifo_Nr)
      is
         CAN : CAN_Controller renames Device.all;
      begin
         case Fifo is
            when FIFO_0 =>
               Enable_Interrupts (CAN, FIFO_0_Message_Pending);
            when FIFO_1 =>
               Enable_Interrupts (CAN, FIFO_1_Message_Pending);
         end case;
      end Enable_Receiver;

      procedure Send is
         CAN : CAN_Controller renames Device.all;
      begin
         while Nof_Messages (Tx_Buffer) > 0 loop
            declare
               Message : CAN_Message;
               Empty_Found : Boolean;
               Mailbox : Mailbox_Type;
            begin
               Get_Empty_Mailbox (CAN, Mailbox, Empty_Found);

               exit when not Empty_Found;

               Get_Next_Message (Tx_Buffer, Message);
               Write_Tx_Message (CAN, Message, Mailbox);
               Enable_Interrupts (CAN, Transmit_Mailbox_Empty);
               Transmission_Request (CAN, Mailbox);
            end;
         end loop;
      end Send;

      procedure IRQ_Handler is
         use HAL;
         CAN : CAN_Controller renames Device.all;
      begin
         if Interrupt_Enabled (CAN, Transmit_Mailbox_Empty) then
            if Transmission_Completed (CAN, Mailbox_0) or else
               Transmission_Completed (CAN, Mailbox_1) or else
               Transmission_Completed (CAN, Mailbox_2)
            then
               --  Either successful or aborted or failed
               Disable_Interrupts (CAN, Transmit_Mailbox_Empty);
               Send;
            end if;
         end if;

         if Interrupt_Enabled (CAN, FIFO_0_Message_Pending) and then
            Nof_Msg_In_Fifo (CAN, FIFO_0) > 0
         then
            --  Get message from fifo 0
            Rx_Msg := Read_Rx_Message (CAN, FIFO_0);
            Release_Fifo (CAN, FIFO_0);
            New_Message := True;

         elsif Interrupt_Enabled (CAN, FIFO_1_Message_Pending) and then
               Nof_Msg_In_Fifo (CAN, FIFO_1) > 0
         then
            --  Get message from fifo 1
            Rx_Msg := Read_Rx_Message (CAN, FIFO_1);
            Release_Fifo (CAN, FIFO_1);
            New_Message := True;
         end if;
      end IRQ_Handler;

   end Controller;

end CAN_Handler;
