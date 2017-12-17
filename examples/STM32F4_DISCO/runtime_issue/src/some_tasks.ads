with System;
with HAL; use HAL;
with Ada.Interrupts.Names;
with STM32.Timers;
with STM32.Device;
with STM32.GPIO;

package Some_Tasks is

   Hall_Debug_Pin : STM32.GPIO.GPIO_Point renames STM32.Device.PE7;
   ADC_Debug_Pin  : STM32.GPIO.GPIO_Point renames STM32.Device.PE9;

   task Hall_Handler with
      Priority => System.Priority'Last,
      Storage_Size => (1 * 1024);

   task Current_Control with
      Priority => System.Priority'Last - 1,
      Storage_Size => (1 * 1024);

   task Inverter_System with
      Priority => System.Priority'Last - 2,
      Storage_Size => (1 * 1024);

   task Logger with
      Priority => System.Priority'Last - 4,
      Storage_Size => (1 * 1024);


   type Sample_Array is array (1 .. 2) of Float;

   protected Hall_Peripheral is
      pragma Interrupt_Priority (System.Interrupt_Priority'Last);

      entry Await_New (New_State            : out UInt32;
                       Time_Delta_s         : out Float;
                       Speed_Timer_Overflow : out Boolean);
   private

      Hall_State_Is_Updated : Boolean := False;

      procedure ISR with
        Attach_Handler => Ada.Interrupts.Names.TIM5_Interrupt;

   end Hall_Peripheral;

   protected ADC_Sampler is
      pragma Interrupt_Priority (System.Interrupt_Priority'Last - 1);

      entry Await_New_Samples (Phase_Voltage_Samples : out Float;
                               Phase_Current_Samples : out Float);
   private

      Samples : Sample_Array := (others => 0.0);
      New_Samples : Boolean := False;

      procedure ISR with
        Attach_Handler => Ada.Interrupts.Names.TIM1_CC_Interrupt;

   end ADC_Sampler;



private

   function Application_Code (X : in Integer)
                              return Float;

   procedure Initialize_GPIOs;

   procedure Init_ADC_Sampler;

   procedure Init_Hall_Timer;

   ADC_Timer : STM32.Timers.Timer renames STM32.Device.Timer_1;

   Hall_Timer : STM32.Timers.Timer renames STM32.Device.Timer_5;

   Init : Boolean := False;

end Some_Tasks;
