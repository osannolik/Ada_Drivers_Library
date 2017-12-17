with Ada.Real_Time;      use Ada.Real_Time;
with STM32.Board;        use STM32.Board;
with STM32.PWM;

package body Some_Tasks is



   task body Hall_Handler is
      State : UInt32;
      Dummy : Float;
      Is_Overflow : Boolean;
   begin
      loop
         Hall_Peripheral.Await_New (New_State            => State,
                                    Time_Delta_s         => Dummy,
                                    Speed_Timer_Overflow => Is_Overflow);

         Turn_On (Blue_LED);
         STM32.GPIO.Clear (Hall_Debug_Pin);

         Dummy := Application_Code (30);

         Turn_Off (Blue_LED);

      end loop;
   end Hall_Handler;


   task body Current_Control is
      V_Samples : Float;
      I_Samples : Float;
      Dummy : Float;
   begin
      loop
         ADC_Sampler.Await_New_Samples
            (Phase_Voltage_Samples => V_Samples,
             Phase_Current_Samples => I_Samples);

         Turn_On (Red_LED);
         STM32.GPIO.Clear (ADC_Debug_Pin);

         Dummy := Application_Code (100);

         Turn_Off (Red_LED);

      end loop;
   end Current_Control;


   task body Inverter_System is
      Next_Release : Time := Clock;
      Dummy : Float;
   begin

      Initialize_GPIOs;
      Initialize_LEDs;

      All_LEDs_Off;

      Init_Hall_Timer;
      Init_ADC_Sampler;

      Init := True;

      loop
         Turn_On (Green_LED);

         Dummy := Application_Code (50);

         Turn_Off (Green_LED);

         Next_Release := Next_Release + Milliseconds (10);
         delay until Next_Release;
      end loop;
   end Inverter_System;


   task body Logger is
      Next_Release : Time := Clock;
      Dummy : Float;
   begin

      while not Init loop
         Next_Release := Next_Release + Milliseconds (10);
         delay until Next_Release;
      end loop;

      loop
         Turn_On (Orange_LED);

         Dummy := Application_Code (50);

         Turn_Off (Orange_LED);

         Next_Release := Next_Release + Milliseconds (1);
         delay until Next_Release;
      end loop;
   end Logger;








   protected body ADC_Sampler is

      entry Await_New_Samples (Phase_Voltage_Samples : out Float;
                               Phase_Current_Samples : out Float) when New_Samples is
      begin
         Phase_Voltage_Samples := Samples (1);
         Phase_Current_Samples := Samples (2);

         New_Samples := False;
      end Await_New_Samples;

      procedure ISR is
         use STM32.Timers;
      begin

         STM32.GPIO.Set (ADC_Debug_Pin);

         if Status (ADC_Timer, Timer_CC1_Indicated) and then
            Interrupt_Enabled (ADC_Timer, Timer_CC1_Interrupt)
         then
            Clear_Pending_Interrupt (ADC_Timer, Timer_CC1_Interrupt);

            Samples := (10.0, 100.0);
            New_Samples := True;
         end if;

      end ISR;

   end ADC_Sampler;


   protected body Hall_Peripheral is

      entry Await_New (New_State            : out UInt32;
                       Time_Delta_s         : out Float;
                       Speed_Timer_Overflow : out Boolean) when Hall_State_Is_Updated is
      begin
         New_State := 1;
         Time_Delta_s := 200.0;
         Speed_Timer_Overflow := True;

         Hall_State_Is_Updated := False;
      end Await_New;

      procedure ISR is
         use STM32.Timers;
      begin

         STM32.GPIO.Set (Hall_Debug_Pin);

         if Status (Hall_Timer, Timer_CC1_Indicated) and then
            Interrupt_Enabled (Hall_Timer, Timer_CC1_Interrupt)
         then
            Clear_Pending_Interrupt (Hall_Timer, Timer_CC1_Interrupt);

            Hall_State_Is_Updated := True;
         end if;

      end ISR;

   end Hall_Peripheral;







   function Application_Code (X : in Integer)
                              return Float
   is
      Y : Float := 1.0;
   begin
      for I in 1 .. X loop
         if X < 10 then
            Y := Y + Float (I + X);
         elsif X < 20 then
            Y := Y + Float (I - X);
         elsif X < 30 then
            Y := Y - Float (I - X);
         else
            Y := 133.0;
         end if;
      end loop;

      return Y;

   end Application_Code;

   procedure Initialize_GPIOs is
      use STM32.GPIO;
      use STM32.Device;

      All_GPIOs : GPIO_Points := Hall_Debug_Pin & ADC_Debug_Pin;
   begin
      Enable_Clock (All_GPIOs);

      Configure_IO (All_GPIOs,
                    (Mode_Out,
                     Resistors   => Floating,
                     Output_Type => Push_Pull,
                     Speed       => Speed_100MHz));

      Clear (All_GPIOs);
   end Initialize_GPIOs;

   procedure Init_ADC_Sampler is
      use STM32.Timers;

      Trigger : STM32.PWM.PWM_Modulator;

   begin

      STM32.PWM.Configure_PWM_Timer (Generator => ADC_Timer'Access,
                                     Frequency => 20_000);

      Trigger.Attach_PWM_Channel (Generator => ADC_Timer'Access,
                                  Channel   => STM32.Timers.Channel_1,
                                  Point     => STM32.Device.PA8,
                                  PWM_AF    => STM32.Device.GPIO_AF_TIM1_1);

      Trigger.Set_Duty_Cycle (Value => 50);  --  Anything /= 0 or /= 100


      Disable_Interrupt (ADC_Timer, Timer_CC1_Interrupt);

      Clear_Pending_Interrupt (ADC_Timer, Timer_CC1_Interrupt);

      Enable_Interrupt  (ADC_Timer, Timer_CC1_Interrupt);

      Trigger.Enable_Output;
   end Init_ADC_Sampler;

   procedure Init_Hall_Timer is
      use STM32.Timers;

      Trigger : STM32.PWM.PWM_Modulator;

   begin

      STM32.PWM.Configure_PWM_Timer (Generator => Hall_Timer'Access,
                                     Frequency => 283);

      Trigger.Attach_PWM_Channel (Generator => Hall_Timer'Access,
                                  Channel   => STM32.Timers.Channel_1,
                                  Point     => STM32.Device.PA0,
                                  PWM_AF    => STM32.Device.GPIO_AF_TIM5_2);

      Trigger.Set_Duty_Cycle (Value => 50);  --  Anything /= 0 or /= 100


      Disable_Interrupt (Hall_Timer, Timer_CC1_Interrupt);

      Clear_Pending_Interrupt (Hall_Timer, Timer_CC1_Interrupt);

      Enable_Interrupt  (Hall_Timer, Timer_CC1_Interrupt);

      Trigger.Enable_Output;
   end Init_Hall_Timer;

end Some_Tasks;
