--  This package was generated by the Ada_Drivers_Library project wizard script
package ADL_Config is
   Vendor                         : constant String  := "STMicro";           -- From board definition
   Max_Mount_Points               : constant         := 2;                   -- From default value
   Max_Mount_Name_Length          : constant         := 128;                 -- From default value
   Runtime_Profile                : constant String  := "ravenscar-full";    -- From command line
   Device_Name                    : constant String  := "STM32F469NIHx";     -- From board definition
   Device_Family                  : constant String  := "STM32F4";           -- From board definition
   Runtime_Name                   : constant String  := "ravenscar-full-stm32f469disco"; -- From default value
   Has_Ravenscar_Full_Runtime     : constant String  := "True";              -- From board definition
   CPU_Core                       : constant String  := "ARM Cortex-M4F";    -- From mcu definition
   Board                          : constant String  := "STM32F469_Discovery"; -- From command line
   Has_ZFP_Runtime                : constant String  := "False";             -- From board definition
   Has_Ravenscar_SFP_Runtime      : constant String  := "True";              -- From board definition
   High_Speed_External_Clock      : constant         := 8000000;             -- From board definition
   Max_Path_Length                : constant         := 1024;                -- From default value
   Runtime_Name_Suffix            : constant String  := "stm32f469disco";    -- From board definition
   Architecture                   : constant String  := "ARM";               -- From board definition
end ADL_Config;
