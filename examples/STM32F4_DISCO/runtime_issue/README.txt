
Description

  This example makes the runtime fail in the following way:
    1. A task that is synchronized to an ISR using an entry-function gets
       blocked forever.
    2. All existing tasks, also periodic, are eventually blocked altogether. 


Configuration

  The following tasks and irqs exist:

    Task               Period                   Priority
    -----------------------------------------------------------------------
    Hall_Handler       Awaits Hall_Peripheral   Priority'Last
    Current_Control    Awaits ADC_Sampler       Priority'Last - 1
    Inverter_System    100 Hz                   Priority'Last - 2
    Logger             1000 Hz                  Priority'Last - 4

    Interrupt          Period                   Priority
    -----------------------------------------------------------------------
    Hall_Peripheral    277 Hz                   Interrupt_Priority'Last
    ADC_Sampler        20_000 Hz                Interrupt_Priority'Last - 1

  A timer is set up to cause the Hall_Peripheral interrupt at highest prio.
  This will trigger the Hall_Handler task at the highest prio.

  Another timer is set up to cause the ADC_Sampler interrupt at the next to
  highest prio. This will trigger the Current_Control task at the next to
  highest prio.

  In the background there are two periodic tasks at lower priorities (Logger
  and Inverter_System).


Problem Description

  After about 1 s a Hall_Peripheral irq occurs, however the Hall_Handler is
  NOT triggered and is blocked forever.

  After about 46 s an ADC_Sampler irq occurs, however the Current_Control is
  NOT triggered and ALL other tasks are also blocked forever. 
 

Debug Outputs

  Event                          Led           Pin    Set to
  ----------------------------------------------------------
  Hall_Peripheral irq occurs     -             PE7    High
  Hall_Handler triggers          -             PE7    Low
                                 Blue_Led      PD15   High
  Hall_Handler done              Blue_Led      PD15   Low

  ADC_Sampler irq occurs         -             PE9    High
  Current_Control triggers       -             PE9    Low
                                 Red_Led       PD14   High
  Current_Control done           Red_Led       PD14   Low

  Inverter_System runs           Green_Led     PD12   High
  Inverter_System done           Green_Led     PD12   Low

  Logger runs                    Orange_Led    PD13   High
  Logger done                    Orange_Led    PD13   Low

  
