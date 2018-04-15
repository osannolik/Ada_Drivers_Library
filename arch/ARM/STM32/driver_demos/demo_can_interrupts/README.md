This program demonstrates usage of the bxCAN peripheral included with
the STM32F4x MCUs. It uses an interrupt based mechanism to send and receive 
CAN messages. Moreover, a filter is added that defines which CAN messages that
shall be ignored or received. 

The example application configures the CAN peripheral into loopback mode. 
That means that the application directly receives the same messages sent by it.
Therefore, no external components or hardware is needed to run the demo. 
