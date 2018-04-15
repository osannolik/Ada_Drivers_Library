This program demonstrates the basic usage of the bxCAN peripheral included with
the STM32F4x MCUs. It shows how to configure the peripheral and how to add a
filter that defines which CAN messages shall be ignored or received. 

The example application configures the CAN peripheral into loopback mode. 
That means that the application directly receives the same messages sent by it.
Therefore, no external components or hardware is needed to run the demo. 
