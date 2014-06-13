#include <benchmark_timers.h>
#include <stdio.h>
#include "../hw-block/avr_compiler.h"
#include <stdlib.h>

/* Start the timers */
/* using two 16-bit timers to get a 32-bit one */
void start_timer(){

  /* Use TCC0 overflow as input for event channel 0. */
  EVSYS.CH0MUX = EVSYS_CHMUX_TCC0_OVF_gc;

  /* Use event channel 0 as clock source for TCC1. */                                                  
  TC1_ConfigClockSource( &TCC1, TC_CLKSEL_EVCH0_gc );                                                  
                                                                                                             
  /* Select system clock as TCC0 clock source. */                                                      
  TC0_ConfigClockSource( &TCC0, TC_CLKSEL_DIV1_gc ); 
}

/* This function returns the current time since beginning of execution */
uint32_t current_time() {

  uint32_t inputCaptureTime;

  uint16_t highWord =  TCC1.CNT;
  uint16_t lowWord = TCC0.CNT;
  inputCaptureTime = ( (uint32_t) highWord << 16 ) | lowWord;

  /* return (inputCaptureTime >> 24); */
  return inputCaptureTime;
}	
