

//#include <stdint.h>
//#include <stdio.h>
//#include <stdlib.h>
#include <Rcpp.h>
using namespace Rcpp;

const uint16_t ADC_256x_TBL[]=
  {
  14289,14199,14105,14009,13911,13809,13705,13599,13489,13378,    // -20..-11
  13263,13146,13025,12903,12778,12651,12521,12389,12255,12119,    // -10..-01
  11981,11842,11701,11558,11413,11267,11119,10970,10820,10669,    //  00..  9
  10516,10363,10209,10054, 9899, 9743, 9587, 9431, 9275, 9119,    //  10.. 19
  8963, 8808, 8653, 8499, 8345, 8192, 8039, 7887, 7736, 7586,    //  20.. 29
  7438, 7290, 7145, 7000, 6857, 6716, 6577, 6439, 6303, 6168,    //  30.. 39
  6036, 5905, 5776, 5649, 5523, 5400, 5279, 5159, 5042, 4927,    //  40.. 49
  4814, 4703, 4593, 4486, 4381, 4279, 4178, 4079, 3982, 3887,    //  50.. 59
  3794, 3705, 3617, 3531, 3448, 3366, 3286, 3208, 3132, 3057,    //  60.. 69
  2984, 2913, 2843, 2775, 2709, 2644, 2580, 2519, 2458, 2400,    //  70.. 79
  2342, 2287, 2232, 2179, 2127, 2077, 2028, 1980, 1933, 1888,    //  80.. 89
  1843, 1800, 1757, 1716, 1675, 1636, 1597, 1560, 1524, 1488,    //  90.. 99
  1453                                                           // 100
  };

int16_t ADC2Degrees(uint32_t adc_result)
{
  uint16_t delta;
  uint16_t i;
  uint16_t sample=(uint16_t)adc_result;
  if(sample <= ADC_256x_TBL[120]) return 1000;
  for(i=0;i<120;i++)
  {
    if(sample >= ADC_256x_TBL[i]) break;
  }
  if(!i) return -200;
  delta=ADC_256x_TBL[i-1]-sample;
  if(delta)
  {
    // delta:dT=(ADC_256x_TBL[i-1]-ADC_256x_TBL[i]):10
    delta=(delta*10+5)/(ADC_256x_TBL[i-1]-ADC_256x_TBL[i]);
  }
  return ((int16_t)(((i-1)*10)+delta))-200;
}

uint32_t Degrees2ADC(int16_t T)
{
  if(T <= -200) return ADC_256x_TBL[0];
  if(T >= 1000) return ADC_256x_TBL[120];
  T+=200;
  uint8_t dT=(uint8_t)(T%10);
  T/=10;
  uint32_t adc_res=ADC_256x_TBL[T];
  adc_res-=(dT*(ADC_256x_TBL[T]-ADC_256x_TBL[T+1]))/10;
  return adc_res;
}

// [[Rcpp::export]]
int16_t Normalize(int16_t T,uint32_t bandgap)
{
  /*
   apply reverse engineering to find back the adc value belonging to T
   */
  uint32_t adc_res=Degrees2ADC(T);
  /*
  printf("measured bandgap=%u\n",bandgap);
  printf("measured T=%d\n",T);
  printf("converted adc result from Degrees2ADC=%lu\n",adc_res);

   the new adc value must be re-normalized according to the battery voltage
   The scale factor is Vadc/3300
   Vadc can be extracted in the TT+ record from the bandgap field using:
   ((1100<<17)/bandgap)/3300 = ((1100<<17)/3300)/bandgap = 43690.67/bandgap
   this is true in the hypothesis that the 3.3V is stable (true if Vbat >= 3.4 V)
   and also on a temperature range from -5°C to +85°C
   */
  adc_res*=43691;
  adc_res/=bandgap;
  /*
  printf("normalized adc result from Normalize=%lu\n",adc_res);
     apply ADC2Degrees to have back the temperature
  */
  return ADC2Degrees(adc_res);
}
