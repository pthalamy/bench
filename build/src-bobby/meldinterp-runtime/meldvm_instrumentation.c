#include "meldvm_instrumentation.h"
#include <stdio.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>


void  hexconvert (unsigned char pci,char * buffer)
{

	int buffer1;
	int buffer2;
	unsigned char bufferchar1;
	unsigned char bufferchar2;

	buffer1 = (int)(pci & 0x0f);
	buffer2 = (int) ((pci >> 4) & 0x0f);
	bufferchar1 = (pci & 0x0f);
	/* bufferchar2 = ((pci & 0xf0))/16; */
	bufferchar2 =((pci >> 4) & 0x0f);

	switch(buffer2) {
        case 0:
	 buffer[0]= '0';
	break;
        case 1: 
	 buffer[0]= '1';
	break;
        case 2: 
	buffer[0]= '2';
	break;
        case 3:
	 buffer[0]= '3';
	break;
        case 4: 
	buffer[0]= '4';
	break;
        case 5:
	 buffer[0]= '5';
	break;
        case 6:
	 buffer[0]= '6';
	break;
        case 7:
	 buffer[0]= '7';
	break;
        case 8:
	 buffer[0]= '8';
	break;
        case 9:
	 buffer[0]= '9';
	break;
        case 10:
	 buffer[0]= 'A';
	break;
        case 11:
	 buffer[0]= 'B';
	break;
        case 12:
	 buffer[0]= 'C';
	break;
        case 13:
	 buffer[0]= 'D';
	break;
        case 14:
	 buffer[0]= 'E';
	break;
        case 15:
	 buffer[0]= 'F';
	break;
        default:
	 break;
        }

	switch(buffer1) {
        case 0:
	 buffer[1]= '0';
	break;
        case 1: 
	 buffer[1]= '1';
	break;
        case 2: 
	buffer[1]= '2';
	break;
        case 3:
	 buffer[1]= '3';
	break;
        case 4: 
	buffer[1]= '4';
	break;
        case 5:
	 buffer[1]= '5';
	break;
        case 6:
	 buffer[1]= '6';
	break;
        case 7:
	 buffer[1]= '7';
	break;
        case 8:
	 buffer[1]= '8';
	break;
        case 9:
	 buffer[1]= '9';
	break;
        case 10:
	 buffer[1]= 'A';
	break;
        case 11:
	 buffer[1]= 'B';
	break;
        case 12:
	 buffer[1]= 'C';
	break;
        case 13:
	 buffer[1]= 'D';
	break;
        case 14:
	 buffer[1]= 'E';
	break;
        case 15:
	 buffer[1]= 'F';
	break;
        default:
	 break;
        }

	buffer[2] = '\0';
}


void meldvm_instru(char * ops_type, int ops_cost, const unsigned char *pc)
{

FILE *log;
char  * pchexbuffer;
char * pchex;
unsigned char * pcopresult;
int i,len;

log = fopen ("executionlog","a");

/* pchex = malloc ((strlen(((const char *)  pc)))*2); */
pchex = malloc(50);
pchexbuffer = malloc(4);
pcopresult = malloc(2);

/* getting the thread id of the current block */
pthread_t id = pthread_self();

/* getting the content of the pc */
/* it must be converted from ascii to printable hex */
strcpy((char*)pchex,"");
/* len = strlen(((const char *)  pc)); */
len = sizeof(pc)-1;

  for (i=0 ; i<len; i++) {
strcpy((char *) pchexbuffer,"");
/*  sprintf (pchexbuffer,"%2x",pc[i]); */
/* fprintf(log,"pchexbuffer: %s, size of pchexbuffer %d,pc_i: %c\n",pchexbuffer,strlen(pchexbuffer),pc[i]); */
/* strncat(pchex,pchexbuffer,2);   */
hexconvert(pc[i],pchexbuffer); 
strcat(pchex,pchexbuffer);
}  


*pcopresult = ((*(const unsigned char*)(pc))&0xfc);
/* writing log */
/* complicated version fprintf(log, "current op on block %lu: %s, op cost: %d, pc: %s,pcopresult=%x,\n",id, ops_type, ops_cost, pchex,*pcopresult); */
fprintf(log, "current op on block %lu: %s, op cost: %d\n",id, ops_type, ops_cost);

free(pchexbuffer); 
free(pchex); 
fclose(log);
	
}

