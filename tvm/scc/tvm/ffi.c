#include "tvm-scc.h"
#include "RCCE.h"

#define RCCE_BUFFER_SIZE 64
static char sendBuffer[RCCE_BUFFER_SIZE];
static char recvBuffer[RCCE_BUFFER_SIZE];
static int sendLength;
static int recvLength;
static int sendCoreID;
static int recvCoreID;
static int success;

int _getCoreID(ECTX ectx, WORD args[])
{
	*(WORD*)args[0] = get_my_coreid();

	return SFFI_OK;
}

int _print(ECTX ectx, WORD args[])
{
	char* str = (char*)args[0];
	int len = args[1];
	int pos = 0;
	volatile char buffer[256];

	while(pos < len)
	{
		buffer[pos] = str[pos];
		pos++;
	}
	buffer[len] = '\0';

	printf(buffer);

	return SFFI_OK;
}

int _sendString(ECTX ectx, WORD args[])
{
	int i;
	sendLength = *(int*)args[1];
	sendCoreID = *(int*)args[0];

	for(i = 0; i < sendLength && i < RCCE_BUFFER_SIZE; i++)
		sendBuffer[i] = ((char*)args[2])[i];
	while(i++ < RCCE_BUFFER_SIZE)
		sendBuffer[i] = '\0';

//	printf("Sending %s with length %d to core %d\n", sendBuffer, sendLength, sendCoreID);

	RCCE_send(sendBuffer, RCCE_BUFFER_SIZE, sendCoreID);

	return SFFI_OK;
}

int _recvString(ECTX ectx, WORD args[])
{
	int i;
	recvLength = 0;
	success = 0;
	recvCoreID = *(int*)args[0];

//	printf("Receiving from core %d\n", recvCoreID);

	RCCE_recv_test(recvBuffer, RCCE_BUFFER_SIZE, recvCoreID, &success);

//	if(success)
//		printf("Received %s with length %d from core %d\n", recvBuffer, recvLength, recvCoreID);

	while(recvBuffer[recvLength] != '\0')
	{
		((char*)args[2])[recvLength] = recvBuffer[recvLength];
		recvLength++;
	}
	((char*)args[2])[recvLength] = '\0';
	*(int*)args[1] = recvLength;
	*(int*)args[4] = success;

	return SFFI_OK;
}

SFFI_FUNCTION sffi_table[] = {_getCoreID, _print, _sendString, _recvString};
const int sffi_table_length = sizeof(sffi_table) / sizeof(SFFI_FUNCTION);
