void ezSensorConfig(void)
{
    unsigned short TWI_Transfer_Count, TWI_SensorAddr, TWI_TX_Num_of_Bytes;

    TWI_Transfer_Count = 6;            // Specifies the number of complete write sequences
    TWI_TX_Num_of_Bytes = 2;        // Specifies the number of data writes per sequence 
    TWI_SensorAddr = OV;            // Device Address (is not part of the count value "TWI_TX_Num_of_Bytes"

    ezTWI_MasterMode_Write(TWI_SensorAddr, TWI_SensorData, TWI_Transfer_Count, TWI_TX_Num_of_Bytes); 
    
    TWI_TX_Num_of_Bytes = 2;        // Specifies the number of data reads per sequence
    
//    ezTWI_MasterMode_Read(TWI_SensorAddr, TWI_SensorDataREAD, TWI_Transfer_Count, TWI_TX_Num_of_Bytes);
    
}


void ezTWI_MasterMode_Write(unsigned short DeviceAddr, unsigned char *TWI_Data_Pointer, unsigned short TX_Count, unsigned short TWI_TX_Length)
{
    #define PRESCALE120M    12        // factor = 12, 120MHz/10MHz 
        *pTWI_FIFO_CTL             |= XMTFLUSH;                // Clear the TX FIFO
        *pTWI_MASTER_STAT         = BUFWRERR | BUFRDERR | DNAK | ANAK | LOSTARB; // Clear all status error
        ssync();

        *pTWI_FIFO_CTL             = 0;                        // Clear the bit manually
        *pTWI_CONTROL            = TWI_ENA | PRESCALE120M;     // PRESCALE = fsclk/10MHz
        *pTWI_CLKDIV            = CLKLOW(50) | CLKHI(50);    // For 100KHz SCL speed: CLKDIV = (1/100KHz)/(1/10MHz) = 100 -> SCL symetric: CLKHI = 50, CLKLOW = 50 
        *pTWI_MASTER_ADDR        = DeviceAddr;                // Target address (7-bits plus the read/write bit the TWI controls
                
    for (i = 0; i < TX_Count; i++)
    {

        *pTWI_XMT_DATA8            = *TWI_Data_Pointer++;        // Pointer to an array and load a value where a list of data is located 

        *pTWI_MASTER_CTL        = (TWI_TX_Length<<6) | MEN;                        // Start transmission
        
        for (j = 0; j < (TWI_TX_Length-1); j++)
        {            
            
            while (*pTWI_FIFO_STAT == XMTSTAT)    
            {                // wait to load the next sample into the TX FIFO
            ssync();
            }
            
            *pTWI_XMT_DATA8        = *TWI_Data_Pointer++;        // Load the next sample into the TX FIFO. Pointer to an array where a list of data is located 
            ssync();
        }
    
    
        while ((*pTWI_INT_STAT & MCOMP) == 0)                // Wait until transmission complete and MCOMP is set
        ssync();
        
        *pTWI_INT_STAT = XMTSERV | MCOMP;                    // service TWI for next transmission 
    }
     
}

void ezTWI_MasterMode_Read(unsigned short DeviceAddr, unsigned char *TWI_SensorDataREAD, unsigned short TX_Count, unsigned short TWI_TX_Length)
{
    #define PRESCALE120M    12        // factor = 12, 120MHz/10MHz 
        *pTWI_FIFO_CTL             |= RCVFLUSH;                // Clear the TX FIFO
        *pTWI_MASTER_STAT         = BUFWRERR | BUFRDERR | DNAK | ANAK | LOSTARB; // Clear all status error
        ssync();

        *pTWI_FIFO_CTL             = 0;                        // Clear the bit manually
        *pTWI_CONTROL            = TWI_ENA | PRESCALE120M;    // PRESCALE = fsclk/10MHz
        *pTWI_CLKDIV            = CLKLOW(50) | CLKHI(50);    // For 100KHz SCL speed: CLKDIV = (1/100KHz)/(1/10MHz) = 100 -> SCL symetric: CLKHI = 50, CLKLOW = 50 
        *pTWI_MASTER_ADDR        = DeviceAddr;                // Target address (7-bits plus the read/write bit the TWI controls
    
    for (i = 0; i < TX_Count; i++)
    {

        *pTWI_MASTER_CTL        = (TWI_TX_Length<<6) | MEN | MDIR;                        // Start transmission 

        for (j = 0; j < (TWI_TX_Length-1); j++)
        {            
        while (*pTWI_FIFO_STAT == 0)                        // wait to load the next sample into the TX FIFO
            ssync();
            TWI_SensorDataREAD[j] = *pTWI_RCV_DATA8;        // Load the next sample into the TX FIFO. Pointer to an array where a list of data is located
            ssync();
        }
    
    
        while ((*pTWI_INT_STAT & MCOMP) == 0)                // Wait until transmission complete and MCOMP is set 
        ssync();
        
        *pTWI_INT_STAT = RCVSTAT | MCOMP;                    // service TWI for next transmission
    }
}
