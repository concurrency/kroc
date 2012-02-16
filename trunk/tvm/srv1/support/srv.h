#ifndef SRV_H
#define SRV_H

/* Misc Init */
void init_io ();
void clear_sdram ();

/* Serial outputs */
void serial_out_version ();
void serial_out_framecount ();
void serial_out_time ();
void serial_out_flashbuffer ();

/* Lasers */
void lasers_on ();
void lasers_off ();

/* LED's */
void led0_on();
void led1_on();

/* Camera */
void get_frame ();
void overlay_on ();
void overlay_off ();
void camera_setup ();
void camera_reset (unsigned int width);
void framesize_160 ();
void framesize_320 ();
void framesize_640 ();
void framesize_1280 ();
void change_image_quality ();
void set_caption (unsigned char *str, unsigned int width);
void move_image (unsigned char *src1, unsigned char *src2, unsigned char *dst, unsigned int width, unsigned int height);

/* Transfer */
int xmodem_receive ();

void start_cinterpreter ();

/* Flash */
void read_user_flash ();
void write_user_flash ();
void write_firmware_flash ();
void write_boot_flash ();

/* Motors */
void init_motors ();
void motor_command ();
void motor_increase_base_speed ();
void motor_decrease_base_speed ();
void motor_action (unsigned char ch);
void motor_set (unsigned char cc, int speed, int *ls, int *rs);
void initPWM ();
void setPWM (int mleft, int mright);

/* Clock */
void initRTC ();
int readRTC ();
void clearRTC ();
void delayMS (int delay);  // delay up to 100000 millisecs (100 secs)
#endif

