
extern void init_terminal(void);
extern void restore_terminal(void);
extern int char_available(void);
extern char  read_char(void);

/* TEMPORARY */
void ext_chan_scr(int count, char* address);
void ext_chan_err(int count, char* address);
