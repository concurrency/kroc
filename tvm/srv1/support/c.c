/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  c.c - C interpreter for SRV-1 robot
 *    Copyright (C) 2005-2007  Surveyor Corporation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details (www.gnu.org/licenses)
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
 
 /* A Little C interpreter. 
    
     Herbert Schildt, "Building your own C interpreter."
     Dr. Dobb's Journal of Software Tools v14, n8 (August, 1989):38 (16 pages).
*/

#define NUM_FUNC        40
#define NUM_GLOBAL_VARS 40
#define NUM_LOCAL_VARS  40
#define NUM_BLOCK       20
#define ID_LEN          31 
#define NUM_PARAMS      15
#define LOOP_NEST       15
#define NULL            0
#define TRUE            1
#define FALSE           0

enum tok_types {DELIMITER, IDENTIFIER, NUMBER, KEYWORD, TEMP, STRING, BLOCK};

enum tokens {ARG, CHAR, INT, IF, ELSE, FOR, DO, WHILE,
             SWITCH, RETURN, EOL, FINISHED, END};

enum double_ops {LT=1, LE, GT, GE, EQ, NE};

enum error_msg
         {SYNTAX, UNBAL_PARENS, NO_EXP, EQUALS_EXPECTED,
            NOT_VAR, PARAM_ERR, SEMI_EXPECTED,
            UNBAL_BRACES, FUNC_UNDEF, TYPE_EXPECTED,
            NEST_FUNC, RET_NOCALL, PAREN_EXPECTED,
            WHILE_EXPECTED, QUOTE_EXPECTED, NOT_TEMP,
            TOO_MANY_LVARS};
char *prog;  
char *p_buf; 

struct variable_type {
    char var_name[ID_LEN];
    int    var_type;
    int    value;
}    global_vars[NUM_GLOBAL_VARS];
struct variable_type local_var_stack[NUM_LOCAL_VARS];
struct func_type {
    char func_name[ID_LEN];
    char *loc;    
} func_table[NUM_FUNC];
int call_stack[NUM_FUNC];
struct commands { 
    char command[20];
    char tok;
} table[] = { 
    {"if",    IF}, 
    {"else",  ELSE},
    {"for",   FOR},
    {"do",    DO},
    {"while", WHILE},
    {"char",  CHAR},
    {"int",   INT},
    {"return",RETURN},
    {"end",   END},
    {"",      END} 
};

char token[80];
char token_type, tok;
int    functos;   
int    func_index;
int    gvar_index;
int    lvartos;   
int    ret_value; 
void prescan();
void decl_global(), call(), putback();
void decl_local(), local_push(struct variable_type);
void eval_exp(int *), sntx_err(int);
void start_check(), finish_check();
void exec_if(), find_eob(), exec_for();
void get_params(), get_args();
void exec_while(), func_push(int), exec_do();
void assign_var(char *, int value);
int  find_var(char *);
void interp_block(), func_ret();
int  func_pop(), is_var(char *), get_token();
char *find_func(char *);
int  strcmp1(char *, char *);
char *strchr1(char *, char);
void strcpy1(char *, char *);

void eval_exp(int *);
void eval_exp0(int *);
void eval_exp1(int *);
void eval_exp2(int *);
void eval_exp3(int *);
void eval_exp4(int *);
void eval_exp5(int *);
void atom(int *);
void sntx_err(int), putback();
void assign_var(char *, int);
int  isdelim(char), isalph(char), look_up(char *), iswhite(char);
int  find_var(char *), get_token();
int  internal_func(char *);
int  is_var(char *);
int  atoi1(char *);
char *find_func(char *);
void call();
void print(), motors(), delay(), imgcap(), scan();
void colorcap(), colorset(), colorget(), bitdir(), bitset();
int time(), blob(), bitget(), input();

int get();
void set();

//int _x, _y, _z, _ymin, _ymax, _umin, _umax, _vmin, _vmax;  

struct intern_func_type {
    char *f_name; 
    int (*p)();   
} intern_func[] = {
    {"print", print},
    {"input", input},
    {"motors", motors},
    {"delay", delay},
    {"time", time},
    {"get", get},
    {"set", set},
/*    {"imgcap", imgcap},
    {"scan", scan},
    {"blob", blob},
    {"range", range},
    {"bitdir", bitdir},
    {"bitset", bitset},
    {"bitget", bitget},
*/
    {"", 0}    
};


int c(char *pc)
{
    p_buf = pc;
    //uart0SendString(p_buf);
    prog = p_buf; 
    prescan(); 
    gvar_index = 0; 
    lvartos = 0;    
    functos = 0;    
    prog = find_func("main");  
    prog--; 
    strcpy1(token, "main");
    call(); 
    return 0;
}

int strcmp1(char *s1, char *s2)
{
        while (*s1 == *s2++)
                if (*s1++ == 0)
                        return (0);
        return (*s1 - *--s2);
}

char *strchr1(char *s, char c)
{
        for (;;)
        {
             if (*s == c) 
                    return s;
             if (!*s++) 
                    return 0;
        }
}

void strcpy1(char *pDst, char *pSrc)
{
        while ((*pDst++ = *pSrc++) != '\0')
                continue;
}


void interp_block()
{
    int    value;
    char block = 0;
    do {
        token_type = get_token();
        if(token_type==IDENTIFIER) { 
            putback(); 
            eval_exp(&value);
            if(*token!=';') sntx_err(SEMI_EXPECTED);
        }
        else if(token_type==BLOCK) {
            if(*token=='{')
                block = 1; 
            else
                return; 
        }
        else switch(tok) {
                case CHAR:
                case INT: 
                    putback();
                    decl_local();
                    break;
                case RETURN:    
                    func_ret();
                    return; 
                case IF:   
                    exec_if();
                    break;
                case ELSE:   
                    find_eob();
                    break;            
                case WHILE: 
                    exec_while();
                    break;
                case DO:    
                    exec_do();
                    break;
                case FOR: exec_for();
                    break;
                case END:
                    return;
        }
    } while (tok != FINISHED && block);
}

void prescan()
{
    char *p;
    char temp[32];
    int    brace = 0;    
    p = prog;
    func_index = 0;
    do { 
        while(brace) { 
            get_token();
            if(*token=='{') brace++;
            if(*token=='}') brace--;
        }
        get_token();
        if(tok==CHAR || tok==INT) { 
            putback();
            decl_global();
        }            
        else if(token_type==IDENTIFIER) { 
            strcpy1(temp, token);
            get_token();
            if(*token=='(') {   
                func_table[func_index].loc = prog;
                strcpy1(func_table[func_index].func_name, temp);
                func_index++;
                while(*prog!=')') prog++;
                prog++;
            }
            else
                putback();
        }
        else if(*token=='{')
            brace++;
    } while(tok!=FINISHED);
    prog = p;
}

char *find_func(char *name)
{
    int i;
    for(i=0; i<func_index; i++)
        if(!strcmp1(name, func_table[i].func_name))
            return func_table[i].loc;
    return NULL;
}

void decl_global()
{
    get_token();  
    global_vars[gvar_index].var_type = tok;
    global_vars[gvar_index].value = 0;  
    do { 
        get_token();
        strcpy1(global_vars[gvar_index].var_name, token);
        get_token();
        gvar_index++;
    } while(*token==',');
    if(*token!=';') sntx_err(SEMI_EXPECTED);
}

void decl_local()
{
    struct variable_type i;
    get_token();  
    i.var_type = tok;
    i.value = 0;  
    do {
        get_token(); 
        strcpy1(i.var_name, token);
        local_push(i);
        get_token();
    } while(*token==',');
    if(*token!=';') sntx_err(SEMI_EXPECTED);
}


void call()
{
    char *loc, *temp;
    int lvartemp;
    loc = find_func(token);
    if(loc==NULL)
        sntx_err(FUNC_UNDEF);
    else {
        lvartemp = lvartos;  
        get_args();  
        temp = prog; 
        func_push(lvartemp);
        prog = loc;         
        get_params();       
                            
        interp_block();     
        prog = temp;        
        lvartos = func_pop();
    }
}

void get_args()
{
    int value, count, temp[NUM_PARAMS];
    struct variable_type i;
    count = 0;
    get_token();
    if(*token!='(') sntx_err(PAREN_EXPECTED);
    do {
        eval_exp(&value);
        temp[count] = value;  
        get_token();
        count++;
    } while(*token==',');
    count--;
    for(; count>=0; count--) {
        i.value = temp[count];
        i.var_type = ARG;
        local_push(i);
    }
}

void get_params()
{
    struct variable_type *p;
    int i;

    i = lvartos-1;
    do { 
        get_token();    
        p = &local_var_stack[i];
        if(*token!=')') {
            if(tok!=INT && tok!=CHAR) sntx_err(TYPE_EXPECTED);
            p->var_type = token_type;
            get_token();
            strcpy1(p->var_name, token);
            get_token();
            i--;
        }
        else
            break;
    } while(*token==',');
    if(*token!=')') sntx_err(PAREN_EXPECTED); 
}

void func_ret()
{
    int value;
    value = 0;
    eval_exp(&value);
    ret_value = value;
}

void local_push(struct variable_type i)
{
    if(lvartos>NUM_LOCAL_VARS)
        sntx_err(TOO_MANY_LVARS);
    local_var_stack[lvartos] = i;
    lvartos++;
}

int func_pop()
{
    functos--;
    if(functos<0) sntx_err(RET_NOCALL);
    return(call_stack[functos]);
}

void func_push(int i)
{
    if(functos>NUM_FUNC)
        sntx_err(NEST_FUNC);
    call_stack[functos] = i;
    functos++;
}

void assign_var(char *var_name, int value)
{
    int i;

    for(i=lvartos-1; i>=call_stack[functos-1]; i--)    {
        if(!strcmp1(local_var_stack[i].var_name, var_name)) {
            local_var_stack[i].value = value;
            return;
        }
    }
    if(i < call_stack[functos-1]) 
        for(i=0; i<NUM_GLOBAL_VARS; i++)
            if(!strcmp1(global_vars[i].var_name, var_name)) {
                global_vars[i].value = value;
                return;
            }
    sntx_err(NOT_VAR); 
}

int find_var(char *s)
{
    int i;

    for(i=lvartos-1; i>=call_stack[functos-1]; i--) 
        if(!strcmp1(local_var_stack[i].var_name, token))
            return local_var_stack[i].value;
    for(i=0; i<NUM_GLOBAL_VARS; i++)
        if(!strcmp1(global_vars[i].var_name, s))
            return global_vars[i].value;
    sntx_err(NOT_VAR);
}

int is_var(char *s)
{
    int i;

    for(i=lvartos-1; i>=call_stack[functos-1]; i--) 
        if(!strcmp1(local_var_stack[i].var_name, token))
            return 1;
    for(i=0; i<NUM_GLOBAL_VARS; i++)
        if(!strcmp1(global_vars[i].var_name, s))
            return 1;
    return 0; 
}

int isalph(char ch)
{
    if (((ch >='A') && (ch <= 'Z')) 
     || ((ch >='a') && (ch <= 'z')) 
     || (ch == '_'))
        return 1;
    return 0;
}

void exec_if()
{
    int cond;

    eval_exp(&cond); 
    if(cond) { 
        interp_block();
    } else { 
        find_eob(); 
        get_token();
        if(tok!=ELSE) {
            putback(); 
            return;
        }
        interp_block();
    }
}

void exec_while()
{
    int cond;
    char *temp;

    putback();
    temp = prog;   
    get_token();
    eval_exp(&cond);
    if(cond)
        interp_block();
    else {   
        find_eob();
        return;
    }
    prog = temp;  
}

void exec_do()
{
    int cond;
    char *temp;
    putback();
    temp = prog;    
    get_token();    
    interp_block(); 
    get_token(); 
    if(tok!=WHILE) 
        sntx_err(WHILE_EXPECTED);
    eval_exp(&cond);
    if(cond) 
        prog = temp; 
}

void find_eob()
{
    int brace;
    get_token();
    brace = 1;
    do {
        get_token();
        if(*token=='{') 
            brace++;
        else if(*token=='}') 
            brace--;
    } while(brace);
}

void exec_for()
{
    int cond;
    char *temp, *temp2;
    int brace ;
    get_token();
    eval_exp(&cond); 
    if(*token!=';')
        sntx_err(SEMI_EXPECTED);
    prog++; 
    temp = prog;
    for(;;) {
        eval_exp(&cond); 
        if(*token!=';')
            sntx_err(SEMI_EXPECTED);
        prog++;     
        temp2 = prog;
        brace = 1;
        while(brace) {
            get_token();
            if(*token=='(') brace++;
            if(*token==')') brace--;
        }
        if(cond)
            interp_block();  
        else {   
            find_eob();
            return;
        }
        prog = temp2;
        eval_exp(&cond); 
        prog = temp;   
    } 
}

void eval_exp(int *value)
{
    get_token();
    if(!*token) {
        sntx_err(NO_EXP);
        return;
    }
    if(*token==';') {
        *value = 0; 
        return;
    }
    eval_exp0(value);
    putback();
}

void eval_exp0(int *value)
{ char temp[ID_LEN];
    int temp_tok;
    if(token_type==IDENTIFIER) {
        if(is_var(token)) { 
            strcpy1(temp, token);
            temp_tok = token_type;
            get_token();
            if(*token=='=') {
                get_token();
                eval_exp0(value);    
                assign_var(temp, *value);
                return;
            }
            else {  
                putback();  
                strcpy1(token, temp);
                token_type = temp_tok;
            }
        }
    }
    eval_exp1(value);
}

char relops[7] = {
    LT, LE, GT, GE, EQ, NE, 0,
};

void eval_exp1(int *value)
{
    int partial_value;
    char op;
    eval_exp2(value);
    op = *token;
    if(strchr1(relops, op)) {
        get_token();
        eval_exp2(&partial_value);
        switch(op) {  
            case LT:
                *value = *value < partial_value;
                break;
            case LE:
                *value = *value <= partial_value;
                break;
            case GT:
                *value = *value > partial_value;
                break;
            case GE:
                *value = *value >= partial_value;
                break;
            case EQ:
                *value = *value == partial_value;
                break;
            case NE:
                *value = *value != partial_value;
                break;
        }
    }
}

void eval_exp2(int *value)
{
    char    op;
    int partial_value;
    eval_exp3(value);
    while((op = *token) == '+' || op == '-') {
        get_token();
        eval_exp3(&partial_value);
        switch(op) { 
            case '-':
                *value = *value - partial_value;
                break;
            case '+':
                *value = *value + partial_value;
                break;
        }
    }
}

void eval_exp3(int *value)
{
    char    op;
    int partial_value, t;
    eval_exp4(value);
    while((op = *token) == '*' || op == '/' || op == '%') {
        get_token();
        eval_exp4(&partial_value);
        switch(op) { 
            case '*':
                *value = *value * partial_value;
                break;
            case '/':
                *value = (*value) / partial_value;
                break;
            case '&':
                *value = (*value) & partial_value;
                break;
            case '|':
                *value = (*value) | partial_value;
                break;
            case '%':
                t = (*value) / partial_value;
                *value = *value-(t * partial_value); 
                break; 
        }
    }
}

void eval_exp4(int *value)
{
    char    op; 
    op = '\0'; 
    if(*token=='+' || *token=='-') {
        op = *token; 
        get_token(); 
    }
    eval_exp5(value); 
    if(op)
        if(op=='-') 
            *value = -(*value); 
}

void eval_exp5(int *value)
{
    if((*token == '(')) {
        get_token(); 
        eval_exp0(value);
        if(*token != ')') sntx_err(PAREN_EXPECTED); 
        get_token(); 
    }
    else
        atom(value); 
}

void atom(int *value)
{
    int i;
    switch(token_type) {
    case IDENTIFIER:
        i = internal_func(token);
        if(i!= -1)     
            *value = (*intern_func[i].p)();
        else if(find_func(token)) { 
            call();
            *value = ret_value;
        }
        else
            *value = find_var(token); 
        get_token();
        return;
    case NUMBER: 
        *value = atoi1(token);
        get_token();
        return; 
    case DELIMITER: 
        if(*token=='\'') {
            *value = *prog;
            prog++;
            if(*prog!='\'') 
                sntx_err(QUOTE_EXPECTED);
            prog++;
            get_token();
        }
        return;
    default:
        if(*token==')') 
            return;
        else
            sntx_err(SYNTAX); 
    }
}

void sntx_err(int error)
{
    char *p, *temp;
    int linecount = 1;
    int i;
    static char *e[]= {     
        "syntax error", 
        "unbalanced parentheses", 
        "no expression present",
        "equals sign expected",
        "not a variable",
        "parameter error",
        "semicolon expected",
        "unbalanced braces",
        "function undefined",
        "type specifier expected",
        "too many nested function calls",
        "return without call",
        "parentheses expected",
        "while expected",
        "closing quote expected",
        "not a string",
        "too many local variables",
    }; 
    uart0SendString(e[error]); 
    p = p_buf;
    while(p != prog) {  
        p++;
        if(*p == '\n') 
            linecount++;
    }
    uart0SendString(" in line ");
    printNumber(10, 8, FALSE, ' ', linecount);
    uart0SendChar('\n');
    temp = p;
    for(i=0; i<20 && p>p_buf && *p!='\n'; i++, p--);
    for(i=0; i<30 && p<=temp; i++, p++) uart0SendChar(*p);
}

int get_token()
{
    char *temp;
    token_type = 0;
    tok = 0;
    temp = token;
    *temp = '\0';
    while(iswhite(*prog) && *prog) 
        ++prog; 
    if(*prog=='\n') { 
        ++prog;
        while(iswhite(*prog) && *prog) 
            ++prog;
    }
    if(*prog=='\0') {
        *token = '\0';
        tok = FINISHED;
        return(token_type=DELIMITER);
    }
    if(strchr1("{}", *prog)) {
        *temp = *prog;
        temp++;
        *temp = '\0';
        prog++;
        return (token_type = BLOCK);
    }
    if(*prog=='/') {
        if(*(prog+1)=='*') {
            prog += 2;
            do { 
                while(*prog!='*') 
                    prog++;
                prog++;
            } while (*prog!='/');
            prog++;
        }
    }
    if(strchr1("!<>=", *prog)) {
        switch(*prog) {
            case '=':
                if(*(prog+1)=='=') {
                    prog++; prog++;
                    *temp = EQ;
                    temp++; *temp = EQ; temp++;
                    *temp = '\0';
                }
                break;
            case '!':
                if(*(prog+1)=='=') {
                    prog++; prog++;
                    *temp = NE;
                    temp++; *temp = NE; temp++;
                    *temp = '\0';
                }
                break;
            case '<':
                if(*(prog+1)=='=') {
                    prog++; prog++;
                    *temp = LE; temp++; *temp = LE;
                } else {
                    prog++;
                    *temp = LT;
                }
                temp++;
                *temp = '\0';
                break;
            case '>':
                if(*(prog+1)=='=') {
                    prog++; prog++;
                    *temp = GE; temp++; *temp = GE;
                } else {
                    prog++;
                    *temp = GT;
                }
                temp++;
                *temp = '\0';
                break;
        }
        if(*token) 
            return(token_type = DELIMITER);
    }
    if(strchr1("+-*^&|/%=;(),'", *prog)){
        *temp = *prog;
        prog++;
        temp++;
        *temp = '\0'; 
        return (token_type=DELIMITER);
    }
    if(*prog=='"') { 
        prog++;
        while(*prog!='"'&& *prog!='\n') 
            *temp++ = *prog++;
        if(*prog=='\n') 
            sntx_err(SYNTAX);
        prog++; *temp = '\0';
        return(token_type=STRING);
    }
    if(isdigit(*prog)) { 
        while(!isdelim(*prog)) 
            *temp++ = *prog++;
        *temp = '\0';
        return(token_type = NUMBER);
    }
    if(isalph(*prog)) { 
        while(!isdelim(*prog)) 
            *temp++ = *prog++;
        token_type=TEMP;
    }
    *temp = '\0';
    if(token_type==TEMP) {
        tok = look_up(token);
        if(tok) 
            token_type = KEYWORD; 
        else 
            token_type = IDENTIFIER;
    }
    return token_type;
}

void putback() 
{
    char *t; 
    t = token; 
    for(; *t; t++) 
        prog--; 
}

int look_up(char *s)
{
    int i;
    char *p;
    p = s;
    while(*p) { 
        *p |= 0x20; 
        p++; 
    }
    for(i=0; *table[i].command; i++)
        if(!strcmp1(table[i].command, s)) 
            return table[i].tok;
    return 0; 
}

int internal_func(char *s)
{
    int i;
    for(i=0; intern_func[i].f_name[0]; i++) {
        if(!strcmp1(intern_func[i].f_name, s))    
            return i;
    }
    return -1;
}

int isdelim(char c)
{
    if(strchr1(" !;,+-<>'/*%^&|=()", c) || c==9 || c=='\n' || c==0) 
        return 1;
    return 0; 
}

int iswhite(char c)
{
    if(c==' ' || c=='\t' || c=='\r' || c==0x1a)
        return 1;
    else
        return 0;
}

int atoi1(char *cp)
{
	int num, neg, hex;

	num = 0;
	neg = 0;
	hex = 0;
	
	for(;;cp++) {
		switch(*cp) {
		    case ' ':
		    case '\t':
			    continue;
		    case '-':
			    neg++;
		    case '+':
			    cp++;
			case '0':
			    cp++;
			    if ((*cp == 'X') || (*cp == 'x')) {
			        hex++;
			        cp++;
			    }
		}
		break;
	}
    if (hex) {
    	while(*cp) {
            if (*cp >= '0' && *cp <= '9')
    		    num = (num << 4) + *cp - '0';
            if ((*cp >= 'A' && *cp <= 'F') || (*cp >= 'a' && *cp <= 'f'))
    		    num = (num << 4) + (*cp & 0x0F) + 9;
    		cp++;
        }
    	return(num);
   } else {
    	while(*cp >= '0' && *cp <= '9')
    		num = num*10 + *cp++ - '0';
    	return(neg ? -num: num);
   }   
}

void start_check()
{
    get_token();
    if(*token!='(')
        sntx_err(PAREN_EXPECTED);
}

void finish_check()
{
    get_token();
    if(*token!=')') 
        sntx_err(PAREN_EXPECTED);
    get_token();
    if(*token!=';') 
        sntx_err(SEMI_EXPECTED);
    putback();
}

void set()  // usage:  set(int *ip, int i) >>>  *ip = i;
{
    int ii, i, *ip;
    
    start_check();
    eval_exp(&ii);

    get_token(); // look for comma separator 
    if(*token != ',')
        sntx_err(SYNTAX); // syntax error 

    eval_exp(&i);
    ip = (int *)ii;
    *ip = i;
    finish_check();
}

int get()  // usage:   int i = get(int *ip) >>>  i = *ip;
{
    int ii, *ip;
    
    start_check();
    eval_exp(&ii);
    finish_check();
    ip = (int *)ii;
    return *ip;
}

void print()
{
    int i;

    start_check();
    get_token();
    while(*token !=')') { 
        if(token_type==STRING) {  
            uart0SendString(token);
        } else { 
            putback();
            eval_exp(&i);
            printNumber(10, 10, FALSE, ' ', i);
        }
        get_token(); 
    } 
    uart0SendChar('\n');
    get_token();
    if(*token!=';') 
        sntx_err(SEMI_EXPECTED);
    putback();
}

int input()
{
    char ch;

    start_check();
    uart0GetChar(&ch);
    finish_check();
    return (unsigned int)ch;
}

void motors()
{
    int lspeed, rspeed;
    
    start_check();
    eval_exp(&lspeed);
    if ((lspeed < -100) || (lspeed > 100))
        sntx_err(PARAM_ERR);

    get_token(); // look for comma separator 
    if(*token != ',')
        sntx_err(SYNTAX); // syntax error 

    eval_exp(&rspeed);
    if ((rspeed < -100) || (rspeed > 100))
        sntx_err(PARAM_ERR);
    setPWM(lspeed, rspeed);
    finish_check();
}

void delay()
{
    int del;
    
    start_check();
    eval_exp(&del);
    if ((del < 0) || (del > 1000000))
        sntx_err(PARAM_ERR);
    delayMS(del );
    finish_check();
}

int time()
{
    start_check();
    finish_check();
    return readRTC();
}

/*
void imgcap()
{
    unsigned int retry;

    start_check();
    retry = 0;
    while (camera_grab_frame() <= 0)    {
        retry++;
        if (retry > 6) {
            camera_reset("IMJ3");
            // uart0SendString("reset\n");
            retry = 0;
        }
    }
    finish_check();
}

void scan()    // divides field of view into 3 overlapping groups of 40 columns
                            //     output 3 column average to 'X', 'Y', 'Z'
{
    int ix, jx;
    unsigned int itmp, tmax[3], tmean[3], tzeros[3];

    start_check();
    eval_exp(&ix);
    if ((ix < 0) || (ix > 15))
        sntx_err(PARAM_ERR);

    vscan(ix);
        
    for (ix=0; ix<3; ix++) {        // in groups of 40 columns, compute max, mean and # of zeros
        tmax[ix] = tmean[ix] = tzeros[ix] = 0;
        for (jx=0; jx<40; jx++) {
            itmp = tvect[(ix*20) + jx];
            if (tmax[ix] < itmp)
                tmax[ix] = itmp;
            tmean[ix] += itmp;
            if (itmp == 0)
                tzeros[ix]++;
        }
        if (tzeros[ix] < 15)
            tmean[ix] /= 40;
        else
            tmean[ix] = 0;
    }
    assign_var("_x", tmean[0]); 
    assign_var("_y", tmean[1]); 
    assign_var("_z", tmean[2]); 
    finish_check();
}

int blob()    //    search for blob by color, return center point X,Y and width Z
{
    int ix, ii;

    start_check();
    eval_exp(&ix);
    if ((ix < 0) || (ix > 15))
        sntx_err(PARAM_ERR);
        
    vblob(ix);
    ii = blobx2[ix] - blobx1[ix] + 1;
    if (blobcnt[ix] == 0) 
        ii = 0;
    assign_var("_x", (blobx1[ix] + blobx2[ix]) / 2); 
    assign_var("_y", (bloby1[ix] + bloby2[ix]) / 2); 
    assign_var("_z", ii); 
    finish_check();
    return blobcnt[ix];
}

void bitdir()    // set direction of i/o pins E9 E10 E11 E12 E13
{
    int ix;
    
    start_check();
    eval_exp(&ix);
    if ((ix < 0) || (ix > 63))
        sntx_err(PARAM_ERR);
    bits_dir((unsigned char)(ix & 0x000000FF));
    finish_check();
}

void bitset()    // write to i/o pins E9 E10 E11 E12 E13
{
    int ix;
    
    start_check();
    eval_exp(&ix);
    if ((ix < 0) || (ix > 63))
        sntx_err(PARAM_ERR);
    bits_write((unsigned char)(ix & 0x000000FF));
    finish_check();
}

int bitget()    // read i/o pins E9 E10 E11 E12 E13
{
    start_check();
    finish_check();
    return (unsigned int)bits_read();
}
*/

