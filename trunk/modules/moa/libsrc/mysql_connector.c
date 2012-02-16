#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "mysql.h"
#include <cif.h>

enum commands{MYSQL_INIT = 0, MYSQL_CONNECT, MYSQL_QUERY, MYSQL_STORE_RESULT, MYSQL_NUM_ROWS, MYSQL_FIELD_COUNT, MYSQL_FETCH_ROW, MYSQL_NULL_VALUES, MYSQL_FREE_RESULT, MYSQL_CLOSE, MYSQL_CLEANUP, MYSQL_QUIT, MYSQL_FETCH_FIELD, MYSQL_AFFECTED_ROWS};
enum status_nr{SUCCESS = 0, FAILURE};
enum mysql_type{TINY = 0, SHORT, LONG, INT24, LONGLONG, DECIMAL, NEWDECIMAL, FLOAT, DOUBLE, BIT, TIMESTAMP, DATE, TIME, DATETIME, YEAR, STRING, VAR_STRING, BLOB, SET, ENUM, GEOMETRY, TYPE_NULL, UNKNOWN};

static char *dummy_arg[] = {"dummy_arg"};

static char *read_string(Workspace wptr, Channel *args){
	mt_array_t *ma;
	char *str = NULL;

	MTChanIn(wptr, args, &ma);
	
	if(ma) {
		int size = ma->dimensions[0];
		
		str = (char*) malloc(sizeof(char) * (size + 1));
		memcpy(str, ma->data, size);
		str[size] = '\0';

		MTRelease (wptr, ma);
	}

	return str;
}

/* MySQL fields are null-terminated arrays of arbitrary data 
 * but the length is specified without the null */
static void write_field(Workspace wptr, Channel *out_data, unsigned int length, const char *str){
	mt_array_t *ma = MTAllocArray(wptr, MT_NUM_BYTE, 1, length);
	memcpy(ma->data, str, length);
	MTChanOut(wptr, out_data, &ma);
}

static void write_null(Workspace wptr, Channel *out_data){
	mt_array_t *ma = NULL;
	MTChanOut(wptr, out_data, &ma);
}

/* take a null-terminated C string and write it as an occam string */
static void write_string(Workspace wptr, Channel *out_data, const char *str){
	const int size = strlen(str);
	mt_array_t *ma = MTAllocArray(wptr, MT_NUM_BYTE, 1, size);
	
	assert(str);

	memcpy(ma->data, str, size);
	MTChanOut(wptr, out_data, &ma);
}

static void report_error(Workspace wptr, Channel *out_status, Channel *out_data, MYSQL *db){
	ChanOutInt(wptr, out_status, FAILURE);
	ChanOutInt(wptr, out_status, mysql_errno(db));
	write_string(wptr, out_data, mysql_error(db));
}				

void MySQL_connector(Workspace wptr){
	Channel *command = ProcGetParam(wptr, 0, Channel *);
	Channel *args = ProcGetParam(wptr, 1, Channel *);
	Channel *argi = ProcGetParam(wptr, 2, Channel *); 
	Channel *out_status = ProcGetParam(wptr, 3, Channel *);
	Channel *out_data = ProcGetParam(wptr, 4, Channel *);
	
	unsigned int cmd, fields, field_type;
	MYSQL *db;
	MYSQL_RES *result;
	MYSQL_ROW row;
	MYSQL_FIELD field;
	unsigned long *lengths;
	my_ulonglong affected_rows;

	mt_array_t *ma;
	char *host, *user, *passwd, *db_name, *unix_socket;
	unsigned int port, fieldnr;

	int i; 
	
	for(;;){
		ChanInInt(wptr, command, &cmd);
		switch(cmd){
		case MYSQL_INIT:
			db = mysql_init(NULL);
			if(db) ChanOutInt(wptr, out_status, SUCCESS);
			else ChanOutInt(wptr, out_status, FAILURE);
			break;
		case MYSQL_CONNECT:	
			host = read_string(wptr, args);
			user = read_string(wptr, args);
			passwd = read_string(wptr, args);
			db_name = read_string(wptr, args);
			ChanInInt(wptr, argi, &port);
			unix_socket = read_string(wptr, args);
			
			if(mysql_real_connect(db,host,user,passwd,db_name,port,unix_socket,0))
				ChanOutInt(wptr, out_status, SUCCESS);
			else
				report_error(wptr, out_status, out_data, db);
			break;
		case MYSQL_QUERY:
			MTChanIn(wptr, args, &ma);
			if(!mysql_real_query(db, (char *) ma->data, (int) ma->dimensions[0]))
				ChanOutInt(wptr, out_status, SUCCESS);
			else
				report_error(wptr, out_status, out_data, db);
			MTRelease(wptr, ma);
			break;
		case MYSQL_STORE_RESULT:
			result = mysql_store_result(db);
			if(result){
				ChanOutInt(wptr, out_status, SUCCESS);
			}
			else
				report_error(wptr, out_status, out_data, db);
			break;
		case MYSQL_NUM_ROWS:
			assert(result);
			ChanOutInt(wptr, out_status, mysql_num_rows(result));
			break;
		case MYSQL_FIELD_COUNT:
			ChanOutInt(wptr, out_status, mysql_field_count(db));
			break;
		case MYSQL_FETCH_ROW:
			assert(result);
			row = mysql_fetch_row(result);
			if(row){
				ChanOutInt(wptr, out_status, SUCCESS);
				lengths = mysql_fetch_lengths(result);
				assert(lengths);
				fields = mysql_field_count(db);
				for(i = 0; i < fields; i++){
					if(lengths[i])
						write_field(wptr, out_data, lengths[i], row[i]);		
					else if(row[i]) /* empty field */
						write_null(wptr, out_data);
					else  /* null field */
						write_null(wptr, out_data);
				}
			}
			else ChanOutInt(wptr, out_status, FAILURE);
			break;
		case MYSQL_NULL_VALUES:
			if(row){
				ma = MTAllocArray(wptr, MT_NUM_BYTE, 1, fields);
				for(i = 0; i < fields; i++)
					((char*) ma->data)[i] = !(row[i]);
				MTChanOut(wptr, out_data, &ma);
			}
			else write_null(wptr, out_data);
			break;
		case MYSQL_FREE_RESULT:
			mysql_free_result(result);
			break;
		case MYSQL_CLOSE:
			mysql_close(db);
			break;
		case MYSQL_CLEANUP:
			free(host);
			free(user);
			free(passwd);
			free(db_name);
			free(unix_socket);
			break;
		case MYSQL_QUIT:
			return;
		case MYSQL_FETCH_FIELD:
			assert(result);
			ChanInInt(wptr, argi, &fieldnr);
			field = *mysql_fetch_field_direct(result, fieldnr);
			write_string(wptr, out_data, field.name);
			write_string(wptr, out_data, field.org_name);		
			write_string(wptr, out_data, field.table);		
			write_string(wptr, out_data, field.org_table);		
			write_string(wptr, out_data, field.db);		
			ChanOutInt(wptr, out_status, field.length);
			ChanOutInt(wptr, out_status, field.max_length);
			ChanOutInt(wptr, out_status, field.decimals);
			ChanOutInt(wptr, out_status, field.charsetnr);
			switch(field.type){
			case MYSQL_TYPE_TINY:
				field_type = TINY;
				break;
			case MYSQL_TYPE_SHORT:
				field_type = SHORT;
				break;
			case MYSQL_TYPE_LONG:
				field_type = LONG;
				break;
			case MYSQL_TYPE_INT24:
				field_type = INT24;
				break;
			case MYSQL_TYPE_LONGLONG:
				field_type = LONGLONG;
				break;
			case MYSQL_TYPE_DECIMAL:
				field_type = DECIMAL;
				break;
			case MYSQL_TYPE_NEWDECIMAL:
				field_type = NEWDECIMAL;
				break;
			case MYSQL_TYPE_FLOAT:
				field_type = FLOAT;
				break;
			case MYSQL_TYPE_DOUBLE:
				field_type = DOUBLE;
				break;
			case MYSQL_TYPE_BIT:
				field_type = BIT;
				break;
			case MYSQL_TYPE_TIMESTAMP:
				field_type = TIMESTAMP;
				break;
			case MYSQL_TYPE_DATE:
				field_type = DATE;
				break;
			case MYSQL_TYPE_TIME:
				field_type = TIME;
				break;
			case MYSQL_TYPE_DATETIME:
				field_type = DATETIME;
				break;
			case MYSQL_TYPE_YEAR:
				field_type = YEAR;
				break;
			case MYSQL_TYPE_STRING:
				field_type = STRING;
				break;
			case MYSQL_TYPE_VAR_STRING:
				field_type = VAR_STRING;
				break;
			case MYSQL_TYPE_BLOB:
				field_type = BLOB;
				break;
			case MYSQL_TYPE_SET:
				field_type = SET;
				break;
			case MYSQL_TYPE_ENUM:
				field_type = ENUM;
				break;
			case MYSQL_TYPE_GEOMETRY:
				field_type = GEOMETRY;
				break;
			case MYSQL_TYPE_NULL:
				field_type = TYPE_NULL;
				break;
			default:
				field_type = -1;
				break;
			}
			ChanOutInt(wptr, out_status, field_type);
			ChanOutInt(wptr, out_status, field.flags && NOT_NULL_FLAG);
			ChanOutInt(wptr, out_status, field.flags && PRI_KEY_FLAG);
			ChanOutInt(wptr, out_status, field.flags && UNIQUE_KEY_FLAG);
			ChanOutInt(wptr, out_status, field.flags && MULTIPLE_KEY_FLAG);
			ChanOutInt(wptr, out_status, field.flags && UNSIGNED_FLAG);
			ChanOutInt(wptr, out_status, field.flags && ZEROFILL_FLAG);
			ChanOutInt(wptr, out_status, field.flags && BINARY_FLAG);
			ChanOutInt(wptr, out_status, field.flags && AUTO_INCREMENT_FLAG);
			ChanOutInt(wptr, out_status, field.charsetnr == 63);	
			break;
		case MYSQL_AFFECTED_ROWS:
			affected_rows = mysql_affected_rows(db);
			if(affected_rows != (my_ulonglong) -1)
				ChanOutInt(wptr, out_status, (int) affected_rows);
			else
				ChanOutInt(wptr, out_status, -1);
			break;
		default:
			printf("\nMoA: MySQL_connector internal error - unknown command\n");
			SetErr();
			return;
		}
	}
}

void _library_init(int **ws){
	int error = mysql_library_init(-1, dummy_arg, NULL);
	**ws = error;
}

void _library_end(){
	mysql_library_end();
}

