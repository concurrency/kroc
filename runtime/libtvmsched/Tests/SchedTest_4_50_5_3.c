/* Automatically generated test with
 * 
 * 4 PROCESSORS (must match constant in wf-sched.h)
 * 5 = dispatch count (must match constant in wf-sched.h)
 * 50 Processes
 * 3 = window size
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <wf-sched.h>
void main()
{
	pthread_t t1, t2, t3, t4;

	logical_processor_t pr1 = malloc(sizeof(struct logical_processor));
	logical_processor_t pr2 = malloc(sizeof(struct logical_processor));
	logical_processor_t pr3 = malloc(sizeof(struct logical_processor));
	logical_processor_t pr4 = malloc(sizeof(struct logical_processor));

	pr1->id = 0;
	pr2->id = 1;
	pr3->id = 2;
	pr4->id = 3;

	pr1->partner = 1;
	pr2->partner = 2;
	pr3->partner = 3;
	pr4->partner = 0;

	global_proc_pointer[0]=pr1;
	global_proc_pointer[1]=pr2;
	global_proc_pointer[2]=pr3;
	global_proc_pointer[3]=pr4;

	int i=0;
	for(i=0;i<NUMBER_OF_PROCS;i++)
		global_procs[i]=1;

	/* CODE TO SET UP PROCESSES AND BATCHES */


	batch_t b11 = malloc(sizeof(struct batch));
	process_t p111 = malloc(sizeof(struct process));

	p111->id = 111;
	b11->head = p111;
	b11->stolen = 0;
	b11->window = 0;

	process_t p112 = malloc(sizeof(struct process));
	p112->id = 112;
	process_t p113 = malloc(sizeof(struct process));
	p113->id = 113;
	process_t p114 = malloc(sizeof(struct process));
	p114->id = 114;
	process_t p115 = malloc(sizeof(struct process));
	p115->id = 115;
	process_t p116 = malloc(sizeof(struct process));
	p116->id = 116;
	process_t p117 = malloc(sizeof(struct process));
	p117->id = 117;
	process_t p118 = malloc(sizeof(struct process));
	p118->id = 118;
	process_t p119 = malloc(sizeof(struct process));
	p119->id = 119;
	process_t p1110 = malloc(sizeof(struct process));
	p1110->id = 1110;
	process_t p1111 = malloc(sizeof(struct process));
	p1111->id = 1111;
	process_t p1112 = malloc(sizeof(struct process));
	p1112->id = 1112;
	process_t p1113 = malloc(sizeof(struct process));
	p1113->id = 1113;
	process_t p1114 = malloc(sizeof(struct process));
	p1114->id = 1114;
	process_t p1115 = malloc(sizeof(struct process));
	p1115->id = 1115;
	process_t p1116 = malloc(sizeof(struct process));
	p1116->id = 1116;

	p111->next = p112;
	p112->next = p113;
	p113->next = p114;
	p114->next = p115;
	p115->next = p116;
	p116->next = p117;
	p117->next = p118;
	p118->next = p119;
	p119->next = p1110;
	p1110->next = p1111;
	p1111->next = p1112;
	p1112->next = p1113;
	p1113->next = p1114;
	p1114->next = p1115;
	p1115->next = p1116;
	p1116->next = NULL;

	b11->window = 1;
	pr1->head = b11;
	pr1->tail = b11;

	batch_t b21 = malloc(sizeof(struct batch));
	process_t p211 = malloc(sizeof(struct process));

	p211->id = 211;
	b21->head = p211;
	b21->stolen = 0;
	b21->window = 0;

	process_t p212 = malloc(sizeof(struct process));
	p212->id = 212;

	p211->next = p212;
	p212->next = NULL;

	batch_t b22 = malloc(sizeof(struct batch));
	process_t p221 = malloc(sizeof(struct process));

	p221->id = 221;
	b22->head = p221;
	b22->stolen = 0;
	b22->window = 0;

	process_t p222 = malloc(sizeof(struct process));
	p222->id = 222;
	process_t p223 = malloc(sizeof(struct process));
	p223->id = 223;

	p221->next = p222;
	p222->next = p223;
	p223->next = NULL;

	batch_t b23 = malloc(sizeof(struct batch));
	process_t p231 = malloc(sizeof(struct process));

	p231->id = 231;
	b23->head = p231;
	b23->stolen = 0;
	b23->window = 0;

	process_t p232 = malloc(sizeof(struct process));
	p232->id = 232;

	p231->next = p232;
	p232->next = NULL;

	batch_t b24 = malloc(sizeof(struct batch));
	process_t p241 = malloc(sizeof(struct process));

	p241->id = 241;
	b24->head = p241;
	b24->stolen = 0;
	b24->window = 0;

	process_t p242 = malloc(sizeof(struct process));
	p242->id = 242;
	process_t p243 = malloc(sizeof(struct process));
	p243->id = 243;
	process_t p244 = malloc(sizeof(struct process));
	p244->id = 244;

	p241->next = p242;
	p242->next = p243;
	p243->next = p244;
	p244->next = NULL;

	b21->window = 1;
	b22->window = 1;
	b23->window = 1;
	pr2->head = b21;
	pr2->tail = b23;

	batch_t b31 = malloc(sizeof(struct batch));
	process_t p311 = malloc(sizeof(struct process));

	p311->id = 311;
	b31->head = p311;
	b31->stolen = 0;
	b31->window = 0;

	process_t p312 = malloc(sizeof(struct process));
	p312->id = 312;
	process_t p313 = malloc(sizeof(struct process));
	p313->id = 313;
	process_t p314 = malloc(sizeof(struct process));
	p314->id = 314;
	process_t p315 = malloc(sizeof(struct process));
	p315->id = 315;
	process_t p316 = malloc(sizeof(struct process));
	p316->id = 316;
	process_t p317 = malloc(sizeof(struct process));
	p317->id = 317;

	p311->next = p312;
	p312->next = p313;
	p313->next = p314;
	p314->next = p315;
	p315->next = p316;
	p316->next = p317;
	p317->next = NULL;

	batch_t b32 = malloc(sizeof(struct batch));
	process_t p321 = malloc(sizeof(struct process));

	p321->id = 321;
	b32->head = p321;
	b32->stolen = 0;
	b32->window = 0;

	process_t p322 = malloc(sizeof(struct process));
	p322->id = 322;
	process_t p323 = malloc(sizeof(struct process));
	p323->id = 323;

	p321->next = p322;
	p322->next = p323;
	p323->next = NULL;

	batch_t b33 = malloc(sizeof(struct batch));
	process_t p331 = malloc(sizeof(struct process));

	p331->id = 331;
	b33->head = p331;
	b33->stolen = 0;
	b33->window = 0;


	p331->next = NULL;

	batch_t b34 = malloc(sizeof(struct batch));
	process_t p341 = malloc(sizeof(struct process));

	p341->id = 341;
	b34->head = p341;
	b34->stolen = 0;
	b34->window = 0;

	process_t p342 = malloc(sizeof(struct process));
	p342->id = 342;

	p341->next = p342;
	p342->next = NULL;

	b31->window = 1;
	b32->window = 1;
	b33->window = 1;
	pr3->head = b31;
	pr3->tail = b33;

	batch_t b41 = malloc(sizeof(struct batch));
	process_t p411 = malloc(sizeof(struct process));

	p411->id = 411;
	b41->head = p411;
	b41->stolen = 0;
	b41->window = 0;

	process_t p412 = malloc(sizeof(struct process));
	p412->id = 412;
	process_t p413 = malloc(sizeof(struct process));
	p413->id = 413;
	process_t p414 = malloc(sizeof(struct process));
	p414->id = 414;
	process_t p415 = malloc(sizeof(struct process));
	p415->id = 415;
	process_t p416 = malloc(sizeof(struct process));
	p416->id = 416;
	process_t p417 = malloc(sizeof(struct process));
	p417->id = 417;

	p411->next = p412;
	p412->next = p413;
	p413->next = p414;
	p414->next = p415;
	p415->next = p416;
	p416->next = p417;
	p417->next = NULL;

	batch_t b42 = malloc(sizeof(struct batch));
	process_t p421 = malloc(sizeof(struct process));

	p421->id = 421;
	b42->head = p421;
	b42->stolen = 0;
	b42->window = 0;

	process_t p422 = malloc(sizeof(struct process));
	p422->id = 422;
	process_t p423 = malloc(sizeof(struct process));
	p423->id = 423;

	p421->next = p422;
	p422->next = p423;
	p423->next = NULL;

	b41->window = 1;
	b42->window = 1;
	pr4->head = b41;
	pr4->tail = b41;

	pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;
	pr1->run_queue_lock = &m1;
	pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;
	pr2->run_queue_lock = &m2;
	pthread_mutex_t m3 = PTHREAD_MUTEX_INITIALIZER;
	pr3->run_queue_lock = &m3;
	pthread_mutex_t m4 = PTHREAD_MUTEX_INITIALIZER;
	pr4->run_queue_lock = &m4;

	pthread_create (&t1, NULL, test_run, pr1);
	pthread_create (&t2, NULL, test_run, pr2);
	pthread_create (&t3, NULL, test_run, pr3);
	pthread_create (&t4, NULL, test_run, pr4);

	pthread_join (t1, NULL);
	pthread_join (t2, NULL);
	pthread_join (t3, NULL);
	pthread_join (t4, NULL);

}
