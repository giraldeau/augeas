/*
 * Benchmark utility
 */

#include <stdio.h>
#include <stdlib.h>

#include <time.h> // for gettimeofday
#include <sys/time.h>
#include <string.h>

#include "list.h"
#include "fa.h"
#include "hash.h"

#define UCHAR_NUM (UCHAR_MAX+1)
#define UCHAR_MIN 0
typedef unsigned char uchar;

struct fa {
    struct state *initial;
    int           deterministic : 1;
    int           minimal : 1;
    unsigned int  nocase : 1;
    int           trans_re : 1;
};

struct state {
    struct state *next;
    hash_val_t    hash;
    unsigned int  accept : 1;
    unsigned int  live : 1;
    unsigned int  reachable : 1;
    /* Array of transitions. The TUSED first entries are used, the array
       has allocated room for TSIZE */
    size_t        tused;
    size_t        tsize;
    struct trans *trans;
};

struct trans {
    struct state *to;
    union {
        struct {
            uchar         min;
            uchar         max;
        };
        struct re *re;
    };
};

#define last_trans(s)  ((s)->trans + (s)->tused - 1)

#define for_each_trans(t, s)                                            \
    for (struct trans *t = (s)->trans;                                  \
         (t - (s)->trans) < (s)->tused;                                 \
         t++)

void test_timer(void);
void test_compile_automaton(void);
char *gen_seq(int i);

void test_timer(){
	struct timeval tv1, tv2;

	gettimeofday(&tv1, NULL);
	do {
		gettimeofday(&tv2, NULL);
	} while (tv1.tv_usec == tv2.tv_usec);

	printf("Precision: %ld us\n", tv2.tv_usec - tv1.tv_usec +
			1000000 * (tv2.tv_sec - tv1.tv_sec));
}

void timed_test(){

}

void test_compile_automaton(){
	struct timeval tv1, tv2;

	struct fa *fa1, *fa2, *fa3 = NULL;
	const char *re1 = "[a-z]*";
	const char *re2 = "abc";
	gettimeofday(&tv1, NULL);

	fa_compile(re1, strlen(re1), &fa1);
	fa_compile(re2, strlen(re2), &fa2);
	fa3 = fa_minus(fa1, fa2);
	fa_minimize(fa3);

	gettimeofday(&tv2, NULL);
	printf("Time: %ld us\n", tv2.tv_usec - tv1.tv_usec +
			1000000 * (tv2.tv_sec - tv1.tv_sec));

	char *res = NULL;
	size_t len;
	fa_as_regexp(fa3, &res, &len);
	fa_example(fa3, &res, &len);
	printf("Res: %s\n", res);

	fa_dot(stdout, fa1);
	fa_dot(stdout, fa2);
	fa_dot(stdout, fa3);
}

char *gen_seq(int len){
	const char *alnum = "0123456789abzdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
	int alnum_len = strlen(alnum);
	int i, r;
	char *array = malloc(len+1);
	if (array == NULL){
		return NULL;
	}

	for(i=0;i<len;i++){
		r = rand() % alnum_len;
		array[i] = alnum[r];
	}
	array[len] = '\0';
	return array;
}

int main(int argc, char**argv)
{
	srand ( time(NULL) );

	//test_timer();
	//test_compile_automaton();

	return 0;
}

