/*
** Original C source, written prior to creating the faulterl code generator.
** Used to compare the generator's output via "diff".  Contents of this
** file are now deprecated.
*/
/*
 -------------------------------------------------------------------

 fault injection primitives

 Copyright (c) 2014 Basho Technologies, Inc. All Rights Reserved.

 This file is provided to you under the Apache License,
 Version 2.0 (the "License"); you may not use this file
 except in compliance with the License.  You may obtain
 a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing,
 software distributed under the License is distributed on an
 "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 KIND, either express or implied.  See the License for the
 specific language governing permissions and limitations
 under the License.

 -------------------------------------------------------------------
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <dlfcn.h>
#include <sys/types.h>
#include <sys/errno.h>
#include <time.h>
#include <string.h>
#include <unistd.h>

u_int8_t  bc_fi_enabled = 1;
u_int8_t  bc_fi_verbose = 1;

u_int8_t  bc_fi_random_enabled = 1;
u_int8_t  bc_fi_random_disabled_trigger = 1;
u_int8_t  bc_fi_random_verbose = 0;
u_int32_t bc_fi_random_seed = 0;
u_int8_t  bc_fi_random_reseed = 0;

u_int8_t  bc_fi_switchpanel_enabled = 1;
u_int8_t  bc_fi_switchpanel_disabled_trigger = 1;
u_int8_t  bc_fi_switchpanel_verbose = 0;
#define SWITCHPANEL_SIZE 128
u_int8_t  bc_fi_switchpanel_array[SWITCHPANEL_SIZE];
u_int8_t  bc_fi_switchpanel_default = 1;

u_int8_t  bc_fi_unlink_verbose = 0;
u_int8_t  bc_fi_unlink_fake_errno = EREMOTE;
int       bc_fi_unlink_fake_return = -1;


typedef struct {
    char *i_name; /* Must be present */
    int percent;
} bc_fi_random_t;


typedef struct {
    char *i_name; /* Must be present */
    int index;
} bc_fi_switchpanel_t;

static void init_random()
{

    time_t t;

    if (!bc_fi_random_seed) {
        bc_fi_random_seed = time(&t);
    }
    srand(bc_fi_random_seed);

}

static void init_switchpanel()
{

    int i;

    for (i = 0; i < SWITCHPANEL_SIZE; i++) {
        bc_fi_switchpanel_array[i] = bc_fi_switchpanel_default;
    }

}

static void init_all_once()
{
    static int done = 0;

    if (!done) {
        init_random();
        init_switchpanel();
        done = 1;
    }
}

static void new_random_instance(bc_fi_random_t *a, char *i_name, int percent)
{
    init_all_once();
    a->i_name = malloc(strlen(i_name) + 1);
    strcpy(a->i_name, i_name);

    a->percent = percent;
}

static void new_switchpanel_instance(bc_fi_switchpanel_t *a, char *i_name, int index)
{
    init_all_once();
    a->i_name = malloc(strlen(i_name) + 1);
    strcpy(a->i_name, i_name);

    a->index = index;
}

int trigger_random(bc_fi_random_t *a, char *intercept_name)
{
    int res;

    if (! bc_fi_random_enabled) {
        res = bc_fi_random_disabled_trigger;
        if (bc_fi_verbose || bc_fi_random_verbose) {
            fprintf(stderr, "trigger_random: not enabled, trigger=%d\r\n", res);
        }
        return res;
    }

    if (bc_fi_random_reseed) {
        srand(bc_fi_random_seed);
        bc_fi_random_reseed = 0;
    }
    res = (rand() % 100) < a->percent;

    if (bc_fi_verbose || bc_fi_random_verbose) {
        fprintf(stderr, "trigger_random: name=%s intercept=%s trigger=%d\r\n",
                a->i_name, intercept_name, res);
    }
    return res;
}

int trigger_switchpanel(bc_fi_switchpanel_t *a, char *intercept_name)
{
    int res;

    if (! bc_fi_switchpanel_enabled) {
        res = bc_fi_switchpanel_disabled_trigger;
        if (bc_fi_verbose || bc_fi_switchpanel_verbose) {
            fprintf(stderr, "trigger_switchpanel: not enabled, trigger=%d\r\n", res);
        }
        return res;
    }

    res = bc_fi_switchpanel_array[a->index];

    if (bc_fi_verbose || bc_fi_switchpanel_verbose) {
        fprintf(stderr, "trigger_switchpanel: name=%s intercept=%s trigger=%d\r\n",
                a->i_name, intercept_name, res);
    }
    return res;
}

int unlink(const char *path)
{
    static bc_fi_switchpanel_t *a_switchpanel_42 = NULL;
    static bc_fi_random_t *a_random_50 = NULL;
    static char *real_name = "unlink";
    static int (*real)();
    int trigger;
    int res;

    if (bc_fi_enabled) {
        if (a_switchpanel_42 == NULL) {
            char *i_name = "switchpanel_42";

            a_switchpanel_42 = malloc(sizeof(bc_fi_switchpanel_t));
            new_switchpanel_instance(a_switchpanel_42, i_name, 42);
        }
        if (a_random_50 == NULL) {
            char *i_name = "random_50";

            a_random_50 = malloc(sizeof(bc_fi_random_t));
            new_random_instance(a_random_50, i_name, 50);
        }
        if (real == NULL) {
            if ((real = (int (*)()) dlsym(RTLD_NEXT, real_name)) == NULL) {
                fprintf(stderr, "Fatal error: %s\r\n", dlerror());
                abort();
            }
        }
        trigger = 1;
        if (trigger) {
            trigger = trigger_switchpanel(a_switchpanel_42, real_name);
        }
        if (trigger) {
            trigger = trigger_random(a_random_50, real_name);
        }
    } else {
        trigger = 0;
    }

    if (trigger) {
        errno = bc_fi_unlink_fake_errno;
        res = bc_fi_unlink_fake_return;
    } else {
        res = (*real)(path);
    }
    if (bc_fi_verbose || bc_fi_unlink_verbose) {
        fprintf(stderr, "intercept: %s: return=%d\r\n",
                real_name, res);
    }
    return res;
}
