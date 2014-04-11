%% -------------------------------------------------------------------
%%
%% fault injection primitives
%%
%% Copyright (c) 2014 Basho Technologies, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% Example:
%%
%% faulterl_nif:poke("bc_fi_enabled", 1, 8).
%% faulterl_nif:poke("opt_bpslimit", 30, 32).
%% timer:tc(fun() -> [net_adm:ping('bar@sbb3') || _ <- lists:seq(1,5)] end).
%%
%% ... which (in a single experiment) takes about 23 seconds.  :-)

%% Continuing with the example: disable throttling for the disterl TCP
%% connection to 'bar@sbb3':
%%
%% trigger_switchpanel:set(faulterl_util:disterl_fd_num('bar@sbb3'), 0).
%% timer:tc(fun() -> [net_adm:ping('bar@sbb3') || _ <- lists:seq(1,5)] end).
%% -> less than 1 msec.
%%
%% Then turn throttling back on for that node:
%% trigger_switchpanel:set(faulterl_util:disterl_fd_num('bar@sbb3'), 1).

-module(intercept_inet).
-export([config/0]).

-include("faulterl.hrl").

config() ->
    IntArgC_CHeaders = ["<sys/fcntl.h>"],
    IntArgC_TStruct = "
typedef struct {
    char *i_name;  /* Must be present */
}",
    IntArgC_TNewInstanceArgs = "char *not_used",
    IntArgC_TNewInstanceBody = "",
    IntArgC_TBody_throttle = "
    int o_value;
    socklen_t o_size = sizeof(o_value);

    if (getsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &o_value, &o_size) == 0) {
        return 1;
    } else {
        return 0;
    }
",

    IntArgC_TBody_switchpanel = "
       if (fd >= SWITCHPANEL_SIZE || fd < 0) {
           res = 0;
       } else {
           res = bc_fi_switchpanel_array[fd];
       }
",

    WritevReturnGeneric = "
#define WRITEV_BYTE_LIMIT (64*1024)
        struct iovec fake[1];
        int byte_max;
        int i;
    
        fake[0].iov_base = iov[0].iov_base;
        if (opt_bpslimit > 131072) {
             byte_max = WRITEV_BYTE_LIMIT;
        } else if (opt_bpslimit > 32768) {
             byte_max = 2048;
        } else {
            byte_max = 64;
        }
        fake[0].iov_len = MIN(iov[0].iov_len, byte_max);

        /* fprintf(stderr, \"len=%zu,\", fake[0].iov_len); */
        nb_limit_io(fake[0].iov_len);
        res = (*real)(fd, fake, 1);
",

    [trigger_random:config()] ++
    [trigger_switchpanel:config()] ++
    [trigger_args_of_intercept:config(
         IntArgC_CHeaders, [], [],
         true, true, false,
         IntArgC_TStruct, "", IntArgC_TNewInstanceArgs, IntArgC_TNewInstanceBody,
         InterceptName, IntArgC_TBody_throttle,
         InterceptName, "fd_throttle") || InterceptName <- ["writev"]
    ] ++ 
    [trigger_args_of_intercept:config(
         [], [], [],
         true, true, false,
         IntArgC_TStruct, "", IntArgC_TNewInstanceArgs, IntArgC_TNewInstanceBody,
         InterceptName, IntArgC_TBody_switchpanel,
         InterceptName, "fd_switchpanel") || InterceptName <- ["writev"]
    ] ++ 
    [
     #fi{
         name = "socket",
         type = intercept,
         intercept_args = "int domain, int type, int protocol",
         intercept_args_call = "domain, type, protocol",
         c_headers = ["<sys/socket.h>"],
         intercept_errno = "EINTR",
         intercept_return_type = "int",
         intercept_return_value = "-1",
         intercept_triggers = [{"random", "random_always", "0"}]
     },
     #fi{
         name = "netbrake_dummy_trigger",
         type = trigger,
         c_headers = ["<sys/time.h>"],
         c_decl_extra = ["u_int32_t opt_bpslimit;"] ++ [netbrake_c()],
         extra_global_syms = ["opt_bpslimit"],
         trigger_struct = "
typedef struct {
    char *i_name; /* Must be present */
}",
         trigger_new_instance_args="int unused"
        },
     #fi{
         name = "writev",
         type = intercept,
         intercept_args = "int fd, const struct iovec *iov, int iovcnt",
         intercept_args_call = "fd, iov, iovcnt",
         c_headers = ["<unistd.h>"],
         intercept_errno = "EINTR",
         intercept_return_type = "int",
         intercept_return_value = "-1",
         intercept_return_generic = WritevReturnGeneric,
         intercept_triggers = [{"random", "random_always", "100"},
                               {"i_arg_writev_fd_switchpanel", "\"unusED1\""},
                               {"i_arg_writev_fd_throttle", "\"unusED2\""}]
     }
    ].

netbrake_c() ->
    "
/* http://www.hping.org/netbrake/ */
/* Begin netbrake code, see COPYRIGHT below */

#ifdef COPYRIGHT

COPYRIGHT AND PERMISSION NOTICE

Copyright (c) 2001 Salvatore Sanfilippo

All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
\"Software\"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, and/or sell copies of the Software, and to permit persons
to whom the Software is furnished to do so, provided that the above
copyright notice(s) and this permission notice appear in all copies of
the Software and that both the above copyright notice(s) and this
permission notice appear in supporting documentation.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT
OF THIRD PARTY RIGHTS. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
HOLDERS INCLUDED IN THIS NOTICE BE LIABLE FOR ANY CLAIM, OR ANY SPECIAL
INDIRECT OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES WHATSOEVER RESULTING
FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

Except as contained in this notice, the name of a copyright holder
shall not be used in advertising or otherwise to promote the sale, use
or other dealings in this Software without prior written authorization
of the copyright holder.

#endif /* COPYRIGHT */

#define NB_LOADTABLE_SZ 		10

#define MIN(x,y) (((x)<(y))?(x):(y))

/* structures */
struct nb_loadtable_ele {
	long long usec;
	size_t load;
};

static struct nb_loadtable_ele nb_loadtable[NB_LOADTABLE_SZ];
static int nb_loadtable_index = 0;

static long long nb_getusec(void);
static void nb_loadtable_add(size_t load);
static void nb_loadtable_get_bps(int *bps, long long *time, size_t *bytes);
static void nb_limit_io(int load);
static void nb_log(char *fmt, ...);

void nb_limit_io(int load)
{
	int bps, average_bps;
	long long time, total_time;
	size_t bytes;
	static size_t total_bytes = 0;
	static long long initial_time = 0;

	/* Syscall on error? */
	if (load < 0)
		return;
	
	/* limit the (rougly speaking) instantaous speed... */
	/* Add the new sample in the table */
	nb_loadtable_add(load);
	nb_loadtable_get_bps(&bps, &time, &bytes);
	nb_log(\"Inst BPS: %d\\r\\n\", bps);
	if (bps > opt_bpslimit) {
		long long ideal_time;
		unsigned int sleep_sec, sleep_usec;

		ideal_time = ((float)bytes/opt_bpslimit)*1000000;
		sleep_sec = (ideal_time - time)/1000000;
		sleep_usec = (ideal_time - time)%1000000;
		nb_log(\"Inst sleep (%d > %d) s=%u us=%u\\r\\n\", bps, opt_bpslimit, sleep_sec, sleep_usec);
		sleep(sleep_sec);
		usleep(sleep_usec);
	}

	/* then limit the average speed -- this saves us from approximations
	 * usleep() limits and so on */
	/* We need the initial time to compute the average speed */
	if (initial_time == 0)
		initial_time = nb_getusec();

	total_bytes += load;
	total_time = nb_getusec() - initial_time;
	if (total_time > 1000000) {
		long long ideal_time;
		unsigned int sleep_sec, sleep_usec;

		average_bps = (float)total_bytes/((float)total_time/1000000);
		nb_log(\"average BPS: %d\\r\\n\", average_bps);
		if (average_bps > opt_bpslimit) {
			ideal_time = ((float)total_bytes/opt_bpslimit)*1000000;
			sleep_sec = (ideal_time - total_time)/1000000;
			sleep_usec = (ideal_time - total_time)%1000000;
			nb_log(\"avr sleep s=%u us=%u\\r\\n\", sleep_sec, sleep_usec);
			sleep(sleep_sec);
			usleep(sleep_usec);
		}
	}
}

/* This function return the byte-per-second using the last
 * NB_LOADTABLE_SZ samples.
 * The first (in report time) load is excluded from the sum
 * since we use this as initial time, otherwise you get it
 * overstimed */
void nb_loadtable_get_bps(int *bps, long long *time, size_t *bytes)
{
	long long min, max = 0;
	size_t sum = 0, load_min;
	int i;
	long difftime;

	min = nb_loadtable[0].usec;
	load_min = nb_loadtable[0].load;
	for (i = 0; i < NB_LOADTABLE_SZ; i++) {
		if (nb_loadtable[i].usec == 0)
			continue;
		if (min > nb_loadtable[i].usec) {
			min = nb_loadtable[i].usec;
			load_min = nb_loadtable[i].load;
		}
		if (max < nb_loadtable[i].usec)
			max = nb_loadtable[i].usec;
		sum += nb_loadtable[i].load;
	}
	difftime = max-min;
	if (!difftime) {
		*bps = 0;
		return;
	}
	sum -= load_min;
	*bps = ((float)sum/((float)difftime/1000000));
	if (time) *time = difftime;
	if (bytes) *bytes = sum;
}

void nb_loadtable_add(size_t load)
{
	nb_loadtable[nb_loadtable_index].usec = nb_getusec();
	nb_loadtable[nb_loadtable_index].load = load;
	nb_loadtable_index = (nb_loadtable_index+1)%NB_LOADTABLE_SZ;
}

long long nb_getusec(void)
{
	struct timeval tv;

	gettimeofday(&tv, NULL);
	return ((long long)tv.tv_sec*1000000)+tv.tv_usec;
}

void nb_log(char *fmt, ...)
{
#ifdef COMMENT
	va_list ap;

	if (0)
		return;
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
#endif /* COMMENT */
}

/* End netbrake code */

".
