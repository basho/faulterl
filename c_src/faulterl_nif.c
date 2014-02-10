// -------------------------------------------------------------------
//
// fault injection primitives
//
// Copyright (c) 2014 Basho Technologies, Inc. All Rights Reserved.
//
// This file is provided to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file
// except in compliance with the License.  You may obtain
// a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//
// -------------------------------------------------------------------
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <stdint.h>
#include <time.h>
#include <assert.h>
#include <string.h>
#include <dlfcn.h>

#include "erl_nif.h"
#include "erl_driver.h"
#include "erl_nif_compat.h"

// Atoms (initialized in on_load)
static ERL_NIF_TERM ATOM_ALLOCATION_ERROR;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_NOT_FOUND;
static ERL_NIF_TERM ATOM_OK;

// Prototypes
ERL_NIF_TERM faulterl_nif_peek(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM faulterl_nif_poke(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

char     *poke_test_scratch = NULL;
int8_t    peek_test_int8  = 42;
int32_t   peek_test_int32 = 3242;
int64_t   peek_test_int64 = 6442;
int8_t    peek_test_intarray8[]  = {42, 43, 44};
int32_t   peek_test_intarray32[] = {4242, 4343, 4444};
int64_t   peek_test_intarray64[] = {424242, 434343, 444444};
char *peek_test_string = "It is 42, yes.";

static ErlNifFunc nif_funcs[] =
{
    {"peek_int", 5, faulterl_nif_peek},
    {"poke_int", 4, faulterl_nif_poke}
};

ERL_NIF_TERM faulterl_nif_peek(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char sym[4096];
    int index;
    int size;
    int as_string = 0;
    int deref_p = 0;
    char **pp, *p;

    if (enif_get_string(env, argv[0], sym, sizeof(sym), ERL_NIF_LATIN1) &&
        enif_get_int(env, argv[1], (int*)&index) &&
        enif_get_int(env, argv[2], (int*)&size) &&
        enif_get_int(env, argv[3], (int*)&as_string) &&
        enif_get_int(env, argv[4], (int*)&deref_p))
    {
        if ((pp = dlsym(RTLD_DEFAULT, sym)) == NULL)
        {
            return ATOM_NOT_FOUND;
        }
        if ((index < 0) ||
            (! ((as_string == 1 && (size == 1)) ||
                (as_string == 0 && (size == 8 || size == 32 || size == 64)))))
        {
            return ATOM_ERROR;
        }

        if (as_string)
        {
            p = *pp;
            return enif_make_tuple2(env, ATOM_OK,
                                    enif_make_string(env, p + (index * size), ERL_NIF_LATIN1));
        } else {
            ErlNifBinary bin;
            uint8_t *base8;
            uint32_t *base32;
            uint64_t *base64;

            bin.size = size / 8;
            if (!enif_alloc_binary_compat(env, bin.size, &bin))
            {
                return enif_make_tuple2(env, ATOM_ERROR, ATOM_ALLOCATION_ERROR);
            }
            switch (size) {
            case 8:
                base8 = (deref_p) ? (uint8_t *) *pp : (uint8_t *) pp;
                memcpy(bin.data, &base8[index], bin.size);
                break;
            case 32:
                base32 = (deref_p) ? (uint32_t *) *pp : (uint32_t *) pp;
                memcpy(bin.data, &base32[index], bin.size);
                break;
            case 64:
                base64 = (deref_p) ? (uint64_t *) *pp : (uint64_t *) pp;
                memcpy(bin.data, &base64[index], bin.size);
                break;
            }
            /* memcpy(bin.data, pp + (index), bin.size); */
            return enif_make_tuple2(env, ATOM_OK, enif_make_binary(env, &bin));
        }
    }
    return ATOM_ERROR;
}

ERL_NIF_TERM faulterl_nif_poke(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char sym[4096], val[4096];
    ErlNifBinary bin;
    int index;
    int deref_p = 0;
    char **pp, *p;
    uint8_t *base8;
    uint32_t *base32;
    uint64_t *base64;

    if (enif_get_string(env, argv[0], sym, sizeof(sym), ERL_NIF_LATIN1) &&
        enif_get_int(env, argv[1], (int*)&index) &&
        /* We will look at argv[2] later */
        enif_get_int(env, argv[3], (int*)&deref_p))
    {
        if ((pp = dlsym(RTLD_DEFAULT, sym)) == NULL)
        {
            return ATOM_NOT_FOUND;
        }
        if (enif_inspect_binary(env, argv[2], &bin))
        {
            switch (bin.size * 8) {
            case 8:
                base8 = (deref_p) ? (uint8_t *) *pp : (uint8_t *) pp;
                memcpy(&base8[index], bin.data, bin.size);
                break;
            case 32:
                base32 = (deref_p) ? (uint32_t *) *pp : (uint32_t *) pp;
                memcpy(&base32[index], bin.data, bin.size);
                break;
            case 64:
                base64 = (deref_p) ? (uint64_t *) *pp : (uint64_t *) pp;
                memcpy(&base64[index], bin.data, bin.size);
                break;
            default:
                return ATOM_ERROR;
            }
            return ATOM_OK;
        }
        if (enif_get_string(env, argv[2], val, sizeof(val), ERL_NIF_LATIN1)) {
            p = *pp;
            strcpy(&p[index], val);
            return ATOM_OK;
        }
    }
    return ATOM_ERROR;
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    // Initialize atoms that we use throughout the NIF.
    ATOM_ALLOCATION_ERROR = enif_make_atom(env, "allocation_error");
    ATOM_ERROR = enif_make_atom(env, "error");
    ATOM_NOT_FOUND = enif_make_atom(env, "not_found");
    ATOM_OK = enif_make_atom(env, "ok");

    if (poke_test_scratch == NULL) {
        poke_test_scratch = calloc(4096, 1);
    }
    return 0;
}

ERL_NIF_INIT(faulterl_nif, nif_funcs, &on_load, NULL, NULL, NULL);

