#include <stdint.h>
#include <stdio.h>
#include <erl_nif.h>

int GNIF6_erlang2__p2_n_n(ERL_NIF_TERM *ret, ErlNifEnv* env, 
        ERL_NIF_TERM term1, ERL_NIF_TERM term2) {
    int i1, i2;
    if (!enif_get_int(env, term1, &i1)) {
        *ret = enif_make_int(env, -1);
        return 0;
    }
    if (!enif_get_int(env, term2, &i2)) {
        *ret = enif_make_int(env, -1);
        return 0;
    }
    *ret = enif_make_int(env, i1 + i2);
    return 1;
}
