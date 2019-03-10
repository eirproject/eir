#include <stdint.h>
#include <stdio.h>

// Size of a pointer.
// On 64 bit this is 64 bits unsigned.
typedef uint64_t ERL_NIF_TERM;

// This will always be a opaque pointer when used.
struct enif_environment_t;
typedef struct enif_environment_t ErlNifEnv;

int enif_get_int(ErlNifEnv *env, ERL_NIF_TERM term, int *ip);
ERL_NIF_TERM enif_make_int(ErlNifEnv *env, int i);

ERL_NIF_TERM enif_make_badarg(ErlNifEnv *env);
ERL_NIF_TERM enif_raise_exception(ErlNifEnv *env, ERL_NIF_TERM term);

//int GNIF7_testing3_woo1_n_n(ERL_NIF_TERM *ret, ErlNifEnv* env, ERL_NIF_TERM term);

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

//ERL_NIF_TERM foo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
//
//    //int intout;
//    //int intret = enif_get_int(env, argv[0], &intout);
//    //printf("GIR %d %d\n", intret, intout);
//
//    ERL_NIF_TERM ret_term = 11;
//    int ret2 = GNIF7_testing3_woo1_n_n(&ret_term, env, argv[0]);
//    if (ret2) {
//        return ret_term;
//    } else {
//        return enif_raise_exception(env, ret_term);
//    }
//    
//    //printf("Ret: %d %lu\n", ret2, ret_term);
//
//    //int x, ret;
//    //if (!enif_get_int(env, argv[0], &x)) {
//	//    return enif_make_badarg(env);
//    //}
//    //return enif_make_int(env, x + 1);
//}


