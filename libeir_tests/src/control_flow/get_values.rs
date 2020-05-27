use crate::lower;

use libeir_passes::PassManager;
use libeir_syntax_erl::ParseConfig;

//-module('Elixir.Keyword').
//
//get_values(_keywords@1, _key@1)
//    when
//        is_list(_keywords@1)
//        andalso
//        is_atom(_key@1) ->
//    _fun@1 =
//        fun({_key@2,_val@1}) when _key@1 =:= _key@2 ->
//               {true,_val@1};
//           ({_,_}) ->
//               false
//        end,
//    lists:filtermap(_fun@1, _keywords@1).

#[test]
fn get_values() {
    let _ = simple_logger::init_by_env();

    let mut eir_mod = lower(
        "-module('Elixir.Keyword').

get_values(_key@1) ->
    A = fun(_key@2) ->
       _key@1 =:= _key@2
    end,
    A.
",
        ParseConfig::default(),
    )
    .unwrap();

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    for fun in eir_mod.function_iter() {
        let _ = fun.function().live_values();
    }
}
