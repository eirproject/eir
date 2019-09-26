#![allow(unused_macros)]

macro_rules! build_ir {
    ($module:ident : $name:ident / $arity:expr => $body:tt) => {
        #[allow(unused_variables, unused_mut, unused_assignments, redundant_semicolon)]
        {
            use libeir_intern::Ident;

            let ident = $crate::FunctionIdent {
                module: Ident::from_str(std::stringify!($module).into()),
                name: Ident::from_str(std::stringify!($name).into()),
                arity: $arity,
            };

            let mut fun = $crate::Function::new(ident);

            {
                let mut b = fun.builder();
                build_ir!(INTERNAL_MACRO; BLOCKS; &mut b; $body);
            }

            fun
        }
    };

    (INTERNAL_MACRO; BLOCKS; $b:expr; { $(
        $block_name:ident ( $($block_arg:ident),* ) {
            $(
                $($body_item:tt)*;
            )*
        };
    )* }) => {
        {
            use std::stringify;
            use std::collections::HashMap;
            use $crate::{Block, Value};

            let mut b = $b;
            let mut entry = None;

            let mut block_map: HashMap<&'static str, Block> =
                HashMap::new();
            let mut value_map: HashMap<&'static str, Value> =
                HashMap::new();

            // First pass, create blocks and arguments
            $(
                let block_name = stringify!($block_name);

                let block = b.block_insert();
                if entry.is_none() {
                    entry = Some(block);
                }

                assert!(!value_map.contains_key(&block_name));
                block_map.insert(block_name, block);
                value_map.insert(block_name, b.value(block));

                $(
                    let arg_name = stringify!($block_arg);
                    let arg_value = b.block_arg_insert(block);
                    assert!(!value_map.contains_key(&arg_name));
                    value_map.insert(arg_name, arg_value);
                )*;

            )*;

            // Second pass, create primops, constants and bodies
            $(
                let block_name = stringify!($block_name);
                let block = block_map[block_name];

                $(
                    build_ir!(INTERNAL_MACRO; BLOCK_ITEM; $b; value_map; block;
                              $($body_item_part)*);
                )*;
            )*;

        }
    };

    (INTERNAL_MACRO; BLOCK_ITEM; $b:expr; $value_map:expr; $block:expr;
     $var_name:ident = $($var_tts:tt)*) => {
    };

    (INTERNAL_MACRO; BLOCK_ITEM; $b:expr; $value_map:expr; $block:expr;
     call $call_name:ident ( $($call_arg:ident),* )) => {
        {
            let call_dest = build_ir!(
                INTERNAL_MACRO; VALUE; $value_map; $call_name);
            let call_args = &[$(build_ir!(
                INTERNAL_MACRO; VALUE; $value_map; $call_arg)),*];

            $b.op_call($block, call_dest, call_args);
        }
    };

    (INTERNAL_MACRO; VALUE; $value_map:expr; $value_name:ident) => {
        {
            let value_name = stringify!($value_name);
            *$value_map.get(value_name).unwrap()
        }
    };
}

#[cfg(test)]
mod tests {

    #[test]
    fn basic_ir_build() {

        build_ir!(
            test:test/0 => {
                b_entry(ret, exc) {
                    call b_1(c_true);
                };
                b_1() {
                    call ret(b_1);
                };
            }
        );

    }


}
