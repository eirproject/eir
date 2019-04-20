use super::NifTarget;
use crate::Data;
use super::beam_data_structures::*;

use eir::FunctionIdent;
use eir::Module as EirModule;

use inkwell::AddressSpace;
use inkwell::values::{ BasicValueEnum, FunctionValue };
use inkwell::types::BasicTypeEnum;

impl NifTarget {

    pub fn gen_export(&self, data: &Data, module: &EirModule,
                      functions: &[FunctionIdent]) {

        let mut funcs = Vec::new();

        for ident in functions.iter() {
            println!("{:?}", ident);
            let fun = &module.functions[ident];
            let proto = data.protos[ident];
            let wrapper = self.gen_wrapper(data, ident, proto);
            funcs.push((ident.clone(), wrapper));
        }

        let entry_t = EnifEntryT {
            major: 2,
            minor: 14,
            name: module.name.as_str().into(),
            funcs: funcs.iter().map(|(ident, fun)| {
                EnifFuncT {
                    name: ident.name.as_str().to_string(),
                    arity: ident.arity as u32,
                    fun: fun.as_global_value().as_pointer_value(),
                    flags: 0,
                }
            }).collect(),
            load: Some(
                self.types.on_load
                    .as_global_value().as_pointer_value()),
            reload: None,
            upgrade: None,
            unload: None,
            vm_variant: "beam.vanilla".into(),
            options: 0,
            sizeof_erlnifresourcetypeinit: 0,
            min_erts: "0.0".into(),
        };
        let nif_entry = make_enif_entry_t(&data.context, &data.module, &entry_t);
        let nif_entry_pointer = nif_entry.as_pointer_value();

        // Make nif init
        let nif_init_fn_type = (nif_entry_pointer.get_type()).fn_type(&[], false);
        let nif_init_fn_val = data.module.add_function("nif_init", nif_init_fn_type, None);
        let builder = data.context.create_builder();
        {
            let basic_block = data.context.append_basic_block(&nif_init_fn_val, "entry");
            builder.position_at_end(&basic_block);

            builder.build_return(Some(&nif_entry_pointer));
        }

        // TODO: Generate wrappers for up to max arity
        self.gen_callarg_wrappers(data, 10);

    }

    fn gen_callarg_wrappers(&self, data: &Data, num: usize) {
        let i8_ptr = data.context.i8_type().ptr_type(AddressSpace::Generic);

        let funs: Vec<_> = (0..num)
            .map(|n| self.gen_callarg_wrapper(data, n)
                 .as_global_value().as_pointer_value()
                 .const_cast(i8_ptr))
            .collect();
        let const_val = i8_ptr.const_array(&funs);

        let val_arr = data.module.add_global(
            const_val.get_type(), Some(AddressSpace::Const),
            "nifrt_callarg_wrappers_arr");
        val_arr.set_initializer(&const_val);
        let val_arr_ptr = val_arr.as_pointer_value();

        let val = data.module.add_global(
            val_arr_ptr.get_type(), Some(AddressSpace::Const),
            "nifrt_callarg_wrappers");
        val.set_initializer(&val_arr_ptr);
    }

    fn gen_callarg_wrapper(&self, data: &Data, num: usize) -> FunctionValue {
        let void_type = data.context.void_type();
        let i64_type = data.context.i64_type();

        let mut inner_fp_args = vec![
            self.types.env_ptr_type,
        ];
        inner_fp_args.extend((0..num).map(|_| self.types.term_type));
        let fp_type = void_type.fn_type(&inner_fp_args, false);

        let callarg_sig = void_type.fn_type(
            &[
                fp_type.ptr_type(AddressSpace::Generic).into(),
                self.types.env_ptr_type.into(),
                self.types.term_ptr_type.into(),
            ],
            false,
        );

        let callarg_fn = data.module.add_function(
            &format!("callarg_{}a", num),
            callarg_sig,
            None,
        );

        let entry_bb = data.context.append_basic_block(&callarg_fn, "entry");

        let builder = data.context.create_builder();
        builder.position_at_end(&entry_bb);

        let mut nif_call_args = vec![
            callarg_fn.get_nth_param(1).unwrap(), // Env
        ];

        let term_arr_ptr = callarg_fn.get_nth_param(2).unwrap();
        nif_call_args.extend((0..num).map(
            |n| {
                let curr_term_ptr = unsafe {
                    builder.build_gep(term_arr_ptr.into_pointer_value(), &[
                        i64_type.const_int(n as u64, false),
                    ], "")
                };
                builder.build_load(curr_term_ptr, "")
            }));

        builder.build_call(
            callarg_fn.get_nth_param(0).unwrap().into_pointer_value(),
            &nif_call_args,
            "",
        );
        builder.build_unreachable();

        callarg_fn
    }

    fn gen_wrapper(&self, data: &Data, ident: &FunctionIdent, inner: FunctionValue) -> FunctionValue {

        assert!(!ident.lambda.is_some());

        let i64_type = data.context.i64_type();
        let i8_type = data.context.i8_type();
        let i8_ptr = i8_type.ptr_type(AddressSpace::Generic);

        let fn_val = data.module.add_function("nif_wrapper", self.types.nif_fun_type, None);

        let builder = data.context.create_builder();

        let entry_bb = fn_val.append_basic_block("entry");
        builder.position_at_end(&entry_bb);

        let ret = builder.build_call(self.types.nifrt_launchpad, &[
            fn_val.get_nth_param(0).unwrap(),
            fn_val.get_nth_param(2).unwrap(),
            fn_val.get_nth_param(1).unwrap(),
            inner.as_global_value().as_pointer_value()
                .const_cast(self.types.i8_ptr.into_pointer_type())
                .into(),
        ], "");
        let ret_val = ret.try_as_basic_value().left().unwrap();
        builder.build_return(Some(&ret_val));

        //let ret_bb = fn_val.append_basic_block("return");
        //let raise_bb = fn_val.append_basic_block("return");

        // Main block
        //builder.position_at_end(&entry_bb);

        //let string_const = crate::primitives::make_c_string_const(
        //    &data.context, &data.module, &format!("NIF\n"));
        //let string_const_ptr = string_const.as_pointer_value()
        //    .const_cast(i8_ptr);
        //builder.build_call(self.types.printf,
        //                   &[string_const_ptr.into()], "debug print");

        //let env_arg = fn_val.get_nth_param(0).unwrap();
        //let arr_arg = fn_val.get_nth_param(2).unwrap();
        //let ret_term_ptr_val = builder.build_alloca(self.types.term_type, "ret term");

        //let mut inner_args: Vec<BasicValueEnum> = vec![
        //    ret_term_ptr_val.into(),
        //    env_arg.into(),
        //];
        //inner_args.extend((0..(ident.arity)).map(|n| {
        //    let num = i64_type.const_int(n as u64, false);
        //    let gep = unsafe { builder.build_gep(
        //        arr_arg.into_pointer_value(), &[num], "arg") };
        //    builder.build_load(gep, "arg")
        //}));

        //let inner_call = builder.build_call(inner, &inner_args, "inner");
        //let inner_call_ret = inner_call.try_as_basic_value().left().unwrap();

        //let ret_term_val = builder.build_load(ret_term_ptr_val, "");

        //builder.build_conditional_branch(inner_call_ret.into_int_value(),
        //                                 &ret_bb, &raise_bb);

        //// Return block
        //builder.position_at_end(&ret_bb);
        //builder.build_return(Some(&ret_term_val));

        //// Throw block
        //builder.position_at_end(&raise_bb);
        //let call = builder.build_call(self.types.enif_raise_exception, &[
        //    env_arg.into(),
        //    ret_term_val.into(),
        //], "raise");
        //builder.build_return(Some(&call.try_as_basic_value().left().unwrap()));

        fn_val
    }

}

