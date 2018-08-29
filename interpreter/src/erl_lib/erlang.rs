use ::{ NativeModule, Term, CallReturn };
use ::num_bigint::{ BigInt, Sign };

fn bignum_to_f64(n: &BigInt) -> Option<f64> {
    // ieee float layout:
    // 1b sign
    // 11b exponent
    // 52b fraction

    let (sign, data) = n.to_bytes_be();
    if data.len() == 0 {
        return Some(0f64);
    }

    assert!(data[0] != 0);

    // Construct exponent
    let mut exp: u64 = n.bits() as u64;
    if exp < (64 - 12) {
        exp = 0;
    } else {
        exp -= 64 - 12;
    }
    exp += 1023;
    if exp >= 2048 {
        return None;
    }
    exp = exp << 52;

    // Collect largest part of number
    let mut fraction_part: u64 = 0;
    if data.len() > 7 {
        for b in &data[0..8] {
            fraction_part = (fraction_part << 8) | (*b as u64);
        }
    } else {
        for b in &data {
            fraction_part = (fraction_part << 8) | (*b as u64);
        }
    }

    // Offset data to be contained within fraction part
    let leading_zeros = fraction_part.leading_zeros();
    if leading_zeros < 12 {
        fraction_part = fraction_part >> (12 - leading_zeros);
    }

    // Construct number
    let mut ret: u64 = 0;

    // Sign
    if sign == Sign::Minus {
        ret |= 1 << 63;
    }

    // Exponent
    ret |= exp;

    // Fraction
    ret |= fraction_part;

    Some(f64::from_bits(ret))
}

fn add(args: &[Term]) -> CallReturn {
    // TODO: Verify semantics
    println!("{:?}", args);
    
    if args.len() != 2 {
        return CallReturn::Throw;
    }
    let a1 = &args[0];
    let a2 = &args[1];

    match (a1, a2) {
        (Term::Integer(ref i1), Term::Integer(ref i2)) =>
            CallReturn::Return { term: Term::Integer(i1 + i2) },
        (Term::Integer(ref i1), Term::Float(f2)) => {
            let f1 = bignum_to_f64(i1);
            if f1 == None {
                CallReturn::Throw
            } else {
                CallReturn::Return { term: Term::Float(f1.unwrap() + f2) }
            }
        }
        (Term::Float(f1), Term::Integer(ref i2)) => {
            let f2 = bignum_to_f64(i2);
            if f2 == None {
                CallReturn::Throw
            } else {
                CallReturn::Return { term: Term::Float(f1 + f2.unwrap()) }
            }
        }
        (Term::Float(f1), Term::Float(f2)) => {
            CallReturn::Return { term: Term::Float(f1 + f2) }
        }
        _ => CallReturn::Throw 
    }
}

pub fn make_erlang() -> NativeModule {
    let mut module = NativeModule::new("erlang".to_string());
    module.add_fun("+".to_string(), 2, Box::new(add));
    module
}
