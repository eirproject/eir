use libeir_util_number::traits::Float;

// Copied from https://github.com/reem/rust-ordered-float

// masks for the parts of the IEEE 754 float
const SIGN_MASK: u64 = 0x8000_0000_0000_0000u64;
const EXP_MASK: u64 = 0x7ff0_0000_0000_0000u64;
const MAN_MASK: u64 = 0x000f_ffff_ffff_ffffu64;

// canonical raw bit patterns (for hashing)
const CANONICAL_NAN_BITS: u64 = 0x7ff8_0000_0000_0000u64;
const CANONICAL_ZERO_BITS: u64 = 0x0u64;

#[inline]
pub fn raw_double_bits<F: Float>(f: &F) -> u64 {
    if f.is_nan() {
        return CANONICAL_NAN_BITS;
    }

    let (man, exp, sign) = f.integer_decode();
    if man == 0 {
        return CANONICAL_ZERO_BITS;
    }

    //let exp_u64 = unsafe { mem::transmute::<i16, u16>(exp) } as u64;
    let exp_u64 = exp as u64;
    let sign_u64 = if sign > 0 { 1u64 } else { 0u64 };
    (man & MAN_MASK) | ((exp_u64 << 52) & EXP_MASK) | ((sign_u64 << 63) & SIGN_MASK)
}
