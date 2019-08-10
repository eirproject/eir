use rug::Integer;
use rug::integer::Order;

use super::{ BitSlice, BitRead, BitWrite };

#[derive(Debug, Copy, Clone)]
pub enum Endian {
    Big,
    Little,
}
impl Endian {
    fn to_order(&self) -> Order {
        match self {
            Endian::Big => Order::Msf,
            Endian::Little => Order::Lsf,
        }
    }
}

pub fn integer_to_carrier(mut int: Integer, bits: usize, endian: Endian) -> BitSlice<Vec<u8>> {

    let negative = int < 0;
    if negative {
        int += 1;
    }

    let keep_bytes = (bits + 7) / 8;
    let aux_bits = bits % 8;

    let needed_bytes = std::cmp::max(keep_bytes, int.significant_digits::<u8>());

    let mut digits: Vec<u8> = Vec::with_capacity(needed_bytes);
    // Safe because write_digits always initializes all elements.
    unsafe { digits.set_len(needed_bytes) };
    int.write_digits(&mut digits, endian.to_order());

    if negative {
        for digit in digits.iter_mut() {
            *digit = !*digit;
        }
    }

    match endian {
        Endian::Big => {
            if aux_bits > 0 {
                digits[0] &= !(!0 << aux_bits);
                BitSlice::with_offset_length(digits, 8 - aux_bits, bits)
            } else {
                BitSlice::with_offset_length(digits, 0, bits)
            }
        }
        Endian::Little => {
            if aux_bits > 0 {
                digits[keep_bytes-1] <<= 8 - aux_bits;
            }
            BitSlice::with_offset_length(digits, 0, bits)
        }
    }

}

fn carrier_to_buf<C>(carrier: C, signed: bool, endian: Endian) -> (Vec<u8>, bool)
where
    C: BitRead<T = u8>
{
    let bit_len = carrier.bit_len();
    let num_bytes = (bit_len + 7) / 8;

    let aux_bits = bit_len % 8;
    let aux_bits_wrap = if aux_bits == 0 { 8 } else { aux_bits };

    let offset = match endian {
        Endian::Big => 8 - aux_bits_wrap,
        Endian::Little => 0,
    };

    let mut buf = vec![0; num_bytes];
    {
        let mut slice = BitSlice::with_offset_length(
            &mut buf, offset, bit_len);
        slice.write(carrier);
    }

    let mut last = match endian {
        Endian::Big => buf[0],
        Endian::Little => buf[num_bytes - 1] >> aux_bits,
    };

    // Sign extend
    let mut sign = false;
    if signed {
        sign = last & (1 << (aux_bits_wrap - 1)) != 0;
        if sign {
            last |= !(!0 >> (8 - aux_bits_wrap));
        }
    }

    match endian {
        Endian::Big => buf[0] = last,
        Endian::Little => buf[num_bytes - 1] = last,
    }

    (buf, sign)
}

pub fn carrier_to_integer<C>(carrier: C, signed: bool, endian: Endian) -> Integer
where
    C: BitRead<T = u8>
{
    let (mut buf, sign) = carrier_to_buf(carrier, signed, endian);

    if sign {
        for elem in buf.iter_mut() {
            *elem = !*elem;
        }
        let mut int = Integer::from_digits(&buf, endian.to_order());
        int *= -1;
        int -= 1;
        int
    } else {
        Integer::from_digits(&buf, endian.to_order())
    }
}

#[cfg(test)]
mod tests {
    use rug::Integer;
    use super::{ integer_to_carrier, carrier_to_integer, carrier_to_buf, Endian };
    use super::super::{ BitSlice, BitWrite };

    #[test]
    fn integer_adapter_basic() {
        let int = Integer::from(0b00001111_00010000);

        {
            let conv = integer_to_carrier(int.clone(), 16, Endian::Little);
            let mut out: [u8; 2] = [0; 2];
            out.write(&conv);
            assert!(out[0] == 0b00010000);
            assert!(out[1] == 0b00001111);
        }

        {
            let conv = integer_to_carrier(int.clone(), 16, Endian::Big);
            let mut out: [u8; 2] = [0; 2];
            out.write(&conv);
            assert!(out[0] == 0b00001111);
            assert!(out[1] == 0b00010000);
        }

    }

    #[test]
    fn integer_adapter_unaligned() {
        let int = Integer::from(0b00001111_00010000);

        {
            let conv = integer_to_carrier(int.clone(), 12, Endian::Little);

            let mut out: [u8; 2] = [0; 2];
            {
                let mut slice = BitSlice::with_offset_length(
                    &mut out as &mut [u8], 0, 12);
                slice.write(conv);
            }

            assert!(out[0] == 0b00010000);
            assert!(out[1] == 0b11110000);
        }

        {
            let conv = integer_to_carrier(int.clone(), 12, Endian::Big);

            let mut out: [u8; 2] = [0; 2];
            {
                let mut slice = BitSlice::with_offset_length(
                    &mut out as &mut [u8], 0, 12);
                slice.write(conv);
            }

            assert!(out[0] == 0b11110001);
            assert!(out[1] == 0b00000000);
        }

    }

    #[test]
    fn integer_adapter_negative() {

        {
            let int = Integer::from(-5);
            let conv = integer_to_carrier(int.clone(), 16, Endian::Big);
            let mut out: i16 = 0;
            out.write(&conv);
            dbg!(out);
            assert!(out == -5);
        }

        {
            let int = Integer::from(-10000);
            let conv = integer_to_carrier(int.clone(), 16, Endian::Big);
            let mut out: i16 = 0;
            out.write(&conv);
            dbg!(out);
            assert!(out == -10000);
        }

    }

    #[test]
    fn integer_carrier_to_buf() {

        {
            let int = Integer::from(5);
            let conv = integer_to_carrier(int.clone(), 16, Endian::Big);
            let mut buf: [u8; 2] = [0; 2];
            buf.write(&conv);

            let (out, sign) = carrier_to_buf(&buf as &[u8], false, Endian::Big);
            assert!(sign == false);
            assert!(out[0] == 0);
            assert!(out[1] == 5);

            let (out, sign) = carrier_to_buf(&buf as &[u8], true, Endian::Big);
            assert!(out[0] == 0);
            assert!(out[1] == 5);
            assert!(sign == false);
        }

        {
            let int = Integer::from(0b00000101_01010101);
            let conv = integer_to_carrier(int.clone(), 12, Endian::Big);

            let mut buf: [u8; 2] = [0; 2];
            let mut carrier = BitSlice::with_offset_length(
                &mut buf as &mut [u8], 0, 12);

            carrier.write(&conv);

            let (out, sign) = carrier_to_buf(&carrier, false, Endian::Big);
            assert!(out[0] == 0b00000101);
            assert!(out[1] == 0b01010101);
            assert!(sign == false);

            let (out, sign) = carrier_to_buf(&carrier, true, Endian::Big);
            assert!(out[0] == 0b00000101);
            assert!(out[1] == 0b01010101);
            assert!(sign == false);
        }

        {
            let int = Integer::from(0b00001010_10101010);
            let conv = integer_to_carrier(int.clone(), 12, Endian::Big);

            let mut buf: [u8; 2] = [0; 2];
            let mut carrier = BitSlice::with_offset_length(
                &mut buf as &mut [u8], 0, 12);

            carrier.write(&conv);

            let (out, sign) = carrier_to_buf(&carrier, false, Endian::Big);
            assert!(out[0] == 0b00001010);
            assert!(out[1] == 0b10101010);
            assert!(sign == false);

            let (out, sign) = carrier_to_buf(&carrier, true, Endian::Big);
            assert!(out[0] == 0b11111010);
            assert!(out[1] == 0b10101010);
            assert!(sign == true);
        }

        {
            let int = Integer::from(0b00001010_10101010);
            let conv = integer_to_carrier(int.clone(), 12, Endian::Little);

            let mut buf: [u8; 2] = [0; 2];
            let mut carrier = BitSlice::with_offset_length(
                &mut buf as &mut [u8], 0, 12);

            carrier.write(&conv);

            let (out, sign) = carrier_to_buf(&carrier, false, Endian::Little);
            assert!(out[0] == 0b10101010);
            assert!(out[1] == 0b00001010);
            assert!(sign == false);

            let (out, sign) = carrier_to_buf(&carrier, true, Endian::Little);
            assert!(out[0] == 0b10101010);
            assert!(out[1] == 0b11111010);
            assert!(sign == true);
        }

    }

    #[test]
    fn integer_round_trip_basic() {

        {
            let int = Integer::from(5);
            let conv = integer_to_carrier(int.clone(), 16, Endian::Big);
            let mut buf: [u8; 2] = [0; 2];
            buf.write(&conv);
            let back = carrier_to_integer(&buf as &[u8], true, Endian::Big);
            assert!(int == back);
        }

        {
            let int = Integer::from(-5);
            let conv = integer_to_carrier(int.clone(), 16, Endian::Big);
            let mut buf: [u8; 2] = [0; 2];
            buf.write(&conv);
            let back = carrier_to_integer(&buf as &[u8], true, Endian::Big);
            assert!(int == back);
        }

        {
            let int = Integer::from(5);
            let conv = integer_to_carrier(int.clone(), 16, Endian::Little);
            let mut buf: [u8; 2] = [0; 2];
            buf.write(&conv);
            let back = carrier_to_integer(&buf as &[u8], true, Endian::Little);
            assert!(int == back);
        }

        {
            let int = Integer::from(-5);
            let conv = integer_to_carrier(int.clone(), 16, Endian::Little);
            let mut buf: [u8; 2] = [0; 2];
            buf.write(&conv);
            let back = carrier_to_integer(&buf as &[u8], true, Endian::Little);
            assert!(int == back);
        }

    }

    #[test]
    fn integer_round_trip_unaligned() {

        {
            let int = Integer::from(5);
            let conv = integer_to_carrier(int.clone(), 12, Endian::Big);

            let mut buf: [u8; 2] = [0; 2];
            let mut carrier = BitSlice::with_offset_length(
                &mut buf as &mut [u8], 0, 12);

            carrier.write(&conv);

            let back = carrier_to_integer(&carrier, true, Endian::Big);
            assert!(int == back);
        }

        {
            let int = Integer::from(5);
            let conv = integer_to_carrier(int.clone(), 12, Endian::Little);

            let mut buf: [u8; 2] = [0; 2];
            let mut carrier = BitSlice::with_offset_length(
                &mut buf as &mut [u8], 0, 12);

            carrier.write(&conv);

            let back = carrier_to_integer(&carrier, true, Endian::Little);
            assert!(int == back);
        }

        {
            let int = Integer::from(-5);
            let conv = integer_to_carrier(int.clone(), 12, Endian::Big);

            let mut buf: [u8; 2] = [0; 2];
            let mut carrier = BitSlice::with_offset_length(
                &mut buf as &mut [u8], 0, 12);

            carrier.write(&conv);

            let back = carrier_to_integer(&carrier, true, Endian::Big);
            assert!(int == back);
        }

        {
            let int = Integer::from(-5);
            let conv = integer_to_carrier(int.clone(), 12, Endian::Little);

            let mut buf: [u8; 2] = [0; 2];
            let mut carrier = BitSlice::with_offset_length(
                &mut buf as &mut [u8], 0, 12);

            carrier.write(&conv);

            let back = carrier_to_integer(&carrier, true, Endian::Little);
            assert!(int == back);
        }

    }

}
