#[allow(dead_code)]
pub mod tag {
    pub const ATOM_CACHE_REF: u8 = 82;
    /// Unsigned 8-bit integer
    pub const SMALL_INTEGER_EXT: u8 = 97;
    /// Signed 32-bit integer in big-endian format
    pub const INTEGER_EXT: u8 = 98;
    /// DEPRECATED
    /// Float stored as string
    pub const FLOAT_EXT: u8 = 99;
    /// (node:atom), (id:u32be as u28) (creation:u8 as u2)
    pub const PORT_EXT: u8 = 102;
    /// (node:atom), (id:u32be as u28) (creation:u32be)
    pub const NEW_PORT_EXT: u8 = 89;
    /// (node:atom), (id:u32be as u15) (serial:u32be as u13) (creation:u8 as u2)
    pub const PID_EXT: u8 = 103;
    /// (node:atom), (id:u32be as u15) (serial:u32be as u13) (creation:u32be)
    pub const NEW_PID_EXT: u8 = 88;
    /// (arity:u8), elements..
    pub const SMALL_TUPLE_EXT: u8 = 104;
    /// (arity:u32be), elements..
    pub const LARGE_TUPLE_EXT: u8 = 105;
    /// (arity:u32be), (key, value)..
    /// Duplicate keys not allowed.
    pub const MAP_EXT: u8 = 116;
    /// only tag
    pub const NIL_EXT: u8 = 106;
    /// (length:u16be), (chars:u8)..
    pub const STRING_EXT: u8 = 107;
    /// (length:u32be), (elements).., (tail)
    pub const LIST_EXT: u8 = 108;
    /// (length:u32be), (data)..
    pub const BINARY_EXT: u8 = 109;
    /// (data_len:u8), (is_neg:u8 as bool), (u8)..
    /// B = 256
    /// (d0*B^0 + d1*B^1 + d2*B^2 + ... d(N-1)*B^(n-1))
    pub const SMALL_BIG_EXT: u8 = 110;
    /// (data_len:u32be), (is_neg:u8 as bool), (u8)..
    /// Otherwise same as small
    pub const LARGE_BIG_EXT: u8 = 111;
    /// (id_len:u16be <= 3) (node:atom) (creation:u32be) (id:u32be)..
    pub const NEWER_REFERENCE_EXT: u8 = 90;
    /// (num_free:u32be) (pid:pid) (module:atom) (index:integer) (uniq:integer) (free)..
    /// uniq is parse hash
    pub const FUN_EXT: u8 = 117;
    /// (size:u32be) (arity:u8) (uniq:u128be) (index:u32be) (num_free:u32be) (module:atom)
    /// (old_index:integer) (old_uniq:integer) (pid:pid) (free)..
    /// size including itself
    pub const NEW_FUN_EXT: u8 = 112;
    /// (module:atom) (function:atom) (arity:small_integer_ext)
    pub const EXPORT_EXT: u8 = 113;
    /// (len:u32be) (bits:u8 <= 7) bytes..
    pub const BIT_BINARY_EXT: u8 = 77;
    /// (num:f32)
    pub const NEW_FLOAT_EXT: u8 = 70;
    /// (len:u16be) name_bytes..
    /// latin1
    pub const ATOM_EXT: u8 = 100;
    /// (len:u8) name_bytes..
    /// latin1
    pub const SMALL_ATOM_EXT: u8 = 115;
    /// (len:u16be) name_bytes..
    pub const ATOM_UTF8_EXT: u8 = 118;
    /// (len:u8) name_bytes..
    pub const SMALL_ATOM_UTF8_EXT: u8 = 119;
}
