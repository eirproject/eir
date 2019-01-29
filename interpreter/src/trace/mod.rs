
#[cfg(not(feature = "trace"))]
#[macro_use]
mod dummy;
#[cfg(not(feature = "trace"))]
use self::dummy as trace;

#[cfg(feature = "trace")]
#[macro_use]
mod trace;

pub use self::trace::*;
