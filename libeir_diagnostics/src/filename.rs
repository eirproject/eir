//! Various source mapping utilities

use std::borrow::Cow;
use std::fmt;
use std::path::{Path, PathBuf};

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum FileName {
    /// A real file on disk
    Real(PathBuf),
    /// A synthetic file, eg. from the REPL
    Virtual(Cow<'static, str>),
}

impl From<PathBuf> for FileName {
    fn from(name: PathBuf) -> FileName {
        FileName::real(name)
    }
}

impl From<FileName> for PathBuf {
    fn from(name: FileName) -> PathBuf {
        match name {
            FileName::Real(path) => path,
            FileName::Virtual(Cow::Owned(owned)) => PathBuf::from(owned),
            FileName::Virtual(Cow::Borrowed(borrowed)) => PathBuf::from(borrowed),
        }
    }
}

impl<'a> From<&'a FileName> for &'a Path {
    fn from(name: &'a FileName) -> &'a Path {
        match *name {
            FileName::Real(ref path) => path,
            FileName::Virtual(ref cow) => Path::new(cow.as_ref()),
        }
    }
}

impl<'a> From<&'a Path> for FileName {
    fn from(name: &Path) -> FileName {
        FileName::real(name)
    }
}

impl From<String> for FileName {
    fn from(name: String) -> FileName {
        FileName::virtual_(name)
    }
}

impl From<&'static str> for FileName {
    fn from(name: &'static str) -> FileName {
        FileName::virtual_(name)
    }
}

impl AsRef<Path> for FileName {
    fn as_ref(&self) -> &Path {
        match *self {
            FileName::Real(ref path) => path.as_ref(),
            FileName::Virtual(ref cow) => Path::new(cow.as_ref()),
        }
    }
}

impl PartialEq<Path> for FileName {
    fn eq(&self, other: &Path) -> bool {
        self.as_ref() == other
    }
}

impl PartialEq<PathBuf> for FileName {
    fn eq(&self, other: &PathBuf) -> bool {
        self.as_ref() == other.as_path()
    }
}

impl FileName {
    pub fn real<T: Into<PathBuf>>(name: T) -> FileName {
        FileName::Real(name.into())
    }

    pub fn virtual_<T: Into<Cow<'static, str>>>(name: T) -> FileName {
        FileName::Virtual(name.into())
    }

    pub fn to_string(&self) -> String {
        match *self {
            FileName::Real(ref path) => match path.to_str() {
                None => path.to_string_lossy().into_owned(),
                Some(s) => s.to_owned(),
            },
            FileName::Virtual(ref s) => s.clone().into_owned(),
        }
    }
}

impl fmt::Display for FileName {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FileName::Real(ref path) => write!(fmt, "{}", path.display()),
            FileName::Virtual(ref name) => write!(fmt, "<{}>", name),
        }
    }
}
