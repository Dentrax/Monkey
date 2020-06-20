use crate::types::object::*;

use std::{fmt};
use std::fmt::{Formatter};
use std::collections::HashMap;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Hashable {
	BOOLEAN(bool),
	INTEGER(isize),
	STRING(String),
}

impl fmt::Display for Hashable {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Hashable::STRING(s) => s.fmt(f),
			Hashable::INTEGER(i) => i.fmt(f),
			Hashable::BOOLEAN(b) => b.fmt(f),
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct Hash {
	pub pairs: HashMap<Hashable, Object>,
}

impl fmt::Display for Hash {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let mut pairs = vec![];
		for pair in &self.pairs {
			pairs.push(format!(r#"{}: {}"#, pair.0, pair.1));
		}
		pairs.sort();
		write!(f, "{{{}}}", pairs.join(", "))
	}
}