use crate::{Integer, LuaState, LuaType, Number};

pub type Table = Vec<(Option<LuaValue>, Option<LuaValue>)>;

pub type Array = Vec<Option<LuaValue>>;

#[derive(Debug, PartialEq)]
pub enum LuaValue
{
  Boolean(bool),
  Integer(Integer),
  Number(Number),
  String(String),
  Table(Table),
}

impl LuaValue
{
  #[inline]
  pub fn unwrap_boolean(self) -> bool
  {
    match self {
      Self::Boolean(val) => val,
      val => panic!("called `LuaValue::unwrap_boolean` on a `{:?}` value", val),
    }
  }

  #[inline]
  pub fn unwrap_integer(self) -> Integer
  {
    match self {
      Self::Integer(val) => val,
      val => panic!("called `LuaValue::unwrap_integer` on a `{:?}` value", val),
    }
  }

  #[inline]
  pub fn unwrap_number(self) -> Number
  {
    match self {
      Self::Number(val) => val,
      val => panic!("called `LuaValue::unwrap_number` on a `{:?}` value", val),
    }
  }

  #[inline]
  pub fn unwrap_string(self) -> String
  {
    match self {
      Self::String(val) => val,
      val => panic!("called `LuaValue::unwrap_string` on a `{:?}` value", val),
    }
  }

  #[inline]
  pub fn unwrap_table(self) -> Table
  {
    match self {
      Self::Table(val) => val,
      val => panic!("called `LuaValue::unwrap_table` on a `{:?}` value", val),
    }
  }

  #[inline]
  pub fn unwrap_array(self) -> Array
  {
    match self {
      Self::Table(val) => val.into_iter().map(|(_, value)| value).collect(),
      val => panic!("called `LuaValue::unwrap_array` on a `{:?}` value", val),
    }
  }

  #[inline]
  pub const fn is_boolean(&self) -> bool
  {
    match self {
      Self::Boolean(_) => true,
      _ => false,
    }
  }

  #[inline]
  pub const fn is_integer(&self) -> bool
  {
    match self {
      Self::Integer(_) => true,
      _ => false,
    }
  }

  #[inline]
  pub const fn is_number(&self) -> bool
  {
    match self {
      Self::Number(_) => true,
      _ => false,
    }
  }

  #[inline]
  pub const fn is_string(&self) -> bool
  {
    match self {
      Self::String(_) => true,
      _ => false,
    }
  }

  #[inline]
  pub const fn is_table(&self) -> bool
  {
    match self {
      Self::Table(_) => true,
      _ => false,
    }
  }
}

impl LuaState
{
  pub fn get_value(&self, index: i32) -> Option<LuaValue>
  {
    match self.type_of(index) {
      LuaType::Boolean => Some(LuaValue::Boolean(self.toboolean(index))),
      LuaType::Number => Some({
        if self.isinteger(index) {
          LuaValue::Integer(self.tointeger(index))
        } else {
          LuaValue::Number(self.tonumber(index))
        }
      }),
      LuaType::String => Some(LuaValue::String(self.tostring(index).unwrap().to_string())),
      LuaType::Table => Some(LuaValue::Table(self.traverse_table(index))),
      _ => None,
    }
  }

  pub fn traverse_table(&self, index: i32) -> Table
  {
    let mut pairs = Vec::new();
    self.pushnil();
    while self.next(index) {
      pairs.push((self.get_value(-2), self.get_value(-1)));
      self.pop(1);
    }
    self.pop(1);
    pairs
  }
}
