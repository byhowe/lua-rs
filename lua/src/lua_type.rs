use crate::LuaValue;
use lua_sys::ffi;

pub type Unsigned = ffi::lua_Unsigned;

pub type Number = ffi::lua_Number;

pub type Integer = ffi::lua_Integer;

#[derive(Debug)]
pub struct Table
{
  elements: Vec<(LuaValue, LuaValue)>,
}

#[derive(Debug, Eq, PartialEq)]
#[repr(i32)]
pub enum LuaType
{
  None = Self::NONE,

  Nil = Self::NIL,
  Boolean = Self::BOOLEAN,
  LightUserdata = Self::LIGHTUSERDATA,
  Number = Self::NUMBER,
  String = Self::STRING,
  Table = Self::TABLE,
  Function = Self::FUNCTION,
  Userdata = Self::USERDATA,
  Thread = Self::THREAD,
}

impl LuaType
{
  const NONE: i32 = ffi::LUA_TNONE as i32;
  const NIL: i32 = ffi::LUA_TNIL as i32;
  const BOOLEAN: i32 = ffi::LUA_TBOOLEAN as i32;
  const LIGHTUSERDATA: i32 = ffi::LUA_TLIGHTUSERDATA as i32;
  const NUMBER: i32 = ffi::LUA_TNUMBER as i32;
  const STRING: i32 = ffi::LUA_TSTRING as i32;
  const TABLE: i32 = ffi::LUA_TTABLE as i32;
  const FUNCTION: i32 = ffi::LUA_TFUNCTION as i32;
  const USERDATA: i32 = ffi::LUA_TUSERDATA as i32;
  const THREAD: i32 = ffi::LUA_TTHREAD as i32;

  pub const NUMTYPES: u32 = 9;
}

impl From<i32> for LuaType
{
  fn from(other: i32) -> Self
  {
    match other {
      Self::NONE => Self::None,

      Self::NIL => Self::Nil,
      Self::BOOLEAN => Self::Boolean,
      Self::LIGHTUSERDATA => Self::LightUserdata,
      Self::NUMBER => Self::Number,
      Self::STRING => Self::String,
      Self::TABLE => Self::Table,
      Self::FUNCTION => Self::Function,
      Self::USERDATA => Self::Userdata,
      Self::THREAD => Self::Thread,

      // This should never run. Hopefully.
      code => panic!("Unknown type code returned from Lua: {}", code),
    }
  }
}
