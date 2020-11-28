use crate::{Integer, Number, Table};
use libc::c_void;

#[derive(Debug)]
pub enum LuaValue
{
  Nil,
  Boolean(bool),
  LightUserdata(*mut c_void),
  Number(Number),
  Integer(Integer),
  String(String),
  Table(Table),
  Function,
}
