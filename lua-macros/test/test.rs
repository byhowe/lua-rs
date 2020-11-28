#[lua_macros::func]
fn lua_func(l: &lua::LuaState) -> i32
{
  l.getglobal("test");
  let s = l.tostring(-1).unwrap();
  l.pop(1);
  println!("Lua: {}", s);
  l.pushnumber(5.5);
  1
}
