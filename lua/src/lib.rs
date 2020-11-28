mod lua_type;

use libc::{c_char, c_int, c_void, size_t};
use lua_sys::ffi;
use std::ffi::CStr;
use std::ffi::CString;
use std::path::Path;

pub use lua_type::{Integer, LuaType, Number, Unsigned};

pub const MULTRET: i32 = ffi::LUA_MULTRET;

#[derive(Debug)]
#[repr(i32)]
pub enum LuaOp
{
  /// performs addition (`+`)
  Add = ffi::LUA_OPADD as i32,
  /// performs subtraction (`-`)
  Sub = ffi::LUA_OPSUB as i32,
  /// performs multiplication (`*`)
  Mul = ffi::LUA_OPMUL as i32,
  /// performs float division (`/`)
  Div = ffi::LUA_OPDIV as i32,
  /// performs floor division (`//`)
  IDiv = ffi::LUA_OPIDIV as i32,
  /// performs modulo (`%`)
  Mod = ffi::LUA_OPMOD as i32,
  /// performs exponentiation (`^`)
  Pow = ffi::LUA_OPPOW as i32,
  /// performs mathematical negation (`unary -`)
  Unm = ffi::LUA_OPUNM as i32,
  /// performs bitwise NOT (`~`)
  BNot = ffi::LUA_OPBNOT as i32,
  /// performs bitwise AND (`&`)
  BAnd = ffi::LUA_OPBAND as i32,
  /// performs bitwise OR (`|`)
  BOr = ffi::LUA_OPBOR as i32,
  /// performs bitwise exclusive OR (`~`)
  BXor = ffi::LUA_OPBXOR as i32,
  /// performs left shift (`<<`)
  Shl = ffi::LUA_OPSHL as i32,
  /// performs right shift (`>>`)
  Shr = ffi::LUA_OPSHR as i32,
}

#[derive(Debug)]
#[repr(i32)]
pub enum LuaCmp
{
  /// compares for equality (`==`)
  Eq = ffi::LUA_OPEQ as i32,
  /// compares for less than (`<`)
  Lt = ffi::LUA_OPLT as i32,
  /// compares for less or equal (`<=`)
  Le = ffi::LUA_OPLE as i32,
}

#[derive(Debug, Eq, PartialEq)]
#[repr(i32)]
pub enum LuaThreadStatus
{
  /// no errors.
  Ok = Self::OK,
  /// a runtime error.
  RuntimeError = Self::ERRRUN,
  /// memory allocation error. For such errors, Lua does not call the message handler.
  MemoryError = Self::ERRMEM,
  /// error while running the message handler.
  MessageHandlerError = Self::ERRERR,
  /// syntax error during precompilation.
  SyntaxError = Self::ERRSYNTAX,
  /// the thread (coroutine) yields.
  Yield = Self::YIELD,
  /// a file-related error; e.g., it cannot open or read the file.
  FileError = Self::ERRFILE,
}

impl LuaThreadStatus
{
  const OK: i32 = ffi::LUA_OK as i32;
  const ERRRUN: i32 = ffi::LUA_ERRRUN as i32;
  const ERRMEM: i32 = ffi::LUA_ERRMEM as i32;
  const ERRERR: i32 = ffi::LUA_ERRERR as i32;
  const ERRSYNTAX: i32 = ffi::LUA_ERRSYNTAX as i32;
  const YIELD: i32 = ffi::LUA_YIELD as i32;
  const ERRFILE: i32 = ffi::LUA_ERRFILE as i32;

  pub fn is_error(&self) -> bool
  {
    match self {
      Self::Ok | Self::Yield => false,
      _ => true,
    }
  }
}

impl From<i32> for LuaThreadStatus
{
  fn from(other: i32) -> Self
  {
    match other {
      Self::OK => Self::Ok,
      Self::ERRRUN => Self::RuntimeError,
      Self::ERRMEM => Self::MemoryError,
      Self::ERRERR => Self::MessageHandlerError,
      Self::ERRSYNTAX => Self::SyntaxError,
      Self::YIELD => Self::Yield,
      Self::ERRFILE => Self::FileError,

      code => panic!("Unknown thread status code returned from Lua: {}", code),
    }
  }
}

pub struct LuaDebug(ffi::lua_Debug);

impl LuaDebug
{
  pub const fn event(&self) -> i32
  {
    self.0.event
  }

  /// The source of the chunk that created the function. If `source` starts with a '`@`', it means
  /// that the function was defined in a file where the file name follows the '`@`'. If `source`
  /// starts with a '`=`', the remainder of its contents describes the source in a user-dependent
  /// manner. Otherwise, the function was defined in a string where `source` is that string.
  pub fn source(&self) -> Option<&str>
  {
    if self.0.source.is_null() {
      None
    } else {
      Some(unsafe { CStr::from_ptr(self.0.source) }.to_str().unwrap())
    }
  }

  /// The length of the string `source`.
  pub const fn srclen(&self) -> u64
  {
    self.0.srclen
  }

  /// A "printable" version of `source`, to be used in error messages.
  pub fn short_src(&self) -> Option<&str>
  {
    if self.0.short_src[0] == 0 {
      None
    } else {
      Some(unsafe { CStr::from_ptr(self.0.short_src.as_ptr()) }.to_str().unwrap())
    }
  }

  /// The line number where the definition of the function starts.
  pub const fn linedefined(&self) -> i32
  {
    self.0.linedefined
  }

  /// The line number where the definition of the function ends.
  pub const fn lastlinedefined(&self) -> i32
  {
    self.0.lastlinedefined
  }

  /// The string "`Lua`" if the function is a Lua function, "`C`" if it is a C function, "`main`" if
  /// it is the main part of a chunk.
  pub fn what(&self) -> Option<&str>
  {
    if self.0.what.is_null() {
      None
    } else {
      Some(unsafe { CStr::from_ptr(self.0.what) }.to_str().unwrap())
    }
  }

  /// The current line where the given function is executing. When no line information is
  /// available, `currentline` is set to -1.
  pub const fn currentline(&self) -> i32
  {
    self.0.currentline
  }

  /// A reasonable name for the given function. Because functions in Lua are first-class values,
  /// they do not have a fixed name: some functions can be the value of multiple global variables,
  /// while others can be stored only in a table field. The `lua_getinfo` function checks how the
  /// function was called to find a suitable name. If it cannot find a name, then `name` is set to
  /// `NULL`.
  pub fn name(&self) -> Option<&str>
  {
    if self.0.name.is_null() {
      None
    } else {
      Some(unsafe { CStr::from_ptr(self.0.name) }.to_str().unwrap())
    }
  }

  /// Explains the `name` field. The value of `namewhat` can be "`global`", "`local`", "`method`",
  /// "`field`", "`upvalue`", or "" (the empty string), according to how the function was called.
  /// (Lua uses the empty string when no other option seems to apply.)
  pub fn namewhat(&self) -> Option<&str>
  {
    if self.0.namewhat.is_null() {
      None
    } else {
      Some(unsafe { CStr::from_ptr(self.0.namewhat) }.to_str().unwrap())
    }
  }

  /// `true` if this function invocation was called by a tail call. In this case, the caller of this
  /// level is not in the stack.
  pub const fn istailcall(&self) -> bool
  {
    self.0.istailcall != 0
  }

  /// The number of upvalues of the function.
  pub const fn nups(&self) -> u8
  {
    self.0.nups
  }

  /// The number of parameters of the function (always 0 for C functions).
  pub const fn nparams(&self) -> u8
  {
    self.0.nparams
  }

  /// `true` if the function is a vararg function (always true for C functions).
  pub const fn isvararg(&self) -> bool
  {
    self.0.isvararg != 0
  }

  /// The index in the stack of the first value being "transferred", that is, parameters in a call
  /// or return values in a return. (The other values are in consecutive indices.) Using this
  /// index, you can access and modify these values through `lua_getlocal` and `lua_setlocal`. This
  /// field is only meaningful during a call hook, denoting the first parameter, or a return hook,
  /// denoting the first value being returned. (For call hooks, this value is always 1.)
  pub const fn ftransfer(&self) -> u16
  {
    self.0.ftransfer
  }

  /// The number of values being transferred (see previous item). (For calls of Lua functions, this
  /// value is always equal to `nparams`.)
  pub const fn ntransfer(&self) -> u16
  {
    self.0.ntransfer
  }
}

impl From<ffi::lua_Debug> for LuaDebug
{
  fn from(other: ffi::lua_Debug) -> Self
  {
    Self(other)
  }
}

pub type RawLuaState = ffi::lua_State;

///      typedef void * (*lua_Alloc) (void *ud,
///                                   void *ptr,
///                                   size_t osize,
///                                   size_t nsize);
///
/// The type of the memory-allocation function used by Lua states. The allocator function must
/// provide a functionality similar to `realloc`, but not exactly the same. Its arguments are `ud`,
/// an opaque pointer passed to `lua_newstate`; `ptr`, a pointer to the block being
/// allocated/reallocated/freed; `osize`, the original size of the block or some code about what is
/// being allocated; and `nsize`, the new size of the block.
///
/// When `ptr` is not `NULL`, `osize` is the size of the block pointed by `ptr`, that is, the size
/// given when it was allocated or reallocated.
///
/// When `ptr` is `NULL`, `osize` encodes the kind of object that Lua is allocating. `osize` is any
/// of `LUA_TSTRING`, `LUA_TTABLE`, `LUA_TFUNCTION`, `LUA_TUSERDATA`, or `LUA_TTHREAD` when (and
/// only when) Lua is creating a new object of that type. When `osize` is some other value, Lua is
/// allocating memory for something else.
///
/// Lua assumes the following behavior from the allocator function:
///
/// When `nsize` is zero, the allocator must behave like `free` and then return `NULL`.
///
/// When `nsize` is not zero, the allocator must behave like `realloc`. In particular, the allocator
/// returns `NULL` if and only if it cannot fulfill the request.
///
/// Here is a simple implementation for the allocator function. It is used in the auxiliary library
/// by `luaL_newstate`.
///
///      static void *l_alloc (void *ud, void *ptr, size_t osize,
///                                                 size_t nsize) {
///        (void)ud;  (void)osize;  /* not used */
///        if (nsize == 0) {
///          free(ptr);
///          return NULL;
///        }
///        else
///          return realloc(ptr, nsize);
///      }
///
/// Note that Standard C ensures that `free(NULL)`` has no effect and that `realloc(NULL,size)` is
/// equivalent to `malloc(size)`.
pub type Alloc =
  Option<unsafe extern "C" fn(ud: *mut c_void, ptr: *mut c_void, osize: size_t, nsize: size_t) -> *mut c_void>;

///      typedef int (*lua_CFunction) (lua_State *L);
///
/// Type for C functions.
///
/// In order to communicate properly with Lua, a C function must use the following protocol, which
/// defines the way parameters and results are passed: a C function receives its arguments from Lua
/// in its stack in direct order (the first argument is pushed first). So, when the function starts,
/// `lua_gettop(L)` returns the number of arguments received by the function. The first argument (if
/// any) is at index 1 and its last argument is at index `lua_gettop(L)`. To return values to Lua, a
/// C function just pushes them onto the stack, in direct order (the first result is pushed first),
/// and returns in C the number of results. Any other value in the stack below the results will be
/// properly discarded by Lua. Like a Lua function, a C function called by Lua can also return many
/// results.
///
/// As an example, the following function receives a variable number of numeric arguments and
/// returns their average and their sum:
///
///      static int foo (lua_State *L) {
///        int n = lua_gettop(L);    /* number of arguments */
///        lua_Number sum = 0.0;
///        int i;
///        for (i = 1; i <= n; i++) {
///          if (!lua_isnumber(L, i)) {
///            lua_pushliteral(L, "incorrect argument");
///            lua_error(L);
///          }
///          sum += lua_tonumber(L, i);
///        }
///        lua_pushnumber(L, sum/n);        /* first result */
///        lua_pushnumber(L, sum);         /* second result */
///        return 2;                   /* number of results */
///      }
pub type CFunction = Option<unsafe extern "C" fn(L: *mut RawLuaState) -> c_int>;

pub type Hook = ffi::lua_Hook;

pub type KContext = ffi::lua_KContext;

pub type KFunction = ffi::lua_KFunction;

pub type Reader = ffi::lua_Reader;

pub type WarnFunction = ffi::lua_WarnFunction;

pub type Writer = ffi::lua_Writer;

#[derive(Debug)]
pub struct LuaState
{
  ptr: *mut RawLuaState,
  owned: bool,
}

impl LuaState
{
  /// Calls [`LuaState::l_newstate`]
  pub fn new() -> Option<Self>
  {
    Self::l_newstate()
  }

  /// Creates a [`LuaState`] from [`*mut RawLuaState`].
  /// Validity of the passed pointer must be checked by the caller.
  pub unsafe fn from_ptr(l: *mut RawLuaState) -> Self
  {
    Self { ptr: l, owned: false }
  }

  /* state manipulation */

  // TODO: implement lua_newstate.

  ///      void lua_close (lua_State *L);
  /// `[-0, +0, –]`
  ///
  /// Close all active to-be-closed variables in the main thread, release all objects in the given
  /// Lua state (calling the corresponding garbage-collection metamethods, if any), and frees all
  /// dynamic memory used by this state.
  ///
  /// On several platforms, you may not need to call this function, because all resources are
  /// naturally released when the host program ends. On the other hand, long-running programs that
  /// create multiple states, such as daemons or web servers, will probably need to close states as
  /// soon as they are not needed.
  pub fn close(&self) -> Result<(), &str>
  {
    if self.owned {
      unsafe { ffi::lua_close(self.ptr) };
      Ok(())
    } else {
      Err("Lua state is not owned")
    }
  }

  ///      lua_State *lua_newthread (lua_State *L);
  /// `[-0, +1, m]`
  ///
  /// Creates a new thread, pushes it on the stack, and returns a pointer to a `lua_State` that
  /// represents this new thread. The new thread returned by this function shares with the original
  /// thread its global environment, but has an independent execution stack.
  ///
  /// Threads are subject to garbage collection, like any Lua object.
  pub fn newthread(&self) -> Self
  {
    unsafe { Self::from_ptr(ffi::lua_newthread(self.ptr)) }
  }

  ///      int lua_resetthread (lua_State *L);
  /// `[-0, +?, –]`
  ///
  /// Resets a thread, cleaning its call stack and closing all pending to-be-closed variables.
  /// Returns a status code: `LUA_OK` for no errors in closing methods, or an error status
  /// otherwise. In case of error, leaves the error object on the top of the stack.
  pub fn resetthread(&self) -> LuaThreadStatus
  {
    unsafe { ffi::lua_resetthread(self.ptr) }.into()
  }

  /* basic stack manipulation */

  ///      int lua_absindex (lua_State *L, int idx);
  /// `[-0, +0, –]`
  ///
  /// Converts the acceptable index `idx` into an equivalent absolute index (that is, one that does
  /// not depend on the stack top).
  pub fn absindex(&self, idx: i32) -> i32
  {
    unsafe { ffi::lua_absindex(self.ptr, idx) }
  }

  ///      int lua_gettop (lua_State *L);
  /// `[-0, +0, –]`
  ///
  /// Returns the index of the top element in the stack. Because indices start at 1, this result is
  /// equal to the number of elements in the stack; in particular, 0 means an empty stack.
  pub fn gettop(&self) -> i32
  {
    unsafe { ffi::lua_gettop(self.ptr) }
  }

  ///      void lua_settop (lua_State *L, int index);
  /// `[-?, +?, –]`
  ///
  /// Accepts any index, or 0, and sets the stack top to this index. If the new top is greater than
  /// the old one, then the new elements are filled with nil. If `index` is 0, then all stack
  /// elements are removed.
  pub fn settop(&self, index: i32)
  {
    unsafe { ffi::lua_settop(self.ptr, index) };
  }

  ///      void lua_pushvalue (lua_State *L, int index);
  /// `[-0, +1, –]`
  ///
  /// Pushes a copy of the element at the given index onto the stack.
  pub fn pushvalue(&self, index: i32)
  {
    unsafe { ffi::lua_pushvalue(self.ptr, index) }
  }

  ///      void lua_rotate (lua_State *L, int idx, int n);
  /// `[-0, +0, –]`
  ///
  /// Rotates the stack elements between the valid index `idx` and the top of the stack. The
  /// elements are rotated `n` positions in the direction of the top, for a positive `n`, or `-n`
  /// positions in the direction of the bottom, for a negative `n`. The absolute value of `n` must
  /// not be greater than the size of the slice being rotated. This function cannot be called with a
  /// pseudo-index, because a pseudo-index is not an actual stack position.
  pub fn rotate(&self, idx: i32, n: i32)
  {
    unsafe { ffi::lua_rotate(self.ptr, idx, n) };
  }

  ///      void lua_copy (lua_State *L, int fromidx, int toidx);
  /// `[-0, +0, –]`
  /// Copies the element at index `fromidx` into the valid index `toidx`, replacing the value at
  /// that position. Values at other positions are not affected.
  pub fn copy(&self, fromidx: i32, toidx: i32)
  {
    unsafe { ffi::lua_copy(self.ptr, fromidx, toidx) };
  }

  ///      int lua_checkstack (lua_State *L, int n);
  /// `[-0, +0, –]`
  ///
  /// Ensures that the stack has space for at least `n` extra elements, that is, that you can safely
  /// push up to `n` values into it. It returns false if it cannot fulfill the request, either
  /// because it would cause the stack to be greater than a fixed maximum size (typically at least
  /// several thousand elements) or because it cannot allocate memory for the extra space. This
  /// function never shrinks the stack; if the stack already has space for the extra elements, it
  /// is left unchanged.
  pub fn checkstack(&self, n: i32) -> bool
  {
    unsafe { ffi::lua_checkstack(self.ptr, n) != 0 }
  }

  /* access function (stack -> C) */

  ///      int lua_isboolean (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Returns `true` if the value at the given index is a boolean, and `false` otherwise.
  pub fn isboolean(&self, index: i32) -> bool
  {
    self.type_of(index) == LuaType::Boolean
  }

  ///      int lua_iscfunction (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Returns `true` if the value at the given index is a C function, and `false` otherwise.
  pub fn iscfunction(&self, index: i32) -> bool
  {
    unsafe { ffi::lua_iscfunction(self.ptr, index) == 1 }
  }

  ///      int lua_isfunction (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Returns `true` if the value at the given index is a function (either C or Lua), and `false`
  /// otherwise.
  pub fn isfunction(&self, index: i32) -> bool
  {
    self.type_of(index) == LuaType::Function
  }

  ///      int lua_isinteger (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Returns `true` if the value at the given index is an integer (that is, the value is a number
  /// and is represented as an integer), and `false` otherwise.
  pub fn isinteger(&self, index: i32) -> bool
  {
    unsafe { ffi::lua_isinteger(self.ptr, index) == 1 }
  }

  ///      int lua_islightuserdata (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Returns `true` if the value at the given index is a light userdata, and `false` otherwise.
  pub fn islightuserdata(&self, index: i32) -> bool
  {
    self.type_of(index) == LuaType::LightUserdata
  }

  ///      int lua_isnil (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Returns `true` if the value at the given index is `nil`, and `false` otherwise.
  pub fn isnil(&self, index: i32) -> bool
  {
    self.type_of(index) == LuaType::Nil
  }

  ///      int lua_isnone (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Returns `true` if the given index is not valid, and `false` otherwise.
  pub fn isnone(&self, index: i32) -> bool
  {
    self.type_of(index) == LuaType::None
  }

  ///      int lua_isnoneornil (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Returns `true` if the given index is not valid or if the value at this index is `nil`, and
  /// `false` otherwise.
  pub fn isnoneornil(&self, index: i32) -> bool
  {
    match self.type_of(index) {
      LuaType::None | LuaType::Nil => true,
      _ => false,
    }
  }

  ///      int lua_isnumber (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Returns `true` if the value at the given index is a number or a string convertible to a
  /// number, and `false` otherwise.
  pub fn isnumber(&self, index: i32) -> bool
  {
    unsafe { ffi::lua_isnumber(self.ptr, index) == 1 }
  }

  ///      int lua_isstring (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Returns `true` if the value at the given index is a string or a number (which is always
  /// convertible to a string), and `false` otherwise.
  pub fn isstring(&self, index: i32) -> bool
  {
    unsafe { ffi::lua_isstring(self.ptr, index) == 1 }
  }

  ///      int lua_istable (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Returns `true` if the value at the given index is a table, and `false` otherwise.
  pub fn istable(&self, index: i32) -> bool
  {
    self.type_of(index) == LuaType::Table
  }

  ///      int lua_isthread (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Returns `true` if the value at the given index is a thread, and `false` otherwise.
  pub fn isthread(&self, index: i32) -> bool
  {
    self.type_of(index) == LuaType::Thread
  }

  ///      int lua_isuserdata (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Returns `true` if the value at the given index is a userdata (either full or light), and
  /// `false` otherwise.
  pub fn isuserdata(&self, index: i32) -> bool
  {
    unsafe { ffi::lua_isuserdata(self.ptr, index) == 1 }
  }

  ///      int lua_isyieldable (lua_State *L);
  /// `[-0, +0, –]`
  ///
  /// Returns `true` if the given coroutine can yield, and `false` otherwise.
  pub fn isyieldable(&self) -> bool
  {
    unsafe { ffi::lua_isyieldable(self.ptr) == 1 }
  }

  ///      int lua_type (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Returns the type of the value in the given valid index, or `LUA_TNONE` for a non-valid but
  /// acceptable index. The types returned by `lua_type` are coded by the following constants
  /// defined in `lua.h`: `LUA_TNIL`, `LUA_TNUMBER`, `LUA_TBOOLEAN`, `LUA_TSTRING`, `LUA_TTABLE`,
  /// `LUA_TFUNCTION`, `LUA_TUSERDATA`, `LUA_TTHREAD`, and `LUA_TLIGHTUSERDATA`.
  pub fn type_of(&self, index: i32) -> LuaType
  {
    unsafe { ffi::lua_type(self.ptr, index) }.into()
  }

  ///      const char *lua_typename (lua_State *L, int tp);
  /// `[-0, +0, –]`
  ///
  /// Returns the name of the type encoded by the value `tp`, which must be one the values returned
  /// by `lua_type`.
  #[deprecated = "Please use LuaType::to_string instead"]
  pub fn typename(&self, tp: LuaType) -> &str
  {
    let typename = unsafe { ffi::lua_typename(self.ptr, tp as _) };
    unsafe { CStr::from_ptr(typename) }.to_str().unwrap()
  }

  ///      int lua_toboolean (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Converts the Lua value at the given index to a C boolean value (0 or 1). Like all tests in
  /// Lua, `lua_toboolean` returns `true` for any Lua value different from `false` and `nil`;
  /// otherwise it returns `false`. (If you want to accept only actual boolean values, use
  /// lua_isboolean to test the value's type.)
  pub fn toboolean(&self, index: i32) -> bool
  {
    unsafe { ffi::lua_toboolean(self.ptr, index) != 0 }
  }

  ///      lua_CFunction lua_tocfunction (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Converts a value at the given index to a C function. That value must be a C function;
  /// otherwise, returns `NULL`.
  pub fn tocfunction(&self, index: i32) -> CFunction
  {
    unsafe { ffi::lua_tocfunction(self.ptr, index) }
  }

  ///      lua_Integer lua_tointeger (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Equivalent to `lua_tointegerx` with `isnum` equal to `NULL`.
  pub fn tointeger(&self, index: i32) -> Integer
  {
    unsafe { ffi::lua_tointegerx(self.ptr, index, std::ptr::null_mut()) }
  }

  ///      lua_Integer lua_tointegerx (lua_State *L, int index, int *isnum);
  /// `[-0, +0, –]`
  ///
  /// Converts the Lua value at the given index to the signed integral type `lua_Integer`. The Lua
  /// value must be an integer, or a number or string convertible to an integer; otherwise,
  /// `lua_tointegerx` returns 0.
  ///
  /// If isnum is not `NULL`, its referent is assigned a boolean value that indicates whether the
  /// operation succeeded.
  pub fn tointegerx(&self, index: i32) -> Option<Integer>
  {
    let mut isnum: i32 = 0;
    let result = unsafe { ffi::lua_tointegerx(self.ptr, index, &mut isnum) };
    if isnum != 0 {
      Some(result)
    } else {
      None
    }
  }

  ///      const char *lua_tolstring (lua_State *L, int index, size_t *len);
  /// `[-0, +0, m]`
  ///
  /// Converts the Lua value at the given index to a C string. If len is not `NULL`, it sets `*len`
  /// with the string length. The Lua value must be a string or a number; otherwise, the function
  /// returns `NULL`. If the value is a number, then `lua_tolstring` also changes the actual value
  /// in the stack to a string. (This change confuses `lua_next` when `lua_tolstring` is applied to
  /// keys during a table traversal.)
  ///
  /// `lua_tolstring` returns a pointer to a string inside the Lua state. This string always has a
  /// zero ('`\0`') after its last character (as in C), but can contain other zeros in its body.
  pub fn tolstring(&self, index: i32) -> Option<&str>
  {
    let mut len: ffi::size_t = 0;
    let string = unsafe { ffi::lua_tolstring(self.ptr, index, &mut len) };
    if string.is_null() {
      None
    } else {
      let slice = unsafe { std::slice::from_raw_parts(string as _, len as usize) };
      Some(std::str::from_utf8(slice).unwrap())
    }
  }

  ///      lua_Unsigned lua_rawlen (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Returns the raw "length" of the value at the given index: for strings, this is the string
  /// length; for tables, this is the result of the length operator ('`#`') with no metamethods; for
  /// userdata, this is the size of the block of memory allocated for the userdata. For other
  /// values, this call returns 0.
  pub fn rawlen(&self, index: i32) -> Unsigned
  {
    unsafe { ffi::lua_rawlen(self.ptr, index) }
  }

  ///      lua_Number lua_tonumber (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Equivalent to `lua_tonumberx` with isnum equal to `NULL`.
  pub fn tonumber(&self, index: i32) -> Number
  {
    unsafe { ffi::lua_tonumberx(self.ptr, index, std::ptr::null_mut()) }
  }

  /// lua_Number lua_tonumberx (lua_State *L, int index, int *isnum);
  /// `[-0, +0, –]`
  ///
  /// Converts the Lua value at the given index to the C type `lua_Number` (see `lua_Number`). The
  /// Lua value must be a number or a string convertible to a number; otherwise, `lua_tonumberx`
  /// returns 0.
  ///
  /// If `isnum` is not `NULL`, its referent is assigned a boolean value that indicates whether the
  /// operation succeeded.
  pub fn tonumberx(&self, index: i32) -> Option<Number>
  {
    let mut isnum: i32 = 0;
    let result = unsafe { ffi::lua_tonumberx(self.ptr, index, &mut isnum) };
    if isnum != 0 {
      Some(result)
    } else {
      None
    }
  }

  ///      const void *lua_topointer (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Converts the value at the given index to a generic C pointer (`void*`). The value can be a
  /// userdata, a table, a thread, a string, or a function; otherwise, `lua_topointer` returns
  /// `NULL`. Different objects will give different pointers. There is no way to convert the pointer
  /// back to its original value.
  ///
  /// Typically this function is used only for hashing and debug information.
  pub fn topointer<T>(&self, index: i32) -> Option<*const T>
  {
    let result = unsafe { ffi::lua_topointer(self.ptr, index) };
    if result.is_null() {
      None
    } else {
      Some(result as *const T)
    }
  }

  ///      const char *lua_tostring (lua_State *L, int index);
  /// `[-0, +0, m]`
  ///
  /// Equivalent to `lua_tolstring` with len equal to `NULL`.
  pub fn tostring(&self, index: i32) -> Option<&str>
  {
    let string = unsafe { ffi::lua_tolstring(self.ptr, index, std::ptr::null_mut()) };
    if string.is_null() {
      None
    } else {
      let cstr = unsafe { CStr::from_ptr(string) };
      Some(cstr.to_str().unwrap())
    }
  }

  ///      lua_State *lua_tothread (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// Converts the value at the given index to a Lua thread (represented as `lua_State*`). This
  /// value must be a thread; otherwise, the function returns `NULL`.
  pub fn tothread(&self, index: i32) -> Option<Self>
  {
    let l = unsafe { ffi::lua_tothread(self.ptr, index) };
    if l.is_null() {
      None
    } else {
      Some(unsafe { Self::from_ptr(l) })
    }
  }

  ///      void *lua_touserdata (lua_State *L, int index);
  /// `[-0, +0, –]`
  ///
  /// If the value at the given index is a full userdata, returns its memory-block address. If the
  /// value is a light userdata, returns its value (a pointer). Otherwise, returns `NULL`.
  pub fn touserdata<T>(&self, index: i32) -> Option<*mut T>
  {
    let data = unsafe { ffi::lua_touserdata(self.ptr, index) };
    if data.is_null() {
      None
    } else {
      Some(data as *mut T)
    }
  }

  /* comparison and arithmetic functions */

  ///      void lua_arith (lua_State *L, int op);
  /// `[-(2|1), +1, e]`
  ///
  /// Performs an arithmetic or bitwise operation over the two values (or one, in the case of
  /// negations) at the top of the stack, with the value on the top being the second operand, pops
  /// these values, and pushes the result of the operation. The function follows the semantics of
  /// the corresponding Lua operator (that is, it may call metamethods).
  ///
  /// The value of `op` must be one of the following constants:
  ///
  /// * [`LuaOp::Add`] performs addition (`+`)
  /// * [`LuaOp::Sub`] performs subtraction (`-`)
  /// * [`LuaOp::Mul`] performs multiplication (`*`)
  /// * [`LuaOp::Div`] performs float division (`/`)
  /// * [`LuaOp::IDiv`] performs floor division (`//`)
  /// * [`LuaOp::Mod`] performs modulo (`%`)
  /// * [`LuaOp::Pow`] performs exponentiation (`^`)
  /// * [`LuaOp::Unm`] performs mathematical negation (`unary -`)
  /// * [`LuaOp::BNot`] performs bitwise NOT (`~`)
  /// * [`LuaOp::BAnd`] performs bitwise AND (`&`)
  /// * [`LuaOp::BOr`] performs bitwise OR (`|`)
  /// * [`LuaOp::BXor`] performs bitwise exclusive OR (`~`)
  /// * [`LuaOp::Shl`] performs left shift (`<<`)
  /// * [`LuaOp::Shr`] performs right shift (`>>`)
  pub fn arith(&self, op: LuaOp)
  {
    unsafe { ffi::lua_arith(self.ptr, op as _) };
  }

  ///      int lua_rawequal (lua_State *L, int index1, int index2);
  /// `[-0, +0, –]`
  ///
  /// Returns `true` if the two values in indices `index1` and `index2` are primitively equal (that
  /// is, equal without calling the `__eq` metamethod). Otherwise returns `false`. Also returns
  /// `false` if any of the indices are not valid.
  pub fn rawequal(&self, index1: i32, index2: i32) -> bool
  {
    unsafe { ffi::lua_rawequal(self.ptr, index1, index2) != 0 }
  }

  ///      int lua_compare (lua_State *L, int index1, int index2, int op);
  /// `[-0, +0, e]`
  ///
  /// Compares two Lua values. Returns 1 if the value at index `index1` satisfies `op` when compared
  /// with the value at index `index2`, following the semantics of the corresponding Lua operator
  /// (that is, it may call metamethods). Otherwise returns 0. Also returns 0 if any of the indices
  /// is not valid.
  ///
  /// The value of `op` must be one of the following constants:
  ///
  /// * [`LuaCmp::Eq`] compares for equality (`==`)
  /// * [`LuaCmp::Lt`] compares for less than (`<`)
  /// * [`LuaCmp::Le`] compares for less or equal (`<=`)
  pub fn compare(&self, index1: i32, index2: i32, op: LuaCmp) -> i32
  {
    unsafe { ffi::lua_compare(self.ptr, index1, index2, op as _) }
  }

  /* push functions (C -> stack) */

  ///      void lua_pushnil (lua_State *L);
  /// `[-0, +1, –]`
  ///
  /// Pushes a nil value onto the stack.
  pub fn pushnil(&self)
  {
    unsafe { ffi::lua_pushnil(self.ptr) }
  }

  ///      void lua_pushnumber (lua_State *L, lua_Number n);
  /// `[-0, +1, –]`
  ///
  /// Pushes a float with value `n` onto the stack.
  pub fn pushnumber(&self, n: Number)
  {
    unsafe { ffi::lua_pushnumber(self.ptr, n) }
  }

  ///      void lua_pushinteger (lua_State *L, lua_Integer n);
  /// `[-0, +1, –]`
  ///
  /// Pushes an integer with value `n` onto the stack.
  pub fn pushinteger(&self, n: Integer)
  {
    unsafe { ffi::lua_pushinteger(self.ptr, n) }
  }

  ///      const char *lua_pushlstring (lua_State *L, const char *s, size_t len);
  /// `[-0, +1, m]`
  ///
  /// Pushes the string pointed to by `s` with size `len` onto the stack. Lua will make or reuse an
  /// internal copy of the given string, so the memory at `s` can be freed or reused immediately
  /// after the function returns. The string can contain any binary data, including embedded zeros.
  ///
  /// Returns a pointer to the internal copy of the string.
  pub fn pushlstring(&self, s: &str)
  {
    let s_cstr = CString::new(s).unwrap();
    let slice = s_cstr.as_bytes();
    unsafe { ffi::lua_pushlstring(self.ptr, slice.as_ptr() as _, slice.len() as _) };
  }

  ///      const char *lua_pushstring (lua_State *L, const char *s);
  /// `[-0, +1, m]`
  ///
  /// Pushes the zero-terminated string pointed to by `s` onto the stack. Lua will make or reuse an
  /// internal copy of the given string, so the memory at `s` can be freed or reused immediately
  /// after the function returns.
  ///
  /// Returns a pointer to the internal copy of the string.
  ///
  /// If `s` is `NULL`, pushes `nil` and returns `NULL`.
  pub fn pushstring(&self, s: &str)
  {
    let s_cstr = CString::new(s).unwrap();
    unsafe { ffi::lua_pushstring(self.ptr, s_cstr.as_ptr()) };
  }

  // TODO: implement lua_pushvfstring.
  // TODO: implement lua_pushfstring.

  ///      void lua_pushcclosure (lua_State *L, lua_CFunction fn, int n);
  /// `[-n, +1, m]`
  ///
  /// Pushes a new C closure onto the stack. This function receives a pointer to a C function and
  /// pushes onto the stack a Lua value of type `function` that, when called, invokes the
  /// corresponding C function. The parameter `n` tells how many upvalues this function will have.
  ///
  /// Any function to be callable by Lua must follow the correct protocol to receive its parameters
  /// and return its results (see `lua_CFunction`).
  ///
  /// When a C function is created, it is possible to associate some values with it, the so called
  /// upvalues; these upvalues are then accessible to the function whenever it is called. This
  /// association is called a C closure. To create a C closure, first the initial values for its
  /// upvalues must be pushed onto the stack. (When there are multiple upvalues, the first value is
  /// pushed first.) Then `lua_pushcclosure` is called to create and push the C function onto the
  /// stack, with the argument `n` telling how many values will be associated with the function.
  /// `lua_pushcclosure` also pops these values from the stack.
  ///
  /// The maximum value for `n` is 255.
  ///
  /// When `n` is zero, this function creates a light C function, which is just a pointer to the C
  /// function. In that case, it never raises a memory error.
  pub fn pushcclosure(&self, func: CFunction, n: i32)
  {
    unsafe { ffi::lua_pushcclosure(self.ptr, func, n) }
  }

  ///      void lua_pushboolean (lua_State *L, int b);
  /// `[-0, +1, –]`
  ///
  /// Pushes a boolean value with value `b` onto the stack.
  pub fn pushboolean(&self, b: bool)
  {
    unsafe { ffi::lua_pushboolean(self.ptr, b as _) }
  }

  ///      void lua_pushlightuserdata (lua_State *L, void *p);
  /// `[-0, +1, –]`
  ///
  /// Pushes a light userdata onto the stack.
  ///
  /// Userdata represent C values in Lua. A `light userdata` represents a pointer, a `void*`. It is
  /// a value (like a number): you do not create it, it has no individual metatable, and it is not
  /// collected (as it was never created). A light userdata is equal to "any" light userdata with
  /// the same C address.
  pub fn pushlightuserdata<T>(&self, p: *mut T)
  {
    unsafe { ffi::lua_pushlightuserdata(self.ptr, p as _) }
  }

  ///      int lua_pushthread (lua_State *L);
  /// `[-0, +1, –]`
  ///
  /// Pushes the thread represented by `L` onto the stack. Returns `true` if this thread is the main
  /// thread of its state.
  pub fn pushthread(&self) -> bool
  {
    unsafe { ffi::lua_pushthread(self.ptr) != 0 }
  }

  ///      void lua_pushcfunction (lua_State *L, lua_CFunction f);
  /// `[-0, +1, –]`
  ///
  /// Pushes a C function onto the stack. This function is equivalent to `lua_pushcclosure` with no
  /// upvalues.
  pub fn pushcfunction(&self, func: CFunction)
  {
    self.pushcclosure(func, 0)
  }

  /* get functions (Lua -> stack) */

  ///      int lua_getglobal (lua_State *L, const char *name);
  /// `[-0, +1, e]`
  ///
  /// Pushes onto the stack the value of the global `name`. Returns the type of that value.
  pub fn getglobal(&self, name: &str) -> LuaType
  {
    let name_cstr = CString::new(name).unwrap();
    unsafe { ffi::lua_getglobal(self.ptr, name_cstr.as_ptr()) }.into()
  }

  ///      int lua_gettable (lua_State *L, int index);
  /// `[-1, +1, e]`
  ///
  /// Pushes onto the stack the value `t[k]`, where `t` is the value at the given index and `k` is
  /// the value on the top of the stack.
  ///
  /// This function pops the key from the stack, pushing the resulting value in its place. As in
  /// Lua, this function may trigger a metamethod for the "index" event.
  ///
  /// Returns the type of the pushed value.
  pub fn gettable(&self, index: i32) -> LuaType
  {
    unsafe { ffi::lua_gettable(self.ptr, index) }.into()
  }

  ///      int lua_getfield (lua_State *L, int index, const char *k);
  /// `[-0, +1, e]`
  ///
  /// Pushes onto the stack the value `t[k]`, where `t` is the value at the given index. As in Lua,
  /// this function may trigger a metamethod for the "index" event.
  ///
  /// Returns the type of the pushed value.
  pub fn getfield(&self, index: i32, k: &str) -> LuaType
  {
    let k_cstr = CString::new(k).unwrap();
    unsafe { ffi::lua_getfield(self.ptr, index, k_cstr.as_ptr()) }.into()
  }

  ///      int lua_geti (lua_State *L, int index, lua_Integer i);
  /// `[-0, +1, e]`
  ///
  /// Pushes onto the stack the value `t[i]`, where `t` is the value at the given index. As in Lua,
  /// this function may trigger a metamethod for the "index" event.
  ///
  /// Returns the type of the pushed value.
  pub fn geti(&self, index: i32, i: Integer) -> LuaType
  {
    unsafe { ffi::lua_geti(self.ptr, index, i) }.into()
  }

  ///      int lua_rawget (lua_State *L, int index);
  /// `[-1, +1, –]`
  ///
  /// Similar to `lua_gettable`, but does a raw access (i.e., without metamethods).
  pub fn rawget(&self, index: i32) -> LuaType
  {
    unsafe { ffi::lua_rawget(self.ptr, index) }.into()
  }

  ///      int lua_rawgeti (lua_State *L, int index, lua_Integer n);
  /// `[-0, +1, –]`
  ///
  /// Pushes onto the stack the value `t[n]`, where `t` is the table at the given index. The access
  /// is raw, that is, it does not use the `__index` metavalue.
  ///
  /// Returns the type of the pushed value.
  pub fn rawgeti(&self, index: i32, n: Integer) -> LuaType
  {
    unsafe { ffi::lua_rawgeti(self.ptr, index, n) }.into()
  }

  ///      int lua_rawgetp (lua_State *L, int index, const void *p);
  /// `[-0, +1, –]`
  ///
  /// Pushes onto the stack the value `t[k]`, where `t` is the table at the given index and `k` is
  /// the pointer `p` represented as a light userdata. The access is raw; that is, it does not use
  /// the `__index` metavalue.
  ///
  /// Returns the type of the pushed value.
  pub fn rawgetp<T>(&self, index: i32, p: *const T) -> LuaType
  {
    unsafe { ffi::lua_rawgetp(self.ptr, index, p as _) }.into()
  }

  ///      void lua_createtable (lua_State *L, int narr, int nrec);
  /// `[-0, +1, m]`
  ///
  /// Creates a new empty table and pushes it onto the stack. Parameter `narr` is a hint for how
  /// many elements the table will have as a sequence; parameter `nrec` is a hint for how many other
  /// elements the table will have. Lua may use these hints to preallocate memory for the new table.
  /// This preallocation may help performance when you know in advance how many elements the table
  /// will have. Otherwise you can use the function `lua_newtable`.
  pub fn createtable(&self, narr: i32, nrec: i32)
  {
    unsafe { ffi::lua_createtable(self.ptr, narr, nrec) };
  }

  ///      void *lua_newuserdatauv (lua_State *L, size_t size, int nuvalue);
  /// `[-0, +1, m]`
  ///
  /// This function creates and pushes on the stack a new full userdata, with `nuvalue` associated
  /// Lua values, called `user values`, plus an associated block of raw memory with `size` bytes.
  /// (The user values can be set and read with the functions `lua_setiuservalue` and
  /// `lua_getiuservalue`.)
  ///
  /// The function returns the address of the block of memory. Lua ensures that this address is
  /// valid as long as the corresponding userdata is alive. Moreover, if the userdata is marked for
  /// finalization, its address is valid at least until the call to its finalizer.
  pub fn newuserdatauv(&self, size: u64, nuvalue: i32)
  {
    unsafe { ffi::lua_newuserdatauv(self.ptr, size, nuvalue) };
  }

  ///      int lua_getmetatable (lua_State *L, int index);
  /// `[-0, +(0|1), –]`
  ///
  /// If the value at the given index has a metatable, the function pushes that metatable onto the
  /// stack and returns `true`. Otherwise, the function returns `false` and pushes nothing on the
  /// stack.
  pub fn getmetatable(&self, index: i32) -> bool
  {
    unsafe { ffi::lua_getmetatable(self.ptr, index) != 0 }
  }

  ///      int lua_getiuservalue (lua_State *L, int index, int n);
  /// `[-0, +1, –]`
  ///
  /// Pushes onto the stack the `n`-th user value associated with the full userdata at the given
  /// index and returns the type of the pushed value.
  ///
  /// If the userdata does not have that value, pushes `nil` and returns `LUA_TNONE`.
  pub fn getiuservalue(&self, index: i32, n: i32) -> LuaType
  {
    unsafe { ffi::lua_getiuservalue(self.ptr, index, n) }.into()
  }

  /* set functions (stack -> Lua) */

  ///      void lua_setglobal (lua_State *L, const char *name);
  /// `[-1, +0, e]`
  ///
  /// Pops a value from the stack and sets it as the new value of global `name`.
  pub fn setglobal(&self, name: &str)
  {
    let name_cstr = CString::new(name).unwrap();
    unsafe { ffi::lua_setglobal(self.ptr, name_cstr.as_ptr()) };
  }

  ///      void lua_settable (lua_State *L, int index);
  /// `[-2, +0, e]`
  ///
  /// Does the equivalent to `t[k] = v`, where `t` is the value at the given index, `v` is the value
  /// on the top of the stack, and `k` is the value just below the top.
  ///
  /// This function pops both the key and the value from the stack. As in Lua, this function may
  /// trigger a metamethod for the "newindex" event.
  pub fn settable(&self, index: i32)
  {
    unsafe { ffi::lua_settable(self.ptr, index) };
  }

  ///      void lua_setfield (lua_State *L, int index, const char *k);
  /// `[-1, +0, e]`
  ///
  /// Does the equivalent to `t[k] = v`, where t is the value at the given index and `v` is the
  /// value on the top of the stack.
  ///
  /// This function pops the value from the stack. As in Lua, this function may trigger a metamethod
  /// for the "newindex" event.
  pub fn setfield(&self, index: i32, k: &str)
  {
    let k_cstr = CString::new(k).unwrap();
    unsafe { ffi::lua_setfield(self.ptr, index, k_cstr.as_ptr()) };
  }

  ///      void lua_seti (lua_State *L, int index, lua_Integer n);
  /// `[-1, +0, e]`
  ///
  /// Does the equivalent to `t[n] = v`, where `t` is the value at the given index and `v` is the
  /// value on the top of the stack.
  ///
  /// This function pops the value from the stack. As in Lua, this function may trigger a metamethod
  /// for the "newindex" event.
  pub fn seti(&self, index: i32, n: Integer)
  {
    unsafe { ffi::lua_seti(self.ptr, index, n) };
  }

  ///      void lua_rawset (lua_State *L, int index);
  /// `[-2, +0, m]`
  ///
  /// Similar to `lua_settable`, but does a raw assignment (i.e., without metamethods).
  pub fn rawset(&self, index: i32)
  {
    unsafe { ffi::lua_rawset(self.ptr, index) };
  }

  ///      void lua_rawseti (lua_State *L, int index, lua_Integer i);
  /// `[-1, +0, m]`
  ///
  /// Does the equivalent of `t[i] = v`, where `t` is the table at the given index and `v` is the
  /// value on the top of the stack.
  ///
  /// This function pops the value from the stack. The assignment is raw, that is, it does not use
  /// the `__newindex` metavalue.
  pub fn rawseti(&self, index: i32, i: Integer)
  {
    unsafe { ffi::lua_rawseti(self.ptr, index, i) };
  }

  ///      void lua_rawsetp (lua_State *L, int index, const void *p);
  /// `[-1, +0, m]`
  ///
  /// Does the equivalent of `t[p] = v`, where `t` is the table at the given index, `p` is encoded
  /// as a light userdata, and `v` is the value on the top of the stack.
  ///
  /// This function pops the value from the stack. The assignment is raw, that is, it does not use
  /// the `__newindex` metavalue.
  pub fn rawsetp<T>(&self, index: i32, p: *const T)
  {
    unsafe { ffi::lua_rawsetp(self.ptr, index, p as _) };
  }

  ///      int lua_setmetatable (lua_State *L, int index);
  /// `[-1, +0, –]`
  ///
  /// Pops a table or `nil` from the stack and sets that value as the new metatable for the value at
  /// the given index. (`nil` means no metatable.)
  ///
  /// (For historical reasons, this function returns an `int`, which now is always 1.)
  pub fn setmetatable(&self, index: i32)
  {
    unsafe { ffi::lua_setmetatable(self.ptr, index) };
  }

  ///      int lua_setiuservalue (lua_State *L, int index, int n);
  /// `[-1, +0, –]`
  ///
  /// Pops a value from the stack and sets it as the new `n`-th user value associated to the full
  /// userdata at the given index. Returns `false` if the userdata does not have that value.
  pub fn setiuservalue(&self, index: i32, n: i32) -> bool
  {
    unsafe { ffi::lua_setiuservalue(self.ptr, index, n) != 0 }
  }

  ///      lua_CFunction lua_atpanic (lua_State *L, lua_CFunction panicf);
  /// `[-0, +0, –]`
  ///
  /// Sets a new panic function and returns the old one.
  pub fn atpanic(&self, panicf: CFunction) -> CFunction
  {
    unsafe { ffi::lua_atpanic(self.ptr, panicf) }
  }

  ///      void lua_call (lua_State *L, int nargs, int nresults);
  /// `[-(nargs+1), +nresults, e]`
  ///
  /// Calls a function. Like regular Lua calls, `lua_call` respects the `__call` metamethod. So,
  /// here the word "function" means any callable value.
  ///
  /// To do a call you must use the following protocol: first, the function to be called is pushed
  /// onto the stack; then, the arguments to the call are pushed in direct order; that is, the first
  /// argument is pushed first. Finally you call `lua_call`; `nargs` is the number of arguments that
  /// you pushed onto the stack. When the function returns, all arguments and the function value
  /// are popped and the call results are pushed onto the stack. The number of results is adjusted
  /// to `nresults`, unless `nresults` is `LUA_MULTRET`. In this case, all results from the
  /// function are pushed; Lua takes care that the returned values fit into the stack space, but
  /// it does not ensure any extra space in the stack. The function results are pushed onto the
  /// stack in direct order (the first result is pushed first), so that after the call the last
  /// result is on the top of the stack.
  ///
  /// Any error while calling and running the function is propagated upwards (with a `longjmp`).
  ///
  /// The following example shows how the host program can do the equivalent to this Lua code:
  ///
  ///      a = f("how", t.x, 14)
  ///
  /// Here it is in C:
  ///
  ///      lua_getglobal(L, "f");                  /* function to be called */
  ///      lua_pushliteral(L, "how");                       /* 1st argument */
  ///      lua_getglobal(L, "t");                    /* table to be indexed */
  ///      lua_getfield(L, -1, "x");        /* push result of t.x (2nd arg) */
  ///      lua_remove(L, -2);                  /* remove 't' from the stack */
  ///      lua_pushinteger(L, 14);                          /* 3rd argument */
  ///      lua_call(L, 3, 1);     /* call 'f' with 3 arguments and 1 result */
  ///      lua_setglobal(L, "a");                         /* set global 'a' */
  ///
  /// Note that the code above is balanced: at its end, the stack is back to its original
  /// configuration. This is considered good programming practice.
  pub fn call(&self, nargs: i32, nresults: i32)
  {
    self.callk(nargs, nresults, 0, None);
  }

  ///      void lua_callk (lua_State *L,
  ///                      int nargs,
  ///                      int nresults,
  ///                      lua_KContext ctx,
  ///                      lua_KFunction k);
  /// `[-(nargs + 1), +nresults, e]`
  ///
  /// This function behaves exactly like `lua_call`, but allows the called function to yield.
  pub fn callk(&self, nargs: i32, nresults: i32, ctx: KContext, k: KFunction)
  {
    unsafe { ffi::lua_callk(self.ptr, nargs, nresults, ctx, k) }
  }

  ///      void lua_concat (lua_State *L, int n);
  /// `[-n, +1, e]`
  ///
  /// Concatenates the `n` values at the top of the stack, pops them, and leaves the result on the
  /// top. If `n` is 1, the result is the single value on the stack (that is, the function does
  /// nothing); if `n` is 0, the result is the empty string. Concatenation is performed following
  /// the usual semantics of Lua.
  pub fn concat(&self, n: i32)
  {
    unsafe { ffi::lua_concat(self.ptr, n) };
  }

  ///      int lua_dump (lua_State *L,
  ///                              lua_Writer writer,
  ///                              void *data,
  ///                              int strip);
  /// `[-0, +0, –]`
  ///
  /// Dumps a function as a binary chunk. Receives a Lua function on the top of the stack and
  /// produces a binary chunk that, if loaded again, results in a function equivalent to the one
  /// dumped. As it produces parts of the chunk, `lua_dump` calls function `writer` (see
  /// `lua_Writer`) with the given `data` to write them.
  ///
  /// If `strip` is true, the binary representation may not include all debug information about the
  /// function, to save space.
  ///
  /// The value returned is the error code returned by the last call to the writer; 0 means no
  /// errors.
  ///
  /// This function does not pop the Lua function from the stack.
  pub fn dump<T>(&self, writer: Writer, data: *mut T, strip: i32) -> i32
  {
    unsafe { ffi::lua_dump(self.ptr, writer, data.cast(), strip) }
  }

  ///      int lua_error (lua_State *L);
  /// `[-1, +0, v]`
  ///
  /// Raises a Lua error, using the value on the top of the stack as the error object. This function
  /// does a long jump, and therefore never returns (see `luaL_error`).
  pub fn error(&self) -> i32
  {
    unsafe { ffi::lua_error(self.ptr) }
  }

  // TODO: implement lua_gc.
  // TODO: implement lua_getallocf.
  // TODO: implement lua_getextraspace.

  ///      lua_Hook lua_gethook (lua_State *L);
  /// `[-0, +0, –]`
  ///
  /// Returns the current hook function.
  pub fn gethook(&self) -> Hook
  {
    unsafe { ffi::lua_gethook(self.ptr) }
  }

  ///      int lua_gethookcount (lua_State *L);
  /// `[-0, +0, –]`
  ///
  /// Returns the current hook count.
  pub fn gethookcount(&self) -> i32
  {
    unsafe { ffi::lua_gethookcount(self.ptr) }
  }

  ///      int lua_gethookmask (lua_State *L);
  /// `[-0, +0, –]`
  ///
  /// Returns the current hook mask.
  pub fn gethookmask(&self) -> i32
  {
    unsafe { ffi::lua_gethookmask(self.ptr) }
  }

  ///      int lua_getinfo (lua_State *L, const char *what, lua_Debug *ar);
  /// `[-(0|1), +(0|1|2), m]`
  ///
  /// Gets information about a specific function or function invocation.
  ///
  /// To get information about a function invocation, the parameter `ar` must be a valid activation
  /// record that was filled by a previous call to `lua_getstack` or given as argument to a hook
  /// (see `lua_Hook`).
  ///
  /// To get information about a function, you push it onto the stack and start the `what` string
  /// with the character '`>`'. (In that case, `lua_getinfo` pops the function from the top of the
  /// stack.) For instance, to know in which line a function `f` was defined, you can write the
  /// following code:
  ///
  ///      lua_Debug ar;
  ///      lua_getglobal(L, "f");  /* get global 'f' */
  ///      lua_getinfo(L, ">S", &ar);
  ///      printf("%d\n", ar.linedefined);
  ///
  /// Each character in the string `what` selects some fields of the structure `ar` to be filled or
  /// a value to be pushed on the stack:
  ///
  /// * '`n`': fills in the field `name` and `namewhat`;
  /// * '`S`': fills in the fields `source`, `short_src`, `linedefined`, `lastlinedefined`, and
  ///   `what`;
  /// * '`l`': fills in the field `currentline`;
  /// * '`t`': fills in the field `istailcall`;
  /// * '`u`': fills in the fields `nups`, `nparams`, and `isvararg`;
  /// * '`f`': pushes onto the stack the function that is running at the given level;
  /// * '`L`': pushes onto the stack a table whose indices are the numbers of the lines that are
  ///   valid on the function. (A `valid line` is a line with some associated code, that is, a line
  ///   where you can put a break point. Non-valid lines include empty lines and comments.)
  ///
  ///     If this option is given together with option '`f`', its table is pushed after the
  /// function.
  ///
  ///     This is the only option that can raise a memory error.
  ///
  /// This function returns 0 to signal an invalid option in `what`; even then the valid options are
  /// handled correctly.
  pub fn getinfo(&self, what: &str) -> LuaDebug
  {
    let what_cstr = CString::new(what).unwrap();
    let mut ar: ffi::lua_Debug = unsafe { std::mem::zeroed() };
    // TODO: handle lua_getinfo's return.
    unsafe { ffi::lua_getinfo(self.ptr, what_cstr.as_ptr(), &mut ar) };
    ar.into()
  }

  // TODO: implement lua_getlocal.
  // TODO: implement lua_getstack.

  // TODO: implement lua_getupvalue.

  ///      void lua_insert (lua_State *L, int index);
  /// `[-1, +1, –]`
  ///
  /// Moves the top element into the given valid index, shifting up the elements above this index to
  /// open space. This function cannot be called with a pseudo-index, because a pseudo-index is not
  /// an actual stack position.
  pub fn insert(&self, index: i32)
  {
    self.rotate(index, 1);
  }

  // TODO: implement lua_len.

  ///      int lua_load (lua_State *L,
  ///                    lua_Reader reader,
  ///                    void *data,
  ///                    const char *chunkname,
  ///                    const char *mode);
  /// `[-0, +1, –]`
  ///
  /// Loads a Lua chunk without running it. If there are no errors, `lua_load` pushes the compiled
  /// chunk as a Lua function on top of the stack. Otherwise, it pushes an error message.
  ///
  /// The `lua_load` function uses a user-supplied `reader` function to read the chunk (see
  /// `lua_Reader`). The `data` argument is an opaque value passed to the reader function.
  ///
  /// The `chunkname` argument gives a name to the chunk, which is used for error messages and in
  /// debug information.
  ///
  /// `lua_load` automatically detects whether the chunk is text or binary and loads it accordingly
  /// (see program `luac`). The string `mode` works as in function `load`, with the addition that a
  /// `NULL` value is equivalent to the string `"bt"`.
  ///
  /// `lua_load` uses the stack internally, so the reader function must always leave the stack
  /// unmodified when returning.
  ///
  /// `lua_load` can return `LUA_OK`, `LUA_ERRSYNTAX`, or `LUA_ERRMEM`. The function may also return
  /// other values corresponding to errors raised by the read function.
  ///
  /// If the resulting function has upvalues, its first upvalue is set to the value of the global
  /// environment stored at index `LUA_RIDX_GLOBALS` in the registry. When loading main chunks, this
  /// upvalue will be the `_ENV` variable. Other upvalues are initialized with `nil`.
  pub fn load<R, T>(&self, mut reader: R, chunkname: &str, mode: &str) -> LuaThreadStatus
  where
    R: FnMut(&Self) -> &[u8],
  {
    extern "C" fn read<R>(l: *mut RawLuaState, ud: *mut c_void, sz: *mut ffi::size_t) -> *const c_char
    where
      R: FnMut(&LuaState) -> &[u8],
    {
      let state = unsafe { LuaState::from_ptr(l) };
      let f: &mut R = unsafe { std::mem::transmute(ud) };
      let slice = f(&state);
      unsafe { *sz = slice.len() as _ };
      slice.as_ptr() as _
    }
    let chunkname_cstr = CString::new(chunkname).unwrap();
    let mode_cstr = CString::new(mode).unwrap();
    unsafe {
      ffi::lua_load(
        self.ptr,
        Some(read::<R>),
        std::mem::transmute(&mut reader),
        chunkname_cstr.as_ptr(),
        mode_cstr.as_ptr(),
      )
    }
    .into()
  }

  ///      void lua_newtable (lua_State *L);
  /// `[-0, +1, m]`
  ///
  /// Creates a new empty table and pushes it onto the stack. It is equivalent to
  /// `lua_createtable(L, 0, 0)`.
  pub fn newtable(&self)
  {
    self.createtable(0, 0);
  }

  ///      int lua_next (lua_State *L, int index);
  /// `[-1, +(2|0), v]`
  ///
  /// Pops a key from the stack, and pushes a key–value pair from the table at the given index, the
  /// "next" pair after the given key. If there are no more elements in the table, then `lua_next`
  /// returns `false` and pushes nothing.
  ///
  /// A typical table traversal looks like this:
  ///
  ///      /* table is in the stack at index 't' */
  ///      lua_pushnil(L);  /* first key */
  ///      while (lua_next(L, t) != 0) {
  ///        /* uses 'key' (at index -2) and 'value' (at index -1) */
  ///        printf("%s - %s\n",
  ///               lua_typename(L, lua_type(L, -2)),
  ///               lua_typename(L, lua_type(L, -1)));
  ///        /* removes 'value'; keeps 'key' for next iteration */
  ///        lua_pop(L, 1);
  ///      }
  ///
  /// While traversing a table, avoid calling `lua_tolstring` directly on a key, unless you know
  /// that the key is actually a string. Recall that `lua_tolstring` may change the value at the
  /// given index; this confuses the next call to `lua_next`.
  ///
  /// This function may raise an error if the given key is neither `nil` nor present in the table.
  /// See function `next` for the caveats of modifying the table during its traversal.
  pub fn next(&self, index: i32) -> bool
  {
    unsafe { ffi::lua_next(self.ptr, index) != 0 }
  }

  ///      int lua_pcall (lua_State *L, int nargs, int nresults, int msgh);
  /// `[-(nargs + 1), +(nresults|1), –]`
  ///
  /// Calls a function (or a callable object) in protected mode.
  ///
  /// Both `nargs` and `nresults` have the same meaning as in `lua_call`. If there are no errors
  /// during the call, `lua_pcall` behaves exactly like `lua_call`. However, if there is any error,
  /// `lua_pcall` catches it, pushes a single value on the stack (the error object), and returns an
  /// error code. Like `lua_call`, `lua_pcall` always removes the function and its arguments from
  /// the stack.
  ///
  /// If `msgh` is 0, then the error object returned on the stack is exactly the original error
  /// object. Otherwise, `msgh` is the stack index of a `message handler`. (This index cannot be a
  /// pseudo-index.) In case of runtime errors, this handler will be called with the error object
  /// and its return value will be the object returned on the stack by `lua_pcall`.
  ///
  /// Typically, the message handler is used to add more debug information to the error object, such
  /// as a stack traceback. Such information cannot be gathered after the return of `lua_pcall`,
  /// since by then the stack has unwound.
  ///
  /// The `lua_pcall` function returns one of the following status codes: `LUA_OK`, `LUA_ERRRUN`,
  /// `LUA_ERRMEM`, or `LUA_ERRERR`.
  pub fn pcall(&self, nargs: i32, nresults: i32, msgh: i32) -> LuaThreadStatus
  {
    self.pcallk(nargs, nresults, msgh, 0, None)
  }

  ///      int lua_pcallk (lua_State *L,
  ///                      int nargs,
  ///                      int nresults,
  ///                      int msgh,
  ///                      lua_KContext ctx,
  ///                      lua_KFunction k);
  /// `[-(nargs + 1), +(nresults|1), –]`
  ///
  /// This function behaves exactly like `lua_pcall`, except that it allows the called function to
  /// yield.
  pub fn pcallk(&self, nargs: i32, nresults: i32, msgh: i32, ctx: KContext, k: KFunction) -> LuaThreadStatus
  {
    unsafe { ffi::lua_pcallk(self.ptr, nargs, nresults, msgh, ctx, k) }.into()
  }

  ///      void lua_pop (lua_State *L, int n);
  /// `[-n, +0, –]`
  ///
  /// Pops `n` elements from the stack.
  pub fn pop(&self, n: i32)
  {
    self.settop(-(n) - 1)
  }

  // TODO: implement lua_pushglobaltable.
  // TODO: implement lua_pushliteral.

  ///      void lua_register (lua_State *L, const char *name, lua_CFunction f);
  /// `[-0, +0, e]`
  ///
  /// Sets the C function `f` as the new value of global `name`. It is defined as a macro:
  ///
  ///      #define lua_register(L,n,f) \
  ///             (lua_pushcfunction(L, f), lua_setglobal(L, n))
  pub fn register(&self, name: &str, f: CFunction)
  {
    self.pushcfunction(f);
    self.setglobal(name);
  }

  // TODO: implement lua_remove.
  // TODO: implement lua_replace.
  // TODO: implement lua_resume.

  // TODO: implement lua_setallocf.
  // TODO: implement lua_setcstacklimit.
  // TODO: implement lua_sethook.
  // TODO: implement lua_setlocal.

  // TODO: implement lua_setupvalue.
  // TODO: implement lua_setwarnf.

  ///      int lua_status (lua_State *L);
  /// `[-0, +0, –]`
  ///
  /// Returns the status of the thread `L`.
  ///
  /// The status can be `LUA_OK` for a normal thread, an error code if the thread finished the
  /// execution of a `lua_resume` with an error, or `LUA_YIELD` if the thread is suspended.
  ///
  /// You can call functions only in threads with status `LUA_OK`. You can resume threads with
  /// status `LUA_OK` (to start a new coroutine) or `LUA_YIELD` (to resume a coroutine).
  pub fn status(&self) -> LuaThreadStatus
  {
    unsafe { ffi::lua_status(self.ptr) }.into()
  }

  ///      size_t lua_stringtonumber (lua_State *L, const char *s);
  /// `[-0, +1, –]`
  ///
  /// Converts the zero-terminated string `s` to a number, pushes that number into the stack, and
  /// returns the total size of the string, that is, its length plus one. The conversion can result
  /// in an integer or a float, according to the lexical conventions of Lua. The string may have
  /// leading and trailing whitespaces and a sign. If the string is not a valid numeral, returns 0
  /// and pushes nothing. (Note that the result can be used as a boolean, true if the conversion
  /// succeeds.)
  pub fn stringtonumber(&self, s: &str) -> u64
  {
    let s_cstr = CString::new(s).unwrap();
    unsafe { ffi::lua_stringtonumber(self.ptr, s_cstr.as_ptr()) }
  }

  ///      void lua_toclose (lua_State *L, int index);
  /// `[-0, +0, m]`
  ///
  /// Marks the given index in the stack as a to-be-closed "variable". Like a to-be-closed variable
  /// in Lua, the value at that index in the stack will be closed when it goes out of scope. Here,
  /// in the context of a C function, to go out of scope means that the running function returns to
  /// Lua, there is an error, or the index is removed from the stack through `lua_settop` or
  /// `lua_pop`. An index marked as to-be-closed should not be removed from the stack by any other
  /// function in the API except `lua_settop` or `lua_pop`.
  ///
  /// This function should not be called for an index that is equal to or below an active
  /// to-be-closed index.
  ///
  /// In the case of an out-of-memory error, the value in the given index is immediately closed, as
  /// if it was already marked.
  ///
  /// Note that, both in case of errors and of a regular return, by the time the `__close`
  /// metamethod runs, the C stack was already unwound, so that any automatic C variable declared in
  /// the calling function will be out of scope.
  pub fn toclose(&self, index: i32)
  {
    unsafe { ffi::lua_toclose(self.ptr, index) }
  }

  // TODO: implement lua_upvalueid.
  // TODO: implement lua_upvalueindex.
  // TODO: implement lua_upvaluejoin.
  // TODO: implement lua_version.
  // TODO: implement lua_warning.
  // TODO: implement lua_xmove.
  // TODO: implement lua_yield.
  // TODO: implement lua_yieldk.
  // TODO: implement luaL_Buffer.
  // TODO: implement luaL_Reg.
  // TODO: implement luaL_Stream.
  // TODO: implement luaL_addchar.
  // TODO: implement luaL_addgsub.
  // TODO: implement luaL_addlstring.
  // TODO: implement luaL_addsize.
  // TODO: implement luaL_addstring.
  // TODO: implement luaL_addvalue.
  // TODO: implement luaL_argcheck.
  // TODO: implement luaL_argerror.
  // TODO: implement luaL_argexpected.
  // TODO: implement luaL_buffaddr.
  // TODO: implement luaL_buffinit.
  // TODO: implement luaL_buffinitsize.
  // TODO: implement luaL_bufflen.
  // TODO: implement luaL_buffsub.
  // TODO: implement luaL_callmeta.

  // TODO: implement luaL_checkany.
  // TODO: implement luaL_checkinteger.
  // TODO: implement luaL_checklstring.
  // TODO: implement luaL_checknumber.
  // TODO: implement luaL_checkoption.
  // TODO: implement luaL_checkstack.
  // TODO: implement luaL_checkstring.
  // TODO: implement luaL_checktype.
  // TODO: implement luaL_checkudata.
  // TODO: implement luaL_checkversion.

  // TODO: implement luaL_error.
  // TODO: implement luaL_execresult.
  // TODO: implement luaL_fileresult.
  // TODO: implement luaL_getmetafield.
  // TODO: implement luaL_getmetatable.
  // TODO: implement luaL_getsubtable.
  // TODO: implement luaL_gsub.
  // TODO: implement luaL_len.

  ///      int luaL_loadbufferx (lua_State *L,
  ///                            const char *buff,
  ///                            size_t sz,
  ///                            const char *name,
  ///                            const char *mode);
  /// `[-0, +1, –]`
  ///
  /// Loads a buffer as a Lua chunk. This function uses `lua_load` to load the chunk in the buffer
  /// pointed to by `buff` with size `sz`.
  ///
  /// This function returns the same results as `lua_load`. `name` is the chunk name, used for debug
  /// information and error messages. The string `mode` works as in the function `lua_load`.
  pub fn loadbuffer(&self, buff: &str, sz: u64, name: &str) -> LuaThreadStatus
  {
    self.loadbufferx(buff, sz, name, "bt")
  }

  ///      int luaL_loadbufferx (lua_State *L,
  ///                            const char *buff,
  ///                            size_t sz,
  ///                            const char *name,
  ///                            const char *mode);
  /// `[-0, +1, –]`
  ///
  /// Loads a buffer as a Lua chunk. This function uses `lua_load` to load the chunk in the buffer
  /// pointed to by `buff` with size `sz`.
  ///
  /// This function returns the same results as `lua_load`. `name` is the chunk name, used for debug
  /// information and error messages. The string `mode` works as in the function `lua_load`.
  pub fn loadbufferx(&self, buff: &str, sz: u64, name: &str, mode: &str) -> LuaThreadStatus
  {
    let buff_cstr = CString::new(buff).unwrap();
    let name_cstr = CString::new(name).unwrap();
    let mode_cstr = CString::new(mode).unwrap();
    unsafe { ffi::luaL_loadbufferx(self.ptr, buff_cstr.as_ptr(), sz, name_cstr.as_ptr(), mode_cstr.as_ptr()) }.into()
  }

  ///      int luaL_loadfile (lua_State *L, const char *filename);
  /// `[-0, +1, m]`
  ///
  /// Equivalent to `luaL_loadfilex` with mode equal to `NULL`.
  pub fn loadfile(&self, filename: &str) -> LuaThreadStatus
  {
    let filename_cstr = CString::new(filename).unwrap();
    unsafe { ffi::luaL_loadfilex(self.ptr, filename_cstr.as_ptr(), std::ptr::null()) }.into()
  }

  ///      int luaL_loadfilex (lua_State *L, const char *filename,
  ///                                                  const char *mode);
  /// `[-0, +1, m]`
  ///
  /// Loads a file as a Lua chunk. This function uses `lua_load` to load the chunk in the file named
  /// `filename`. If `filename` is `NULL`, then it loads from the standard input. The first line in
  /// the file is ignored if it starts with a `#`.
  ///
  /// The string `mode` works as in the function `lua_load`.
  ///
  /// This function returns the same results as `lua_load` or `LUA_ERRFILE` for file-related errors.
  ///
  /// As `lua_load`, this function only loads the chunk; it does not run it.
  pub fn loadfilex(&self, filename: &str, mode: &str) -> LuaThreadStatus
  {
    let filename_cstr = CString::new(filename).unwrap();
    let mode_cstr = CString::new(mode).unwrap();
    unsafe { ffi::luaL_loadfilex(self.ptr, filename_cstr.as_ptr(), mode_cstr.as_ptr()) }.into()
  }

  ///      int luaL_loadstring (lua_State *L, const char *s);
  /// `[-0, +1, –]`
  ///
  /// Loads a string as a Lua chunk. This function uses `lua_load` to load the chunk in the
  /// zero-terminated string `s`.
  ///
  /// This function returns the same results as `lua_load`.
  ///
  /// Also as `lua_load`, this function only loads the chunk; it does not run it.
  pub fn loadstring(&self, s: &str) -> LuaThreadStatus
  {
    let s_cstr = CString::new(s).unwrap();
    unsafe { ffi::luaL_loadstring(self.ptr, s_cstr.as_ptr()) }.into()
  }

  ///      int luaL_dofile (lua_State *L, const char *filename);
  /// `[-0, +?, m]`
  ///
  /// Loads and runs the given file. It is defined as the following macro:
  ///
  ///      (luaL_loadfile(L, filename) || lua_pcall(L, 0, LUA_MULTRET, 0))
  ///
  /// It returns `LUA_OK` if there are no errors, or an error code in case of errors.
  pub fn dofile<P: AsRef<Path>>(&self, filepath: P) -> bool
  {
    // Lua macro defined in the C api only returns either 0 or 1. This means it would return
    // LUA_YIELD in case of an error. LUA_YIELD is not an error so it makes more sense to just
    // return bool.
    !self.loadfile(filepath.as_ref().to_str().unwrap()).is_error() && !self.pcall(0, MULTRET, 0).is_error()
  }

  ///      int luaL_dostring (lua_State *L, const char *str);
  /// `[-0, +?, –]`
  ///
  /// Loads and runs the given string. It is defined as the following macro:
  ///
  ///      (luaL_loadstring(L, str) || lua_pcall(L, 0, LUA_MULTRET, 0))
  ///
  /// It returns `LUA_OK` if there are no errors, or an error code in case of errors.
  pub fn dostring(&self, s: &str) -> bool
  {
    !self.loadstring(s).is_error() && !self.pcall(0, MULTRET, 0).is_error()
  }

  // TODO: implement luaL_newlib.
  // TODO: implement luaL_newlibtable.
  // TODO: implement luaL_newmetatable.

  ///      lua_State *luaL_newstate (void);
  /// `[-0, +0, –]`
  ///
  /// Creates a new Lua state. It calls `lua_newstate` with an allocator based on the standard C
  /// allocation functions and then sets a warning function and a panic function that print messages
  /// to the standard error output.
  ///
  /// Returns the new state, or `None` if there is a memory allocation error.
  pub fn l_newstate() -> Option<Self>
  {
    let l = unsafe { ffi::luaL_newstate() };
    if l.is_null() {
      None
    } else {
      Some(Self { ptr: l, owned: true })
    }
  }

  ///      void luaL_openlibs (lua_State *L);
  /// `[-0, +0, e]`
  ///
  /// Opens all standard Lua libraries into the given state.
  pub fn openlibs(&self)
  {
    unsafe { ffi::luaL_openlibs(self.ptr) };
  }

  // TODO: implement luaL_opt.
  // TODO: implement luaL_optinteger.
  // TODO: implement luaL_optlstring.
  // TODO: implement luaL_optnumber.
  // TODO: implement luaL_optstring.
  // TODO: implement luaL_prepbuffer.
  // TODO: implement luaL_prepbuffsize.
  // TODO: implement luaL_pushfail.
  // TODO: implement luaL_pushresult.
  // TODO: implement luaL_pushresultsize.
  // TODO: implement luaL_ref.

  /// void luaL_requiref (lua_State *L, const char *modname,
  ///                     lua_CFunction openf, int glb);
  /// `[-0, +1, e]`
  ///
  /// If `package.loaded[modname]` is not true, calls the function `openf` with the string `modname`
  /// as an argument and sets the call result to `package.loaded[modname]`, as if that function has
  /// been called through `require`.
  ///
  /// If `glb` is true, also stores the module into the global `modname`.
  ///
  /// Leaves a copy of the module on the stack.
  pub fn requiref(&self, modname: &str, openf: CFunction, glb: bool)
  {
    let modname_cstr = CString::new(modname).unwrap();
    unsafe { ffi::luaL_requiref(self.ptr, modname_cstr.as_ptr(), openf, glb as _) };
  }

  // TODO: implement luaL_setfuncs.
  // TODO: implement luaL_setmetatable.
  // TODO: implement luaL_testudata.
  // TODO: implement luaL_tolstring.
  // TODO: implement luaL_traceback.
  // TODO: implement luaL_typeerror.
  // TODO: implement luaL_typename.
  // TODO: implement luaL_unref.
  // TODO: implement luaL_where.

  pub fn open_base(&self) -> i32
  {
    unsafe { ffi::luaopen_base(self.ptr) }
  }

  pub fn open_coroutine(&self) -> i32
  {
    unsafe { ffi::luaopen_coroutine(self.ptr) }
  }

  pub fn open_debug(&self) -> i32
  {
    unsafe { ffi::luaopen_debug(self.ptr) }
  }

  pub fn open_io(&self) -> i32
  {
    unsafe { ffi::luaopen_io(self.ptr) }
  }

  pub fn open_math(&self) -> i32
  {
    unsafe { ffi::luaopen_math(self.ptr) }
  }

  pub fn open_os(&self) -> i32
  {
    unsafe { ffi::luaopen_os(self.ptr) }
  }

  pub fn open_package(&self) -> i32
  {
    unsafe { ffi::luaopen_package(self.ptr) }
  }

  pub fn open_string(&self) -> i32
  {
    unsafe { ffi::luaopen_string(self.ptr) }
  }

  pub fn open_table(&self) -> i32
  {
    unsafe { ffi::luaopen_table(self.ptr) }
  }

  pub fn open_utf8(&self) -> i32
  {
    unsafe { ffi::luaopen_utf8(self.ptr) }
  }

  // TODO: implement LUA_HOOKCALL.
  // TODO: implement LUA_HOOKCOUNT.
  // TODO: implement LUA_HOOKLINE.
  // TODO: implement LUA_HOOKRET.
  // TODO: implement LUA_HOOKTAILCALL.
  // TODO: implement LUAL_BUFFERSIZE.
  // TODO: implement LUA_MASKCALL.
  // TODO: implement LUA_MASKCOUNT.
  // TODO: implement LUA_MASKLINE.
  // TODO: implement LUA_MASKRET.
  // TODO: implement LUA_MAXINTEGER.
  // TODO: implement LUA_MININTEGER.
  // TODO: implement LUA_MINSTACK.
  // TODO: implement LUA_MULTRET.
  // TODO: implement LUA_NOREF.
  // TODO: implement LUA_REFNIL.
  // TODO: implement LUA_REGISTRYINDEX.
  // TODO: implement LUA_RIDX_GLOBALS.
  // TODO: implement LUA_RIDX_MAINTHREAD.
  // TODO: implement LUA_USE_APICHECK.
}

impl Drop for LuaState
{
  fn drop(&mut self)
  {
    if self.owned {
      self.close().unwrap()
    }
  }
}
