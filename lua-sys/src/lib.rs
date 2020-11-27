pub mod ffi;

// #![allow(non_camel_case_types)]
// #![allow(non_upper_case_globals)]
//
// mod ffi;
//
// pub const LUA_VERSION_MAJOR: &[u8] = ffi::LUA_VERSION_MAJOR;
// pub const LUA_VERSION_MINOR: &[u8] = ffi::LUA_VERSION_MINOR;
// pub const LUA_VERSION_RELEASE: &[u8] = ffi::LUA_VERSION_RELEASE;
//
// pub const LUA_VERSION_NUM: u32 = ffi::LUA_VERSION_NUM;
// pub const LUA_VERSION_RELEASE_NUM: u32 = ffi::LUA_VERSION_RELEASE_NUM;
//
// pub const LUA_VERSION: &[u8] = ffi::LUA_VERSION;
// pub const LUA_RELEASE: &[u8] = ffi::LUA_RELEASE;
// pub const LUA_COPYRIGHT: &[u8] = ffi::LUA_COPYRIGHT;
// pub const LUA_AUTHORS: &[u8] = ffi::LUA_AUTHORS;
//
// /// mark for precompiled code ('<esc>Lua')
// pub const LUA_SIGNATURE: &[u8] = ffi::LUA_SIGNATURE;
//
// /// option for multiple returns in 'lua_pcall' and 'lua_call'
// pub const LUA_MULTRET: i32 = ffi::LUA_MULTRET;
//
// /// Pseudo-indices
// /// (-LUAI_MAXSTACK is the minimum valid index; we keep some free empty
// /// space after that to help overflow detection)
// pub const LUA_REGISTRYINDEX: i32 = ffi::LUA_REGISTRYINDEX;
//
// #[macro_export]
// macro_rules! lua_upvalueindex {
//     ($i:ident) => {
//         (LUA_REGISTRYINDEX - $i)
//     };
// }
//
// /* thread status */
// pub const LUA_OK: u32 = ffi::LUA_OK;
// pub const LUA_YIELD: u32 = ffi::LUA_YIELD;
// pub const LUA_ERRRUN: u32 = ffi::LUA_ERRRUN;
// pub const LUA_ERRSYNTAX: u32 = ffi::LUA_ERRSYNTAX;
// pub const LUA_ERRMEM: u32 = ffi::LUA_ERRMEM;
// pub const LUA_ERRERR: u32 = ffi::LUA_ERRERR;
//
// pub type lua_State = ffi::lua_State;
//
// /*
// ** basic types
// */
// pub const LUA_TNONE: i32 = ffi::LUA_TNONE;
//
// pub const LUA_TNIL: u32 = ffi::LUA_TNIL;
// pub const LUA_TBOOLEAN: u32 = ffi::LUA_TBOOLEAN;
// pub const LUA_TLIGHTUSERDATA: u32 = ffi::LUA_TLIGHTUSERDATA;
// pub const LUA_TNUMBER: u32 = ffi::LUA_TNUMBER;
// pub const LUA_TSTRING: u32 = ffi::LUA_TSTRING;
// pub const LUA_TTABLE: u32 = ffi::LUA_TTABLE;
// pub const LUA_TFUNCTION: u32 = ffi::LUA_TFUNCTION;
// pub const LUA_TUSERDATA: u32 = ffi::LUA_TUSERDATA;
// pub const LUA_TTHREAD: u32 = ffi::LUA_TTHREAD;
//
// pub const LUA_NUMTYPES: u32 = ffi::LUA_NUMTYPES;
//
// /// minimum Lua stack available to a C function
// pub const LUA_MINSTACK: u32 = ffi::LUA_MINSTACK;
//
// /* predefined values in the registry */
// pub const LUA_RIDX_MAINTHREAD: u32 = ffi::LUA_RIDX_MAINTHREAD;
// pub const LUA_RIDX_GLOBALS: u32 = ffi::LUA_RIDX_GLOBALS;
// pub const LUA_RIDX_LAST: u32 = ffi::LUA_RIDX_LAST;
//
// /// type of numbers in Lua
// pub type LUA_NUMBER = ffi::lua_Number;
//
// /// type for integer functions
// pub type LUA_INTEGER = ffi::lua_Integer;
//
// /// unsigned integer type
// pub type LUA_UNSIGNED = ffi::lua_Unsigned;
//
// /// type for continuation-function contexts
// pub type LUA_KCONTEXT = ffi::lua_KContext;
//
// /// Type for C functions registered with Lua
// pub type lua_CFunction = ffi::lua_CFunction;
//
// /// Type for continuation functions
// pub type lua_KFunction = ffi::lua_KFunction;
//
// /// Type for functions that read/write blocks when loading/dumping Lua chunks
// pub type lua_Reader = ffi::lua_Reader;
//
// pub type lua_Writer = ffi::lua_Writer;
//
// /// Type for memory-allocation functions
// pub type lua_Alloc = ffi::lua_Alloc;
//
// /// Type for warning functions
// pub type lua_WarnFunction = ffi::lua_WarnFunction;
//
// /// RCS ident string
// pub use ffi::lua_ident;
//
// // state manipulation
// pub use ffi::{
//     lua_newstate,
//     lua_close,
//     lua_newthread,
//     lua_resetthread,
// };
//
// // LUA_API lua_CFunction (lua_atpanic) (lua_State *L, lua_CFunction panicf);
//
// // LUA_API lua_Number (lua_version) (lua_State *L);
//
// // ** basic stack manipulation
// // LUA_API int   (lua_absindex) (lua_State *L, int idx);
// // LUA_API int   (lua_gettop) (lua_State *L);
// // LUA_API void  (lua_settop) (lua_State *L, int idx);
// // LUA_API void  (lua_pushvalue) (lua_State *L, int idx);
// // LUA_API void  (lua_rotate) (lua_State *L, int idx, int n);
// // LUA_API void  (lua_copy) (lua_State *L, int fromidx, int toidx);
// // LUA_API int   (lua_checkstack) (lua_State *L, int n);
//
// // LUA_API void  (lua_xmove) (lua_State *from, lua_State *to, int n);
//
// // ** access functions (stack -> C)
// // LUA_API int             (lua_isnumber) (lua_State *L, int idx);
// // LUA_API int             (lua_isstring) (lua_State *L, int idx);
// // LUA_API int             (lua_iscfunction) (lua_State *L, int idx);
// // LUA_API int             (lua_isinteger) (lua_State *L, int idx);
// // LUA_API int             (lua_isuserdata) (lua_State *L, int idx);
// // LUA_API int             (lua_type) (lua_State *L, int idx);
// // LUA_API const char     *(lua_typename) (lua_State *L, int tp);
//
// // LUA_API lua_Number      (lua_tonumberx) (lua_State *L, int idx, int *isnum);
// // LUA_API lua_Integer     (lua_tointegerx) (lua_State *L, int idx, int *isnum);
// // LUA_API int             (lua_toboolean) (lua_State *L, int idx);
// // LUA_API const char     *(lua_tolstring) (lua_State *L, int idx, size_t *len);
// // LUA_API lua_Unsigned    (lua_rawlen) (lua_State *L, int idx);
// // LUA_API lua_CFunction   (lua_tocfunction) (lua_State *L, int idx);
// // LUA_API void	       *(lua_touserdata) (lua_State *L, int idx);
// // LUA_API lua_State      *(lua_tothread) (lua_State *L, int idx);
// // LUA_API const void     *(lua_topointer) (lua_State *L, int idx);
//
// // ** Comparison and arithmetic functions
// // pub const LUA_OPADD	0	/* ORDER TM, ORDER OP */
// // pub const LUA_OPSUB	1
// // pub const LUA_OPMUL	2
// // pub const LUA_OPMOD	3
// // pub const LUA_OPPOW	4
// // pub const LUA_OPDIV	5
// // pub const LUA_OPIDIV	6
// // pub const LUA_OPBAND	7
// // pub const LUA_OPBOR	8
// // pub const LUA_OPBXOR	9
// // pub const LUA_OPSHL	10
// // pub const LUA_OPSHR	11
// // pub const LUA_OPUNM	12
// // pub const LUA_OPBNOT	13
//
// // LUA_API void  (lua_arith) (lua_State *L, int op);
//
// // pub const LUA_OPEQ	0
// // pub const LUA_OPLT	1
// // pub const LUA_OPLE	2
//
// // LUA_API int   (lua_rawequal) (lua_State *L, int idx1, int idx2);
// // LUA_API int   (lua_compare) (lua_State *L, int idx1, int idx2, int op);
//
// // ** push functions (C -> stack)
// // LUA_API void        (lua_pushnil) (lua_State *L);
// // LUA_API void        (lua_pushnumber) (lua_State *L, lua_Number n);
// // LUA_API void        (lua_pushinteger) (lua_State *L, lua_Integer n);
// // LUA_API const char *(lua_pushlstring) (lua_State *L, const char *s, size_t len);
// // LUA_API const char *(lua_pushstring) (lua_State *L, const char *s);
// // LUA_API const char *(lua_pushvfstring) (lua_State *L, const char *fmt,
//
// // LUA_API const char *(lua_pushfstring) (lua_State *L, const char *fmt, ...);
// // LUA_API void  (lua_pushcclosure) (lua_State *L, lua_CFunction fn, int n);
// // LUA_API void  (lua_pushboolean) (lua_State *L, int b);
// // LUA_API void  (lua_pushlightuserdata) (lua_State *L, void *p);
// // LUA_API int   (lua_pushthread) (lua_State *L);
//
// // ** get functions (Lua -> stack)
// // LUA_API int (lua_getglobal) (lua_State *L, const char *name);
// // LUA_API int (lua_gettable) (lua_State *L, int idx);
// // LUA_API int (lua_getfield) (lua_State *L, int idx, const char *k);
// // LUA_API int (lua_geti) (lua_State *L, int idx, lua_Integer n);
// // LUA_API int (lua_rawget) (lua_State *L, int idx);
// // LUA_API int (lua_rawgeti) (lua_State *L, int idx, lua_Integer n);
// // LUA_API int (lua_rawgetp) (lua_State *L, int idx, const void *p);
//
// // LUA_API void  (lua_createtable) (lua_State *L, int narr, int nrec);
// // LUA_API void *(lua_newuserdatauv) (lua_State *L, size_t sz, int nuvalue);
// // LUA_API int   (lua_getmetatable) (lua_State *L, int objindex);
// // LUA_API int  (lua_getiuservalue) (lua_State *L, int idx, int n);
//
// // ** set functions (stack -> Lua)
// // LUA_API void  (lua_setglobal) (lua_State *L, const char *name);
// // LUA_API void  (lua_settable) (lua_State *L, int idx);
// // LUA_API void  (lua_setfield) (lua_State *L, int idx, const char *k);
// // LUA_API void  (lua_seti) (lua_State *L, int idx, lua_Integer n);
// // LUA_API void  (lua_rawset) (lua_State *L, int idx);
// // LUA_API void  (lua_rawseti) (lua_State *L, int idx, lua_Integer n);
// // LUA_API void  (lua_rawsetp) (lua_State *L, int idx, const void *p);
// // LUA_API int   (lua_setmetatable) (lua_State *L, int objindex);
// // LUA_API int   (lua_setiuservalue) (lua_State *L, int idx, int n);
//
// // ** 'load' and 'call' functions (load and run Lua code)
// // LUA_API void  (lua_callk) (lua_State *L, int nargs, int nresults,
// //                            lua_KContext ctx, lua_KFunction k);
// // pub const lua_call(L,n,r)		lua_callk(L, (n), (r), 0, NULL)
// //
// // LUA_API int   (lua_pcallk) (lua_State *L, int nargs, int nresults, int errfunc,
// //                             lua_KContext ctx, lua_KFunction k);
// // pub const lua_pcall(L,n,r,f)	lua_pcallk(L, (n), (r), (f), 0, NULL)
// //
// // LUA_API int   (lua_load) (lua_State *L, lua_Reader reader, void *dt,
// //                           const char *chunkname, const char *mode);
// //
// // LUA_API int (lua_dump) (lua_State *L, lua_Writer writer, void *data, int strip);
// //
// //
// // /*
// // ** coroutine functions
// // */
// // LUA_API int  (lua_yieldk)     (lua_State *L, int nresults, lua_KContext ctx,
// //                                lua_KFunction k);
// // LUA_API int  (lua_resume)     (lua_State *L, lua_State *from, int narg,
// //                                int *nres);
// // LUA_API int  (lua_status)     (lua_State *L);
// // LUA_API int (lua_isyieldable) (lua_State *L);
// //
// // pub const lua_yield(L,n)		lua_yieldk(L, (n), 0, NULL)
// //
// //
// // /*
// // ** Warning-related functions
// // */
// // LUA_API void (lua_setwarnf) (lua_State *L, lua_WarnFunction f, void *ud);
// // LUA_API void (lua_warning)  (lua_State *L, const char *msg, int tocont);
// //
// //
// // /*
// // ** garbage-collection function and options
// // */
// //
// // pub const LUA_GCSTOP		0
// // pub const LUA_GCRESTART		1
// // pub const LUA_GCCOLLECT		2
// // pub const LUA_GCCOUNT		3
// // pub const LUA_GCCOUNTB		4
// // pub const LUA_GCSTEP		5
// // pub const LUA_GCSETPAUSE		6
// // pub const LUA_GCSETSTEPMUL	7
// // pub const LUA_GCISRUNNING		9
// // pub const LUA_GCGEN		10
// // pub const LUA_GCINC		11
// //
// // LUA_API int (lua_gc) (lua_State *L, int what, ...);
// //
// //
// // /*
// // ** miscellaneous functions
// // */
// //
// // LUA_API int   (lua_error) (lua_State *L);
// //
// // LUA_API int   (lua_next) (lua_State *L, int idx);
// //
// // LUA_API void  (lua_concat) (lua_State *L, int n);
// // LUA_API void  (lua_len)    (lua_State *L, int idx);
// //
// // LUA_API size_t   (lua_stringtonumber) (lua_State *L, const char *s);
// //
// // LUA_API lua_Alloc (lua_getallocf) (lua_State *L, void **ud);
// // LUA_API void      (lua_setallocf) (lua_State *L, lua_Alloc f, void *ud);
// //
// // LUA_API void  (lua_toclose) (lua_State *L, int idx);
// //
// //
// // /*
// // ** {==============================================================
// // ** some useful macros
// // ** ===============================================================
// // */
// //
// // pub const lua_getextraspace(L)	((void *)((char *)(L) - LUA_EXTRASPACE))
// //
// // pub const lua_tonumber(L,i)	lua_tonumberx(L,(i),NULL)
// // pub const lua_tointeger(L,i)	lua_tointegerx(L,(i),NULL)
// //
// // pub const lua_pop(L,n)		lua_settop(L, -(n)-1)
// //
// // pub const lua_newtable(L)		lua_createtable(L, 0, 0)
// //
// // pub const lua_register(L,n,f) (lua_pushcfunction(L, (f)), lua_setglobal(L, (n)))
// //
// // pub const lua_pushcfunction(L,f)	lua_pushcclosure(L, (f), 0)
// //
// // pub const lua_isfunction(L,n)	(lua_type(L, (n)) == LUA_TFUNCTION)
// // pub const lua_istable(L,n)	(lua_type(L, (n)) == LUA_TTABLE)
// // pub const lua_islightuserdata(L,n)	(lua_type(L, (n)) == LUA_TLIGHTUSERDATA)
// // pub const lua_isnil(L,n)		(lua_type(L, (n)) == LUA_TNIL)
// // pub const lua_isboolean(L,n)	(lua_type(L, (n)) == LUA_TBOOLEAN)
// // pub const lua_isthread(L,n)	(lua_type(L, (n)) == LUA_TTHREAD)
// // pub const lua_isnone(L,n)		(lua_type(L, (n)) == LUA_TNONE)
// // pub const lua_isnoneornil(L, n)	(lua_type(L, (n)) <= 0)
// //
// // pub const lua_pushliteral(L, s)	lua_pushstring(L, "" s)
// //
// // pub const lua_pushglobaltable(L)  \
// // 	((void)lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS))
// //
// // pub const lua_tostring(L,i)	lua_tolstring(L, (i), NULL)
// //
// //
// // pub const lua_insert(L,idx)	lua_rotate(L, (idx), 1)
// //
// // pub const lua_remove(L,idx)	(lua_rotate(L, (idx), -1), lua_pop(L, 1))
// //
// // pub const lua_replace(L,idx)	(lua_copy(L, -1, (idx)), lua_pop(L, 1))
// //
// // /* }============================================================== */
// //
// //
// // /*
// // ** {==============================================================
// // ** compatibility macros
// // ** ===============================================================
// // */
// // #if defined(LUA_COMPAT_APIINTCASTS)
// //
// // pub const lua_pushunsigned(L,n)	lua_pushinteger(L, (lua_Integer)(n))
// // pub const lua_tounsignedx(L,i,is)	((lua_Unsigned)lua_tointegerx(L,i,is))
// // pub const lua_tounsigned(L,i)	lua_tounsignedx(L,(i),NULL)
// //
// // #endif
// //
// // pub const lua_newuserdata(L,s)	lua_newuserdatauv(L,s,1)
// // pub const lua_getuservalue(L,idx)	lua_getiuservalue(L,idx,1)
// // pub const lua_setuservalue(L,idx)	lua_setiuservalue(L,idx,1)
// //
// // pub const LUA_NUMTAGS		LUA_NUMTYPES
// //
// // /* }============================================================== */
// //
// // /*
// // ** {======================================================================
// // ** Debug API
// // ** =======================================================================
// // */
// //
// //
// // /*
// // ** Event codes
// // */
// // pub const LUA_HOOKCALL	0
// // pub const LUA_HOOKRET	1
// // pub const LUA_HOOKLINE	2
// // pub const LUA_HOOKCOUNT	3
// // pub const LUA_HOOKTAILCALL 4
// //
// //
// // /*
// // ** Event masks
// // */
// // pub const LUA_MASKCALL	(1 << LUA_HOOKCALL)
// // pub const LUA_MASKRET	(1 << LUA_HOOKRET)
// // pub const LUA_MASKLINE	(1 << LUA_HOOKLINE)
// // pub const LUA_MASKCOUNT	(1 << LUA_HOOKCOUNT)
// //
// // pub type struct lua_Debug lua_Debug;  /* activation record */
// //
// //
// // /* Functions to be called by the debugger in specific events */
// // pub type void (*lua_Hook) (lua_State *L, lua_Debug *ar);
// //
// //
// // LUA_API int (lua_getstack) (lua_State *L, int level, lua_Debug *ar);
// // LUA_API int (lua_getinfo) (lua_State *L, const char *what, lua_Debug *ar);
// // LUA_API const char *(lua_getlocal) (lua_State *L, const lua_Debug *ar, int n);
// // LUA_API const char *(lua_setlocal) (lua_State *L, const lua_Debug *ar, int n);
// // LUA_API const char *(lua_getupvalue) (lua_State *L, int funcindex, int n);
// // LUA_API const char *(lua_setupvalue) (lua_State *L, int funcindex, int n);
// //
// // LUA_API void *(lua_upvalueid) (lua_State *L, int fidx, int n);
// // LUA_API void  (lua_upvaluejoin) (lua_State *L, int fidx1, int n1,
// //                                                int fidx2, int n2);
// //
// // LUA_API void (lua_sethook) (lua_State *L, lua_Hook func, int mask, int count);
// // LUA_API lua_Hook (lua_gethook) (lua_State *L);
// // LUA_API int (lua_gethookmask) (lua_State *L);
// // LUA_API int (lua_gethookcount) (lua_State *L);
// //
// // LUA_API int (lua_setcstacklimit) (lua_State *L, unsigned int limit);
// //
// // struct lua_Debug {
// //   int event;
// //   const char *name;	/* (n) */
// //   const char *namewhat;	/* (n) 'global', 'local', 'field', 'method' */
// //   const char *what;	/* (S) 'Lua', 'C', 'main', 'tail' */
// //   const char *source;	/* (S) */
// //   size_t srclen;	/* (S) */
// //   int currentline;	/* (l) */
// //   int linedefined;	/* (S) */
// //   int lastlinedefined;	/* (S) */
// //   unsigned char nups;	/* (u) number of upvalues */
// //   unsigned char nparams;/* (u) number of parameters */
// //   char isvararg;        /* (u) */
// //   char istailcall;	/* (t) */
// //   unsigned short ftransfer;   /* (r) index of first value transferred */
// //   unsigned short ntransfer;   /* (r) number of transferred values */
// //   char short_src[LUA_IDSIZE]; /* (S) */
// //   /* private part */
// //   struct CallInfo *i_ci;  /* active function */
// // };
