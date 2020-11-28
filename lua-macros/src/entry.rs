use proc_macro::TokenStream;
use quote::quote;

pub(crate) fn func(_: TokenStream, item: TokenStream) -> TokenStream
{
  let input = syn::parse_macro_input!(item as syn::ItemFn);
  let vis = &input.vis;
  let body = &input.block;
  let fn_name = &input.sig.ident;
  let mut state_name: Option<&syn::Ident> = None;
  for arg in &input.sig.inputs {
    if let syn::FnArg::Typed(typed) = arg {
      if let syn::Pat::Ident(ident) = &*typed.pat {
        state_name = Some(&ident.ident);
      }
    }
  }
  if state_name.is_none() {
    let msg = "First argument passed to this function must be a &LuaState";
    return syn::Error::new_spanned(&input.sig.ident, msg).to_compile_error().into();
  }
  let state_name = state_name.unwrap();
  let return_fn = quote! {
    #vis fn #fn_name(#state_name: *mut ::lua::RawLuaState) -> ::std::os::raw::c_int
    {
      let #state_name = unsafe { ::lua::LuaState::from_ptr(l) };
      #body
    }
  };
  return_fn.into()
}
