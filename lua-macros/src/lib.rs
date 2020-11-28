mod entry;

use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn func(args: TokenStream, item: TokenStream) -> TokenStream
{
  entry::func(args, item)
}
