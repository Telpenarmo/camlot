mod handlers;
mod lsp;
mod lsp_utils;
mod server;

fn main() {
    match lsp::main() {
        Ok(()) => (),
        Err(e) => eprintln!("LSP failed: {e}"),
    }
}
