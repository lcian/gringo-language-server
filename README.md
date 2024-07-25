# gringo-language-server

This is an LSP server for the gringo (clasp, Answer Set Programming) language.

It provides errors, warnings and code completion.

![](./media/demo.gif)

# Installation (Neovim)

0. install Rust: `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`

1. install this LSP server: `cargo install --git https://github.com/lcian/gringo-language-server`

2. add the following code to your `init.lua`:

```lua
vim.filetype.add({
    extension = {
        lp = "gringo"
    }
})

local name = "gringo-language-server"
local client = vim.lsp.start_client {
    name = name,
    cmd = { name },
}
if not client then
    vim.notify ("Something went wrong when starting " .. name)
    return
end

vim.api.nvim_create_autocmd("FileType", {
    pattern = "gringo",
    callback = function ()
        vim.lsp.buf_attach_client(0, client)
    end,
})
```
