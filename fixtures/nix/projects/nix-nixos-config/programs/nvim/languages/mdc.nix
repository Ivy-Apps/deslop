_:

{
  autoCmd = [
    {
      event = [
        "BufRead"
        "BufNewFile"
      ];
      pattern = [ "*.mdc" ];
      callback = {
        __raw = "function() vim.bo.filetype = 'markdown' end";
      };
    }
  ];
}
