map <buffer> <leader>r :split \| :terminal stack runghc --package tasty --package tasty-hunit
  \ --package tasty-quickcheck --package tasty-smallcheck --package pipes --cwd %:p:h %:t<CR>
