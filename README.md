# code-jam-2018

My solutions to Code Jam 2018 problems. Posted after every finished round.

Tests and code are executed with the following command during development:

```
stack runghc \
  --package tasty \
  --package tasty-hunit \
  --package tasty-quickcheck \
  --package tasty-smallcheck \
  --package pipes \
  <FILE_NAME>
```

Used via the following vim binding:

```
:map <leader>r :split \| :terminal stack runghc --package tasty --package tasty-hunit --package tasty-quickcheck --package tasty-smallcheck --package pipes %<CR>
```
