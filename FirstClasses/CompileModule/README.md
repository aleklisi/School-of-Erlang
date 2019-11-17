## Ex 0 - Compile module

- Open file `intro_ex1.erl`.
- Open terminal and go to: `$REPO/FirstClasses/CompileModule/`.
- Run `erl` (BEAM) in terminal. Notice the prompt should change to command counter: `1>`.
- Use `ls().` to list files in current directory, You should see `README.md` and `intro_ex1.erl` files.
- To compile a module type to terminal `c(intro_ex1).`, do not forget about the dot (`.`) at the end of expression.
- Check the result, it should look like `{ok, intro_ex1}`.
- Use `ls().` to list files in current directory. Notice that `intro_ex1.beam` file was created next to `intro_ex1.erl`, it is BEAM binary file.
- Try calling the function `intro_ex1:hello_name/1` with your name as string argument. In my case it is: `intro_ex1:hello_name("Aleksander").`

```erlang 
Hello "Aleksander"!
ok
```

should be printed to terminal. Well done, welcome to the World of Erlang :)

- Next, try calling the function `hello/2` with your name and surname as string arguments: `intro_ex1:hello("Aleksander", "Lisiecki").`

```
** exception error: undefined function intro_ex1:hello/2
```

should be printed to terminal. The function `intro_ex1:hello/2` is not exported from module, that is why the error occurred. To fix it add `hello/2` to `-export([...]).`, recompile and try again.

#### Tips

- Notice the directive `-module(intro_ex1).` in the first line, it must be the same as filename without extension.
- Remember to export all of the functions you want to have access to outside module with `-export([foo/2]).`
- Have you noticed 2 warnings when compiling the module for the first time?
  - `intro_ex1.erl:8: Warning: function hello/2 is unused` means that the function `hello/2` form the module is not exported and is not used anywhere in the module so it will never be called.
  - `intro_ex1.erl:8: Warning: variable 'Surname' is unused` means that the variable `Surname` is passed to the function but is not used anywhere inside, this warning often helps a programmer to spot arguments he missed to use, but if you do not want to warning to appear explicitly just rename variable to `_` or `_Surname` which means that the passed argument is ignored.