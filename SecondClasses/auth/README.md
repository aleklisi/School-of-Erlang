## EX 2.4.1 Auth module - finish tests implementation 

Open `auth_service_test.erl` file. Go to eunit's tests section called `authenticate a user`. Based on the names of the tests and the tests above implement the tests from this section. Try adding one or 2 more test scenarios, like the following: authenticate a user, when many users are stored in the state.

Implement also `state_size/1` and `init_state/1` for maps.

### Tips

Use following snippet to recompile the modules, and run the tests `rebar3 eunit`.
You should see something like `All 12 tests passed.` if everything goes well.
Remember that if you want to add an eunit test the test name must end with `..._test()` and it is 0 arguments function. 

See http://erlang.org/doc/apps/eunit/chapter.html for more info.

## Ex EX 2.4.2 - Auth module - map implementation

Create file `map_based_auth_service.erl` in the `Introduction` directory. Create your own implementation of auth service. Replace current `IMPLEMENTATION` macro value with your module name, and try implementing the auth module based on erlang maps.

### Tips

Use `list_based_auth_service.erl` as a reference.
See http://erlang.org/doc/man/maps.html for some help.
Try making your code shorter with http://erlang.org/doc/reference_manual/expressions.html#map-expressions
