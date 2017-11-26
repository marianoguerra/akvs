A Key Value Store
=================

We first need to have erlang installed, I will show you how to setup any version
you want to use, and a way to have the version I will use for this without
affecting any other installation you may have.

Setting up kerl
---------------

For this we will use `kerl <https://github.com/kerl/kerl>`_, from it's github README::

    Easy building and installing of Erlang/OTP instances.

    Kerl aims to be shell agnostic and its only dependencies, excluding what's
    required to actually build Erlang/OTP, are curl and git.

So, first we need to fetch kerl:

.. code:: sh

    # create bin folder in our home directory if it's not already there
    mkdir -p ~/bin

    # cd to it
    cd ~/bin

    # download kerl script
    curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl

    # set execution permitions for our user
    chmod u+x kerl

You will need to add ~/bin to your PATH variable so your shell can find the
kerl script, you can do it like this in your shell:

.. code:: sh

    # set the PATH environment variable to the value it had before plus a colon
    # (path separator) and a new path which points to the bin folder we just
    # created
    PATH=$PATH:$HOME/bin

If you want to make this work every time you start a shell you need to put it
it the rc file of your shell of choice, for bash it's ~/.bashrc, for zsh it's
.zshrc, check your shell's docs for other shells, you will have to add a line like this:

.. code:: sh

    export PATH=$PATH:$HOME/bin

After this, start a new shell or source your rc file so that it picks up your
new PATH variable, you can check that it's set correctly by running:

.. code:: sh

    echo $PATH

Building an Erlang release with kerl
------------------------------------

We have kerl installed and available in our shell, now we need to build an
Erlang release of our choice, for this we will need a compiler and other
tools and libraries needed to compile it:

This are instructions on ubuntu 17.10, check the names for those packages
on your distribution.

.. code:: sh

    # required: basic tools and libraries needed
    # (compiler, curses for the shell, ssl for crypto)
    sudo apt-get -y install build-essential m4 libncurses5-dev libssl-dev

    # optonal: if you want odbc support (database connectivity)
    sudo apt-get install unixodbc-dev

    # optonal: if you want pdf docs you need apache fop and xslt tools and java (fop is a java project)
    sudo apt-get install -y fop xsltproc default-jdk

    # optional: if you want to build jinterface you need a JDK
    sudo apt-get install -y default-jdk

    # optional: if you want wx (desktop GUI modules)
    sudo apt-get install -y libwxgtk3.0-dev

Now that we have everything we need we can finally build our erlang release.

First we fetch an updated list of releases:

.. code:: sh

    kerl update releases

The output in my case::

    The available releases are:
	R10B-0 R10B-10 R10B-1a R10B-2 R10B-3 R10B-4 R10B-5 R10B-6 R10B-7 R10B-8
	R10B-9 R11B-0 R11B-1 R11B-2 R11B-3 R11B-4 R11B-5 R12B-0 R12B-1 R12B-2 R12B-3
	R12B-4 R12B-5 R13A R13B01 R13B02-1 R13B02 R13B03 R13B04 R13B R14A R14B01
	R14B02 R14B03 R14B04 R14B_erts-5.8.1.1 R14B R15B01 R15B02
	R15B02_with_MSVCR100_installer_fix R15B03-1 R15B03 R15B
	R16A_RELEASE_CANDIDATE R16B01 R16B02 R16B03-1 R16B03 R16B 17.0-rc1 17.0-rc2
	17.0 17.1 17.3 17.4 17.5 18.0 18.1 18.2.1 18.2 18.3 19.0 19.1 19.2 19.3
	20.0 20.1

Let's build the 20.1 version:

.. code:: sh

    # this will take a while
    kerl build 20.1 20.1

And install it:

.. code:: sh

   kerl install 20.1 ~/bin/erl-20.1

Now everytime we want to use this version of erlang we need to run:

.. code:: sh

    . $HOME/bin/erl-20.1/activate

Setting up rebar3
-----------------

Now we have erlang, we need a build tool, we are going to use `rebar3 <https://s3.amazonaws.com/rebar3/rebar3>`_:

.. code:: sh

    # download rebar3 to our bin directory
    wget https://s3.amazonaws.com/rebar3/rebar3 -O $HOME/bin/rebar3

    # set execution permissions for our user
    chmod u+x rebar3

Just in case you have problems running the rebar3 commands with a different
version, here's the version I'm using:

.. code:: sh

	rebar3 version

Output::

	rebar 3.4.7 on Erlang/OTP 20 Erts 9.1

Setting up our project
----------------------

We are ready to start our project, go to a folder where you keep your code and
if you haven't done it yet, add $HOME/bin to your path and activate erlang 20.1
as shown above, then run:

.. code:: sh

    rebar3 new release name=akvs

The output should be something like this::

    ===> Writing akvs/apps/akvs/src/akvs_app.erl
    ===> Writing akvs/apps/akvs/src/akvs_sup.erl
    ===> Writing akvs/apps/akvs/src/akvs.app.src
    ===> Writing akvs/rebar.config
    ===> Writing akvs/config/sys.config
    ===> Writing akvs/config/vm.args
    ===> Writing akvs/.gitignore
    ===> Writing akvs/LICENSE
    ===> Writing akvs/README.md

Let's see what each file does:

First of all, we created a release, which is a kind of project that can have
more than one application (a common way to structure a project is into applications
and libraries)

Under the apps folder are all the applications we mantain for this release,
in our case we only have one application, named akvc.

Under the akvs application folder we have a src folder where all the source code
for that application will live, we can add other folders there, for tests, header files, private files etc.

apps/akvs/src/akvs_app.erl

    The $APPNAME_app module is called when starting and stopping the app to do
    the setup and tear down of the application.

    Check Erlang's `manual for application <http://erlang.org/doc/man/application.html>`_ or the `user's guide entry for application <http://erlang.org/doc/design_principles/applications.html>`_ for more information.

apps/akvs/src/akvs_sup.erl

    The $APPNAME_sup module defines the root supervisor for the application, it
    implements the supervisor behavior and will be "hooked" into the supervisor
    hierarchy of this release when initialized.

    Check Erlang's `manual for supervisor <http://erlang.org/doc/man/supervisor.html>`_ or the `user's guide entry for supervisor <http://erlang.org/doc/design_principles/sup_princ.html>`_ for more information.

apps/akvs/src/akvs.app.src

    The $APPNANE.app.src is a file that contains metadata about this app.

    Check ERlang's `manual for application resource file <http://erlang.org/doc/man/app.html>`_ for more information.

rebar.config

    Contains information about the project, dependencies, how to build it,
    test it, and how to build a release.

    Check `rebar3 docs <http://www.rebar3.org/docs>`_ for details.

config/sys.config

    Configuration parameters for the application.

    Check `sys.config's manual page <http://erlang.org/doc/man/config.html>`_ for more information.

config/vm.args

    Configuration parameters for the Erlang VM.

.gitignore

    Git specific, files to ignore.

LICENSE

    The license for this project, you should change it if the Apache License 2.0
    isn't the one you want.

README.md

    Project's readme.

Starting it for the first time
-------------------------------

First we need to build a release:

.. code:: sh

    cd akvs

    # build a release, the result will be at _build/default/rel/akvs
    rebar3 release

    # start the release and attach to the console
    ./_build/default/rel/akvs/bin/akvs console

The output in my case is (redacted for clarity)::

	Exec: bin/erl-20.1/erts-9.1/bin/erlexec
		 -boot src/erl/akvs/_build/default/rel/akvs/releases/0.1.0/akvs
		 -mode embedded -boot_var ERTS_LIB_DIR bin/erl-20.1/lib
		 -config src/erl/akvs/_build/default/rel/akvs/releases/0.1.0/sys.config
		 -args_file src/erl/akvs/_build/default/rel/akvs/releases/0.1.0/vm.args
		 -pa -- console

	Root: src/erl/akvs/_build/default/rel/akvs
	src/erl/akvs/_build/default/rel/akvs

	Erlang/OTP 20 [erts-9.1] [source] [64-bit] [smp:4:4] [ds:4:4:10]
				  [async-threads:30] [kernel-poll:true]


	=PROGRESS REPORT==== 25-Nov-2017::22:28:34 ===
			  supervisor: {local,sasl_safe_sup}
				 started: [{pid,<0.225.0>},
						   {id,alarm_handler},
						   {mfargs,{alarm_handler,start_link,[]}},
						   {restart_type,permanent},
						   {shutdown,2000},
						   {child_type,worker}]

	=PROGRESS REPORT==== 25-Nov-2017::22:28:34 ===
			  supervisor: {local,sasl_sup}
				 started: [{pid,<0.224.0>},
						   {id,sasl_safe_sup},
						   {mfargs,
							   {supervisor,start_link,
								   [{local,sasl_safe_sup},sasl,safe]}},
						   {restart_type,permanent},
						   {shutdown,infinity},
						   {child_type,supervisor}]

	=PROGRESS REPORT==== 25-Nov-2017::22:28:34 ===
			  supervisor: {local,sasl_sup}
				 started: [{pid,<0.226.0>},
						   {id,release_handler},
						   {mfargs,{release_handler,start_link,[]}},
						   {restart_type,permanent},
						   {shutdown,2000},
						   {child_type,worker}]

	=PROGRESS REPORT==== 25-Nov-2017::22:28:34 ===
			 application: sasl
			  started_at: akvs@ganesha
	Eshell V9.1  (abort with ^G)
	(akvs@ganesha)1>

There's not much we can do with our project at this stage, so we will just stop
it and exit by running the `q().` function in the shell:

.. code:: erl

	(akvs@ganesha)1> q().
	ok

Coding (and testing) the Key Value store modules
------------------------------------------------

The way I usually code in erlang is to first build a stateless module that has
an init function that returns some state, all other functions expect that state
as first parameter, then those functions do something and return the state and
the result.

This modules are really easy to use in the shell and test.

This will be our first module, we will call it akvs_kv and it will have the
following API:

.. code:: erl

    %% types:

    -type error() :: {error, {atom(), iolist(), map()}}.
    -type key()   :: binary().
    -type value() :: any().

    % we don't want other modules to know/care about the internal structure of
    % the state type
    -opaque state() :: map().

    %% functions:

    %% @doc create a new instance of a key value store
    -spec new(map()) -> {ok, state()} | error().

    %% @doc dispose resources associated with a previously created kv store
    -spec dispose(state()) -> ok | error().

    %% @doc set a value for a key in a kv store
    -spec set(state(), key(), value()) -> {ok, state()} | error().

    %% @doc get a value for a key or an error if not found
    -spec get(state(), key()) -> {ok, value()} | error().

    %% @doc get a value for a key or a default value if not found
    -spec get(state(), key(), value()) -> {ok, value()} | error().

    %% @doc remove a value for a key, if not found do nothing
    -spec del(state(), key()) -> {ok, state()} | error().

Notice that to specify the API I used a specification of the types and functions,
this is called spec, read more about it at the `Types and Function Specifications sectio in the erlang reference manual <http://erlang.org/doc/reference_manual/typespec.html>`_.

Also for documentation comments I'm using the edoc format, read more about it at the `edoc user's guide section <http://erlang.org/doc/apps/edoc/chapter.html>`_.

You can see the full code of this module here: `akvs_kv <https://github.com/marianoguerra/akvs/blob/77188c096275aa1df9b519e55a08fd57fcfeedc3/apps/akvs/src/akvs_kv.erl>`_

But how do we know if it works?

At this point there are two ways: testing it in the shell, or writing tests for
it, let's do the right thing and write some tests.

We are going to use `Common Test <http://erlang.org/doc/apps/common_test/introduction.html>`_ for our tests.

First we need to create the test folder for our tests:

.. code:: sh

    mkdir apps/akvs/test

Inside it we will create a module called akvs_kv_SUITE that will contain the
tests for the akvs_kv module.

You can see the full code of this module here: `akvs_kv_SUITE <https://github.com/marianoguerra/akvs/blob/77188c096275aa1df9b519e55a08fd57fcfeedc3/apps/akvs/test/akvs_kv_SUITE.erl>`_

To run the tests:

.. code:: sh

    rebar3 ct

We can also use the type specs we defined to check our code using `dialyzer <http://erlang.org/doc/apps/dialyzer/dialyzer_chapter.html>`_:

.. code:: sh

    rebar3 dialyzer

Everything seems to be right, let's move on to the next step.

But before that, in case you want to generate API docs for our code taking advantage
of the edoc annotations, you can do so by running:

.. code:: sh

    rebar3 edoc

And opening apps/akvs/doc/index.html with a browser.

Wrapping the state
------------------

Stateless modules are a good start and are really easy to test and use, but we
don't want to pass the burden of threading the state to the users of our code,
also we want to centralize the state management so that more than one process
can call our module and see the state changes of other callers.

In this case we are using ETS to make it simpler but if our kv was backed by a
map, or if we had some kind of cache, then state management would become really
important to get right, otherwise the results seen by each caller would
diverge.

To manage the state of our module we are going to wrap it in a process, a `gen_server <http://erlang.org/doc/man/gen_server.html>`_ in this case.

The module will be called akvs_kv_s (_s for server, don't know if there's a
convention for it).

The module is a basic gen_server that exposes a couple functions to call
the kv API from the akvs_kv module, you can read the code here: `akvs_kv_s <https://github.com/marianoguerra/akvs/blob/82b10a423b587a52c890a56cbfc90d24ebe5e6f0/apps/akvs/src/akvs_kv_s.erl>`_.

We write tests for this module too, you can read the test's code here: `akvs_kv_s_SUITE <https://github.com/marianoguerra/akvs/blob/82b10a423b587a52c890a56cbfc90d24ebe5e6f0/apps/akvs/test/akvs_kv_s_SUITE.erl>`_.

Run the tests:

.. code:: sh

    rebar3 ct

An API for our key value stores
-------------------------------

Now we can spawn a key value store in a gen_server and apply operations to it,
but like with the stateless module, someone has to keep a reference to the
process and provide a nicer way to find and operate on our key value stores, if
it was only one it's easy to just start it as a registered process with a name
and send messages to it by it's name, but in our case, we want to provide
namespaces where each namespace holds a key value store of its own.

The abstract API or this module should be like this:

.. code:: erl

    -type ns() :: binary().
    -type key() :: akvs_kv:key().
    -type value() :: akvs_kv:value().
    -type error() :: akvs_kv:value().

    %% @doc set Key to Value in namespace Ns
    -spec set(ns(), key(), value()) -> ok | error().

    %% @doc get Key from namespace Ns
    -spec get(ns(), key()) -> {ok, value()} | error().

    %% @doc get Key from namespace Ns or DefaultValue if Key not found
    -spec get(ns(), key(), value()) -> {ok, value()} | error().

    %% @doc delete  Key in namespace Ns
    -spec del(ns(), key()) -> ok | error().

Right now we are going to solve the problem of who keeps the namespace to
process mapping really simple so we can continue, we are going to setup a
public ETS table at application startup and lookup the processes by namespace
there, if not found we are going to start the process and register it under
that namespace.

This solution is not recommendable at all but it will allow us to continue and
since the API doesn't know a thing about the way we register/lookup namespaces
we can explore different alternatives later.

You can view the source code for akvs module here: `akvs <https://github.com/marianoguerra/akvs/blob/c6a8c4ae5d28610f153d13515994f4456209c232/apps/akvs/src/akvs.erl>`_ and the tests here `akvs_SUITE <https://github.com/marianoguerra/akvs/blob/c6a8c4ae5d28610f153d13515994f4456209c232/apps/akvs/test/akvs_SUITE.erl>`_.

An HTTP API for our key value stores
------------------------------------

We are at the point where we can expose our APIs to the world, we are going to
do it by exposing a really basic HTTP API for it.

The API will look like this:

.. code:: http

    # set key in namespace to the binary value sent in body
    # return status: 201
    POST /kv/<namespace>/<key>
    <body>

    # get key in namespace
    # return status:
    #  200: if found
    #  404: if not found
    GET /kv/<namespace>/<key>

    # delete key from namespace
    # return status: 200
    DELETE /kv/<namespace>/<key>

To create an HTTP API we need an HTTP server, in this case we will use `Cowboy 2 <https://ninenines.eu/docs/en/cowboy/2.1/guide/>`_.

First we need to `add it as a dependency in our rebar.config file in the deps
section and in the release dependencies section <https://github.com/marianoguerra/akvs/commit/c6a8c4ae5d28610f153d13515994f4456209c232#diff-31d7a50c99c265ca2793c20961b60979L1>`_.

Then we need to `setup the routes in our application initialization code <https://github.com/marianoguerra/akvs/blob/c6a8c4ae5d28610f153d13515994f4456209c232/apps/akvs/src/akvs_app.erl#L30>`_.

We are going to have only one route and handler, we are going to use a basic
HTTP to keep it simple, you can read the handler's code here: `akvs_h_kv <https://github.com/marianoguerra/akvs/blob/c6a8c4ae5d28610f153d13515994f4456209c232/apps/akvs/src/akvs_h_kv.erl>`_.

Now we can test it by building a release, starting it and playing with the API using curl:

.. code:: sh

    rebar3 release
    _build/default/rel/akvs/bin/akvs console

In another shell:

.. code:: sh

    curl http://localhost:8080/kv/foo/bar
    Not Found

    curl -X POST http://localhost:8080/kv/foo/bar -d "hello world"
    Created

    curl http://localhost:8080/kv/foo/bar
    hello world

    curl -X DELETE http://localhost:8080/kv/foo/bar
    OK

    curl http://localhost:8080/kv/foo/bar
    Not Found

    curl -X PUT http://localhost:8080/kv/foo/bar -d "hello world"
    Method Not Allowed

Seems to work fine.

Now we can build a production release and try it:

.. code:: sh

    rebar3 as prod release
    cd _build/prod/rel
    tar -czf akvs.tar.gz akvs
    cd -
    mv _build/prod/rel/akvs.tar.gz /tmp
    cd /tmp
    tar -xzf akvs.tar.gz
    cd akvs
    ./bin/akvs start

The application is started, you can check it's running by pinging it:

.. code:: sh

    ./bin/akvs ping

In case you need, you can attach to it (you should exit with Ctrl+D, using q()
won't only detach your console but also stop the system!):

.. code:: sh

    ./bin/akvs attach

You can try it again:

.. code:: sh

    curl http://localhost:8080/kv/foo/bar
    curl -X POST http://localhost:8080/kv/foo/bar -d "hello world"
    curl http://localhost:8080/kv/foo/bar
    curl -X DELETE http://localhost:8080/kv/foo/bar
    curl http://localhost:8080/kv/foo/bar
    curl -X PUT http://localhost:8080/kv/foo/bar -d "hello world"

When you are finished, you can stop it:

.. code:: sh

    ./bin/akvs stop

Now you can upload akvs.tar.gz to any bare server and start akvs there, as long
as the operating system is similar (better if the same) as the one where you
built the release, this is because when building the release we bundle the
erlang runtime for simplicity, this assumes specific versions of libraries like
libssl which may not be available on the target system if it's too different.

Another way is to build the release without bundling the erlang runtime and
having it available on the target system, just make sure that the erlang
runtime in the target system has the same version you used to build it,
otherwise you may experience errors due to modules/functions not being
available or bytecode incompatibility if the target runtime is older than the
one used for the release.
