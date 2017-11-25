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

so, first we need to fetch kerl:

.. code:: sh

    # create bin folder in our home directory if it's not already there
    mkdir -p ~/bin

    # cd to it
    cd ~/bin

    # download kerl script
    curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl

    # set execution permitions for our user
    chmod u+x kerl

you will need to add ~/bin to your PATH variable so your shell can find the
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

after this, start a new shell or source your rc file so that it picks up your
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

under the apps folder are all the applications we mantain for this release,
in our case we only have one application, named akvc.

under the akvs application folder we have a src folder where all the source code
for that application will live, we can add other folders there, for tests, header files, private files etc.

apps/akvs/src/akvs_app.erl

    The $APPNAME_app module is called when starting and stopping the app to do
    the setup and tear down of the application

    Check Erlang's `manual for application <http://erlang.org/doc/man/application.html>`_ or the `user's guide entry for application <http://erlang.org/doc/design_principles/applications.html>`_ for more information

apps/akvs/src/akvs_sup.erl

    The $APPNAME_sup module defines the root supervisor for the application, it
    implements the supervisor behavior and will be "hooked" into the supervisor
    hierarchy of this release when initialized.

    Check Erlang's `manual for supervisor <http://erlang.org/doc/man/supervisor.html>`_ or the `user's guide entry for supervisor <http://erlang.org/doc/design_principles/sup_princ.html>`_ for more information

apps/akvs/src/akvs.app.src

    The $APPNANE.app.src is a file that contains metadata about this app

    Check ERlang's `manual for application resource file <http://erlang.org/doc/man/app.html>`_ for more information

rebar.config

    Contains information about the project, dependencies, how to build it,
    test it, and how to build a release.
    
    Check `rebar3 docs <http://www.rebar3.org/docs>`_ for details

config/sys.config

    Configuration parameters for the application.
    
    Check `sys.config's manual page <http://erlang.org/doc/man/config.html>`_ for more information

config/vm.args

    Configuration parameters for the Erlang VM.

.gitignore

    Git specific, files to ignore

LICENSE

    The license for this project, you should change it if the Apache License 2.0
    isn't the one you want

README.md

    Project's readme

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


