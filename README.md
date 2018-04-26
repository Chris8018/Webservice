# haskell-webservice

This project contains a web service written in Haskell using Happstack and SQLite.
The web service provides an interface to (fictitious, randomly generated) weather 
readings taken at the North Pole during 2017. It has 4 endpoints,
the response to which will depend on the request type.

If the webservice is running on `localhost` on port 8000 and receives a `GET` or
`POST` request to `http://localhost:8000/weather/date/YYYY-mm-dd` then it will
return a JSON object with the data for that date. If weather data is held for that
date, the response code will be 200 and the body will consist of JSON something like 
this:

    [{"date":"2017-01-01","temperature":-23.164497}]

If no data exists for the date in question, the response code will be 404 and the body 
will consist of an empty JSON object.

We can also make `PUT` requests to insert or update a date/temperature pair in the database.
In this case the URL needs to include the temperature, e.g.

    http://localhost:8000/date/1970-01-01/0

If the date already exists, the temperature is updated. If not, a new record is added to the 
database. This call will return 200 and an empty JSON object if everything went OK.

## Installation

**If you are using Windows, you should install CygWin and use the CygWin terminal instead 
of the Windows command prompt, as several of the packages you need require a bash-like environment.**

**If you are working on this code in the labs, do not install it on a Windows drive
(e.g. `~/W_DRIVE` or `~/M_DRIVE`), as cabal is known to have problems with Windows network shares.**

To set the project up you need to start by making sure that you have the latest version of
`cabal-install` on this machine:

    $ cabal update
    $ cabal install cabal-install
	
Now you can grab the code and start building it:

    $ git clone https://github.com/jimburton/haskell-webservice
    $ cd haskell-webservice
    $ cabal sandbox init
    $ cabal configure
	
This last step may report that several libraries are missing. For example:

    $ cabal configure
    Resolving dependencies...
    Configuring haskell-webservice-0.1.0.0...
    cabal: At least the following dependencies are missing:
    aeson -any,
    happstack-server -any,
    hslogger -any,
    sqlite-simple -any

If so, install them now:

	$ cabal install aeson happstack-server hslogger sqlite-simple

Now you should be able to continue building the application:

    $ cabal configure
	$ cabal build
	$ cabal install
	$ cabal run DB-setup

The step to build the database only needs to be done once but you can
run it any time you want to restore the database to its original
state. Now you can start the webservice:

    $ cabal run webservice 
    Listening for http:// on port 8000


Once the service is running you can interact with it in the
browser or using something like `curl`:

    $ curl http://localhost:8000/weather/date/2017-01-01
	[{"date":"2017-01-01","temperature":-23.164497}]
	
While the service is running open a new terminal and run the tests with `cabal test`. 
Read the log, which will be in a file called something like 
`dist/test/haskell-webservice-0.1.0.0-test-webservice.log`.

