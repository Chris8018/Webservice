# Haskell-Webservice

## Notes and Instuctions

** Please read Jim's note in oldREADME.md before this if you are new

This project contains a web service written in Haskell using Happstack and SQLite.
The web service provides an interface to (fictitious, randomly generated) weather 
readings taken at the North Pole during 2017.

** Home Page (GET, POST): http://localhost:8000/weather/
Show some messages and instructions.

** Single date query (GET): weather/date/YYYY-mm-dd
Return JSON file if weather data for that date is held.

** Insert/Update date query (PUT):
Insert or update a date/temperature pair in the database.
*  webservice:

    $ curl -X PUT weather/put/YYYY-mm-dd/temperature

*  oldwebservice:

    $ curl -X PUT weather/date/YYYY-mm-dd/temperature

** Range dates query (GET): weather/range/d1/d2
**                   where: d1  = YYYY-mm-dd
**                          d2  = YYYY-mm-dd
**                          d1  != d2
Return JSON file if weather data for dates between date d1 and d2 are held.

** Max dates query (GET): weather/max/d1/d2
**                 where: d1  = YYYY-mm-dd
**                        d2  = YYYY-mm-dd
**                        d1  != d2
Return JSON file if weather data for dates between date d1 and d2 with highest temperature are held.

** Above temperature query (GET): weather/above/t
**                         where: t is a real number
Return JSON file if weather data for dates that have temperature higher than t.

** Return 405 Method not allowed and an empty JSON object on wrong request methods.

## Installation

** If you are using Windows, you should install CygWin and use the CygWin terminal instead
**of the Windows command prompt, as several of the packages you need require a bash-like environment.

** If you are working on this code in the labs, do not install it on a Windows drive
**(e.g. `~/W_DRIVE` or `~/M_DRIVE`), as cabal is known to have problems with Windows network shares.

** Check for update:

    $ cabal update
    $ cabal install cabal-install
	
** Clone project

    $ git clone https://github.com/ChrisTran96/Haskell-Webservice.git
    $ cd haskell-webservice

** Create sandbox (optional)

    $ cabal sandbox init

** Configure and install missing package

    $ cabal configure

** Build and install

	  $ cabal build
	  $ cabal install

** Run DB-setup

	  $ cabal run DB-setup

**Running the weather service
*Run webservice

    $ cabal run webservice

*Run old webservice

    $ cabal run oldwebservice

**Try test (work on both webservice and oldwebservice)
While THE SERIVICE is RUNNING open a new terminal and run the tests:

    $ cabal test

A log can be found at:
dist/test/haskell-webservice-0.1.0.0-test-webservice.log

