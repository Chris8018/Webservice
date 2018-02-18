# haskell-webservice

This project contains a web service written in Haskell using Happstack and SQLite.
The web service provides an interface to (fictitious, randomly generated) weather 
readings taken at North Pole during 2017. It has one endpoint, `weather/date/`,  
the response to which will depend on the request type.

If the webservice is running on `localhost` on port 8000 and receives a `GET` or
`POST` request to `http://localhost:8000/weather/date/YYYY-mm-dd` then it will
return a JSON object with the data for that date. If weather data is held for that
date, the response code will be 200 and the body will consist of JSON something like 
this:

    [{"date":"2017-01-01","temperature":-23.164497}"]

If no data exists for the date in question, the response code will be 404 and the body 
will consist of an empty JSON object.

We can also make `PUT` requests to insert or update a date/temperature pair in the database.
In this case the URL needs to include the temperature, e.g.

    http://localhost:8000/date/1970-01-01/0

If the date already exists, the temperature is updated. If not, a new record is added to the 
database. This call will return 200 and an empty JSON object if everything went OK.

To set the project up you need to start by building the code and populating the database:

    $ cabal sandbox init
	$ cabal configure
	$ cabal install
	$ runhaskell src/DB/Main.hs
	$ .cabal-sandbox/bin/haskell-webservice 
    Listening for http:// on port 8000

The step to build the datbase only needs to be done once. Once the service is running you 
can interact with it in the browser or using something like `curl`:

    $ curl http://localhost:8000/weather/date/2017-01-01
	[{"date":"2017-01-01","temperature":-23.164497}]
	
And you can run the tests using `cabal test`.

## Assignment

Your assignment is to extend the webservice in various ways. Before doing so you should read 
the first two chapters of the [HappStack Book](http://happstack.com/docs/crashcourse/index.html). 
You may also need to check the docs for [HappStack](https://hackage.haskell.org/package/happstack-server)
and [sqlite-simple](https://hackage.haskell.org/package/sqlite-simple-0.4.14.0/docs/Database-SQLite-Simple.html), 
the library we are using to interact with the database.

1. Add a new endpoint to the webservice, `weather/range/d1/d2`, where `d1` and `d2` are dates in the format 
`YYYY-mm-dd`. When this endpoint receives a `GET` request it should return all records in the database that
fall between `d1` and `d2` (i.e. greater than or equal to `d1` and less than or equal to `d2`) as an array of 
JSON objects. Other request methods should result in a 405 response code ("Method not allowed") and an empty
JSON object.

2. Add a new endpoint to the webservice, `weather/max/d1/d2`, where `d1` and `d2` are dates in the format 
`YYYY-mm-dd`. When this endpoint receives a `GET` request it should return the details of the day with the
maximum temperature between `d1` and `d2` (i.e. greater than or equal to `d1` and less than or equal to `d2`) 
as an array containing a single JSON. Other request methods should result in a 405 response code ("Method not allowed") 
and an empty JSON object.

3. Add a new endpoint to the webservice, `weather/above/t`, where `t` is a signed floating point number. 
When this endpoint receives a `GET` request it should return all records in the database where the
temperature is greater than or equal to `t` as an array of JSON objects. Other request methods should result 
in a 405 response code ("Method not allowed") and an empty JSON object.

4. **Extension** Convert the web service to use the `web-routes` library as described in 
[chapter 7 of the HappStack Book](http://happstack.com/docs/crashcourse/WebRoutes.html#web-routes).

## Submission instructions

Make a github account if you don't have one already. Clone this repository and save your work there.
To submit your work, submit the address of your repository on studentcentral. No other form of submission
is accepted.

## Marking criteria

20 marks are available for each of the four criteria, with 20 marks available for the elegance, efficiency
and readability of the code. Read one of the Haskell style guides linked to from studentcentral and use
the `hlint` tool to improve your mark in this criterion. These marking criteria are indicative, meaning that
I may award extra marks for work that uses an interesting approach and shows independent learning. 