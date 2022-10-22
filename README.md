# Fae - The Fuzzy Analysis Engine

Fae is constituted by a fuzzy-set library, and a engine + query language which leverage that library in order to allow the user to make declarative tabular databases and then run analysis on the database in real time via a REPL.

Here are the defining features:
- Tables are simply titled fuzzy-membership functions (concepts)
- Evaluation of a table means to apply the function over the domain 
- Fuzzy-set logic is used to analyse the database using a simple DSL
- New tables (concepts) can be innovated by combining tables functionally (this is done by currying) E.G., the fuzzy-intersection of hotness wetness may be used as the concept for 'humidity'.

TODO:
- Compilation and parsing of scripts on startup
- JSON loading + reading of domain data (currently supports only lisp files)
- Cascading
- Error handling (let's leverage the excellent CL condition system please!)

## License

MIT License

