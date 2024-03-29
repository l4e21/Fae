# Fae - The Fuzzy Analysis Engine

Fae is constituted by a fuzzy-set library, and a engine + query language which leverage that library in order to allow the user to make declarative tabular databases and then run fuzzy analysis on the database in real time via a REPL.

## Defining Features
- Tables are simply titled fuzzy-membership functions (concepts)
- Evaluation of a table means to apply the function over the domain 
- Fuzzy-set logic is used to analyse the database using a simple DSL
- New tables (concepts) can be innovated by combining tables functionally (this is done by currying) E.G., the fuzzy-intersection of hotness wetness may be used as the concept for 'humidity'.

## TODO
- Compilation and parsing of scripts on startup
- JSON loading + reading of domain data (currently supports only lisp files)
- Cascading
- Error handling (let's leverage the excellent CL condition system please!)
- Data simulations
- Bitemporal domain handling

## Documentation
`(init!)` from a REPL to hook into Fae. You'll begin with an empty domain and only one table, `t`, which is crisp & true. Entities are maps/plists

`(load filename)` load domain data from a lisp file 

`(save filename)` write domain data to a file

`(rule name body)` is used to define new rules via combinations of tables

`(table name body)` is used to define a new table programmatically (allowing you to break into Lisp to define a membership function directly).

`(attr keyword)` makes a crisp membership function for an attribute of an entity (accessed by the relevant symbol)

`(tables)` displays tables

`(cut r)` and `(s-cut r)` are the alpha-cut and strong-alpha-cut of an evaluated result

`(eval fn)` is used to evaluate tables over the domain

`(delete table/keyword)` deletes from the domain via applying a table and deleting those resultt or deleting via an attribute keyword

`(mutate table/keyword)` mutates the domain via applying a table and keeping the results or by an attribute keyword

`(undo)` and `(redo)` do what they sound like

## Learn More
Fuzzy Set Theory Lectures By Prof S Chakraverty  https://www.youtube.com/watch?v=oWqXwCEfY78&list=PLjZ9ULh2Ff9FWvv-DiZHP6RN0kMr_4aW4

"BayesDB: Query the Probable Implications of Data" by Richard Tibbetts  https://www.youtube.com/watch?v=7_m7JCLKmTY

## License

MIT License

