# Dunai Gloss  

A gloss interface for Dunai where stream function (MSF Identity a b) are used to handle events and updates.  

Monads are droped as they would never be fully executed because Gloss callback system would implies to bind the context indefinitly without an end (so without executing the monads).  

## Examples  

Examples are located in the `examples/` folder.  
Each examples are stored in subfolders that the same name as the corresponding executable.  
To execute an example either use stack with `stack run <example name>` or cabal with `cabal run <example name>` where `<example name>` can be one of:  
- basic-circle  
- show-events  
- follow-circle  

The examples are so basic for now that they simply use Dunai as if it was classical function.
