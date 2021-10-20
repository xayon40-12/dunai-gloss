# Dunai Gloss  

A gloss interface for Dunai where stream function (`MSF Identity a b`) are used to handle events and updates.  
This is inspired by how 'reflex-gloss' and the Yampa code in 'frp-zoo' are using only one global reactive network.

Monads are droped as they would never be fully executed because Gloss callback system would implies to bind the context indefinitly without an end (so without executing the monads).  

## How to use

The `Dunai.Gloss` reexports most useful modules that permits to use Dunai and Gloss.  
Everything should be put in one Dunai Stream function which is the reactive network.  
As Steam Functions arrows are used, they can store internal state and computations (with `accumulateWith` for instance) and thus no state (called `world` in gloss) needs to be passed as input and returned as output from the network.  
Therefor only the delta time and events are passed as input of the network (as a tuple `(dt,e) :: (Float, Maybe Event)`, and the picture to be rendered is returned as output.  
When events are performed the time delta is `0`, and when time update are performed the event is `Nothing`.  
In order to store the global time the following sample can be used:  
```haskell
proc (dt,e) -> do
  t <- sumS -< dt
  returnA -< Blank
```

## Examples  

Examples are located in the `examples/` folder.  
Each examples are stored in subfolders that the same name as the corresponding executable.  
To execute an example either use stack with `stack run <example name>` or cabal with `cabal run <example name>` where `<example name>` can be one of:  
- basic-circle  
- show-events  
- follow-circle  
