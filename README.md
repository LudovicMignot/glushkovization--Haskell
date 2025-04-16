# glushkovization

This project is related to the study of a conversion method from NFA to regular expression based on the Caron and Ziadi characterization of Glushkov automata, i.e. the NFAs computed from the Glushkov method.

The strong stabilization is the first step toward the computation, from any NFA, of an equivalent Glushkov NFA.

This project is an implementation of the differzent algorithms described in a forthcoming paper.
It includes a web app made ith Reflex and compiled with the GHC WASM Backend to produce a js application.

## Compilation

### Using cabal

`cabal build` can be used to compile all the elements of the project, except the one using marshal JS values (GHC.WASM.Prim module).
It can be useful to link infos to Haskell Language Server for example, to use advanced features in VS Code.

### Using GHC Backend
A script file (build.sh) can be launched `./build.sh` with no optimization, `./build.sh -O4` to build with optimization.

## Test

Random property tests defined using Quickcheck can be launched via `cabal test`.

## Web App

After compilation with `build.sh`, the resulting web app is copied in the frontend folder.
Launch `index.html`in a web browser to see the application:

* A random NFA is computed at launch.
* The **Commands** panel allows the user to homogenize, standardize and trim NFA.
* The **Construction** panel allows the user to switch initiality and finality of states, and existence of transitions.
To perform these operations, the NFA has to be defined over integer states.
If it is not (e.g., after homogenization or standardization), it can be renumeroted using the **State Renumerotation** button in the top of the application.
This panel also allows the computation of a random NFA. 
* The **Orbital operations** panel allows the computation, for integer state based trim, standard and homogeneous NFAs, of
  * outgate isolation by selecting an outgate
  * ingate isolation by selecting the chosen ingate
  * orbital nfa by selecting a state of the chosen orbit
  * substitutes a stable orbit by an equivalent strongly stable one, selecting a state of the orbit
* The **Informations** panel contains info about the properties of the NFA (e.g., homogeneity or stability)
* The **Orbital Informations** panel contains info about the proserties of the orbits(e.g., isolation or stability).
