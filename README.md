# Glushkovization

This project focuses on the study and implementation of a conversion method from Non-deterministic Finite Automata (NFA) to regular expressions, based on the Caron and Ziadi characterization of Glushkov automata. These automata are isomorphic to the ones obtained from the Glushkov method.

The first step in this process involves strong stabilization, a structural property of Glushkov NFAs.

This repository provides an implementation of various algorithms described in a forthcoming paper. Additionally, it includes a web application built with Reflex and compiled using the GHC WASM Backend to produce a JavaScript application.

## Features

- **Algorithm Implementation**: Implements algorithms for trim, standardize, homogenize and stabilize NFAs.
- **Web Application**: Interactive web app for visualizing and manipulating NFAs.
- **Randomized Testing**: Property-based tests using QuickCheck.

## Compilation

### Using Cabal

Run `cabal build` to compile most parts of the project, except components relying on marshaling JavaScript values (i.e. elements using the `GHC.WASM.Prim` module). This can be useful for integrating with tools like Haskell Language Server for enhanced development features in editors like VS Code.

### Using GHC WASM Backend

Use the provided script `build.sh` to compile the project:
- Run `./build.sh` for a basic build.
- Run `./build.sh -O4` for an optimized build.

## Testing

Random property-based tests are defined using QuickCheck. Run the tests with:

```bash
cabal test
```

## Web Application

After building the project with `build.sh`, the resulting web application is copied to the `frontend` folder. Open `index.html` in a web browser to use the application.

### Web App Features

- **Random NFA Generation**: A random NFA is generated at launch.
- **Commands Panel**: Perform operations like homogenization, standardization, and trimming of NFAs.
- **Construction Panel**: Modify the NFA by:
  - Switching initiality and finality of states.
  - Adding or removing transitions.
  - Generating a new random NFA.
Renumbering states can be required after homogenization or standardization, using the **State Renumbering** button, to perform some of the first two operations above.
- **Orbital Operations Panel**: For integer state-based, trimmed, standardized, and homogeneous NFAs:
  - Isolate outgates or ingates.
  - Compute orbital NFAs.
  - Replace a stable orbit with an equivalent strongly stable one.
- **Information Panel**: Displays properties of the NFA, such as homogeneity and stability.
- **Orbital Information Panel**: Provides details about the properties of orbits, such as isolation and stability.

