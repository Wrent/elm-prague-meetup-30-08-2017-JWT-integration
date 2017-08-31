# Common authentication component

## Compiling the component

The component has to be compiled before use. To do so, run 
`elm-make src/CommonAuthenticationComponent.elm --output=main.js`.

## Embedding of component into an Elm application

If you want to embed the app directly to Elm, it is quite complicated
(at least until this [Elm issue](https://github.com/elm-lang/elm-package/issues/87) is resolved.)

1. Copy the `authentication` folder into you application, e.g. to 
`yourApp/src/lib/authentication`
2. Add the location of this folder to your `elm-package.json` under 
`source-directories`
3. Add all dependencies from the component's [elm-package.json](elm-package.json)
to your `elm-package.json` (of course excluding the duplicates).
4. Install the packages (you might need to switch to using [elm-install 
package manager](https://github.com/gdotdesign/elm-github-install)).
5. Add flags TBD
6. Add component to your model, initialize it, add message, set up
msg routing

## Embedding of component into a JavaScript application

The component can be embedded into any application using 
simple Javascript. You can look at the [example](usage_example.html).

## Styling the component

The elements rendered by the component have the following classes:

  - **Name of logged user / information about logging in**: `auth-component-logged-user`
  
## Obtaining token of logged in user

The token is saved to the browser Session storage under the key `jwttoken`.

If you need to access the storage from an Elm application, 
take a look into `gdotdesign/elm-storage` package, which is also used by
this component.