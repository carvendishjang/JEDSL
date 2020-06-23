{- | An EDSL used to define and print JSON -}

import Data.List (intercalate)

----------------
-- JSON Model --
----------------

-- | The JASONValue data type represents a JSON Value
data JSONValue
    = JString String
    | JNumber Double
    | JBool Bool
    | JArray [JSONValue]
    | JObject [NamedValue]
    | JNull
    
-- | A named value
data NamedValue = NamedValue String JSONValue

---------------
-- utilities --
---------------

-- Utility functions for construction of a JSON (or the API)

-- | Create a string
str_ :: String -> JSONValue
str_ = JString

-- | Create a number
num_ :: Double -> JSONValue
num_ = JNumber

-- | The value `true`
true_ :: JSONValue
true_ = JBool True

-- | The value `false`
false_ :: JSONValue
false_ = JBool False

-- | The value `null`
null_ :: JSONValue
null_ = JNull

-- | A function for creating a JS array for a list of `JSONValue`s

arr_ :: [JSONValue] -> JSONValue
arr_ = JArray

-- | A function for creating a JS object for a list of `namedValue`s
obj_ :: [NamedValue] -> JSONValue
obj_ = JObject

-- | A utility function for field definition in an object
(.=) :: String -> JSONValue -> NamedValue
name .= value = NamedValue name value
-- this defines the associativity (in this case l - left) and the precedence (here, 7) of the operator .=
infixl 7 .=

---------------------------------------------
-- Generate a JSON string from a JSONValue --
---------------------------------------------

-- we are going to use the function `show` which can escape strings and convert Int to String

-- | Pretty print a JSON value
ppJSON :: JSONValue -> String
ppJSON jsval = case jsval of
    JString str -> 
        show str

    JNumber n -> 
        show n

    JBool True -> 
        "true"

    JBool False -> 
        "false"

    JNull ->
        "null"

    JArray arr -> 
        "["
         ++ intercalate ", " (map ppJSON arr) -- connect the elements of the string list with ", "
         ++ "]"                               -- e.g. intercalate ", " ["Alice", "Bob"] = "[Alice, Bob]"

    JObject elems ->
        "{"
         ++ intercalate ", " (map ppElement elems) -- similar to the above array
         ++ "}"

    where 
        -- | Pretty print a JSON object element, optionall place a comma at the end
        ppElement :: NamedValue -> String
        ppElement (NamedValue str jvalue) =
            show str ++ ": " ++ ppJSON jvalue 

----------
-- main --
----------

main :: IO()
main = do
    putStrLn (ppJSON langInfo)

langInfo :: JSONValue
langInfo = 
    obj_
    [ "language" .= str_ "Haskell"
    , "age" .= num_ 29
    , "fun" .= true_
    , "get_started" .= str_ "https://haskell.fpcomplete.com/get-started"
    , "learn" .= str_ "https://github.com/soupi/haskell-study-plan/"
    , "tools" .= obj_
      [ "hoogle" .= str_ "http://hoogle.haskell.org"
      , "ghcid" .= str_ "https://github.com/ndmitchell/ghcid"
      , "minimal-haskell-emacs" .= str_ "https://github.com/soupi/minimal-haskell-emacs"
      , "atom-haskell" .= str_ "https://atom.io/packages/atom-haskell"
      , "vscode_haskero" .= str_ "https://marketplace.visualstudio.com/items?itemName=Vans.haskero"
      ]
    , "libraries" .= arr_
      [ str_ "https://hackage.haskell.org/"
      , str_ "https://stackage.org/"
      ]
    ]