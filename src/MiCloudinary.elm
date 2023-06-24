module MiCloudinary exposing (url)

url : String -> String -> String
url transforms asset =
    let
        base =
            "https://res.cloudinary.com/rolojf/image/upload"
    in
    (base ++ "/" ++ transforms ++ "/" ++ asset)
