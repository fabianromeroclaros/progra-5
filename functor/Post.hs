module Post where

-- Definición de tipos de ejemplo
data Post = Post { postId :: Int, title :: String } deriving Show

-- Función para encontrar un post por su ID en un array de posts
findPost :: Int -> [Post] -> Maybe Post
findPost _ [] = Nothing
findPost id (post:posts)
    | postId post == id = Just post
    | otherwise = findPost id posts

-- Función para obtener el título de un post
getPostTitle :: Post -> String
getPostTitle post = title post

-- Array de posts de ejemplo
posts :: [Post]
posts = [Post 1 "Título del post 1", Post 2 "Título del post 2", Post 3 "Título del post 3"]

-- Utilizando fmap
ejemploFmap :: Maybe String
ejemploFmap = fmap getPostTitle (findPost 1 posts)

-- Utilizando <$>
ejemploInfix :: Maybe String
ejemploInfix = getPostTitle <$> (findPost 2 posts)

-- Ejecución y salida
main :: IO ()
main = do
    putStrLn "Ejemplo con fmap:"
    print ejemploFmap
    putStrLn "\nEjemplo con <$> (infix):"
    print ejemploInfix