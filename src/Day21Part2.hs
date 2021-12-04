import           Data.List
import           Data.List.Split
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           System.Environment
import           System.IO

type Ingredient = String
type Allergen = String
type FoodMap = Map Allergen (Set Ingredient)

parseLine :: String -> ([Ingredient], [Allergen])
parseLine line =
  let [ingredientsStr, allergensStr] = splitOn " (contains " line
      ingredients                    = words ingredientsStr
      allergens                      = splitOn ", " (init allergensStr)
  in  (ingredients, allergens)

updateMap :: Allergen -> [Ingredient] -> FoodMap -> FoodMap
updateMap allergen ingredients currentMap =
  let setIngredients = Set.fromList ingredients
  in  Map.insertWith Set.intersection allergen setIngredients currentMap

mkMap :: [([Ingredient], [Allergen])] -> FoodMap -> FoodMap
mkMap [] currentMap = currentMap
mkMap ((ingredients, allergens) : rest) currentMap =
  let restMap      = mkMap rest currentMap
      addAllergens = foldr (`updateMap` ingredients) currentMap allergens
  in  Map.unionWith Set.intersection addAllergens restMap

getAllIngredients :: [([Ingredient], [Allergen])] -> [Ingredient]
getAllIngredients [] = []
getAllIngredients ((ingredients, allergens) : rest) = ingredients ++ getAllIngredients rest

getSafeIngredients :: [Ingredient] -> [Ingredient] -> [Ingredient]
getSafeIngredients allIngredients containAllergens =
  let difference = allIngredients \\ containAllergens
  in  if length allIngredients == length difference
        then difference
        else getSafeIngredients difference containAllergens

findSingleIngredients :: FoodMap -> FoodMap
findSingleIngredients map' =
  let assocs  = Map.assocs map'
      singles = map snd (filter (\(k, v) -> Set.size v == 1) assocs)
  in  if singles == Map.elems map' then map' else findSingleIngredients $ removeSingles singles map'

removeSingles :: [Set Ingredient] -> FoodMap -> FoodMap
removeSingles ingredients foodMap = foldl removeMapFunc foodMap ingredients
 where
  remove ingredient set = if Set.size set /= 1 then set Set.\\ ingredient else set
  removeMapFunc foodMap ingredient = Map.map (remove ingredient) foodMap

main :: IO ()
main = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  contents <- hGetContents inHandle
  let parsed            = map parseLine $ lines contents
  let map'              = mkMap parsed Map.empty
  let singleIngredients = Map.elems $ findSingleIngredients map'
  let lists             = map Set.toList singleIngredients
  putStrLn $ intercalate "," (concat lists)

