import Data.List (minimumBy, maximumBy, subsequences)
import Data.Ord (comparing)

data Stats = S Int Int Int deriving (Show)
data Item = I Int Int Int deriving (Show)

playerHP = 100

weapons =
    [ I 8 4 0
    , I 10 5 0
    , I 25 6 0
    , I 40 7 0
    , I 74 8 0 ]

armor =
    [ I 0 0 0
    , I 13 0 1
    , I 31 0 2
    , I 53 0 3
    , I 75 0 4
    , I 102 0 5 ]

rings =
    [ I 0 0 0
    , I 25 1 0
    , I 50 2 0
    , I 100 3 0
    , I 20 0 1
    , I 40 0 2
    , I 80 0 3 ]

main = do
    [hp, dmg, armor] <- map (read . last . words) . lines <$> readFile "inputs/21.txt"
    let bossStats = S hp dmg armor

    print $ fst $ minimumBy (comparing fst) $ filter snd $ map (`simulateFight` bossStats) generateItemCombinations
    print $ fst $ maximumBy (comparing fst) $ filter (not . snd) $ map (`simulateFight` bossStats) generateItemCombinations

simulateFight :: Item -> Stats -> (Int, Bool)
simulateFight (I c d a) s = (c, fight (S playerHP d a) s)
    where
        fight :: Stats -> Stats -> Bool
        fight (S playerHp playerDamage playerArmor) (S bossHp bossDamage bossArmor)
            | nextBossHp <= 0 = True
            | nextPlayerHp <= 0 = False
            | otherwise = fight (S nextPlayerHp playerDamage playerArmor) (S nextBossHp bossDamage bossArmor)
            where nextBossHp = bossHp - max 1 (playerDamage - bossArmor)
                  nextPlayerHp = playerHp - max 1 (bossDamage - playerArmor)

generateItemCombinations :: [Item]
generateItemCombinations = [ mergeItems w a r | w <- weapons, a <- armor, r <- combinations 2 rings]

mergeItems :: Item -> Item -> [Item] -> Item
mergeItems (I wc wd wa) (I ac ad aa) r = I (wc + ac + rc) (wd + ad + rd) (wa + aa + ra)
    where (I rc rd ra) = foldl (\(I r1c r1d r1a) (I r2c r2d r2a) -> I (r1c + r2c) (r1d + r2d) (r1a + r2a)) (I 0 0 0) r

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n - 1) xs) ++ combinations n xs
