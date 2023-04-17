type Robot = (String, Int, Int)  

robot :: Robot -> (Robot -> t) -> t
robot botStats = \message -> message botStats

name :: Robot -> String
name (n,_,_) = n
attack :: Robot -> Int
attack (_,a,_) = a
hp :: Robot -> Int
hp (_,_,hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))

printRobot aRobot = aRobot (\(n,a,h) -> n ++
    " attack:" ++ show a ++
    " hp:"++ show h)

damage aRobot attackDamage = aRobot (\(n,a,h) ->
    robot (n,a,h-attackDamage))

fight aRobot defender = damage defender attack
    where attack = if getHP aRobot > 10
            then getAttack aRobot
            else 0

getHPs :: [(Robot -> Int) -> Int] -> [Int]
getHPs = map getHP

nRoundsFightSim fastRobot slowRobot rounds =
    if rounds  == 0
        then (fastRobot, slowRobot)
    else
        nRoundsFightSim (fight fastRobot slowRobot) (fight slowRobot fastRobot) (rounds - 1)