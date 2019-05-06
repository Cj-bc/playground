import Shapes
import Person
import Locker
import MyList
import Tree
import TrafficLight
import YesNo

main = do
        -- Shapes
        let rec = Rectangle (Point 0 0) (Point 10 10)
            cir = Circle (Point 0 0) 10.0
        putStrLn $ "surface of " ++ (show rec) ++ " | " ++ (show cir)
        print $ surface rec
        print $ surface cir
        putStrLn $ "nudge them by (10, 10)"
        print $ nudge rec 10 10
        print $ nudge cir 10 10

        -- Person
        putStrLn "=========================================="
        let person = Person { firstName="Mirei"
                            , lastName ="Hayasaka"
                            , age=14
                            , height=147
                            , phoneNumber="883-3010-0000"
                            , flavor="Puppets"
                            }

        print person

        -- Locker
        putStrLn "=========================================="
        putStrLn "Current Lockers:"
        print lockers
        putStrLn "Knock! Can I use Locker num.102?"
        print $ lockerLookup 102 lockers
        putStrLn "Knock! How about num.105?"
        print $ lockerLookup 105 lockers


        -- MyList
        putStrLn "=========================================="
        putStrLn "List [1,5,3,8,9]"
        print (1 :-: (5 :-: (3 :-: (8 :-:  (9 :-: EmptyList)))))


        -- Tree
        putStrLn "=========================================="
        putStrLn $ "Tree of " ++ show [1,4,2,9,8]
        print numTree
        putStrLn $ "Is '5' member of numTree?"
        print $ 5 `treeElem` numTree
        putStrLn $ "Is '1' member of numTree?"
        print $ 1 `treeElem` numTree


        -- TrafficLight
        putStrLn "=========================================="
        putStrLn $ "Red == Red:"
        print $ Red == Red
        putStrLn $ "Red == Yellow:"
        print $ Red == Yellow
        putStrLn $ "show Red:"
        print Red


        -- YesNo
        putStrLn "=========================================="
        putStrLn "[]:"
        print $ yesno []
        putStrLn "\"hoge\":"
        print $ yesno "hoge"
