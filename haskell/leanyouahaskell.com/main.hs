import Shapes
import Person

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
        let person = Person { firstName="Mirei"
                            , lastName ="Hayasaka"
                            , age=14
                            , height=147
                            , phoneNumber="883-3010-0000"
                            , flavor="Puppets"
                            }

        print person
