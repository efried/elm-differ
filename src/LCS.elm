module LCS exposing (lcs)

import Array exposing (Array)



-- Build the LCS DP table as a flat array


lcsTable : String -> String -> Array Int
lcsTable stringA stringB =
    let
        rows =
            String.length stringA

        cols =
            String.length stringB

        totalSize =
            (rows + 1) * (cols + 1)

        index : Int -> Int -> Int
        index row col =
            row * (cols + 1) + col

        getValue : Int -> Int -> Array Int -> Int
        getValue row col table =
            Array.get (index row col) table |> Maybe.withDefault 0

        updateCell : Int -> Int -> Array Int -> Array Int
        updateCell row col table =
            let
                charA =
                    String.slice (row - 1) row stringA

                charB =
                    String.slice (col - 1) col stringB

                newValue =
                    if charA == charB then
                        getValue (row - 1) (col - 1) table + 1

                    else
                        max (getValue (row - 1) col table) (getValue row (col - 1) table)
            in
            Array.set (index row col) newValue table

        initialTable =
            Array.repeat totalSize 0
    in
    List.range 1 rows
        |> List.foldl
            (\row tableSoFar ->
                List.range 1 cols
                    |> List.foldl (\col currentTable -> updateCell row col currentTable) tableSoFar
            )
            initialTable



-- Reconstruct the LCS string from the DP table


lcsReconstruct : String -> String -> Array Int -> String
lcsReconstruct stringA stringB table =
    let
        rows =
            String.length stringA

        cols =
            String.length stringB

        index : Int -> Int -> Int
        index row col =
            row * (cols + 1) + col

        getValue : Int -> Int -> Int
        getValue row col =
            Array.get (index row col) table |> Maybe.withDefault 0

        backtrack : Int -> Int -> List String -> List String
        backtrack row col acc =
            if row == 0 || col == 0 then
                acc

            else
                let
                    charA =
                        String.slice (row - 1) row stringA

                    charB =
                        String.slice (col - 1) col stringB
                in
                if charA == charB then
                    backtrack (row - 1) (col - 1) (charA :: acc)

                else if getValue (row - 1) col >= getValue row (col - 1) then
                    backtrack (row - 1) col acc

                else
                    backtrack row (col - 1) acc
    in
    backtrack rows cols []
        |> String.concat


lcs : String -> String -> String
lcs stringA stringB =
    let
        table =
            lcsTable stringA stringB
    in
    lcsReconstruct stringA stringB table
