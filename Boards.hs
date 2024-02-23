module Boards where
import Sudoku ( Board )

-- valid blank board
bdBlank :: Board
bdBlank = replicate 9 (replicate 9 '.') 

-- valid novice board
bdNovice :: Board
bdNovice = [
    "..78....2",
    "......84.",
    "8.91..67.",
    ".6.7....4",
    "475.9.163",
    "2....4.8.",
    ".81..53.9",
    ".24......",
    "9....74.."
    ]

-- novice board solution
bdNoviceSoln :: Board
bdNoviceSoln = [
    "517846932",
    "632579841",
    "849132675",
    "168753294",
    "475298163",
    "293614587",
    "781465329",
    "324981756",
    "956327418"
    ] 

-- valid easy board
bdEasy :: Board
bdEasy = [
    "2.5..7..6",
    "4..96..2.",
    "....8..45",
    "98..74...",
    "57.8.2.69",
    "...63..57",
    "75..2....",
    ".6..51..2",
    "3..4..5.8"
    ]

-- easy board solution
bdEasySoln :: Board
bdEasySoln = [
    "235147986",
    "418965723",
    "697283145",
    "986574231",
    "573812469",
    "142639857",
    "759328614",
    "864751392",
    "321496578"
    ]

-- valid moderate board
bdModerate :: Board
bdModerate = [
    "......2.8",
    "92...4...",
    "...2.8.71",
    ".36......",
    "...7.9...",
    "......64.",
    "86.4.1...",
    "...9...27",
    "2.9......"
    ]

-- moderate board solution
bdModerateSoln :: Board
bdModerateSoln = [
    "471635298",
    "928174536",
    "653298471",
    "136542789",
    "584769312",
    "792813645",
    "867421953",
    "345986127",
    "219357864"
    ]

-- valid hard board
bdHard :: Board
bdHard = [
    ".........",
    "8...2...5",
    ".....624.",
    ".38..71..",
    "2.4...3.9",
    "..74..52.",
    ".725.....",
    "6...8...1",
    "........."
    ]

-- hard board solution
bdHardSoln :: Board
bdHardSoln = [
    "426935817",
    "813724695",
    "795816243",
    "938257164",
    "254168379",
    "167493528",
    "372541986",
    "649382751",
    "581679432"
    ]

-- full board all columns and boxes invalid
bd1 :: Board
bd1 = replicate 9 "123456789"

bd2 :: Board
bd2 = ['1'..'9'] : replicate 8 (replicate 9 '.')

-- valid board with one blank value
bd3 :: Board
bd3 = [
    "517846932",
    "632579841",
    "849132675",
    "168753294",
    "475298163",
    "293614587",
    "7814653.9",
    "324981756",
    "956327418"
    ] 

-- invalid board (row 6)
bd4 :: Board
bd4 = [
    "517846932",
    "632579841",
    "849132675",
    "168753294",
    "475298163",
    "293614587",
    "781465349",
    "324981756",
    "956327418"
    ] 

-- valid board with no solution
bd5 :: Board
bd5 = [
    "12345678.",
    "........8",
    "........7",
    "........6",
    "........5",
    "........4",
    "........3",
    "........2",
    "........1"
    ] 
    
