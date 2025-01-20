import Data.List qualified as L
import Data.Map qualified as Map
import Data.Maybe (isNothing)

type Name = String

type PhoneNumber = String

-- type PhoneBook = [(Name, PhoneNumber)]
type PhoneBook = AssociationList Name PhoneNumber

phoneBook :: PhoneBook
phoneBook =
  [ ("jack", "323-3322"),
    ("jill", "983-4822"),
    ("christine", "993-2210"),
    ("charlie", "999-3243")
  ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name phoneNo pbook = (name, phoneNo) `elem` pbook

-- lookUp :: (Eq k) => k -> [(k, v)] -> Maybe (k, v)
lookUp :: (Eq k) => k -> AssociationList k v -> Maybe v
lookUp key dict =
  case value of
    Nothing -> Nothing
    Just (_, v) -> Just v
  where
    value = L.find (\(k, _) -> k == key) dict

type AssociationList k v = [(k, v)]

-- type IntMap v = Map Int v
type IntMap = Map.Map Int

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist."
    Just (state, code) ->
      if state /= Taken
        then Right code
        else Left $ "Locker " ++ show lockerNumber ++ " is taken."

lockers :: LockerMap
lockers =
  Map.fromList
    [ (100, (Taken, "ZD39I")),
      (101, (Free, "JAH3I")),
      (103, (Free, "IQSA9")),
      (105, (Free, "QOTSA")),
      (109, (Taken, "893JJ")),
      (110, (Taken, "99292"))
    ]
