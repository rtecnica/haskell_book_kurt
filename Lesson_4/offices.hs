sfOffice name =
  if lastName < "L"
    then
      nameText
        ++ " - PO Box 1234 - San Francisco, CA, 94111"
    else
      nameText
        ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where
    lastName = snd name
    nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where
    nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where
    nameText = snd name

-- Q4.1
compareLastNames name1 name2 = case compare (snd name1) (snd name2) of
  GT -> GT
  LT -> LT
  EQ -> compare (fst name1) (fst name2)

-- Q4.2

dcOffice name = nameText ++ "Esq"
  where
    nameText = snd name

getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "dc" -> dcOffice
  _ -> (\name -> (fst name) ++ " " ++ (snd name))
