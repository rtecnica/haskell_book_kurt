-- QC12.1
type FirstName = String

type MiddleName = String

type LastName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName

data Sex = M | F

data ABOType = A | B | AB | O

data RhType = POS | NEG

data BloodType = BloodType ABOType RhType

data Patient = Patient
  { name :: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
  }

bloodTypeCompatibility :: BloodType -> BloodType -> Bool
bloodTypeCompatibility (BloodType O _) _ = True
bloodTypeCompatibility _ (BloodType AB _) = True
bloodTypeCompatibility (BloodType A _) (BloodType A _) = True
bloodTypeCompatibility (BloodType B _) (BloodType B _) = True
bloodTypeCompatibility _ _ = False -- otherwise

-- Q12.1
canDonateTo :: Patient -> Patient -> Bool
canDonateTo donor receipient =
  bloodTypeCompatibility (bloodType donor) (bloodType receipient)

-- Q12.2
