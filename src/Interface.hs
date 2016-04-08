module Interface where

class HasDatabaseModel model where
  toDbModel   :: model -> dbModel
  fromDbModel :: dbModel -> model
