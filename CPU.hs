module CPU where 

{----- A simple CPU simulator --}

{- Identifiers for our registers -}

data RegName = R1 
             | R2
             | R3
             | R4
             | R5
               deriving (Show, Eq) 


data Instructions = MOVI RegName Int 
                  | MOV  RegName RegName 
                  | ADD  RegName RegName RegName 
                  | SUB  RegName RegName RegName 
                  | MUL  RegName RegName RegName 
                    deriving (Show, Eq) 

data RegValue=  Undefined --Register values that can be take
              | Modified Int
                  deriving Show

data State a b c d e = State a b c d e -- state of five registers 
                     deriving Show

{- Given below is a sample instruction sequence, 
 - you execute function should be capable of executing 
 - such a sequence of instructions, starting from the 
 - initial state, where all 5 registers are in undefined 
 - state. We will test you execute function using the 
 - following sequence and two other sequences.
 -}

samplePro = [
 MOVI R1  10,
 MOVI R2  2,
 MOVI R3  3,
 ADD  R4  R1 R2, 
 MUL  R5  R4 R3]

initialState= State Undefined Undefined Undefined Undefined Undefined



{-
  The CPU state is just the five registers
updating a register is simply creating a new state with the specific register
changed while the others remain as is. 
-}                  





