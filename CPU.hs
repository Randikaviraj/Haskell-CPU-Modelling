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
              | Int
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

initialState= State Undefined Undefined Undefined Undefined Undefined --initial state undifined

-- Register file updating functions
registerFileUpdate::State->RegName->Int->State
registerFileUpdate (State a b c d e) R1 value=State value b c d e
registerFileUpdate (State a b c d e) R2 value=State a value c d e
registerFileUpdate (State a b c d e) R3 value=State a b value d e
registerFileUpdate (State a b c d e) R4 value=State a b c value e
registerFileUpdate (State a b c d e) R5 value=State a b c d value


-- Register file read functions
registerFileRead::State->RegName->RegValue
registerFileRead (State a _ _ _ _) R1=a
registerFileRead (State _ b _ _ _) R2=b
registerFileRead (State _ _ c _ _) R3=c
registerFileRead (State _ _ _ d _) R4=d
registerFileRead (State _ _ _ _ e) R5=e


-- instruction decoding functions
instructionDecode::Instructions->State->State

execute::[Instructions]->State->State
execute (instruction:remainInstructions) state=execute remainInstructions (instructionDecode instruction state)



{-
  The CPU state is just the five registers
updating a register is simply creating a new state with the specific register
changed while the others remain as is. 
-}                  





