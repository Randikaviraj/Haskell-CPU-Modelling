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
                |Modified Int
                  deriving Show

-- instance Show RegValue where
--   show Undefined="Undifined"
--   show x=show x

data State a b c d e = State a b c d e -- state of five registers 
                     deriving Show

-- instance Show (State a b c d e) where
--     show(State a b c d e) = "State is "+show a+" "+show b+" "+show c+" "+show d
    

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
registerFileUpdate::(State RegValue RegValue RegValue RegValue RegValue)-> RegName-> RegValue-> (State RegValue RegValue RegValue RegValue RegValue)
registerFileUpdate (State a b c d e) R1 value=State value b c d e
registerFileUpdate (State a b c d e) R2 value=State a value c d e
registerFileUpdate (State a b c d e) R3 value=State a b value d e
registerFileUpdate (State a b c d e) R4 value=State a b c value e
registerFileUpdate (State a b c d e) R5 value=State a b c d value


-- Register file read functions
registerFileRead :: State RegValue RegValue RegValue RegValue RegValue -> RegName -> RegValue
registerFileRead (State a _ _ _ _) R1=a
registerFileRead (State _ b _ _ _) R2=b
registerFileRead (State _ _ c _ _) R3=c
registerFileRead (State _ _ _ d _) R4=d
registerFileRead (State _ _ _ _ e) R5=e

-- Adder
adder::RegValue->RegValue->(Int->Int->Int)->RegValue
adder Undefined _ _=Undefined
adder _ Undefined _=Undefined
adder (Modified a) (Modified b) op = Modified (op a b)




-- instruction decoding functions
instructionDecode::Instructions->(State RegValue RegValue RegValue RegValue RegValue)->(State RegValue RegValue RegValue RegValue RegValue)
instructionDecode (MOVI rx value) state=registerFileUpdate state rx (Modified value)
instructionDecode (MOV rx ry) state=registerFileUpdate state rx (registerFileRead state ry)
instructionDecode (ADD rx ry rz) state=registerFileUpdate state rx (adder (registerFileRead state ry) (registerFileRead state rz) (+))
instructionDecode (SUB rx ry rz) state=registerFileUpdate state rx (adder (registerFileRead state ry) (registerFileRead state rz) (-))
instructionDecode (MUL rx ry rz) state=registerFileUpdate state rx (adder (registerFileRead state ry) (registerFileRead state rz) (*))


execute::[Instructions]->(State RegValue RegValue RegValue RegValue RegValue)->(State RegValue RegValue RegValue RegValue RegValue)
execute [] state=state
execute (instruction:remainInstructions) state=execute remainInstructions (instructionDecode instruction state)



main = do
    state<=execute samplePro initialState
    putStrLn $ show state





