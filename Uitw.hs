module Uitw where

import Bag
import Expr

data Line = Lhs Expr 
      | Both Expr Expr
type Program = [Line]

        --Matroesjka exercise
opdr1 = Mul $ fromList [(Con 32) , sub, sub]
	where sub = Add $ fromList [(Con 1), (Negate $ Double (0.25))]      

ex1cor = (opdr1, [uitw1_1, uitw1_2, uitw1_3, uitw1_4, uitw1_5, uitw1_6, uitw1_7, uitw1_8, uitw1_9, uitw1_10])
ex1cor' = (opdr1, [uitw1_6, uitw1_7, uitw1_8, uitw1_9])
    
uitw1_1 :: Program
uitw1_1 = [Both (Mul $ fromList [Double 0.25 ,(Con 32)]) (Con 8),
     Both (Add $ fromList [(Con 32), (Con (-8))]) (Con 24),
     Both (Mul $ fromList [Div (Con 4), (Con 24)]) (Con 6),
     Both (Add $ fromList [(Con 24), (Con (-6))]) (Con 18)]

uitw1_2 :: Program
uitw1_2 = [Both (Mul $ fromList [(Con 32), Div (Con 4)]) (Con 8),
     Both (Add $ fromList [(Con 32), (Con (-8))]) (Con 24),
     Lhs  (Con 18)]

uitw1_3 :: Program
uitw1_3 = [Both (Add $ fromList [(Con 32), (Con (-8))]) (Con 24),
     Both (Add $ fromList [(Con 24), (Con (-6))]) (Con 18)]

uitw1_4 :: Program
uitw1_4 = [Both (Mul $ fromList [(Con 32), Div (Con 4)]) (Con 8),
         Both (Mul $ fromList [(Con 24), Div (Con 4)]) (Con 6),
         Both (Add $ fromList [(Con 32), (Con (-8))]) (Con 24),
         Both (Add $ fromList [(Con 24), (Con (-6))]) (Con 18)]
       
uitw1_5 :: Program
uitw1_5 = [Both (Mul $ fromList [(Con 32), (Double 0.75), (Double 0.75)]) (Mul $ fromList [(Con 24), (Double 0.75)]),
         Both (Mul $ fromList [(Con 24), (Double 0.75)]) (Con 18)]
         
uitw1_6 :: Program
uitw1_6 = [Both (Mul $ fromList [Con 32, Con 3]) (Con 96),
          Both (Mul $ fromList [Con 96, Div (Con 4)]) (Con 24),
          Both (Add $ fromList [Con 24, Negate (Con 6)]) (Con 18)]

uitw1_7 :: Program
uitw1_7 = [Both (Mul $ fromList [Div (Con 4), Con 32]) (Con 8),
       Both (Add $ fromList [Con 32, Negate $ Con 8]) (Con 24),
       Both (Mul $ fromList [Con 24, Div (Con 4)]) (Con 6),
       Both (Add $ fromList [Con 24, Negate $ Con 6]) (Con 18)]
uitw1_8 :: Program
uitw1_8 = [Lhs (Con 32),
       Lhs (Con 24),
       Lhs (Con 18)]
uitw1_9 :: Program
uitw1_9 = [Lhs  (Mul $ fromList [Double 0.75, Double 0.75]),
       Both (Mul $ fromList [part, part]) (Mul $ fromList [Con 9, Div $ Con 16]),
       Both (Mul $ fromList [Mul $ fromList [Con 9, Div $ Con 16], Con 32]) (Con 18)]
	where part = Mul $ fromList [Con 3, Div (Con 4)]


uitw1_10 :: Program
uitw1_10 = [Both (Mul $ fromList [Mul $ fromList [Con 3, Div (Con 4)], Mul $ fromList [Con 32, Div (Con 1)]]) (Mul $ fromList [Con 96, Div (Con 4)])
           ,Both (Mul $ fromList [Con 96, Div (Con 4)]) (Con 24)
           ,Both (Mul $ fromList [Mul $ fromList [Con 3, Div (Con 4)]
                                 ,Mul $ fromList [Con 24, Div (Con 1)]]) (Mul $ fromList [Con 72, Div (Con 4)])
           ,Both (Mul $ fromList [Con 72, Div (Con 4)]) ( Con 18)
           ]

uitw1_incorrect :: Program
uitw1_incorrect = [Both (Mul $ fromList [Con 32, Div (Con 100), Con 25]) (Con 8),
                   Both (Mul $ fromList [Con 32, Negate (Con 8)]) (Con 26)]
          
         --Chocolate exercise
opdr2 = Mul $ fromList [Add $ fromList [Add $ fromList [(Con 11), (Con (-2))], Add $ fromList [(Con 7), (Con (-4))]], Div (Add $ fromList [(Con 18), (Con 12)]), (Con 100)]    

ex2cor = (opdr2, [uitw2_1,uitw2_2])
        
uitw2_1 :: Program
uitw2_1 = [Lhs (Con 9),
           Lhs (Con 3),
           Lhs (Con 12),
           Lhs (Con 30),
           Both (Mul $ fromList [(Con 12), Div (Con 30), (Con 100)]) (Con 40)]

uitw2_2 :: Program
uitw2_2 = [Both (Add $ fromList [(Con 18), (Con 12)]) (Con 30),
           Both (Add $ fromList [(Con 11), (Con (-2))]) (Con 9),
           Lhs (Con 3),
           Both (Mul $ fromList [(Con 12), Div (Con 30), (Con 100)]) (Con 40)]
 
        --Wine exercise
opdr3 = Mul $ fromList [(Con 225), Div (Mul $ fromList [(Con 3), Div (Con 4)])] 

ex3cor = (opdr3, [uitw3_1, uitw3_2, uitw3_3, uitw3_4])
ex3cor' = (opdr3, [uitw3_2, uitw3_3, uitw3_4])

uitw3_1 :: Program 
uitw3_1 = [Both (Mul $ fromList [(Con 225), Div (Mul $ fromList [(Con 3), Div (Con 4)])]) (Mul $ fromList [(Con 225), (Mul $ fromList [(Con 4), Div (Con 3)])]),
           Both (Mul $ fromList [(Con 225), (Mul $ fromList [(Con 4), Div (Con 3)])]) (Mul $ fromList [(Con 900), Div (Con 3)]),
           Both (Mul $ fromList [(Con 900), Div (Con 3)]) (Con 300)]       

uitw3_2 :: Program
uitw3_2 = [Both (Mul $ fromList [Con 225, Con 4]) (Con 900),
	   Both (Mul $ fromList [Con 900, Div (Con 3)]) (Con 300)]

uitw3_3 :: Program
uitw3_3 = [Lhs (Mul $ fromList [Con 225, Div (Double 0.75)]),
	   Both (Mul $ fromList [Con 900, Div (Con 3)]) (Con 300)]

uitw3_4 :: Program
uitw3_4 = [Both (Div (Mul $ fromList [Con 3, Div (Con 4)])) (Mul $ fromList [Con 4, Div (Con 3)]),
	   Both (Mul $ fromList [Con 4, Div (Con 3), Con 225]) (Mul $ fromList [Con 900, Div (Con 3)]),
	   Both (Mul $ fromList [Con 900, Div (Con 3)]) (Con 300)]


        --Stamp exercise
opdr4 = Mul $ fromList [Add $ fromList [Double 4.74, Negate $ Mul $ fromList [Con 6, Double 0.25]], Div (Con 6)]

ex4cor = (opdr4, [uitw4_1,uitw4_2,uitw4_3,uitw4_4])
ex4cor' = (opdr4, [uitw4_3,uitw4_4])

uitw4_1 :: Program 
uitw4_1 = [Both (Mul $ fromList [(Double 4.74), Div (Con 6)]) (Double 0.79),
           Both (Add $ fromList [(Double 0.79), (Double (-0.25))]) (Double 0.54)]       

uitw4_2 :: Program 
uitw4_2 = [Both (Mul $ fromList [(Double 0.25), (Con 6)]) (Double 1.50),
           Both (Add $ fromList [(Double 4.74), (Double (-1.50))]) (Double 3.24),
           Both (Mul $ fromList [(Double 3.24), Div (Con 6)]) (Double 0.54)]       

uitw4_3 :: Program
uitw4_3 = [Both (Mul $ fromList [Con 6, Double 0.25]) (Double 1.5),
	   Both (Add $ fromList [Double 4.74, Negate $ Double 1.5]) (Double 3.24),
	   Both (Mul $ fromList [Double 3.24, Div (Con 6)]) (Double 0.54)]

uitw4_4 :: Program
uitw4_4 = [Both (Add $ fromList [Double 4.74, Negate $ Double 1.5]) (Double 3.24),
	   Both (Mul $ fromList [Double 3.24, Div (Con 6)]) (Double 0.54)]

-- uitw4_5 would be the same as uitw4_1

	   
        --Work pay exercise
opdr5 = Mul $ fromList [Add $ fromList [mado, vr], Double 4.80]
  where mado = Mul $ fromList [Add $ fromList [(Add $ fromList [Double 16.5, Con (-8)]), Negate (Add $ fromList [Double 12.75, Con (-12)])], Con 4]
        vr   = Add $ fromList [Add $ fromList [Con 14, Con (-7)], Negate (Add $ fromList [Double (-11.5), Con 12])]

ex5cor = (opdr5, [uitw5_1, uitw5_2, uitw5_3])
          
uitw5_1 :: Program 
uitw5_1 = [ Both
              (Add $ fromList [ Double (-0.5)
                              , Add $ fromList [ Add $ fromList [ Mul $ fromList [ Double 8.5
                                                                                 , Con 4]
                                                                , Con 7]
                                               , Mul $ fromList [ Con (-4)
                                                                , Double 0.75]
                                               ]
                              ]
              )
              (Add $ fromList [ Add $ fromList [ Add $ fromList [Con 34, Con 7],  Con (-3) ], Double (-0.5)])
          , Both
              (Add $ fromList [ Add $ fromList [ Add $ fromList [Con 34, Con 7],  Con (-3)],  Double (-0.5)])
              (Double 37.5)
          , Both
              (Mul $ fromList [Double 37.5, Double 4.80])
              (Con 180)
          ]
           
uitw5_2 :: Program 
uitw5_2 = [Lhs (Double 8.5),
           Lhs (Double 0.75),
           Both (Mul $ fromList [(Con 4), (Double 8.5)]) (Double 34.0),
           Both (Add $ fromList [(Double 34.0), (Con (-3))]) (Double 31.0),
           Both (Add $ fromList [(Double 31.0), (Con 7)]) (Double 38.0),
           Both (Add $ fromList [(Double 38.0), (Double (-0.5))]) (Double 37.5),
           Both (Add $ fromList [(Double 32.0), (Con 2)]) (Double 34.0),
           Both (Mul $ fromList [(Double 37.5), (Double 4.80)]) (Double 180.0)]       

uitw5_3 :: Program 
uitw5_3 = [Lhs (Double 8.5),
           Lhs (Double 0.75),
           Both (Mul $ fromList [(Con 4), (Double 8.5)]) (Add $ fromList [(Double 32.0), (Con 2)]),
           Both (Add $ fromList [(Double 32.0), (Con 2)]) (Double 34.0),
           Both (Add $ fromList [(Double 34.0), (Con (-3))]) (Double 31.0),
           Both (Add $ fromList [(Double 31.0), (Con 7)]) (Double 38.0),
           Both (Add $ fromList [(Double 38.0), (Double (-0.5))]) (Double 37.5),
           Both (Add $ fromList [(Double 32.0), (Con 2)]) (Double 34.0),
           Both (Mul $ fromList [(Double 37.5), (Double 4.80)]) (Double 180.0)]   

uitw5_4 :: Program 
uitw5_4 = [Both (Add $ fromList [Double 8.5, Double 0.75]) (Double 7.75),
           Both (Mul $ fromList [Con 7, Con 4]) (Con 28),
           Both (Mul $ fromList [Double 0.75, Con 4]) (Con 3),
           Lhs  (Con 31),
           Both (Add $ fromList [Con 7, (Double 0.5)]) (Double 6.5),
           Both (Mul $ fromList [Con 37, (Double 4.80)]) (Double 177.60),
           Both (Mul $ fromList [(Double 177.60), (Double 2.80)]) (Double 180.0)] 
           
        --Recipe exercise        
--opdr6 = Mul (Div (Con 600) (Con 800)) (Con 300)   
opdr6 =  Mul $ fromList [ Div (Con 800), Con 600, Con 300]
ex6cor = (opdr6, [uitw6_1, uitw6_2])


uitw6_1 :: Program
uitw6_1 = [ Both
              (Mul $ fromList [Div (Con 800), Con 600])
              (Mul $ fromList [Div (Con 4), Con 3])
          , Both
              (Mul $ fromList [Con 300, Div (Mul $ fromList [Con 4, Div (Con 3)])])
              (Con 225)
          ]

uitw6_2 :: Program
uitw6_2 = [Both (Mul $ fromList [Con 600, Div (Con 800), Con 100]) (Con 75),
           Both (Mul $ fromList [Con 300, Div (Con 100), Con 75]) (Con 225)]
           
