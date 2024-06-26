{-# OPTIONS_GHC -w #-}
module Solcore.Frontend.Parser.SolcoreParser where

import Solcore.Frontend.Lexer.SolcoreLexer hiding (lexer)
import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (CompUnit)
	| HappyAbsSyn5 ([Import])
	| HappyAbsSyn6 (Import)
	| HappyAbsSyn7 ([Contract])
	| HappyAbsSyn8 (Contract)
	| HappyAbsSyn9 ([Decl])
	| HappyAbsSyn10 (Decl)
	| HappyAbsSyn11 (Field)
	| HappyAbsSyn12 (DataTy)
	| HappyAbsSyn13 ([Constr])
	| HappyAbsSyn14 (Constr)
	| HappyAbsSyn15 (TySym)
	| HappyAbsSyn16 (Class)
	| HappyAbsSyn17 ([Signature])
	| HappyAbsSyn18 ([Tyvar])
	| HappyAbsSyn20 ([Pred])
	| HappyAbsSyn23 (Pred)
	| HappyAbsSyn25 (Signature)
	| HappyAbsSyn27 ([(Name, Ty)])
	| HappyAbsSyn28 ((Name, Ty))
	| HappyAbsSyn29 (Instance)
	| HappyAbsSyn30 ([Ty])
	| HappyAbsSyn32 ([FunDef])
	| HappyAbsSyn34 (FunDef)
	| HappyAbsSyn35 (Maybe Ty)
	| HappyAbsSyn36 (Constructor)
	| HappyAbsSyn37 ([Stmt])
	| HappyAbsSyn39 (Stmt)
	| HappyAbsSyn40 ([Exp])
	| HappyAbsSyn41 (Maybe Exp)
	| HappyAbsSyn42 (Exp)
	| HappyAbsSyn46 ([([Pat], [Stmt])])
	| HappyAbsSyn47 (([Pat], [Stmt]))
	| HappyAbsSyn48 ([Pat])
	| HappyAbsSyn49 (Pat)
	| HappyAbsSyn52 (Literal)
	| HappyAbsSyn53 (Ty)
	| HappyAbsSyn54 (Tyvar)
	| HappyAbsSyn55 (Name)
	| HappyAbsSyn56 ([Name])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,403) ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,1026,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,16,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,0,0,0,32,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,444,0,0,0,0,0,2048,0,0,0,512,444,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,4096,0,0,0,0,0,16,0,0,0,8,0,0,0,0,2048,0,0,0,0,0,2,0,0,0,0,0,0,1,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,4096,0,0,0,0,0,16,0,0,0,0,4096,0,0,0,2560,0,0,0,0,0,0,0,0,0,0,2560,0,0,0,0,0,2,0,0,0,0,2560,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,2048,0,0,0,0,0,8192,0,0,0,0,4096,0,0,0,0,0,8,0,0,0,0,2048,0,0,0,0,256,0,0,0,0,0,1,0,0,0,0,0,256,0,0,0,2560,0,0,0,0,0,0,512,0,0,0,0,4096,0,0,0,0,0,8,0,0,0,2560,0,0,0,0,0,2,0,0,0,0,0,0,4,0,0,0,0,8192,0,0,0,512,0,0,0,0,0,10,0,0,0,0,2048,0,0,0,0,0,8,0,0,0,0,2048,0,0,0,0,0,10,0,0,0,0,0,32768,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,10,0,0,0,0,0,0,32,0,0,0,0,16,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40448,576,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,1024,0,0,0,0,768,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,7680,0,1,0,0,0,30,256,0,0,0,7680,0,1,0,0,0,0,32,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,2560,0,0,0,0,0,512,512,0,0,0,0,2,0,0,0,0,0,1024,0,0,0,0,4098,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,30,256,0,0,0,0,0,0,0,0,0,30,256,0,0,0,7680,0,1,0,0,0,2,0,0,0,0,40448,576,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,2,0,0,0,0,512,16,0,0,0,0,0,32,0,0,0,0,512,0,0,0,2560,0,0,0,0,0,30,256,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,32768,0,0,0,0,0,0,8,0,0,0,0,4,0,0,0,512,0,0,0,0,0,0,4096,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,16384,0,0,0,7680,16384,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,256,0,0,0,0,32768,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7680,16384,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,2,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,7680,16384,1,0,0,0,30,320,0,0,0,40448,576,1,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,7680,16384,1,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","CompilationUnit","ImportList","Import","ContractList","Contract","DeclList","Decl","FieldDef","DataDef","Constrs","Constr","SymDef","ClassDef","ClassBody","OptParam","VarCommaList","ContextOpt","Context","ConstraintList","Constraint","Signatures","Signature","ConOpt","ParamList","Param","InstDef","OptTypeParam","TypeCommaList","Functions","InstBody","Function","OptRetTy","Constructor","Body","StmtList","Stmt","MatchArgList","InitOpt","Expr","ConArgs","FunArgs","ExprCommaList","Equations","Equation","PatCommaList","Pattern","PatternList","PatList","Literal","Type","Var","Con","QualName","Name","identifier","number","tycon","stringlit","'contract'","'import'","'let'","'='","'.'","'class'","'instance'","'data'","'type'","'match'","'function'","'constructor'","'return'","';'","':'","','","'->'","'_'","'=>'","'('","')'","'{'","'}'","'['","']'","'|'","%eof"]
        bit_start = st Prelude.* 88
        bit_end = (st Prelude.+ 1) Prelude.* 88
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..87]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_3

action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (63) = happyShift action_6
action_2 (6) = happyGoto action_4
action_2 (7) = happyGoto action_5
action_2 _ = happyReduce_6

action_3 (88) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_2

action_5 (62) = happyShift action_11
action_5 (8) = happyGoto action_10
action_5 _ = happyReduce_1

action_6 (60) = happyShift action_9
action_6 (55) = happyGoto action_7
action_6 (56) = happyGoto action_8
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_101

action_8 (66) = happyShift action_13
action_8 (75) = happyShift action_14
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_100

action_10 _ = happyReduce_5

action_11 (60) = happyShift action_9
action_11 (55) = happyGoto action_12
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (85) = happyShift action_17
action_12 (18) = happyGoto action_16
action_12 _ = happyReduce_26

action_13 (60) = happyShift action_9
action_13 (55) = happyGoto action_15
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_4

action_15 _ = happyReduce_102

action_16 (83) = happyShift action_22
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (58) = happyShift action_21
action_17 (19) = happyGoto action_18
action_17 (54) = happyGoto action_19
action_17 (57) = happyGoto action_20
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (86) = happyShift action_40
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (77) = happyShift action_39
action_19 _ = happyReduce_28

action_20 _ = happyReduce_99

action_21 _ = happyReduce_103

action_22 (58) = happyShift action_21
action_22 (67) = happyShift action_33
action_22 (68) = happyShift action_34
action_22 (69) = happyShift action_35
action_22 (70) = happyShift action_36
action_22 (72) = happyShift action_37
action_22 (73) = happyShift action_38
action_22 (9) = happyGoto action_23
action_22 (10) = happyGoto action_24
action_22 (11) = happyGoto action_25
action_22 (12) = happyGoto action_26
action_22 (15) = happyGoto action_27
action_22 (16) = happyGoto action_28
action_22 (29) = happyGoto action_29
action_22 (34) = happyGoto action_30
action_22 (36) = happyGoto action_31
action_22 (57) = happyGoto action_32
action_22 _ = happyReduce_9

action_23 (84) = happyShift action_52
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (58) = happyShift action_21
action_24 (67) = happyShift action_33
action_24 (68) = happyShift action_34
action_24 (69) = happyShift action_35
action_24 (70) = happyShift action_36
action_24 (72) = happyShift action_37
action_24 (73) = happyShift action_38
action_24 (9) = happyGoto action_51
action_24 (10) = happyGoto action_24
action_24 (11) = happyGoto action_25
action_24 (12) = happyGoto action_26
action_24 (15) = happyGoto action_27
action_24 (16) = happyGoto action_28
action_24 (29) = happyGoto action_29
action_24 (34) = happyGoto action_30
action_24 (36) = happyGoto action_31
action_24 (57) = happyGoto action_32
action_24 _ = happyReduce_9

action_25 _ = happyReduce_10

action_26 _ = happyReduce_11

action_27 _ = happyReduce_12

action_28 _ = happyReduce_13

action_29 _ = happyReduce_14

action_30 _ = happyReduce_15

action_31 _ = happyReduce_16

action_32 (76) = happyShift action_50
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (85) = happyShift action_48
action_33 (20) = happyGoto action_49
action_33 (21) = happyGoto action_47
action_33 _ = happyReduce_29

action_34 (85) = happyShift action_48
action_34 (20) = happyGoto action_46
action_34 (21) = happyGoto action_47
action_34 _ = happyReduce_29

action_35 (60) = happyShift action_9
action_35 (55) = happyGoto action_45
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (60) = happyShift action_9
action_36 (55) = happyGoto action_44
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (58) = happyShift action_21
action_37 (57) = happyGoto action_43
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (81) = happyShift action_42
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (58) = happyShift action_21
action_39 (19) = happyGoto action_41
action_39 (54) = happyGoto action_19
action_39 (57) = happyGoto action_20
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_25

action_41 _ = happyReduce_27

action_42 (58) = happyShift action_21
action_42 (27) = happyGoto action_65
action_42 (28) = happyGoto action_66
action_42 (57) = happyGoto action_67
action_42 _ = happyReduce_42

action_43 (85) = happyShift action_64
action_43 (26) = happyGoto action_63
action_43 _ = happyReduce_38

action_44 (85) = happyShift action_17
action_44 (18) = happyGoto action_62
action_44 _ = happyReduce_26

action_45 (85) = happyShift action_17
action_45 (18) = happyGoto action_61
action_45 _ = happyReduce_26

action_46 (58) = happyShift action_21
action_46 (60) = happyShift action_9
action_46 (53) = happyGoto action_60
action_46 (54) = happyGoto action_54
action_46 (55) = happyGoto action_55
action_46 (57) = happyGoto action_20
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_30

action_48 (58) = happyShift action_21
action_48 (60) = happyShift action_9
action_48 (22) = happyGoto action_57
action_48 (23) = happyGoto action_58
action_48 (53) = happyGoto action_59
action_48 (54) = happyGoto action_54
action_48 (55) = happyGoto action_55
action_48 (57) = happyGoto action_20
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (58) = happyShift action_21
action_49 (54) = happyGoto action_56
action_49 (57) = happyGoto action_20
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (58) = happyShift action_21
action_50 (60) = happyShift action_9
action_50 (53) = happyGoto action_53
action_50 (54) = happyGoto action_54
action_50 (55) = happyGoto action_55
action_50 (57) = happyGoto action_20
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_8

action_52 _ = happyReduce_7

action_53 _ = happyReduce_17

action_54 _ = happyReduce_98

action_55 (85) = happyShift action_81
action_55 (30) = happyGoto action_80
action_55 _ = happyReduce_46

action_56 (76) = happyShift action_79
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (86) = happyShift action_78
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (77) = happyShift action_77
action_58 _ = happyReduce_33

action_59 (76) = happyShift action_76
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (76) = happyShift action_75
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (65) = happyShift action_74
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (65) = happyShift action_73
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (81) = happyShift action_72
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (58) = happyShift action_21
action_64 (60) = happyShift action_9
action_64 (22) = happyGoto action_71
action_64 (23) = happyGoto action_58
action_64 (53) = happyGoto action_59
action_64 (54) = happyGoto action_54
action_64 (55) = happyGoto action_55
action_64 (57) = happyGoto action_20
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (82) = happyShift action_70
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (77) = happyShift action_69
action_66 _ = happyReduce_40

action_67 (76) = happyShift action_68
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (58) = happyShift action_21
action_68 (60) = happyShift action_9
action_68 (53) = happyGoto action_98
action_68 (54) = happyGoto action_54
action_68 (55) = happyGoto action_55
action_68 (57) = happyGoto action_20
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (58) = happyShift action_21
action_69 (27) = happyGoto action_97
action_69 (28) = happyGoto action_66
action_69 (57) = happyGoto action_67
action_69 _ = happyReduce_42

action_70 (83) = happyShift action_96
action_70 (37) = happyGoto action_95
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (86) = happyShift action_94
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (58) = happyShift action_21
action_72 (27) = happyGoto action_93
action_72 (28) = happyGoto action_66
action_72 (57) = happyGoto action_67
action_72 _ = happyReduce_42

action_73 (58) = happyShift action_21
action_73 (60) = happyShift action_9
action_73 (53) = happyGoto action_92
action_73 (54) = happyGoto action_54
action_73 (55) = happyGoto action_55
action_73 (57) = happyGoto action_20
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (60) = happyShift action_9
action_74 (13) = happyGoto action_89
action_74 (14) = happyGoto action_90
action_74 (55) = happyGoto action_91
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (60) = happyShift action_9
action_75 (55) = happyGoto action_88
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (60) = happyShift action_9
action_76 (55) = happyGoto action_87
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (58) = happyShift action_21
action_77 (60) = happyShift action_9
action_77 (22) = happyGoto action_86
action_77 (23) = happyGoto action_58
action_77 (53) = happyGoto action_59
action_77 (54) = happyGoto action_54
action_77 (55) = happyGoto action_55
action_77 (57) = happyGoto action_20
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (80) = happyShift action_85
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (60) = happyShift action_9
action_79 (55) = happyGoto action_84
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_97

action_81 (58) = happyShift action_21
action_81 (60) = happyShift action_9
action_81 (31) = happyGoto action_82
action_81 (53) = happyGoto action_83
action_81 (54) = happyGoto action_54
action_81 (55) = happyGoto action_55
action_81 (57) = happyGoto action_20
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (86) = happyShift action_118
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (77) = happyShift action_117
action_83 _ = happyReduce_48

action_84 (85) = happyShift action_17
action_84 (18) = happyGoto action_116
action_84 _ = happyReduce_26

action_85 _ = happyReduce_31

action_86 _ = happyReduce_32

action_87 (85) = happyShift action_81
action_87 (30) = happyGoto action_115
action_87 _ = happyReduce_46

action_88 (85) = happyShift action_81
action_88 (30) = happyGoto action_114
action_88 _ = happyReduce_46

action_89 _ = happyReduce_18

action_90 (87) = happyShift action_113
action_90 _ = happyReduce_20

action_91 (85) = happyShift action_81
action_91 (30) = happyGoto action_112
action_91 _ = happyReduce_46

action_92 _ = happyReduce_22

action_93 (82) = happyShift action_111
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_39

action_95 _ = happyReduce_55

action_96 (58) = happyShift action_21
action_96 (59) = happyShift action_105
action_96 (60) = happyShift action_9
action_96 (61) = happyShift action_106
action_96 (64) = happyShift action_107
action_96 (71) = happyShift action_108
action_96 (74) = happyShift action_109
action_96 (81) = happyShift action_110
action_96 (38) = happyGoto action_99
action_96 (39) = happyGoto action_100
action_96 (42) = happyGoto action_101
action_96 (52) = happyGoto action_102
action_96 (55) = happyGoto action_103
action_96 (57) = happyGoto action_104
action_96 _ = happyReduce_58

action_97 _ = happyReduce_41

action_98 _ = happyReduce_43

action_99 (84) = happyShift action_139
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (75) = happyShift action_138
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (65) = happyShift action_136
action_101 (66) = happyShift action_137
action_101 _ = happyReduce_61

action_102 _ = happyReduce_70

action_103 (85) = happyShift action_135
action_103 (43) = happyGoto action_134
action_103 _ = happyReduce_76

action_104 (81) = happyShift action_133
action_104 (44) = happyGoto action_132
action_104 _ = happyReduce_68

action_105 _ = happyReduce_95

action_106 _ = happyReduce_96

action_107 (58) = happyShift action_21
action_107 (57) = happyGoto action_131
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (58) = happyShift action_21
action_108 (59) = happyShift action_105
action_108 (60) = happyShift action_9
action_108 (61) = happyShift action_106
action_108 (81) = happyShift action_110
action_108 (40) = happyGoto action_129
action_108 (42) = happyGoto action_130
action_108 (52) = happyGoto action_102
action_108 (55) = happyGoto action_103
action_108 (57) = happyGoto action_104
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (58) = happyShift action_21
action_109 (59) = happyShift action_105
action_109 (60) = happyShift action_9
action_109 (61) = happyShift action_106
action_109 (81) = happyShift action_110
action_109 (42) = happyGoto action_128
action_109 (52) = happyGoto action_102
action_109 (55) = happyGoto action_103
action_109 (57) = happyGoto action_104
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (58) = happyShift action_21
action_110 (59) = happyShift action_105
action_110 (60) = happyShift action_9
action_110 (61) = happyShift action_106
action_110 (81) = happyShift action_110
action_110 (42) = happyGoto action_127
action_110 (52) = happyGoto action_102
action_110 (55) = happyGoto action_103
action_110 (57) = happyGoto action_104
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (78) = happyShift action_126
action_111 (35) = happyGoto action_125
action_111 _ = happyReduce_54

action_112 _ = happyReduce_21

action_113 (60) = happyShift action_9
action_113 (13) = happyGoto action_124
action_113 (14) = happyGoto action_90
action_113 (55) = happyGoto action_91
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (83) = happyShift action_123
action_114 (33) = happyGoto action_122
action_114 _ = happyFail (happyExpListPerState 114)

action_115 _ = happyReduce_34

action_116 (83) = happyShift action_121
action_116 (17) = happyGoto action_120
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (58) = happyShift action_21
action_117 (60) = happyShift action_9
action_117 (31) = happyGoto action_119
action_117 (53) = happyGoto action_83
action_117 (54) = happyGoto action_54
action_117 (55) = happyGoto action_55
action_117 (57) = happyGoto action_20
action_117 _ = happyFail (happyExpListPerState 117)

action_118 _ = happyReduce_45

action_119 _ = happyReduce_47

action_120 _ = happyReduce_23

action_121 (72) = happyShift action_156
action_121 (24) = happyGoto action_154
action_121 (25) = happyGoto action_155
action_121 _ = happyReduce_36

action_122 _ = happyReduce_44

action_123 (72) = happyShift action_37
action_123 (32) = happyGoto action_152
action_123 (34) = happyGoto action_153
action_123 _ = happyReduce_50

action_124 _ = happyReduce_19

action_125 (83) = happyShift action_96
action_125 (37) = happyGoto action_151
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (58) = happyShift action_21
action_126 (60) = happyShift action_9
action_126 (53) = happyGoto action_150
action_126 (54) = happyGoto action_54
action_126 (55) = happyGoto action_55
action_126 (57) = happyGoto action_20
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (66) = happyShift action_137
action_127 (82) = happyShift action_149
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (66) = happyShift action_137
action_128 _ = happyReduce_62

action_129 (83) = happyShift action_148
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (66) = happyShift action_137
action_130 (77) = happyShift action_147
action_130 _ = happyReduce_64

action_131 (76) = happyShift action_146
action_131 _ = happyFail (happyExpListPerState 131)

action_132 _ = happyReduce_74

action_133 (58) = happyShift action_21
action_133 (59) = happyShift action_105
action_133 (60) = happyShift action_9
action_133 (61) = happyShift action_106
action_133 (81) = happyShift action_110
action_133 (42) = happyGoto action_143
action_133 (45) = happyGoto action_145
action_133 (52) = happyGoto action_102
action_133 (55) = happyGoto action_103
action_133 (57) = happyGoto action_104
action_133 _ = happyReduce_79

action_134 _ = happyReduce_69

action_135 (58) = happyShift action_21
action_135 (59) = happyShift action_105
action_135 (60) = happyShift action_9
action_135 (61) = happyShift action_106
action_135 (81) = happyShift action_110
action_135 (42) = happyGoto action_143
action_135 (45) = happyGoto action_144
action_135 (52) = happyGoto action_102
action_135 (55) = happyGoto action_103
action_135 (57) = happyGoto action_104
action_135 _ = happyReduce_79

action_136 (58) = happyShift action_21
action_136 (59) = happyShift action_105
action_136 (60) = happyShift action_9
action_136 (61) = happyShift action_106
action_136 (81) = happyShift action_110
action_136 (42) = happyGoto action_142
action_136 (52) = happyGoto action_102
action_136 (55) = happyGoto action_103
action_136 (57) = happyGoto action_104
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (58) = happyShift action_21
action_137 (57) = happyGoto action_141
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (58) = happyShift action_21
action_138 (59) = happyShift action_105
action_138 (60) = happyShift action_9
action_138 (61) = happyShift action_106
action_138 (64) = happyShift action_107
action_138 (71) = happyShift action_108
action_138 (74) = happyShift action_109
action_138 (81) = happyShift action_110
action_138 (38) = happyGoto action_140
action_138 (39) = happyGoto action_100
action_138 (42) = happyGoto action_101
action_138 (52) = happyGoto action_102
action_138 (55) = happyGoto action_103
action_138 (57) = happyGoto action_104
action_138 _ = happyReduce_58

action_139 _ = happyReduce_56

action_140 _ = happyReduce_57

action_141 (81) = happyShift action_133
action_141 (44) = happyGoto action_170
action_141 _ = happyReduce_72

action_142 (66) = happyShift action_137
action_142 _ = happyReduce_59

action_143 (66) = happyShift action_137
action_143 (77) = happyShift action_169
action_143 _ = happyReduce_78

action_144 (86) = happyShift action_168
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (82) = happyShift action_167
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (58) = happyShift action_21
action_146 (60) = happyShift action_9
action_146 (53) = happyGoto action_166
action_146 (54) = happyGoto action_54
action_146 (55) = happyGoto action_55
action_146 (57) = happyGoto action_20
action_146 _ = happyFail (happyExpListPerState 146)

action_147 (58) = happyShift action_21
action_147 (59) = happyShift action_105
action_147 (60) = happyShift action_9
action_147 (61) = happyShift action_106
action_147 (81) = happyShift action_110
action_147 (40) = happyGoto action_165
action_147 (42) = happyGoto action_130
action_147 (52) = happyGoto action_102
action_147 (55) = happyGoto action_103
action_147 (57) = happyGoto action_104
action_147 _ = happyFail (happyExpListPerState 147)

action_148 (87) = happyShift action_164
action_148 (46) = happyGoto action_162
action_148 (47) = happyGoto action_163
action_148 _ = happyReduce_82

action_149 _ = happyReduce_71

action_150 _ = happyReduce_53

action_151 _ = happyReduce_52

action_152 (84) = happyShift action_161
action_152 _ = happyFail (happyExpListPerState 152)

action_153 (72) = happyShift action_37
action_153 (32) = happyGoto action_160
action_153 (34) = happyGoto action_153
action_153 _ = happyReduce_50

action_154 (84) = happyShift action_159
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (75) = happyShift action_158
action_155 _ = happyFail (happyExpListPerState 155)

action_156 (58) = happyShift action_21
action_156 (57) = happyGoto action_157
action_156 _ = happyFail (happyExpListPerState 156)

action_157 (85) = happyShift action_64
action_157 (26) = happyGoto action_184
action_157 _ = happyReduce_38

action_158 (72) = happyShift action_156
action_158 (24) = happyGoto action_183
action_158 (25) = happyGoto action_155
action_158 _ = happyReduce_36

action_159 _ = happyReduce_24

action_160 _ = happyReduce_49

action_161 _ = happyReduce_51

action_162 (84) = happyShift action_182
action_162 _ = happyFail (happyExpListPerState 162)

action_163 (87) = happyShift action_164
action_163 (46) = happyGoto action_181
action_163 (47) = happyGoto action_163
action_163 _ = happyReduce_82

action_164 (58) = happyShift action_21
action_164 (59) = happyShift action_105
action_164 (60) = happyShift action_9
action_164 (61) = happyShift action_106
action_164 (79) = happyShift action_179
action_164 (81) = happyShift action_180
action_164 (48) = happyGoto action_174
action_164 (49) = happyGoto action_175
action_164 (52) = happyGoto action_176
action_164 (55) = happyGoto action_177
action_164 (57) = happyGoto action_178
action_164 _ = happyFail (happyExpListPerState 164)

action_165 _ = happyReduce_65

action_166 (65) = happyShift action_173
action_166 (41) = happyGoto action_172
action_166 _ = happyReduce_66

action_167 _ = happyReduce_77

action_168 _ = happyReduce_75

action_169 (58) = happyShift action_21
action_169 (59) = happyShift action_105
action_169 (60) = happyShift action_9
action_169 (61) = happyShift action_106
action_169 (81) = happyShift action_110
action_169 (42) = happyGoto action_143
action_169 (45) = happyGoto action_171
action_169 (52) = happyGoto action_102
action_169 (55) = happyGoto action_103
action_169 (57) = happyGoto action_104
action_169 _ = happyReduce_79

action_170 _ = happyReduce_73

action_171 _ = happyReduce_80

action_172 _ = happyReduce_60

action_173 (58) = happyShift action_21
action_173 (59) = happyShift action_105
action_173 (60) = happyShift action_9
action_173 (61) = happyShift action_106
action_173 (81) = happyShift action_110
action_173 (42) = happyGoto action_191
action_173 (52) = happyGoto action_102
action_173 (55) = happyGoto action_103
action_173 (57) = happyGoto action_104
action_173 _ = happyFail (happyExpListPerState 173)

action_174 (80) = happyShift action_190
action_174 _ = happyFail (happyExpListPerState 174)

action_175 (77) = happyShift action_189
action_175 _ = happyReduce_84

action_176 _ = happyReduce_89

action_177 (85) = happyShift action_188
action_177 (50) = happyGoto action_187
action_177 _ = happyReduce_92

action_178 _ = happyReduce_86

action_179 _ = happyReduce_88

action_180 (58) = happyShift action_21
action_180 (59) = happyShift action_105
action_180 (60) = happyShift action_9
action_180 (61) = happyShift action_106
action_180 (79) = happyShift action_179
action_180 (81) = happyShift action_180
action_180 (49) = happyGoto action_186
action_180 (52) = happyGoto action_176
action_180 (55) = happyGoto action_177
action_180 (57) = happyGoto action_178
action_180 _ = happyFail (happyExpListPerState 180)

action_181 _ = happyReduce_81

action_182 _ = happyReduce_63

action_183 _ = happyReduce_35

action_184 (81) = happyShift action_185
action_184 _ = happyFail (happyExpListPerState 184)

action_185 (58) = happyShift action_21
action_185 (27) = happyGoto action_197
action_185 (28) = happyGoto action_66
action_185 (57) = happyGoto action_67
action_185 _ = happyReduce_42

action_186 (82) = happyShift action_196
action_186 _ = happyFail (happyExpListPerState 186)

action_187 _ = happyReduce_87

action_188 (58) = happyShift action_21
action_188 (59) = happyShift action_105
action_188 (60) = happyShift action_9
action_188 (61) = happyShift action_106
action_188 (79) = happyShift action_179
action_188 (81) = happyShift action_180
action_188 (49) = happyGoto action_194
action_188 (51) = happyGoto action_195
action_188 (52) = happyGoto action_176
action_188 (55) = happyGoto action_177
action_188 (57) = happyGoto action_178
action_188 _ = happyFail (happyExpListPerState 188)

action_189 (58) = happyShift action_21
action_189 (59) = happyShift action_105
action_189 (60) = happyShift action_9
action_189 (61) = happyShift action_106
action_189 (79) = happyShift action_179
action_189 (81) = happyShift action_180
action_189 (48) = happyGoto action_193
action_189 (49) = happyGoto action_175
action_189 (52) = happyGoto action_176
action_189 (55) = happyGoto action_177
action_189 (57) = happyGoto action_178
action_189 _ = happyFail (happyExpListPerState 189)

action_190 (58) = happyShift action_21
action_190 (59) = happyShift action_105
action_190 (60) = happyShift action_9
action_190 (61) = happyShift action_106
action_190 (64) = happyShift action_107
action_190 (71) = happyShift action_108
action_190 (74) = happyShift action_109
action_190 (81) = happyShift action_110
action_190 (38) = happyGoto action_192
action_190 (39) = happyGoto action_100
action_190 (42) = happyGoto action_101
action_190 (52) = happyGoto action_102
action_190 (55) = happyGoto action_103
action_190 (57) = happyGoto action_104
action_190 _ = happyReduce_58

action_191 (66) = happyShift action_137
action_191 _ = happyReduce_67

action_192 _ = happyReduce_83

action_193 _ = happyReduce_85

action_194 (77) = happyShift action_200
action_194 _ = happyReduce_93

action_195 (86) = happyShift action_199
action_195 _ = happyFail (happyExpListPerState 195)

action_196 _ = happyReduce_90

action_197 (82) = happyShift action_198
action_197 _ = happyFail (happyExpListPerState 197)

action_198 (78) = happyShift action_126
action_198 (35) = happyGoto action_202
action_198 _ = happyReduce_54

action_199 _ = happyReduce_91

action_200 (58) = happyShift action_21
action_200 (59) = happyShift action_105
action_200 (60) = happyShift action_9
action_200 (61) = happyShift action_106
action_200 (79) = happyShift action_179
action_200 (81) = happyShift action_180
action_200 (49) = happyGoto action_194
action_200 (51) = happyGoto action_201
action_200 (52) = happyGoto action_176
action_200 (55) = happyGoto action_177
action_200 (57) = happyGoto action_178
action_200 _ = happyFail (happyExpListPerState 200)

action_201 _ = happyReduce_94

action_202 _ = happyReduce_37

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (CompUnit happy_var_1 happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 : happy_var_1
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn56  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Import (QualName happy_var_2)
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_2 : happy_var_1
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_0  7 happyReduction_6
happyReduction_6  =  HappyAbsSyn7
		 ([]
	)

happyReduce_7 = happyReduce 6 8 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	(HappyAbsSyn55  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Contract happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_2  9 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  9 happyReduction_9
happyReduction_9  =  HappyAbsSyn9
		 ([]
	)

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (FieldDecl happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn10
		 (DataDecl happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn10
		 (SymDecl happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn10
		 (ClassDecl happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  10 happyReduction_14
happyReduction_14 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn10
		 (InstDecl happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  10 happyReduction_15
happyReduction_15 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn10
		 (FunDecl happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn10
		 (ConstrDecl happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  11 happyReduction_17
happyReduction_17 (HappyAbsSyn53  happy_var_3)
	_
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn11
		 (Field happy_var_1 happy_var_3 Nothing
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 5 12 happyReduction_18
happyReduction_18 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	(HappyAbsSyn55  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (DataTy happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_3  13 happyReduction_19
happyReduction_19 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  14 happyReduction_21
happyReduction_21 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn14
		 (Constr happy_var_1 happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 5 15 happyReduction_22
happyReduction_22 ((HappyAbsSyn53  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	(HappyAbsSyn55  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (TySym happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 7 16 happyReduction_23
happyReduction_23 ((HappyAbsSyn17  happy_var_7) `HappyStk`
	(HappyAbsSyn18  happy_var_6) `HappyStk`
	(HappyAbsSyn55  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn54  happy_var_3) `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (Class happy_var_2 happy_var_5 happy_var_6 happy_var_3 happy_var_7
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  17 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  18 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  18 happyReduction_26
happyReduction_26  =  HappyAbsSyn18
		 ([]
	)

happyReduce_27 = happySpecReduce_3  19 happyReduction_27
happyReduction_27 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  19 happyReduction_28
happyReduction_28 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_0  20 happyReduction_29
happyReduction_29  =  HappyAbsSyn20
		 ([]
	)

happyReduce_30 = happySpecReduce_1  20 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happyReduce 4 21 happyReduction_31
happyReduction_31 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (happy_var_2
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_3  22 happyReduction_32
happyReduction_32 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 : happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  22 happyReduction_33
happyReduction_33 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn20
		 ([happy_var_1]
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happyReduce 4 23 happyReduction_34
happyReduction_34 ((HappyAbsSyn30  happy_var_4) `HappyStk`
	(HappyAbsSyn55  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (InCls happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_3  24 happyReduction_35
happyReduction_35 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 : happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_0  24 happyReduction_36
happyReduction_36  =  HappyAbsSyn17
		 ([]
	)

happyReduce_37 = happyReduce 7 25 happyReduction_37
happyReduction_37 ((HappyAbsSyn35  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	(HappyAbsSyn55  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (Signature happy_var_2 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_0  26 happyReduction_38
happyReduction_38  =  HappyAbsSyn20
		 ([]
	)

happyReduce_39 = happySpecReduce_3  26 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  27 happyReduction_40
happyReduction_40 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 ([happy_var_1]
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  27 happyReduction_41
happyReduction_41 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1 : happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_0  27 happyReduction_42
happyReduction_42  =  HappyAbsSyn27
		 ([]
	)

happyReduce_43 = happySpecReduce_3  28 happyReduction_43
happyReduction_43 (HappyAbsSyn53  happy_var_3)
	_
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn28
		 ((happy_var_1, happy_var_3)
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happyReduce 7 29 happyReduction_44
happyReduction_44 ((HappyAbsSyn32  happy_var_7) `HappyStk`
	(HappyAbsSyn30  happy_var_6) `HappyStk`
	(HappyAbsSyn55  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_3) `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 (Instance happy_var_2 happy_var_5 happy_var_6 happy_var_3 happy_var_7
	) `HappyStk` happyRest

happyReduce_45 = happySpecReduce_3  30 happyReduction_45
happyReduction_45 _
	(HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (happy_var_2
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_0  30 happyReduction_46
happyReduction_46  =  HappyAbsSyn30
		 ([]
	)

happyReduce_47 = happySpecReduce_3  31 happyReduction_47
happyReduction_47 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1 : happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  31 happyReduction_48
happyReduction_48 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn30
		 ([happy_var_1]
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  32 happyReduction_49
happyReduction_49 (HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1 : happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_0  32 happyReduction_50
happyReduction_50  =  HappyAbsSyn32
		 ([]
	)

happyReduce_51 = happySpecReduce_3  33 happyReduction_51
happyReduction_51 _
	(HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn32
		 (happy_var_2
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happyReduce 8 34 happyReduction_52
happyReduction_52 ((HappyAbsSyn37  happy_var_8) `HappyStk`
	(HappyAbsSyn35  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	(HappyAbsSyn55  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 (FunDef happy_var_2 happy_var_3 happy_var_7 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_53 = happySpecReduce_2  35 happyReduction_53
happyReduction_53 (HappyAbsSyn53  happy_var_2)
	_
	 =  HappyAbsSyn35
		 (Just happy_var_2
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_0  35 happyReduction_54
happyReduction_54  =  HappyAbsSyn35
		 (Nothing
	)

happyReduce_55 = happyReduce 5 36 happyReduction_55
happyReduction_55 ((HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 (Constructor happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_56 = happySpecReduce_3  37 happyReduction_56
happyReduction_56 _
	(HappyAbsSyn37  happy_var_2)
	_
	 =  HappyAbsSyn37
		 (happy_var_2
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  38 happyReduction_57
happyReduction_57 (HappyAbsSyn37  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1 : happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_0  38 happyReduction_58
happyReduction_58  =  HappyAbsSyn37
		 ([]
	)

happyReduce_59 = happySpecReduce_3  39 happyReduction_59
happyReduction_59 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1 := happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happyReduce 5 39 happyReduction_60
happyReduction_60 ((HappyAbsSyn41  happy_var_5) `HappyStk`
	(HappyAbsSyn53  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn55  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (Let happy_var_2 (Just happy_var_4) happy_var_5
	) `HappyStk` happyRest

happyReduce_61 = happySpecReduce_1  39 happyReduction_61
happyReduction_61 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn39
		 (StmtExp happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  39 happyReduction_62
happyReduction_62 (HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn39
		 (Return happy_var_2
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happyReduce 5 39 happyReduction_63
happyReduction_63 (_ `HappyStk`
	(HappyAbsSyn46  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (Match happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_64 = happySpecReduce_1  40 happyReduction_64
happyReduction_64 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn40
		 ([happy_var_1]
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  40 happyReduction_65
happyReduction_65 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1 : happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_0  41 happyReduction_66
happyReduction_66  =  HappyAbsSyn41
		 (Nothing
	)

happyReduce_67 = happySpecReduce_2  41 happyReduction_67
happyReduction_67 (HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn41
		 (Just happy_var_2
	)
happyReduction_67 _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  42 happyReduction_68
happyReduction_68 (HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn42
		 (Var happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_2  42 happyReduction_69
happyReduction_69 (HappyAbsSyn40  happy_var_2)
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn42
		 (Con happy_var_1 happy_var_2
	)
happyReduction_69 _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  42 happyReduction_70
happyReduction_70 (HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn42
		 (Lit happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  42 happyReduction_71
happyReduction_71 _
	(HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn42
		 (happy_var_2
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  42 happyReduction_72
happyReduction_72 (HappyAbsSyn55  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (FieldAccess happy_var_1 happy_var_3
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happyReduce 4 42 happyReduction_73
happyReduction_73 ((HappyAbsSyn40  happy_var_4) `HappyStk`
	(HappyAbsSyn55  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn42
		 (Call (Just happy_var_1) happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_74 = happySpecReduce_2  42 happyReduction_74
happyReduction_74 (HappyAbsSyn40  happy_var_2)
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn42
		 (Call Nothing happy_var_1 happy_var_2
	)
happyReduction_74 _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  43 happyReduction_75
happyReduction_75 _
	(HappyAbsSyn40  happy_var_2)
	_
	 =  HappyAbsSyn40
		 (happy_var_2
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_0  43 happyReduction_76
happyReduction_76  =  HappyAbsSyn40
		 ([]
	)

happyReduce_77 = happySpecReduce_3  44 happyReduction_77
happyReduction_77 _
	(HappyAbsSyn40  happy_var_2)
	_
	 =  HappyAbsSyn40
		 (happy_var_2
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  45 happyReduction_78
happyReduction_78 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn40
		 ([happy_var_1]
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_0  45 happyReduction_79
happyReduction_79  =  HappyAbsSyn40
		 ([]
	)

happyReduce_80 = happySpecReduce_3  45 happyReduction_80
happyReduction_80 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1 : happy_var_3
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_2  46 happyReduction_81
happyReduction_81 (HappyAbsSyn46  happy_var_2)
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn46
		 (happy_var_1 : happy_var_2
	)
happyReduction_81 _ _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_0  46 happyReduction_82
happyReduction_82  =  HappyAbsSyn46
		 ([]
	)

happyReduce_83 = happyReduce 4 47 happyReduction_83
happyReduction_83 ((HappyAbsSyn37  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn47
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_84 = happySpecReduce_1  48 happyReduction_84
happyReduction_84 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn48
		 ([happy_var_1]
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_3  48 happyReduction_85
happyReduction_85 (HappyAbsSyn48  happy_var_3)
	_
	(HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1 : happy_var_3
	)
happyReduction_85 _ _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_1  49 happyReduction_86
happyReduction_86 (HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn49
		 (PVar happy_var_1
	)
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_2  49 happyReduction_87
happyReduction_87 (HappyAbsSyn48  happy_var_2)
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn49
		 (PCon happy_var_1 happy_var_2
	)
happyReduction_87 _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  49 happyReduction_88
happyReduction_88 _
	 =  HappyAbsSyn49
		 (PWildcard
	)

happyReduce_89 = happySpecReduce_1  49 happyReduction_89
happyReduction_89 (HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn49
		 (PLit happy_var_1
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_3  49 happyReduction_90
happyReduction_90 _
	(HappyAbsSyn49  happy_var_2)
	_
	 =  HappyAbsSyn49
		 (happy_var_2
	)
happyReduction_90 _ _ _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  50 happyReduction_91
happyReduction_91 _
	(HappyAbsSyn48  happy_var_2)
	_
	 =  HappyAbsSyn48
		 (happy_var_2
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_0  50 happyReduction_92
happyReduction_92  =  HappyAbsSyn48
		 ([]
	)

happyReduce_93 = happySpecReduce_1  51 happyReduction_93
happyReduction_93 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn48
		 ([happy_var_1]
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_3  51 happyReduction_94
happyReduction_94 (HappyAbsSyn48  happy_var_3)
	_
	(HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1 : happy_var_3
	)
happyReduction_94 _ _ _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_1  52 happyReduction_95
happyReduction_95 (HappyTerminal (Token _ (TNumber happy_var_1)))
	 =  HappyAbsSyn52
		 (IntLit $ toInteger happy_var_1
	)
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  52 happyReduction_96
happyReduction_96 (HappyTerminal (Token _ (TString happy_var_1)))
	 =  HappyAbsSyn52
		 (StrLit happy_var_1
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_2  53 happyReduction_97
happyReduction_97 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn53
		 (TyCon happy_var_1 happy_var_2
	)
happyReduction_97 _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_1  53 happyReduction_98
happyReduction_98 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn53
		 (TyVar  happy_var_1
	)
happyReduction_98 _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_1  54 happyReduction_99
happyReduction_99 (HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn54
		 (TVar happy_var_1
	)
happyReduction_99 _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_1  55 happyReduction_100
happyReduction_100 (HappyTerminal (Token _ (TTycon happy_var_1)))
	 =  HappyAbsSyn55
		 (Name happy_var_1
	)
happyReduction_100 _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_1  56 happyReduction_101
happyReduction_101 (HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn56
		 ([happy_var_1]
	)
happyReduction_101 _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_3  56 happyReduction_102
happyReduction_102 (HappyAbsSyn55  happy_var_3)
	_
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn56
		 (happy_var_3 : happy_var_1
	)
happyReduction_102 _ _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_1  57 happyReduction_103
happyReduction_103 (HappyTerminal (Token _ (TIdent happy_var_1)))
	 =  HappyAbsSyn55
		 (Name happy_var_1
	)
happyReduction_103 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TEOF -> action 88 88 tk (HappyState action) sts stk;
	Token _ (TIdent happy_dollar_dollar) -> cont 58;
	Token _ (TNumber happy_dollar_dollar) -> cont 59;
	Token _ (TTycon happy_dollar_dollar) -> cont 60;
	Token _ (TString happy_dollar_dollar) -> cont 61;
	Token _ TContract -> cont 62;
	Token _ TImport -> cont 63;
	Token _ TLet -> cont 64;
	Token _ TEq -> cont 65;
	Token _ TDot -> cont 66;
	Token _ TClass -> cont 67;
	Token _ TInstance -> cont 68;
	Token _ TData -> cont 69;
	Token _ TType -> cont 70;
	Token _ TMatch -> cont 71;
	Token _ TFunction -> cont 72;
	Token _ TConstructor -> cont 73;
	Token _ TReturn -> cont 74;
	Token _ TSemi -> cont 75;
	Token _ TColon -> cont 76;
	Token _ TComma -> cont 77;
	Token _ TArrow -> cont 78;
	Token _ TWildCard -> cont 79;
	Token _ TDArrow -> cont 80;
	Token _ TLParen -> cont 81;
	Token _ TRParen -> cont 82;
	Token _ TLBrace -> cont 83;
	Token _ TRBrace -> cont 84;
	Token _ TLBrack -> cont 85;
	Token _ TRBrack -> cont 86;
	Token _ TBar -> cont 87;
	_ -> happyError' (tk, [])
	})

happyError_ explist 88 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = ((>>=))
happyReturn :: () => a -> Alex a
happyReturn = (return)
happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [Prelude.String]) -> Alex a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
parser = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: Token -> Alex a
parseError _ 
  = do 
        (AlexPn _ line column, _, _, _) <- alexGetInput
        alexError $ "Parse error at line " ++ show line ++ 
                    ", column " ++ show column

lexer :: (Token -> Alex a) -> Alex a 
lexer = (=<< alexMonadScan)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
