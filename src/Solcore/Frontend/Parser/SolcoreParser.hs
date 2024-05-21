{-# OPTIONS_GHC -w #-}
module Solcore.Frontend.Parser.SolcoreParser (solcoreParser) where

import Solcore.Frontend.Lexer.SolcoreLexer
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
	| HappyAbsSyn15 (Constr)
	| HappyAbsSyn16 (TySym)
	| HappyAbsSyn17 (Class)
	| HappyAbsSyn18 ([Tyvar])
	| HappyAbsSyn20 ([Pred])
	| HappyAbsSyn22 (Pred)
	| HappyAbsSyn23 ([Signature])
	| HappyAbsSyn24 (Signature)
	| HappyAbsSyn25 ([(Name, Ty)])
	| HappyAbsSyn27 ((Name, Ty))
	| HappyAbsSyn28 (Instance)
	| HappyAbsSyn29 ([Ty])
	| HappyAbsSyn31 ([FunDef])
	| HappyAbsSyn32 (FunDef)
	| HappyAbsSyn33 ([Stmt])
	| HappyAbsSyn35 (Stmt)
	| HappyAbsSyn36 (Exp)
	| HappyAbsSyn37 ([Exp])
	| HappyAbsSyn39 ([(Pat, [Stmt])])
	| HappyAbsSyn40 ((Pat, [Stmt]))
	| HappyAbsSyn41 (Pat)
	| HappyAbsSyn42 ([Pat])
	| HappyAbsSyn44 (Literal)
	| HappyAbsSyn45 (Ty)
	| HappyAbsSyn46 (Tyvar)
	| HappyAbsSyn47 (Name)
	| HappyAbsSyn48 ([Name])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
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
 action_182 :: () => Prelude.Int -> ({-HappyReduction (P) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (P) HappyAbsSyn)

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
 happyReduce_85 :: () => ({-HappyReduction (P) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (P) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,342) ([0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,2048,0,0,0,0,0,1025,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,8192,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,34693,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,40,512,0,0,0,20,256,0,0,0,2,0,0,0,0,4,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,64,0,0,0,1,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,512,0,0,0,640,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,80,0,0,0,0,0,0,0,0,0,0,16,0,0,0,2,0,0,0,0,0,4,0,0,0,2,0,0,0,16384,1,0,0,0,32768,0,0,0,0,0,2,0,0,0,2048,0,0,0,0,1024,0,0,0,0,0,0,1,0,0,0,1024,0,0,0,0,256,0,0,0,0,256,0,0,0,160,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,256,0,0,0,0,4,0,0,0,0,1024,0,0,0,0,64,0,0,16384,1,0,0,0,0,16384,0,0,0,0,0,128,0,0,10240,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,1024,0,0,16384,0,0,0,0,40960,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,8192,0,0,0,2560,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1024,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,64,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,448,4122,0,0,0,0,0,1,0,0,16,0,0,0,0,0,4096,0,0,0,0,4,0,0,0,0,128,0,0,0,0,256,0,0,0,0,256,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,32768,0,1,0,0,0,0,0,0,0,1792,16392,0,0,0,896,8196,0,0,0,448,4098,0,0,0,224,2049,0,0,0,0,2048,0,0,0,0,512,0,0,0,0,1024,0,0,0,0,512,0,0,0,0,256,0,0,32768,1027,32,0,0,49152,513,16,0,0,57344,256,8,0,0,28672,1664,4,0,0,0,0,0,0,0,0,512,0,0,0,512,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,8192,0,0,0,0,4,0,0,0,0,0,0,0,0,0,8,0,0,0,0,128,0,0,0,0,64,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,16,0,0,0,896,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16440,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,32768,2,0,0,0,0,0,1,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,448,1024,0,0,0,224,2061,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,57344,0,2,0,0,0,16384,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","CompilationUnit","ImportList","Import","ContractList","Contract","DeclList","Decl","FieldDef","DataDef","Constrs","ConstrList","Constr","SymDef","ClassDef","OptParam","VarCommaList","Context","ConstraintList","Constraint","Signatures","Signature","ParamList","ParamCommaList","Param","InstDef","OptTypeParam","TypeCommaList","Functions","Function","Body","StmtList","Stmt","Expr","ExprList","ExprCommaList","Equations","Equation","Pattern","PatList","PatternCommaList","Literal","Type","Var","Con","QualName","Name","identifier","number","tycon","'contract'","'import'","'='","'.'","'class'","'instance'","'data'","'type'","'switch'","'case'","'if'","'while'","'function'","';'","':'","','","'->'","'_'","'=>'","'('","')'","'{'","'}'","'['","']'","'|'","%eof"]
        bit_start = st Prelude.* 79
        bit_end = (st Prelude.+ 1) Prelude.* 79
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..78]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_3

action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (54) = happyShift action_6
action_2 (6) = happyGoto action_4
action_2 (7) = happyGoto action_5
action_2 _ = happyReduce_6

action_3 (79) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_2

action_5 (53) = happyShift action_11
action_5 (8) = happyGoto action_10
action_5 _ = happyReduce_1

action_6 (50) = happyShift action_9
action_6 (48) = happyGoto action_7
action_6 (49) = happyGoto action_8
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (56) = happyShift action_13
action_7 (66) = happyShift action_14
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_83

action_9 _ = happyReduce_85

action_10 _ = happyReduce_5

action_11 (50) = happyShift action_9
action_11 (49) = happyGoto action_12
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (74) = happyShift action_16
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (50) = happyShift action_9
action_13 (49) = happyGoto action_15
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_4

action_15 _ = happyReduce_84

action_16 (9) = happyGoto action_17
action_16 _ = happyReduce_9

action_17 (50) = happyShift action_9
action_17 (52) = happyShift action_29
action_17 (57) = happyShift action_30
action_17 (58) = happyShift action_31
action_17 (59) = happyShift action_32
action_17 (60) = happyShift action_33
action_17 (65) = happyShift action_34
action_17 (75) = happyShift action_35
action_17 (10) = happyGoto action_18
action_17 (11) = happyGoto action_19
action_17 (12) = happyGoto action_20
action_17 (16) = happyGoto action_21
action_17 (17) = happyGoto action_22
action_17 (28) = happyGoto action_23
action_17 (32) = happyGoto action_24
action_17 (45) = happyGoto action_25
action_17 (46) = happyGoto action_26
action_17 (47) = happyGoto action_27
action_17 (49) = happyGoto action_28
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_8

action_19 _ = happyReduce_10

action_20 _ = happyReduce_11

action_21 _ = happyReduce_12

action_22 _ = happyReduce_13

action_23 _ = happyReduce_14

action_24 _ = happyReduce_15

action_25 (50) = happyShift action_9
action_25 (49) = happyGoto action_46
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_80

action_27 (76) = happyShift action_45
action_27 (29) = happyGoto action_44
action_27 _ = happyReduce_44

action_28 _ = happyReduce_81

action_29 _ = happyReduce_82

action_30 (50) = happyShift action_9
action_30 (52) = happyShift action_29
action_30 (72) = happyShift action_42
action_30 (20) = happyGoto action_43
action_30 (22) = happyGoto action_40
action_30 (45) = happyGoto action_41
action_30 (46) = happyGoto action_26
action_30 (47) = happyGoto action_27
action_30 (49) = happyGoto action_28
action_30 _ = happyReduce_29

action_31 (50) = happyShift action_9
action_31 (52) = happyShift action_29
action_31 (72) = happyShift action_42
action_31 (20) = happyGoto action_39
action_31 (22) = happyGoto action_40
action_31 (45) = happyGoto action_41
action_31 (46) = happyGoto action_26
action_31 (47) = happyGoto action_27
action_31 (49) = happyGoto action_28
action_31 _ = happyReduce_29

action_32 (50) = happyShift action_9
action_32 (49) = happyGoto action_38
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (52) = happyShift action_29
action_33 (47) = happyGoto action_37
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (50) = happyShift action_9
action_34 (49) = happyGoto action_36
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_7

action_36 (72) = happyShift action_55
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (76) = happyShift action_54
action_37 (18) = happyGoto action_53
action_37 _ = happyReduce_25

action_38 (55) = happyShift action_52
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (71) = happyShift action_51
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_28

action_41 (67) = happyShift action_50
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (50) = happyShift action_9
action_42 (52) = happyShift action_29
action_42 (22) = happyGoto action_49
action_42 (45) = happyGoto action_41
action_42 (46) = happyGoto action_26
action_42 (47) = happyGoto action_27
action_42 (49) = happyGoto action_28
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (71) = happyShift action_48
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_79

action_45 (50) = happyShift action_9
action_45 (52) = happyShift action_29
action_45 (45) = happyGoto action_47
action_45 (46) = happyGoto action_26
action_45 (47) = happyGoto action_27
action_45 (49) = happyGoto action_28
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_16

action_47 (68) = happyShift action_70
action_47 (30) = happyGoto action_69
action_47 _ = happyReduce_46

action_48 (50) = happyShift action_9
action_48 (46) = happyGoto action_68
action_48 (49) = happyGoto action_28
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (68) = happyShift action_67
action_49 (21) = happyGoto action_66
action_49 _ = happyReduce_32

action_50 (52) = happyShift action_29
action_50 (47) = happyGoto action_65
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (50) = happyShift action_9
action_51 (52) = happyShift action_29
action_51 (45) = happyGoto action_64
action_51 (46) = happyGoto action_26
action_51 (47) = happyGoto action_27
action_51 (49) = happyGoto action_28
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (52) = happyShift action_29
action_52 (13) = happyGoto action_61
action_52 (15) = happyGoto action_62
action_52 (47) = happyGoto action_63
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (55) = happyShift action_60
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (50) = happyShift action_9
action_54 (46) = happyGoto action_59
action_54 (49) = happyGoto action_28
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (50) = happyShift action_9
action_55 (25) = happyGoto action_56
action_55 (27) = happyGoto action_57
action_55 (49) = happyGoto action_58
action_55 _ = happyReduce_37

action_56 (73) = happyShift action_87
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (68) = happyShift action_86
action_57 (26) = happyGoto action_85
action_57 _ = happyReduce_40

action_58 (67) = happyShift action_84
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (68) = happyShift action_83
action_59 (19) = happyGoto action_82
action_59 _ = happyReduce_27

action_60 (50) = happyShift action_9
action_60 (52) = happyShift action_29
action_60 (45) = happyGoto action_81
action_60 (46) = happyGoto action_26
action_60 (47) = happyGoto action_27
action_60 (49) = happyGoto action_28
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_17

action_62 (78) = happyShift action_80
action_62 (14) = happyGoto action_79
action_62 _ = happyReduce_20

action_63 (72) = happyShift action_78
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (67) = happyShift action_77
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (76) = happyShift action_45
action_65 (29) = happyGoto action_76
action_65 _ = happyReduce_44

action_66 (73) = happyShift action_75
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (50) = happyShift action_9
action_67 (52) = happyShift action_29
action_67 (22) = happyGoto action_74
action_67 (45) = happyGoto action_41
action_67 (46) = happyGoto action_26
action_67 (47) = happyGoto action_27
action_67 (49) = happyGoto action_28
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (67) = happyShift action_73
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (77) = happyShift action_72
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (50) = happyShift action_9
action_70 (52) = happyShift action_29
action_70 (45) = happyGoto action_71
action_70 (46) = happyGoto action_26
action_70 (47) = happyGoto action_27
action_70 (49) = happyGoto action_28
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (68) = happyShift action_70
action_71 (30) = happyGoto action_98
action_71 _ = happyReduce_46

action_72 _ = happyReduce_43

action_73 (52) = happyShift action_29
action_73 (47) = happyGoto action_97
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (68) = happyShift action_67
action_74 (21) = happyGoto action_96
action_74 _ = happyReduce_32

action_75 _ = happyReduce_30

action_76 _ = happyReduce_33

action_77 (52) = happyShift action_29
action_77 (47) = happyGoto action_95
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (76) = happyShift action_45
action_78 (29) = happyGoto action_94
action_78 _ = happyReduce_44

action_79 _ = happyReduce_18

action_80 (52) = happyShift action_29
action_80 (15) = happyGoto action_93
action_80 (47) = happyGoto action_63
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_22

action_82 (77) = happyShift action_92
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (50) = happyShift action_9
action_83 (46) = happyGoto action_91
action_83 (49) = happyGoto action_28
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (50) = happyShift action_9
action_84 (52) = happyShift action_29
action_84 (45) = happyGoto action_90
action_84 (46) = happyGoto action_26
action_84 (47) = happyGoto action_27
action_84 (49) = happyGoto action_28
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_38

action_86 (50) = happyShift action_9
action_86 (27) = happyGoto action_89
action_86 (49) = happyGoto action_58
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (69) = happyShift action_88
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (50) = happyShift action_9
action_88 (52) = happyShift action_29
action_88 (45) = happyGoto action_105
action_88 (46) = happyGoto action_26
action_88 (47) = happyGoto action_27
action_88 (49) = happyGoto action_28
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (68) = happyShift action_86
action_89 (26) = happyGoto action_104
action_89 _ = happyReduce_40

action_90 _ = happyReduce_41

action_91 (68) = happyShift action_83
action_91 (19) = happyGoto action_103
action_91 _ = happyReduce_27

action_92 _ = happyReduce_24

action_93 (78) = happyShift action_80
action_93 (14) = happyGoto action_102
action_93 _ = happyReduce_20

action_94 (73) = happyShift action_101
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (76) = happyShift action_100
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_31

action_97 (76) = happyShift action_54
action_97 (18) = happyGoto action_99
action_97 _ = happyReduce_25

action_98 _ = happyReduce_45

action_99 (74) = happyShift action_109
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (76) = happyShift action_45
action_100 (29) = happyGoto action_108
action_100 _ = happyReduce_44

action_101 _ = happyReduce_21

action_102 _ = happyReduce_19

action_103 _ = happyReduce_26

action_104 _ = happyReduce_39

action_105 (74) = happyShift action_107
action_105 (33) = happyGoto action_106
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_49

action_107 (50) = happyShift action_9
action_107 (51) = happyShift action_120
action_107 (52) = happyShift action_29
action_107 (61) = happyShift action_121
action_107 (63) = happyShift action_122
action_107 (64) = happyShift action_123
action_107 (72) = happyShift action_124
action_107 (34) = happyGoto action_114
action_107 (35) = happyGoto action_115
action_107 (36) = happyGoto action_116
action_107 (44) = happyGoto action_117
action_107 (47) = happyGoto action_118
action_107 (49) = happyGoto action_119
action_107 _ = happyReduce_52

action_108 (77) = happyShift action_113
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (50) = happyShift action_9
action_109 (23) = happyGoto action_110
action_109 (24) = happyGoto action_111
action_109 (49) = happyGoto action_112
action_109 _ = happyReduce_35

action_110 (75) = happyShift action_138
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (66) = happyShift action_137
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (72) = happyShift action_136
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (74) = happyShift action_135
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (75) = happyShift action_134
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (66) = happyShift action_133
action_115 _ = happyFail (happyExpListPerState 115)

action_116 _ = happyReduce_56

action_117 _ = happyReduce_59

action_118 (72) = happyShift action_132
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (55) = happyShift action_130
action_119 (72) = happyShift action_131
action_119 _ = happyReduce_57

action_120 _ = happyReduce_78

action_121 (50) = happyShift action_9
action_121 (51) = happyShift action_120
action_121 (52) = happyShift action_29
action_121 (61) = happyShift action_121
action_121 (72) = happyShift action_124
action_121 (36) = happyGoto action_129
action_121 (44) = happyGoto action_117
action_121 (47) = happyGoto action_118
action_121 (49) = happyGoto action_126
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (50) = happyShift action_9
action_122 (51) = happyShift action_120
action_122 (52) = happyShift action_29
action_122 (61) = happyShift action_121
action_122 (72) = happyShift action_124
action_122 (36) = happyGoto action_128
action_122 (44) = happyGoto action_117
action_122 (47) = happyGoto action_118
action_122 (49) = happyGoto action_126
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (50) = happyShift action_9
action_123 (51) = happyShift action_120
action_123 (52) = happyShift action_29
action_123 (61) = happyShift action_121
action_123 (72) = happyShift action_124
action_123 (36) = happyGoto action_127
action_123 (44) = happyGoto action_117
action_123 (47) = happyGoto action_118
action_123 (49) = happyGoto action_126
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (50) = happyShift action_9
action_124 (51) = happyShift action_120
action_124 (52) = happyShift action_29
action_124 (61) = happyShift action_121
action_124 (72) = happyShift action_124
action_124 (36) = happyGoto action_125
action_124 (44) = happyGoto action_117
action_124 (47) = happyGoto action_118
action_124 (49) = happyGoto action_126
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (73) = happyShift action_151
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (72) = happyShift action_131
action_126 _ = happyReduce_57

action_127 (74) = happyShift action_107
action_127 (33) = happyGoto action_150
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (74) = happyShift action_107
action_128 (33) = happyGoto action_149
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (74) = happyShift action_148
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (50) = happyShift action_9
action_130 (51) = happyShift action_120
action_130 (52) = happyShift action_29
action_130 (61) = happyShift action_121
action_130 (72) = happyShift action_124
action_130 (36) = happyGoto action_147
action_130 (44) = happyGoto action_117
action_130 (47) = happyGoto action_118
action_130 (49) = happyGoto action_126
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (50) = happyShift action_9
action_131 (51) = happyShift action_120
action_131 (52) = happyShift action_29
action_131 (61) = happyShift action_121
action_131 (72) = happyShift action_124
action_131 (36) = happyGoto action_144
action_131 (37) = happyGoto action_146
action_131 (44) = happyGoto action_117
action_131 (47) = happyGoto action_118
action_131 (49) = happyGoto action_126
action_131 _ = happyReduce_63

action_132 (50) = happyShift action_9
action_132 (51) = happyShift action_120
action_132 (52) = happyShift action_29
action_132 (61) = happyShift action_121
action_132 (72) = happyShift action_124
action_132 (36) = happyGoto action_144
action_132 (37) = happyGoto action_145
action_132 (44) = happyGoto action_117
action_132 (47) = happyGoto action_118
action_132 (49) = happyGoto action_126
action_132 _ = happyReduce_63

action_133 (50) = happyShift action_9
action_133 (51) = happyShift action_120
action_133 (52) = happyShift action_29
action_133 (61) = happyShift action_121
action_133 (63) = happyShift action_122
action_133 (64) = happyShift action_123
action_133 (72) = happyShift action_124
action_133 (34) = happyGoto action_143
action_133 (35) = happyGoto action_115
action_133 (36) = happyGoto action_116
action_133 (44) = happyGoto action_117
action_133 (47) = happyGoto action_118
action_133 (49) = happyGoto action_119
action_133 _ = happyReduce_52

action_134 _ = happyReduce_50

action_135 (65) = happyShift action_34
action_135 (31) = happyGoto action_141
action_135 (32) = happyGoto action_142
action_135 _ = happyReduce_48

action_136 (50) = happyShift action_9
action_136 (25) = happyGoto action_140
action_136 (27) = happyGoto action_57
action_136 (49) = happyGoto action_58
action_136 _ = happyReduce_37

action_137 (50) = happyShift action_9
action_137 (23) = happyGoto action_139
action_137 (24) = happyGoto action_111
action_137 (49) = happyGoto action_112
action_137 _ = happyReduce_35

action_138 _ = happyReduce_23

action_139 _ = happyReduce_34

action_140 (73) = happyShift action_161
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (75) = happyShift action_160
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (65) = happyShift action_34
action_142 (31) = happyGoto action_159
action_142 (32) = happyGoto action_142
action_142 _ = happyReduce_48

action_143 _ = happyReduce_51

action_144 (68) = happyShift action_158
action_144 (38) = happyGoto action_157
action_144 _ = happyReduce_66

action_145 (73) = happyShift action_156
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (73) = happyShift action_155
action_146 _ = happyFail (happyExpListPerState 146)

action_147 _ = happyReduce_53

action_148 (62) = happyShift action_154
action_148 (39) = happyGoto action_152
action_148 (40) = happyGoto action_153
action_148 _ = happyReduce_68

action_149 _ = happyReduce_54

action_150 _ = happyReduce_55

action_151 _ = happyReduce_60

action_152 (75) = happyShift action_170
action_152 _ = happyFail (happyExpListPerState 152)

action_153 (62) = happyShift action_154
action_153 (39) = happyGoto action_169
action_153 (40) = happyGoto action_153
action_153 _ = happyReduce_68

action_154 (50) = happyShift action_9
action_154 (51) = happyShift action_120
action_154 (52) = happyShift action_29
action_154 (70) = happyShift action_168
action_154 (41) = happyGoto action_164
action_154 (44) = happyGoto action_165
action_154 (47) = happyGoto action_166
action_154 (49) = happyGoto action_167
action_154 _ = happyFail (happyExpListPerState 154)

action_155 _ = happyReduce_61

action_156 _ = happyReduce_58

action_157 _ = happyReduce_64

action_158 (50) = happyShift action_9
action_158 (51) = happyShift action_120
action_158 (52) = happyShift action_29
action_158 (61) = happyShift action_121
action_158 (72) = happyShift action_124
action_158 (36) = happyGoto action_163
action_158 (44) = happyGoto action_117
action_158 (47) = happyGoto action_118
action_158 (49) = happyGoto action_126
action_158 _ = happyFail (happyExpListPerState 158)

action_159 _ = happyReduce_47

action_160 _ = happyReduce_42

action_161 (67) = happyShift action_162
action_161 _ = happyFail (happyExpListPerState 161)

action_162 (50) = happyShift action_9
action_162 (52) = happyShift action_29
action_162 (45) = happyGoto action_174
action_162 (46) = happyGoto action_26
action_162 (47) = happyGoto action_27
action_162 (49) = happyGoto action_28
action_162 _ = happyFail (happyExpListPerState 162)

action_163 (68) = happyShift action_158
action_163 (38) = happyGoto action_173
action_163 _ = happyReduce_66

action_164 (67) = happyShift action_172
action_164 _ = happyFail (happyExpListPerState 164)

action_165 _ = happyReduce_73

action_166 (72) = happyShift action_171
action_166 _ = happyFail (happyExpListPerState 166)

action_167 _ = happyReduce_70

action_168 _ = happyReduce_72

action_169 _ = happyReduce_67

action_170 _ = happyReduce_62

action_171 (50) = happyShift action_9
action_171 (51) = happyShift action_120
action_171 (52) = happyShift action_29
action_171 (70) = happyShift action_168
action_171 (41) = happyGoto action_176
action_171 (42) = happyGoto action_177
action_171 (44) = happyGoto action_165
action_171 (47) = happyGoto action_166
action_171 (49) = happyGoto action_167
action_171 _ = happyReduce_74

action_172 (50) = happyShift action_9
action_172 (51) = happyShift action_120
action_172 (52) = happyShift action_29
action_172 (61) = happyShift action_121
action_172 (63) = happyShift action_122
action_172 (64) = happyShift action_123
action_172 (72) = happyShift action_124
action_172 (34) = happyGoto action_175
action_172 (35) = happyGoto action_115
action_172 (36) = happyGoto action_116
action_172 (44) = happyGoto action_117
action_172 (47) = happyGoto action_118
action_172 (49) = happyGoto action_119
action_172 _ = happyReduce_52

action_173 _ = happyReduce_65

action_174 _ = happyReduce_36

action_175 _ = happyReduce_69

action_176 (68) = happyShift action_180
action_176 (43) = happyGoto action_179
action_176 _ = happyReduce_77

action_177 (73) = happyShift action_178
action_177 _ = happyFail (happyExpListPerState 177)

action_178 _ = happyReduce_71

action_179 _ = happyReduce_75

action_180 (50) = happyShift action_9
action_180 (51) = happyShift action_120
action_180 (52) = happyShift action_29
action_180 (70) = happyShift action_168
action_180 (41) = happyGoto action_181
action_180 (44) = happyGoto action_165
action_180 (47) = happyGoto action_166
action_180 (49) = happyGoto action_167
action_180 _ = happyFail (happyExpListPerState 180)

action_181 (68) = happyShift action_180
action_181 (43) = happyGoto action_182
action_181 _ = happyReduce_77

action_182 _ = happyReduce_76

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
	(HappyAbsSyn48  happy_var_2)
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

happyReduce_7 = happyReduce 5 8 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Contract happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_2  9 happyReduction_8
happyReduction_8 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_2 : happy_var_1
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
happyReduction_12 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn10
		 (SymDecl happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn10
		 (ClassDecl happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  10 happyReduction_14
happyReduction_14 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn10
		 (InstDecl happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  10 happyReduction_15
happyReduction_15 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn10
		 (FunDecl happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  11 happyReduction_16
happyReduction_16 (HappyAbsSyn47  happy_var_2)
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn11
		 (Field happy_var_2 happy_var_1 Nothing
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 4 12 happyReduction_17
happyReduction_17 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (DataTy happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_2  13 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  14 happyReduction_19
happyReduction_19 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_0  14 happyReduction_20
happyReduction_20  =  HappyAbsSyn13
		 ([]
	)

happyReduce_21 = happyReduce 4 15 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Constr happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 5 16 happyReduction_22
happyReduction_22 ((HappyAbsSyn45  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	(HappyAbsSyn47  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (TySym happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 10 17 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_7) `HappyStk`
	(HappyAbsSyn47  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (Class happy_var_2 happy_var_6 happy_var_7 happy_var_4 happy_var_9
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 4 18 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	(HappyAbsSyn46  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (happy_var_2 : happy_var_3
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_0  18 happyReduction_25
happyReduction_25  =  HappyAbsSyn18
		 ([]
	)

happyReduce_26 = happySpecReduce_3  19 happyReduction_26
happyReduction_26 (HappyAbsSyn18  happy_var_3)
	(HappyAbsSyn46  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2 : happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_0  19 happyReduction_27
happyReduction_27  =  HappyAbsSyn18
		 ([]
	)

happyReduce_28 = happySpecReduce_1  20 happyReduction_28
happyReduction_28 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn20
		 ([ happy_var_1 ]
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_0  20 happyReduction_29
happyReduction_29  =  HappyAbsSyn20
		 ([]
	)

happyReduce_30 = happyReduce 4 20 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (happy_var_2 : happy_var_3
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_3  21 happyReduction_31
happyReduction_31 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2 : happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_0  21 happyReduction_32
happyReduction_32  =  HappyAbsSyn20
		 ([]
	)

happyReduce_33 = happyReduce 4 22 happyReduction_33
happyReduction_33 ((HappyAbsSyn29  happy_var_4) `HappyStk`
	(HappyAbsSyn47  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn45  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (Pred happy_var_3 (happy_var_1 : happy_var_4)
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_3  23 happyReduction_34
happyReduction_34 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1 : happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_0  23 happyReduction_35
happyReduction_35  =  HappyAbsSyn23
		 ([]
	)

happyReduce_36 = happyReduce 6 24 happyReduction_36
happyReduction_36 ((HappyAbsSyn45  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (Signature happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_0  25 happyReduction_37
happyReduction_37  =  HappyAbsSyn25
		 ([]
	)

happyReduce_38 = happySpecReduce_2  25 happyReduction_38
happyReduction_38 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1 : happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  26 happyReduction_39
happyReduction_39 (HappyAbsSyn25  happy_var_3)
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2 : happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_0  26 happyReduction_40
happyReduction_40  =  HappyAbsSyn25
		 ([]
	)

happyReduce_41 = happySpecReduce_3  27 happyReduction_41
happyReduction_41 (HappyAbsSyn45  happy_var_3)
	_
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn27
		 ((happy_var_1, happy_var_3)
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happyReduce 12 28 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn31  happy_var_11) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn45  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (Instance happy_var_2 happy_var_6 happy_var_8 happy_var_4 happy_var_11
	) `HappyStk` happyRest

happyReduce_43 = happyReduce 4 29 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	(HappyAbsSyn45  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 (happy_var_2 : happy_var_3
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_0  29 happyReduction_44
happyReduction_44  =  HappyAbsSyn29
		 ([]
	)

happyReduce_45 = happySpecReduce_3  30 happyReduction_45
happyReduction_45 (HappyAbsSyn29  happy_var_3)
	(HappyAbsSyn45  happy_var_2)
	_
	 =  HappyAbsSyn29
		 (happy_var_2 : happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_0  30 happyReduction_46
happyReduction_46  =  HappyAbsSyn29
		 ([]
	)

happyReduce_47 = happySpecReduce_2  31 happyReduction_47
happyReduction_47 (HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1 : happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_0  31 happyReduction_48
happyReduction_48  =  HappyAbsSyn31
		 ([]
	)

happyReduce_49 = happyReduce 8 32 happyReduction_49
happyReduction_49 ((HappyAbsSyn33  happy_var_8) `HappyStk`
	(HappyAbsSyn45  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (FunDef happy_var_2 happy_var_7 happy_var_4 happy_var_8
	) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_3  33 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn33
		 (happy_var_2
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  34 happyReduction_51
happyReduction_51 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1 : happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_0  34 happyReduction_52
happyReduction_52  =  HappyAbsSyn33
		 ([]
	)

happyReduce_53 = happySpecReduce_3  35 happyReduction_53
happyReduction_53 (HappyAbsSyn36  happy_var_3)
	_
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1 := happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  35 happyReduction_54
happyReduction_54 (HappyAbsSyn33  happy_var_3)
	(HappyAbsSyn36  happy_var_2)
	_
	 =  HappyAbsSyn35
		 (If happy_var_2 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  35 happyReduction_55
happyReduction_55 (HappyAbsSyn33  happy_var_3)
	(HappyAbsSyn36  happy_var_2)
	_
	 =  HappyAbsSyn35
		 (While happy_var_2 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  35 happyReduction_56
happyReduction_56 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn35
		 (StmtExp happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  36 happyReduction_57
happyReduction_57 (HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn36
		 (Var happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happyReduce 4 36 happyReduction_58
happyReduction_58 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 (Con happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_59 = happySpecReduce_1  36 happyReduction_59
happyReduction_59 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn36
		 (Lit happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  36 happyReduction_60
happyReduction_60 _
	(HappyAbsSyn36  happy_var_2)
	_
	 =  HappyAbsSyn36
		 (happy_var_2
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happyReduce 4 36 happyReduction_61
happyReduction_61 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 (Call happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_62 = happyReduce 5 36 happyReduction_62
happyReduction_62 (_ `HappyStk`
	(HappyAbsSyn39  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 (Switch happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_63 = happySpecReduce_0  37 happyReduction_63
happyReduction_63  =  HappyAbsSyn37
		 ([]
	)

happyReduce_64 = happySpecReduce_2  37 happyReduction_64
happyReduction_64 (HappyAbsSyn37  happy_var_2)
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1 : happy_var_2
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  38 happyReduction_65
happyReduction_65 (HappyAbsSyn37  happy_var_3)
	(HappyAbsSyn36  happy_var_2)
	_
	 =  HappyAbsSyn37
		 (happy_var_2 : happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_0  38 happyReduction_66
happyReduction_66  =  HappyAbsSyn37
		 ([]
	)

happyReduce_67 = happySpecReduce_2  39 happyReduction_67
happyReduction_67 (HappyAbsSyn39  happy_var_2)
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1 : happy_var_2
	)
happyReduction_67 _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_0  39 happyReduction_68
happyReduction_68  =  HappyAbsSyn39
		 ([]
	)

happyReduce_69 = happyReduce 4 40 happyReduction_69
happyReduction_69 ((HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn40
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_70 = happySpecReduce_1  41 happyReduction_70
happyReduction_70 (HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn41
		 (PVar happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happyReduce 4 41 happyReduction_71
happyReduction_71 (_ `HappyStk`
	(HappyAbsSyn42  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn41
		 (PCon happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_72 = happySpecReduce_1  41 happyReduction_72
happyReduction_72 _
	 =  HappyAbsSyn41
		 (PWildcard
	)

happyReduce_73 = happySpecReduce_1  41 happyReduction_73
happyReduction_73 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn41
		 (PLit happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_0  42 happyReduction_74
happyReduction_74  =  HappyAbsSyn42
		 ([]
	)

happyReduce_75 = happySpecReduce_2  42 happyReduction_75
happyReduction_75 (HappyAbsSyn42  happy_var_2)
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1 : happy_var_2
	)
happyReduction_75 _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_3  43 happyReduction_76
happyReduction_76 (HappyAbsSyn42  happy_var_3)
	(HappyAbsSyn41  happy_var_2)
	_
	 =  HappyAbsSyn42
		 (happy_var_2 : happy_var_3
	)
happyReduction_76 _ _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_0  43 happyReduction_77
happyReduction_77  =  HappyAbsSyn42
		 ([]
	)

happyReduce_78 = happySpecReduce_1  44 happyReduction_78
happyReduction_78 (HappyTerminal (Token _ (TNumber happy_var_1)))
	 =  HappyAbsSyn44
		 (IntLit $ toInteger happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_2  45 happyReduction_79
happyReduction_79 (HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn45
		 (TyCon happy_var_1 happy_var_2
	)
happyReduction_79 _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  45 happyReduction_80
happyReduction_80 (HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn45
		 (TyVar  happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  46 happyReduction_81
happyReduction_81 (HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn46
		 (TVar happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  47 happyReduction_82
happyReduction_82 (HappyTerminal (Token _ (TTycon happy_var_1)))
	 =  HappyAbsSyn47
		 (Name happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  48 happyReduction_83
happyReduction_83 (HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn48
		 ([happy_var_1]
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_3  48 happyReduction_84
happyReduction_84 (HappyAbsSyn47  happy_var_3)
	_
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_3 : happy_var_1
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  49 happyReduction_85
happyReduction_85 (HappyTerminal (Token _ (TIdent happy_var_1)))
	 =  HappyAbsSyn47
		 (Name happy_var_1
	)
happyReduction_85 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 79 79 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Token _ (TIdent happy_dollar_dollar) -> cont 50;
	Token _ (TNumber happy_dollar_dollar) -> cont 51;
	Token _ (TTycon happy_dollar_dollar) -> cont 52;
	Token _ TContract -> cont 53;
	Token _ TImport -> cont 54;
	Token _ TEq -> cont 55;
	Token _ TDot -> cont 56;
	Token _ TClass -> cont 57;
	Token _ TInstance -> cont 58;
	Token _ TData -> cont 59;
	Token _ TType -> cont 60;
	Token _ TSwitch -> cont 61;
	Token _ TCase -> cont 62;
	Token _ TIf -> cont 63;
	Token _ TWhile -> cont 64;
	Token _ TFunction -> cont 65;
	Token _ TSemi -> cont 66;
	Token _ TColon -> cont 67;
	Token _ TComma -> cont 68;
	Token _ TArrow -> cont 69;
	Token _ TWildCard -> cont 70;
	Token _ TDArrow -> cont 71;
	Token _ TLParen -> cont 72;
	Token _ TRParen -> cont 73;
	Token _ TLBrace -> cont 74;
	Token _ TRBrace -> cont 75;
	Token _ TLBrack -> cont 76;
	Token _ TRBrack -> cont 77;
	Token _ TBar -> cont 78;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 79 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = ((>>=))
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> P a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> P a
happyError' = (\(tokens, _) -> parseError tokens)
parser tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type P = Either String

parseError :: [Token] -> P a
parseError ts 
  = Left $ "syntax error at " <> (pErr ts)
  where  
    loc (x,y) = unwords ["line:", show x, "column:", show y]
    pErr (t : _) = loc (pos t)
    pErr _ = "EOF"


solCoreParser :: String -> Either String CompUnit
solCoreParser = parser . lexer
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
