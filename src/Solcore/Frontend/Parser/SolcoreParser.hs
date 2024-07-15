{-# OPTIONS_GHC -w #-}
module Solcore.Frontend.Parser.SolcoreParser where

import Solcore.Frontend.Lexer.SolcoreLexer hiding (lexer)
import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty
import Solcore.Primitives.Primitives
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (CompUnit Name)
	| HappyAbsSyn5 ([Import])
	| HappyAbsSyn6 (Import)
	| HappyAbsSyn7 ([Contract Name])
	| HappyAbsSyn8 (Contract Name)
	| HappyAbsSyn9 ([Decl Name])
	| HappyAbsSyn10 (Decl Name)
	| HappyAbsSyn11 (Field Name)
	| HappyAbsSyn12 (DataTy)
	| HappyAbsSyn13 ([Constr])
	| HappyAbsSyn14 (Constr)
	| HappyAbsSyn15 (TySym)
	| HappyAbsSyn16 (Class Name)
	| HappyAbsSyn17 ([Signature Name])
	| HappyAbsSyn18 ([Tyvar])
	| HappyAbsSyn20 ([Pred])
	| HappyAbsSyn23 (Pred)
	| HappyAbsSyn25 (Signature Name)
	| HappyAbsSyn27 ([Param Name])
	| HappyAbsSyn28 (Param Name)
	| HappyAbsSyn29 (Instance Name)
	| HappyAbsSyn30 ([Ty])
	| HappyAbsSyn32 ([FunDef Name])
	| HappyAbsSyn34 (FunDef Name)
	| HappyAbsSyn35 (Maybe Ty)
	| HappyAbsSyn36 (Constructor Name)
	| HappyAbsSyn37 ([Stmt Name])
	| HappyAbsSyn39 (Stmt Name)
	| HappyAbsSyn40 ([Exp Name])
	| HappyAbsSyn41 (Maybe (Exp Name))
	| HappyAbsSyn42 (Exp Name)
	| HappyAbsSyn46 ([([Pat], [Stmt Name])])
	| HappyAbsSyn47 (([Pat], [Stmt Name]))
	| HappyAbsSyn48 ([Pat])
	| HappyAbsSyn49 (Pat)
	| HappyAbsSyn52 (Literal)
	| HappyAbsSyn53 (Ty)
	| HappyAbsSyn54 (([Ty], Ty))
	| HappyAbsSyn55 (Tyvar)
	| HappyAbsSyn56 (Name)
	| HappyAbsSyn57 ([Name])
	| HappyAbsSyn59 (YulBlock)
	| HappyAbsSyn61 ([YulStmt])
	| HappyAbsSyn62 (YulStmt)
	| HappyAbsSyn63 (YulFor)
	| HappyAbsSyn65 (YulCases)
	| HappyAbsSyn66 ((Literal, YulBlock))
	| HappyAbsSyn67 (Maybe YulBlock)
	| HappyAbsSyn72 (YulExp)
	| HappyAbsSyn73 ([YulExp])

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
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237,
 action_238,
 action_239,
 action_240,
 action_241,
 action_242,
 action_243,
 action_244,
 action_245,
 action_246,
 action_247,
 action_248,
 action_249,
 action_250,
 action_251,
 action_252,
 action_253,
 action_254,
 action_255,
 action_256,
 action_257,
 action_258,
 action_259,
 action_260,
 action_261,
 action_262,
 action_263,
 action_264,
 action_265,
 action_266,
 action_267,
 action_268 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
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
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122,
 happyReduce_123,
 happyReduce_124,
 happyReduce_125,
 happyReduce_126,
 happyReduce_127,
 happyReduce_128,
 happyReduce_129,
 happyReduce_130,
 happyReduce_131,
 happyReduce_132,
 happyReduce_133,
 happyReduce_134,
 happyReduce_135,
 happyReduce_136,
 happyReduce_137,
 happyReduce_138,
 happyReduce_139,
 happyReduce_140,
 happyReduce_141,
 happyReduce_142 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,556) ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6148,1728,0,0,0,0,0,0,0,2048,0,0,0,0,1024,49176,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,256,0,0,0,0,0,0,4096,0,0,0,0,4096,0,0,0,0,0,0,0,1,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,256,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,16,0,0,0,0,0,0,256,0,0,0,0,320,0,256,0,0,0,0,0,0,0,0,0,0,0,16384,1,0,1,0,0,0,0,4,0,0,0,0,0,0,320,0,256,0,0,0,0,0,0,0,0,0,0,0,49152,19,402,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,512,0,0,0,0,0,1536,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,64,0,0,0,0,960,0,257,0,0,0,0,15360,0,4112,0,0,0,0,0,0,0,1,0,0,0,0,60,4096,16,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,320,0,256,0,0,0,0,0,0,128,0,0,0,0,0,0,0,32,0,0,0,0,0,0,1,0,0,0,0,0,0,8,0,0,0,0,0,0,128,0,0,0,0,0,32,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,256,0,0,0,0,5120,0,4096,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1,0,0,0,0,0,0,8,0,0,0,0,5120,0,4096,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,8192,0,0,0,0,1024,0,0,0,0,0,0,16384,1,0,1,0,0,0,0,16,0,0,0,0,0,0,256,0,0,0,0,0,0,4096,0,0,0,0,0,0,16384,1,0,1,0,0,0,0,0,0,8,0,0,0,0,256,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,320,0,256,0,0,0,0,0,0,32,0,0,0,0,49152,3,256,1,0,0,0,0,1024,0,32,0,0,0,0,64,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,4,0,0,0,0,1024,0,1,0,0,0,0,0,0,0,0,0,0,0,11264,7393,16384,0,0,0,0,0,32,2048,0,0,0,0,0,0,0,0,0,0,0,0,960,0,257,0,0,0,0,0,0,0,0,0,0,0,49152,3,256,1,0,0,0,0,60,4096,16,0,0,0,0,64,0,0,0,0,0,0,15360,8193,4121,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,4,0,0,0,0,0,0,64,4096,0,0,0,0,0,0,0,512,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,16384,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,272,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,704,0,0,0,0,0,0,0,0,16384,0,0,0,0,49152,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,3,256,1,0,0,0,0,0,0,1024,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,5120,0,4096,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,20,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,128,0,0,0,0,0,0,16384,0,0,0,0,15360,0,5120,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,16,0,0,0,0,0,11264,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,1,0,0,0,0,0,0,4,0,0,0,0,11264,0,0,0,0,0,0,49152,52754,1,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,49152,2,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,3,256,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,44,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,8192,0,0,0,0,0,0,0,1,0,0,0,0,0,640,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,60,0,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,2,0,0,0,0,0,0,0,8,0,0,0,0,0,8192,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,49152,3,16384,1,0,0,0,0,60,0,20,0,0,0,0,5056,37376,257,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,704,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,960,0,320,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","CompilationUnit","ImportList","Import","ContractList","Contract","DeclList","Decl","FieldDef","DataDef","Constrs","Constr","SymDef","ClassDef","ClassBody","OptParam","VarCommaList","ContextOpt","Context","ConstraintList","Constraint","Signatures","Signature","ConOpt","ParamList","Param","InstDef","OptTypeParam","TypeCommaList","Functions","InstBody","Function","OptRetTy","Constructor","Body","StmtList","Stmt","MatchArgList","InitOpt","Expr","ConArgs","FunArgs","ExprCommaList","Equations","Equation","PatCommaList","Pattern","PatternList","PatList","Literal","Type","LamType","Var","Con","QualName","Name","AsmBlock","YulBlock","YulStmts","YulStmt","YulFor","YulSwitch","YulCases","YulCase","YulDefault","YulIf","YulVarDecl","YulAssignment","IdentifierList","YulExp","YulFunArgs","YulExpCommaList","identifier","number","tycon","stringlit","'contract'","'import'","'let'","'='","'.'","'class'","'instance'","'if'","'for'","'switch'","'case'","'default'","'leave'","'continue'","'break'","'assembly'","'data'","'type'","'match'","'function'","'constructor'","'return'","'lam'","';'","':='","':'","','","'->'","'_'","'=>'","'('","')'","'{'","'}'","'['","']'","'|'","%eof"]
        bit_start = st Prelude.* 116
        bit_end = (st Prelude.+ 1) Prelude.* 116
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..115]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_3

action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (80) = happyShift action_6
action_2 (6) = happyGoto action_4
action_2 (7) = happyGoto action_5
action_2 _ = happyReduce_6

action_3 (116) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_2

action_5 (79) = happyShift action_11
action_5 (8) = happyGoto action_10
action_5 _ = happyReduce_1

action_6 (77) = happyShift action_9
action_6 (56) = happyGoto action_7
action_6 (57) = happyGoto action_8
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_107

action_8 (83) = happyShift action_13
action_8 (102) = happyShift action_14
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_106

action_10 _ = happyReduce_5

action_11 (77) = happyShift action_9
action_11 (56) = happyGoto action_12
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (113) = happyShift action_17
action_12 (18) = happyGoto action_16
action_12 _ = happyReduce_26

action_13 (77) = happyShift action_9
action_13 (56) = happyGoto action_15
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_4

action_15 _ = happyReduce_108

action_16 (111) = happyShift action_22
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (75) = happyShift action_21
action_17 (19) = happyGoto action_18
action_17 (55) = happyGoto action_19
action_17 (58) = happyGoto action_20
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (114) = happyShift action_41
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (105) = happyShift action_40
action_19 _ = happyReduce_28

action_20 _ = happyReduce_105

action_21 _ = happyReduce_109

action_22 (75) = happyShift action_21
action_22 (84) = happyShift action_34
action_22 (85) = happyShift action_35
action_22 (95) = happyShift action_36
action_22 (96) = happyShift action_37
action_22 (98) = happyShift action_38
action_22 (99) = happyShift action_39
action_22 (9) = happyGoto action_23
action_22 (10) = happyGoto action_24
action_22 (11) = happyGoto action_25
action_22 (12) = happyGoto action_26
action_22 (15) = happyGoto action_27
action_22 (16) = happyGoto action_28
action_22 (25) = happyGoto action_29
action_22 (29) = happyGoto action_30
action_22 (34) = happyGoto action_31
action_22 (36) = happyGoto action_32
action_22 (58) = happyGoto action_33
action_22 _ = happyReduce_9

action_23 (112) = happyShift action_55
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (75) = happyShift action_21
action_24 (84) = happyShift action_34
action_24 (85) = happyShift action_35
action_24 (95) = happyShift action_36
action_24 (96) = happyShift action_37
action_24 (98) = happyShift action_38
action_24 (99) = happyShift action_39
action_24 (9) = happyGoto action_54
action_24 (10) = happyGoto action_24
action_24 (11) = happyGoto action_25
action_24 (12) = happyGoto action_26
action_24 (15) = happyGoto action_27
action_24 (16) = happyGoto action_28
action_24 (25) = happyGoto action_29
action_24 (29) = happyGoto action_30
action_24 (34) = happyGoto action_31
action_24 (36) = happyGoto action_32
action_24 (58) = happyGoto action_33
action_24 _ = happyReduce_9

action_25 _ = happyReduce_10

action_26 _ = happyReduce_11

action_27 _ = happyReduce_12

action_28 _ = happyReduce_13

action_29 (111) = happyShift action_53
action_29 (37) = happyGoto action_52
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_14

action_31 _ = happyReduce_15

action_32 _ = happyReduce_16

action_33 (104) = happyShift action_51
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (113) = happyShift action_49
action_34 (20) = happyGoto action_50
action_34 (21) = happyGoto action_48
action_34 _ = happyReduce_29

action_35 (113) = happyShift action_49
action_35 (20) = happyGoto action_47
action_35 (21) = happyGoto action_48
action_35 _ = happyReduce_29

action_36 (77) = happyShift action_9
action_36 (56) = happyGoto action_46
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (77) = happyShift action_9
action_37 (56) = happyGoto action_45
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (75) = happyShift action_21
action_38 (58) = happyGoto action_44
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (109) = happyShift action_43
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (75) = happyShift action_21
action_40 (19) = happyGoto action_42
action_40 (55) = happyGoto action_19
action_40 (58) = happyGoto action_20
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_25

action_42 _ = happyReduce_27

action_43 (75) = happyShift action_21
action_43 (27) = happyGoto action_85
action_43 (28) = happyGoto action_86
action_43 (58) = happyGoto action_87
action_43 _ = happyReduce_42

action_44 (113) = happyShift action_84
action_44 (26) = happyGoto action_83
action_44 _ = happyReduce_38

action_45 (113) = happyShift action_17
action_45 (18) = happyGoto action_82
action_45 _ = happyReduce_26

action_46 (113) = happyShift action_17
action_46 (18) = happyGoto action_81
action_46 _ = happyReduce_26

action_47 (75) = happyShift action_21
action_47 (77) = happyShift action_9
action_47 (109) = happyShift action_75
action_47 (53) = happyGoto action_80
action_47 (54) = happyGoto action_72
action_47 (55) = happyGoto action_73
action_47 (56) = happyGoto action_74
action_47 (58) = happyGoto action_20
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_30

action_49 (75) = happyShift action_21
action_49 (77) = happyShift action_9
action_49 (109) = happyShift action_75
action_49 (22) = happyGoto action_77
action_49 (23) = happyGoto action_78
action_49 (53) = happyGoto action_79
action_49 (54) = happyGoto action_72
action_49 (55) = happyGoto action_73
action_49 (56) = happyGoto action_74
action_49 (58) = happyGoto action_20
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (75) = happyShift action_21
action_50 (55) = happyGoto action_76
action_50 (58) = happyGoto action_20
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (75) = happyShift action_21
action_51 (77) = happyShift action_9
action_51 (109) = happyShift action_75
action_51 (53) = happyGoto action_71
action_51 (54) = happyGoto action_72
action_51 (55) = happyGoto action_73
action_51 (56) = happyGoto action_74
action_51 (58) = happyGoto action_20
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_53

action_53 (75) = happyShift action_21
action_53 (76) = happyShift action_63
action_53 (77) = happyShift action_9
action_53 (78) = happyShift action_64
action_53 (81) = happyShift action_65
action_53 (94) = happyShift action_66
action_53 (97) = happyShift action_67
action_53 (100) = happyShift action_68
action_53 (101) = happyShift action_69
action_53 (109) = happyShift action_70
action_53 (38) = happyGoto action_56
action_53 (39) = happyGoto action_57
action_53 (42) = happyGoto action_58
action_53 (52) = happyGoto action_59
action_53 (56) = happyGoto action_60
action_53 (58) = happyGoto action_61
action_53 (59) = happyGoto action_62
action_53 _ = happyReduce_59

action_54 _ = happyReduce_8

action_55 _ = happyReduce_7

action_56 (112) = happyShift action_121
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (102) = happyShift action_120
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (82) = happyShift action_118
action_58 (83) = happyShift action_119
action_58 _ = happyReduce_63

action_59 _ = happyReduce_73

action_60 (113) = happyShift action_117
action_60 (43) = happyGoto action_116
action_60 _ = happyReduce_80

action_61 (109) = happyShift action_115
action_61 (44) = happyGoto action_114
action_61 _ = happyReduce_71

action_62 _ = happyReduce_66

action_63 _ = happyReduce_99

action_64 _ = happyReduce_100

action_65 (75) = happyShift action_21
action_65 (58) = happyGoto action_113
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (111) = happyShift action_112
action_66 (60) = happyGoto action_111
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (75) = happyShift action_21
action_67 (76) = happyShift action_63
action_67 (77) = happyShift action_9
action_67 (78) = happyShift action_64
action_67 (101) = happyShift action_69
action_67 (109) = happyShift action_70
action_67 (40) = happyGoto action_109
action_67 (42) = happyGoto action_110
action_67 (52) = happyGoto action_59
action_67 (56) = happyGoto action_60
action_67 (58) = happyGoto action_61
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (75) = happyShift action_21
action_68 (76) = happyShift action_63
action_68 (77) = happyShift action_9
action_68 (78) = happyShift action_64
action_68 (101) = happyShift action_69
action_68 (109) = happyShift action_70
action_68 (42) = happyGoto action_108
action_68 (52) = happyGoto action_59
action_68 (56) = happyGoto action_60
action_68 (58) = happyGoto action_61
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (109) = happyShift action_107
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (75) = happyShift action_21
action_70 (76) = happyShift action_63
action_70 (77) = happyShift action_9
action_70 (78) = happyShift action_64
action_70 (101) = happyShift action_69
action_70 (109) = happyShift action_70
action_70 (42) = happyGoto action_106
action_70 (52) = happyGoto action_59
action_70 (56) = happyGoto action_60
action_70 (58) = happyGoto action_61
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (82) = happyShift action_105
action_71 (41) = happyGoto action_104
action_71 _ = happyReduce_69

action_72 _ = happyReduce_103

action_73 _ = happyReduce_102

action_74 (113) = happyShift action_103
action_74 (30) = happyGoto action_102
action_74 _ = happyReduce_47

action_75 (75) = happyShift action_21
action_75 (77) = happyShift action_9
action_75 (109) = happyShift action_75
action_75 (31) = happyGoto action_100
action_75 (53) = happyGoto action_101
action_75 (54) = happyGoto action_72
action_75 (55) = happyGoto action_73
action_75 (56) = happyGoto action_74
action_75 (58) = happyGoto action_20
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (104) = happyShift action_99
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (114) = happyShift action_98
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (105) = happyShift action_97
action_78 _ = happyReduce_33

action_79 (104) = happyShift action_96
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (104) = happyShift action_95
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (82) = happyShift action_94
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (82) = happyShift action_93
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (109) = happyShift action_92
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (75) = happyShift action_21
action_84 (77) = happyShift action_9
action_84 (109) = happyShift action_75
action_84 (22) = happyGoto action_91
action_84 (23) = happyGoto action_78
action_84 (53) = happyGoto action_79
action_84 (54) = happyGoto action_72
action_84 (55) = happyGoto action_73
action_84 (56) = happyGoto action_74
action_84 (58) = happyGoto action_20
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (110) = happyShift action_90
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (105) = happyShift action_89
action_86 _ = happyReduce_40

action_87 (104) = happyShift action_88
action_87 _ = happyReduce_44

action_88 (75) = happyShift action_21
action_88 (77) = happyShift action_9
action_88 (109) = happyShift action_75
action_88 (53) = happyGoto action_171
action_88 (54) = happyGoto action_72
action_88 (55) = happyGoto action_73
action_88 (56) = happyGoto action_74
action_88 (58) = happyGoto action_20
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (75) = happyShift action_21
action_89 (27) = happyGoto action_170
action_89 (28) = happyGoto action_86
action_89 (58) = happyGoto action_87
action_89 _ = happyReduce_42

action_90 (111) = happyShift action_53
action_90 (37) = happyGoto action_169
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (114) = happyShift action_168
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (75) = happyShift action_21
action_92 (27) = happyGoto action_167
action_92 (28) = happyGoto action_86
action_92 (58) = happyGoto action_87
action_92 _ = happyReduce_42

action_93 (75) = happyShift action_21
action_93 (77) = happyShift action_9
action_93 (109) = happyShift action_75
action_93 (53) = happyGoto action_166
action_93 (54) = happyGoto action_72
action_93 (55) = happyGoto action_73
action_93 (56) = happyGoto action_74
action_93 (58) = happyGoto action_20
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (77) = happyShift action_9
action_94 (13) = happyGoto action_163
action_94 (14) = happyGoto action_164
action_94 (56) = happyGoto action_165
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (77) = happyShift action_9
action_95 (56) = happyGoto action_162
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (77) = happyShift action_9
action_96 (56) = happyGoto action_161
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (75) = happyShift action_21
action_97 (77) = happyShift action_9
action_97 (109) = happyShift action_75
action_97 (22) = happyGoto action_160
action_97 (23) = happyGoto action_78
action_97 (53) = happyGoto action_79
action_97 (54) = happyGoto action_72
action_97 (55) = happyGoto action_73
action_97 (56) = happyGoto action_74
action_97 (58) = happyGoto action_20
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (108) = happyShift action_159
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (77) = happyShift action_9
action_99 (56) = happyGoto action_158
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (110) = happyShift action_157
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (105) = happyShift action_156
action_101 _ = happyReduce_49

action_102 _ = happyReduce_101

action_103 (75) = happyShift action_21
action_103 (77) = happyShift action_9
action_103 (109) = happyShift action_75
action_103 (31) = happyGoto action_155
action_103 (53) = happyGoto action_101
action_103 (54) = happyGoto action_72
action_103 (55) = happyGoto action_73
action_103 (56) = happyGoto action_74
action_103 (58) = happyGoto action_20
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (102) = happyShift action_154
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (75) = happyShift action_21
action_105 (76) = happyShift action_63
action_105 (77) = happyShift action_9
action_105 (78) = happyShift action_64
action_105 (101) = happyShift action_69
action_105 (109) = happyShift action_70
action_105 (42) = happyGoto action_153
action_105 (52) = happyGoto action_59
action_105 (56) = happyGoto action_60
action_105 (58) = happyGoto action_61
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (83) = happyShift action_119
action_106 (110) = happyShift action_152
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (75) = happyShift action_21
action_107 (27) = happyGoto action_151
action_107 (28) = happyGoto action_86
action_107 (58) = happyGoto action_87
action_107 _ = happyReduce_42

action_108 (83) = happyShift action_119
action_108 _ = happyReduce_64

action_109 (111) = happyShift action_150
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (83) = happyShift action_119
action_110 (105) = happyShift action_149
action_110 _ = happyReduce_67

action_111 _ = happyReduce_110

action_112 (75) = happyShift action_21
action_112 (76) = happyShift action_63
action_112 (78) = happyShift action_64
action_112 (81) = happyShift action_142
action_112 (86) = happyShift action_143
action_112 (87) = happyShift action_144
action_112 (88) = happyShift action_145
action_112 (91) = happyShift action_146
action_112 (92) = happyShift action_147
action_112 (93) = happyShift action_148
action_112 (111) = happyShift action_112
action_112 (112) = happyReduce_113
action_112 (52) = happyGoto action_130
action_112 (58) = happyGoto action_131
action_112 (60) = happyGoto action_132
action_112 (61) = happyGoto action_133
action_112 (62) = happyGoto action_134
action_112 (63) = happyGoto action_135
action_112 (64) = happyGoto action_136
action_112 (68) = happyGoto action_137
action_112 (69) = happyGoto action_138
action_112 (70) = happyGoto action_139
action_112 (71) = happyGoto action_140
action_112 (72) = happyGoto action_141
action_112 _ = happyReduce_134

action_113 (82) = happyShift action_105
action_113 (104) = happyShift action_129
action_113 (41) = happyGoto action_128
action_113 _ = happyReduce_69

action_114 _ = happyReduce_77

action_115 (75) = happyShift action_21
action_115 (76) = happyShift action_63
action_115 (77) = happyShift action_9
action_115 (78) = happyShift action_64
action_115 (101) = happyShift action_69
action_115 (109) = happyShift action_70
action_115 (42) = happyGoto action_125
action_115 (45) = happyGoto action_127
action_115 (52) = happyGoto action_59
action_115 (56) = happyGoto action_60
action_115 (58) = happyGoto action_61
action_115 _ = happyReduce_83

action_116 _ = happyReduce_72

action_117 (75) = happyShift action_21
action_117 (76) = happyShift action_63
action_117 (77) = happyShift action_9
action_117 (78) = happyShift action_64
action_117 (101) = happyShift action_69
action_117 (109) = happyShift action_70
action_117 (42) = happyGoto action_125
action_117 (45) = happyGoto action_126
action_117 (52) = happyGoto action_59
action_117 (56) = happyGoto action_60
action_117 (58) = happyGoto action_61
action_117 _ = happyReduce_83

action_118 (75) = happyShift action_21
action_118 (76) = happyShift action_63
action_118 (77) = happyShift action_9
action_118 (78) = happyShift action_64
action_118 (101) = happyShift action_69
action_118 (109) = happyShift action_70
action_118 (42) = happyGoto action_124
action_118 (52) = happyGoto action_59
action_118 (56) = happyGoto action_60
action_118 (58) = happyGoto action_61
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (75) = happyShift action_21
action_119 (58) = happyGoto action_123
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (75) = happyShift action_21
action_120 (76) = happyShift action_63
action_120 (77) = happyShift action_9
action_120 (78) = happyShift action_64
action_120 (81) = happyShift action_65
action_120 (94) = happyShift action_66
action_120 (97) = happyShift action_67
action_120 (100) = happyShift action_68
action_120 (101) = happyShift action_69
action_120 (109) = happyShift action_70
action_120 (38) = happyGoto action_122
action_120 (39) = happyGoto action_57
action_120 (42) = happyGoto action_58
action_120 (52) = happyGoto action_59
action_120 (56) = happyGoto action_60
action_120 (58) = happyGoto action_61
action_120 (59) = happyGoto action_62
action_120 _ = happyReduce_59

action_121 _ = happyReduce_57

action_122 _ = happyReduce_58

action_123 (109) = happyShift action_115
action_123 (44) = happyGoto action_202
action_123 _ = happyReduce_75

action_124 (83) = happyShift action_119
action_124 _ = happyReduce_60

action_125 (83) = happyShift action_119
action_125 (105) = happyShift action_201
action_125 _ = happyReduce_82

action_126 (114) = happyShift action_200
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (110) = happyShift action_199
action_127 _ = happyFail (happyExpListPerState 127)

action_128 _ = happyReduce_62

action_129 (75) = happyShift action_21
action_129 (77) = happyShift action_9
action_129 (109) = happyShift action_75
action_129 (53) = happyGoto action_198
action_129 (54) = happyGoto action_72
action_129 (55) = happyGoto action_73
action_129 (56) = happyGoto action_74
action_129 (58) = happyGoto action_20
action_129 _ = happyFail (happyExpListPerState 129)

action_130 _ = happyReduce_136

action_131 (105) = happyShift action_196
action_131 (109) = happyShift action_197
action_131 (73) = happyGoto action_195
action_131 _ = happyReduce_137

action_132 _ = happyReduce_115

action_133 (112) = happyShift action_194
action_133 _ = happyFail (happyExpListPerState 133)

action_134 (102) = happyShift action_193
action_134 _ = happyFail (happyExpListPerState 134)

action_135 _ = happyReduce_120

action_136 _ = happyReduce_119

action_137 _ = happyReduce_118

action_138 _ = happyReduce_116

action_139 _ = happyReduce_114

action_140 (103) = happyShift action_192
action_140 _ = happyFail (happyExpListPerState 140)

action_141 _ = happyReduce_117

action_142 (75) = happyShift action_21
action_142 (58) = happyGoto action_190
action_142 (71) = happyGoto action_191
action_142 _ = happyReduce_134

action_143 (75) = happyShift action_21
action_143 (76) = happyShift action_63
action_143 (78) = happyShift action_64
action_143 (52) = happyGoto action_130
action_143 (58) = happyGoto action_186
action_143 (72) = happyGoto action_189
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (111) = happyShift action_112
action_144 (60) = happyGoto action_188
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (75) = happyShift action_21
action_145 (76) = happyShift action_63
action_145 (78) = happyShift action_64
action_145 (52) = happyGoto action_130
action_145 (58) = happyGoto action_186
action_145 (72) = happyGoto action_187
action_145 _ = happyFail (happyExpListPerState 145)

action_146 _ = happyReduce_123

action_147 _ = happyReduce_121

action_148 _ = happyReduce_122

action_149 (75) = happyShift action_21
action_149 (76) = happyShift action_63
action_149 (77) = happyShift action_9
action_149 (78) = happyShift action_64
action_149 (101) = happyShift action_69
action_149 (109) = happyShift action_70
action_149 (40) = happyGoto action_185
action_149 (42) = happyGoto action_110
action_149 (52) = happyGoto action_59
action_149 (56) = happyGoto action_60
action_149 (58) = happyGoto action_61
action_149 _ = happyFail (happyExpListPerState 149)

action_150 (115) = happyShift action_184
action_150 (46) = happyGoto action_182
action_150 (47) = happyGoto action_183
action_150 _ = happyReduce_86

action_151 (110) = happyShift action_181
action_151 _ = happyFail (happyExpListPerState 151)

action_152 _ = happyReduce_74

action_153 (83) = happyShift action_119
action_153 _ = happyReduce_70

action_154 _ = happyReduce_17

action_155 (114) = happyShift action_180
action_155 _ = happyFail (happyExpListPerState 155)

action_156 (75) = happyShift action_21
action_156 (77) = happyShift action_9
action_156 (109) = happyShift action_75
action_156 (31) = happyGoto action_179
action_156 (53) = happyGoto action_101
action_156 (54) = happyGoto action_72
action_156 (55) = happyGoto action_73
action_156 (56) = happyGoto action_74
action_156 (58) = happyGoto action_20
action_156 _ = happyFail (happyExpListPerState 156)

action_157 (106) = happyShift action_178
action_157 _ = happyFail (happyExpListPerState 157)

action_158 (113) = happyShift action_17
action_158 (18) = happyGoto action_177
action_158 _ = happyReduce_26

action_159 _ = happyReduce_31

action_160 _ = happyReduce_32

action_161 (113) = happyShift action_103
action_161 (30) = happyGoto action_176
action_161 _ = happyReduce_47

action_162 (113) = happyShift action_103
action_162 (30) = happyGoto action_175
action_162 _ = happyReduce_47

action_163 _ = happyReduce_18

action_164 (115) = happyShift action_174
action_164 _ = happyReduce_20

action_165 (113) = happyShift action_103
action_165 (30) = happyGoto action_173
action_165 _ = happyReduce_47

action_166 _ = happyReduce_22

action_167 (110) = happyShift action_172
action_167 _ = happyFail (happyExpListPerState 167)

action_168 _ = happyReduce_39

action_169 _ = happyReduce_56

action_170 _ = happyReduce_41

action_171 _ = happyReduce_43

action_172 (106) = happyShift action_233
action_172 (35) = happyGoto action_232
action_172 _ = happyReduce_55

action_173 _ = happyReduce_21

action_174 (77) = happyShift action_9
action_174 (13) = happyGoto action_231
action_174 (14) = happyGoto action_164
action_174 (56) = happyGoto action_165
action_174 _ = happyFail (happyExpListPerState 174)

action_175 (111) = happyShift action_230
action_175 (33) = happyGoto action_229
action_175 _ = happyFail (happyExpListPerState 175)

action_176 _ = happyReduce_34

action_177 (111) = happyShift action_228
action_177 (17) = happyGoto action_227
action_177 _ = happyFail (happyExpListPerState 177)

action_178 (75) = happyShift action_21
action_178 (77) = happyShift action_9
action_178 (109) = happyShift action_75
action_178 (53) = happyGoto action_226
action_178 (54) = happyGoto action_72
action_178 (55) = happyGoto action_73
action_178 (56) = happyGoto action_74
action_178 (58) = happyGoto action_20
action_178 _ = happyFail (happyExpListPerState 178)

action_179 _ = happyReduce_48

action_180 _ = happyReduce_46

action_181 (111) = happyShift action_53
action_181 (37) = happyGoto action_225
action_181 _ = happyFail (happyExpListPerState 181)

action_182 (112) = happyShift action_224
action_182 _ = happyFail (happyExpListPerState 182)

action_183 (115) = happyShift action_184
action_183 (46) = happyGoto action_223
action_183 (47) = happyGoto action_183
action_183 _ = happyReduce_86

action_184 (75) = happyShift action_21
action_184 (76) = happyShift action_63
action_184 (77) = happyShift action_9
action_184 (78) = happyShift action_64
action_184 (107) = happyShift action_221
action_184 (109) = happyShift action_222
action_184 (48) = happyGoto action_216
action_184 (49) = happyGoto action_217
action_184 (52) = happyGoto action_218
action_184 (56) = happyGoto action_219
action_184 (58) = happyGoto action_220
action_184 _ = happyFail (happyExpListPerState 184)

action_185 _ = happyReduce_68

action_186 (109) = happyShift action_197
action_186 (73) = happyGoto action_195
action_186 _ = happyReduce_137

action_187 (89) = happyShift action_215
action_187 (65) = happyGoto action_213
action_187 (66) = happyGoto action_214
action_187 _ = happyReduce_127

action_188 (75) = happyShift action_21
action_188 (76) = happyShift action_63
action_188 (78) = happyShift action_64
action_188 (52) = happyGoto action_130
action_188 (58) = happyGoto action_186
action_188 (72) = happyGoto action_212
action_188 _ = happyFail (happyExpListPerState 188)

action_189 (111) = happyShift action_112
action_189 (60) = happyGoto action_211
action_189 _ = happyFail (happyExpListPerState 189)

action_190 (105) = happyShift action_196
action_190 _ = happyFail (happyExpListPerState 190)

action_191 (103) = happyShift action_210
action_191 _ = happyFail (happyExpListPerState 191)

action_192 (75) = happyShift action_21
action_192 (76) = happyShift action_63
action_192 (78) = happyShift action_64
action_192 (52) = happyGoto action_130
action_192 (58) = happyGoto action_186
action_192 (72) = happyGoto action_209
action_192 _ = happyFail (happyExpListPerState 192)

action_193 (75) = happyShift action_21
action_193 (76) = happyShift action_63
action_193 (78) = happyShift action_64
action_193 (81) = happyShift action_142
action_193 (86) = happyShift action_143
action_193 (87) = happyShift action_144
action_193 (88) = happyShift action_145
action_193 (91) = happyShift action_146
action_193 (92) = happyShift action_147
action_193 (93) = happyShift action_148
action_193 (111) = happyShift action_112
action_193 (112) = happyReduce_113
action_193 (52) = happyGoto action_130
action_193 (58) = happyGoto action_131
action_193 (60) = happyGoto action_132
action_193 (61) = happyGoto action_208
action_193 (62) = happyGoto action_134
action_193 (63) = happyGoto action_135
action_193 (64) = happyGoto action_136
action_193 (68) = happyGoto action_137
action_193 (69) = happyGoto action_138
action_193 (70) = happyGoto action_139
action_193 (71) = happyGoto action_140
action_193 (72) = happyGoto action_141
action_193 _ = happyReduce_134

action_194 _ = happyReduce_111

action_195 _ = happyReduce_138

action_196 (75) = happyShift action_21
action_196 (58) = happyGoto action_190
action_196 (71) = happyGoto action_207
action_196 _ = happyReduce_134

action_197 (75) = happyShift action_21
action_197 (76) = happyShift action_63
action_197 (78) = happyShift action_64
action_197 (52) = happyGoto action_130
action_197 (58) = happyGoto action_186
action_197 (72) = happyGoto action_205
action_197 (74) = happyGoto action_206
action_197 _ = happyReduce_141

action_198 (82) = happyShift action_105
action_198 (41) = happyGoto action_204
action_198 _ = happyReduce_69

action_199 _ = happyReduce_81

action_200 _ = happyReduce_79

action_201 (75) = happyShift action_21
action_201 (76) = happyShift action_63
action_201 (77) = happyShift action_9
action_201 (78) = happyShift action_64
action_201 (101) = happyShift action_69
action_201 (109) = happyShift action_70
action_201 (42) = happyGoto action_125
action_201 (45) = happyGoto action_203
action_201 (52) = happyGoto action_59
action_201 (56) = happyGoto action_60
action_201 (58) = happyGoto action_61
action_201 _ = happyReduce_83

action_202 _ = happyReduce_76

action_203 _ = happyReduce_84

action_204 _ = happyReduce_61

action_205 (105) = happyShift action_251
action_205 _ = happyReduce_140

action_206 (110) = happyShift action_250
action_206 _ = happyFail (happyExpListPerState 206)

action_207 _ = happyReduce_135

action_208 _ = happyReduce_112

action_209 _ = happyReduce_133

action_210 (75) = happyShift action_21
action_210 (76) = happyShift action_63
action_210 (78) = happyShift action_64
action_210 (52) = happyGoto action_130
action_210 (58) = happyGoto action_186
action_210 (72) = happyGoto action_249
action_210 _ = happyFail (happyExpListPerState 210)

action_211 _ = happyReduce_131

action_212 (111) = happyShift action_112
action_212 (60) = happyGoto action_248
action_212 _ = happyFail (happyExpListPerState 212)

action_213 (90) = happyShift action_247
action_213 (67) = happyGoto action_246
action_213 _ = happyReduce_130

action_214 (89) = happyShift action_215
action_214 (65) = happyGoto action_245
action_214 (66) = happyGoto action_214
action_214 _ = happyReduce_127

action_215 (76) = happyShift action_63
action_215 (78) = happyShift action_64
action_215 (52) = happyGoto action_244
action_215 _ = happyFail (happyExpListPerState 215)

action_216 (108) = happyShift action_243
action_216 _ = happyFail (happyExpListPerState 216)

action_217 (105) = happyShift action_242
action_217 _ = happyReduce_88

action_218 _ = happyReduce_93

action_219 (113) = happyShift action_241
action_219 (50) = happyGoto action_240
action_219 _ = happyReduce_96

action_220 _ = happyReduce_90

action_221 _ = happyReduce_92

action_222 (75) = happyShift action_21
action_222 (76) = happyShift action_63
action_222 (77) = happyShift action_9
action_222 (78) = happyShift action_64
action_222 (107) = happyShift action_221
action_222 (109) = happyShift action_222
action_222 (49) = happyGoto action_239
action_222 (52) = happyGoto action_218
action_222 (56) = happyGoto action_219
action_222 (58) = happyGoto action_220
action_222 _ = happyFail (happyExpListPerState 222)

action_223 _ = happyReduce_85

action_224 _ = happyReduce_65

action_225 _ = happyReduce_78

action_226 _ = happyReduce_104

action_227 _ = happyReduce_23

action_228 (98) = happyShift action_38
action_228 (24) = happyGoto action_237
action_228 (25) = happyGoto action_238
action_228 _ = happyReduce_36

action_229 _ = happyReduce_45

action_230 (98) = happyShift action_38
action_230 (25) = happyGoto action_29
action_230 (32) = happyGoto action_235
action_230 (34) = happyGoto action_236
action_230 _ = happyReduce_51

action_231 _ = happyReduce_19

action_232 _ = happyReduce_37

action_233 (75) = happyShift action_21
action_233 (77) = happyShift action_9
action_233 (109) = happyShift action_75
action_233 (53) = happyGoto action_234
action_233 (54) = happyGoto action_72
action_233 (55) = happyGoto action_73
action_233 (56) = happyGoto action_74
action_233 (58) = happyGoto action_20
action_233 _ = happyFail (happyExpListPerState 233)

action_234 _ = happyReduce_54

action_235 (112) = happyShift action_264
action_235 _ = happyFail (happyExpListPerState 235)

action_236 (98) = happyShift action_38
action_236 (25) = happyGoto action_29
action_236 (32) = happyGoto action_263
action_236 (34) = happyGoto action_236
action_236 _ = happyReduce_51

action_237 (112) = happyShift action_262
action_237 _ = happyFail (happyExpListPerState 237)

action_238 (102) = happyShift action_261
action_238 _ = happyFail (happyExpListPerState 238)

action_239 (110) = happyShift action_260
action_239 _ = happyFail (happyExpListPerState 239)

action_240 _ = happyReduce_91

action_241 (75) = happyShift action_21
action_241 (76) = happyShift action_63
action_241 (77) = happyShift action_9
action_241 (78) = happyShift action_64
action_241 (107) = happyShift action_221
action_241 (109) = happyShift action_222
action_241 (49) = happyGoto action_258
action_241 (51) = happyGoto action_259
action_241 (52) = happyGoto action_218
action_241 (56) = happyGoto action_219
action_241 (58) = happyGoto action_220
action_241 _ = happyFail (happyExpListPerState 241)

action_242 (75) = happyShift action_21
action_242 (76) = happyShift action_63
action_242 (77) = happyShift action_9
action_242 (78) = happyShift action_64
action_242 (107) = happyShift action_221
action_242 (109) = happyShift action_222
action_242 (48) = happyGoto action_257
action_242 (49) = happyGoto action_217
action_242 (52) = happyGoto action_218
action_242 (56) = happyGoto action_219
action_242 (58) = happyGoto action_220
action_242 _ = happyFail (happyExpListPerState 242)

action_243 (75) = happyShift action_21
action_243 (76) = happyShift action_63
action_243 (77) = happyShift action_9
action_243 (78) = happyShift action_64
action_243 (81) = happyShift action_65
action_243 (94) = happyShift action_66
action_243 (97) = happyShift action_67
action_243 (100) = happyShift action_68
action_243 (101) = happyShift action_69
action_243 (109) = happyShift action_70
action_243 (38) = happyGoto action_256
action_243 (39) = happyGoto action_57
action_243 (42) = happyGoto action_58
action_243 (52) = happyGoto action_59
action_243 (56) = happyGoto action_60
action_243 (58) = happyGoto action_61
action_243 (59) = happyGoto action_62
action_243 _ = happyReduce_59

action_244 (111) = happyShift action_112
action_244 (60) = happyGoto action_255
action_244 _ = happyFail (happyExpListPerState 244)

action_245 _ = happyReduce_126

action_246 _ = happyReduce_125

action_247 (111) = happyShift action_112
action_247 (60) = happyGoto action_254
action_247 _ = happyFail (happyExpListPerState 247)

action_248 (111) = happyShift action_112
action_248 (60) = happyGoto action_253
action_248 _ = happyFail (happyExpListPerState 248)

action_249 _ = happyReduce_132

action_250 _ = happyReduce_139

action_251 (75) = happyShift action_21
action_251 (76) = happyShift action_63
action_251 (78) = happyShift action_64
action_251 (52) = happyGoto action_130
action_251 (58) = happyGoto action_186
action_251 (72) = happyGoto action_205
action_251 (74) = happyGoto action_252
action_251 _ = happyReduce_141

action_252 _ = happyReduce_142

action_253 _ = happyReduce_124

action_254 _ = happyReduce_129

action_255 _ = happyReduce_128

action_256 _ = happyReduce_87

action_257 _ = happyReduce_89

action_258 (105) = happyShift action_267
action_258 _ = happyReduce_97

action_259 (114) = happyShift action_266
action_259 _ = happyFail (happyExpListPerState 259)

action_260 _ = happyReduce_94

action_261 (98) = happyShift action_38
action_261 (24) = happyGoto action_265
action_261 (25) = happyGoto action_238
action_261 _ = happyReduce_36

action_262 _ = happyReduce_24

action_263 _ = happyReduce_50

action_264 _ = happyReduce_52

action_265 _ = happyReduce_35

action_266 _ = happyReduce_95

action_267 (75) = happyShift action_21
action_267 (76) = happyShift action_63
action_267 (77) = happyShift action_9
action_267 (78) = happyShift action_64
action_267 (107) = happyShift action_221
action_267 (109) = happyShift action_222
action_267 (49) = happyGoto action_258
action_267 (51) = happyGoto action_268
action_267 (52) = happyGoto action_218
action_267 (56) = happyGoto action_219
action_267 (58) = happyGoto action_220
action_267 _ = happyFail (happyExpListPerState 267)

action_268 _ = happyReduce_98

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
	(HappyAbsSyn57  happy_var_2)
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
	(HappyAbsSyn56  happy_var_2) `HappyStk`
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

happyReduce_17 = happyReduce 5 11 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn41  happy_var_4) `HappyStk`
	(HappyAbsSyn53  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Field happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 5 12 happyReduction_18
happyReduction_18 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	(HappyAbsSyn56  happy_var_2) `HappyStk`
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
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn14
		 (Constr happy_var_1 happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 5 15 happyReduction_22
happyReduction_22 ((HappyAbsSyn53  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	(HappyAbsSyn56  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (TySym happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 7 16 happyReduction_23
happyReduction_23 ((HappyAbsSyn17  happy_var_7) `HappyStk`
	(HappyAbsSyn18  happy_var_6) `HappyStk`
	(HappyAbsSyn56  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn55  happy_var_3) `HappyStk`
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
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  19 happyReduction_28
happyReduction_28 (HappyAbsSyn55  happy_var_1)
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
	(HappyAbsSyn56  happy_var_3) `HappyStk`
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
	(HappyAbsSyn56  happy_var_2) `HappyStk`
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
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn28
		 (Typed happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  28 happyReduction_44
happyReduction_44 (HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn28
		 (Untyped happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happyReduce 7 29 happyReduction_45
happyReduction_45 ((HappyAbsSyn32  happy_var_7) `HappyStk`
	(HappyAbsSyn30  happy_var_6) `HappyStk`
	(HappyAbsSyn56  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_3) `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 (Instance happy_var_2 happy_var_5 happy_var_6 happy_var_3 happy_var_7
	) `HappyStk` happyRest

happyReduce_46 = happySpecReduce_3  30 happyReduction_46
happyReduction_46 _
	(HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (happy_var_2
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_0  30 happyReduction_47
happyReduction_47  =  HappyAbsSyn30
		 ([]
	)

happyReduce_48 = happySpecReduce_3  31 happyReduction_48
happyReduction_48 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1 : happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  31 happyReduction_49
happyReduction_49 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn30
		 ([happy_var_1]
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_2  32 happyReduction_50
happyReduction_50 (HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1 : happy_var_2
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_0  32 happyReduction_51
happyReduction_51  =  HappyAbsSyn32
		 ([]
	)

happyReduce_52 = happySpecReduce_3  33 happyReduction_52
happyReduction_52 _
	(HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn32
		 (happy_var_2
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  34 happyReduction_53
happyReduction_53 (HappyAbsSyn37  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn34
		 (FunDef happy_var_1 happy_var_2
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2  35 happyReduction_54
happyReduction_54 (HappyAbsSyn53  happy_var_2)
	_
	 =  HappyAbsSyn35
		 (Just happy_var_2
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_0  35 happyReduction_55
happyReduction_55  =  HappyAbsSyn35
		 (Nothing
	)

happyReduce_56 = happyReduce 5 36 happyReduction_56
happyReduction_56 ((HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 (Constructor happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_57 = happySpecReduce_3  37 happyReduction_57
happyReduction_57 _
	(HappyAbsSyn37  happy_var_2)
	_
	 =  HappyAbsSyn37
		 (happy_var_2
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  38 happyReduction_58
happyReduction_58 (HappyAbsSyn37  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1 : happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_0  38 happyReduction_59
happyReduction_59  =  HappyAbsSyn37
		 ([]
	)

happyReduce_60 = happySpecReduce_3  39 happyReduction_60
happyReduction_60 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1 := happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happyReduce 5 39 happyReduction_61
happyReduction_61 ((HappyAbsSyn41  happy_var_5) `HappyStk`
	(HappyAbsSyn53  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn56  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (Let happy_var_2 (Just happy_var_4) happy_var_5
	) `HappyStk` happyRest

happyReduce_62 = happySpecReduce_3  39 happyReduction_62
happyReduction_62 (HappyAbsSyn41  happy_var_3)
	(HappyAbsSyn56  happy_var_2)
	_
	 =  HappyAbsSyn39
		 (Let happy_var_2 Nothing happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  39 happyReduction_63
happyReduction_63 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn39
		 (StmtExp happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_2  39 happyReduction_64
happyReduction_64 (HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn39
		 (Return happy_var_2
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happyReduce 5 39 happyReduction_65
happyReduction_65 (_ `HappyStk`
	(HappyAbsSyn46  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (Match happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_66 = happySpecReduce_1  39 happyReduction_66
happyReduction_66 (HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn39
		 (Asm happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  40 happyReduction_67
happyReduction_67 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn40
		 ([happy_var_1]
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  40 happyReduction_68
happyReduction_68 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1 : happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_0  41 happyReduction_69
happyReduction_69  =  HappyAbsSyn41
		 (Nothing
	)

happyReduce_70 = happySpecReduce_2  41 happyReduction_70
happyReduction_70 (HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn41
		 (Just happy_var_2
	)
happyReduction_70 _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  42 happyReduction_71
happyReduction_71 (HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn42
		 (Var happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_2  42 happyReduction_72
happyReduction_72 (HappyAbsSyn40  happy_var_2)
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn42
		 (Con happy_var_1 happy_var_2
	)
happyReduction_72 _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  42 happyReduction_73
happyReduction_73 (HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn42
		 (Lit happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  42 happyReduction_74
happyReduction_74 _
	(HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn42
		 (happy_var_2
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  42 happyReduction_75
happyReduction_75 (HappyAbsSyn56  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (FieldAccess happy_var_1 happy_var_3
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happyReduce 4 42 happyReduction_76
happyReduction_76 ((HappyAbsSyn40  happy_var_4) `HappyStk`
	(HappyAbsSyn56  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn42
		 (Call (Just happy_var_1) happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_77 = happySpecReduce_2  42 happyReduction_77
happyReduction_77 (HappyAbsSyn40  happy_var_2)
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn42
		 (Call Nothing happy_var_1 happy_var_2
	)
happyReduction_77 _ _  = notHappyAtAll 

happyReduce_78 = happyReduce 5 42 happyReduction_78
happyReduction_78 ((HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn42
		 (Lam happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_79 = happySpecReduce_3  43 happyReduction_79
happyReduction_79 _
	(HappyAbsSyn40  happy_var_2)
	_
	 =  HappyAbsSyn40
		 (happy_var_2
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_0  43 happyReduction_80
happyReduction_80  =  HappyAbsSyn40
		 ([]
	)

happyReduce_81 = happySpecReduce_3  44 happyReduction_81
happyReduction_81 _
	(HappyAbsSyn40  happy_var_2)
	_
	 =  HappyAbsSyn40
		 (happy_var_2
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  45 happyReduction_82
happyReduction_82 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn40
		 ([happy_var_1]
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_0  45 happyReduction_83
happyReduction_83  =  HappyAbsSyn40
		 ([]
	)

happyReduce_84 = happySpecReduce_3  45 happyReduction_84
happyReduction_84 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1 : happy_var_3
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_2  46 happyReduction_85
happyReduction_85 (HappyAbsSyn46  happy_var_2)
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn46
		 (happy_var_1 : happy_var_2
	)
happyReduction_85 _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_0  46 happyReduction_86
happyReduction_86  =  HappyAbsSyn46
		 ([]
	)

happyReduce_87 = happyReduce 4 47 happyReduction_87
happyReduction_87 ((HappyAbsSyn37  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn47
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_88 = happySpecReduce_1  48 happyReduction_88
happyReduction_88 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn48
		 ([happy_var_1]
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_3  48 happyReduction_89
happyReduction_89 (HappyAbsSyn48  happy_var_3)
	_
	(HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1 : happy_var_3
	)
happyReduction_89 _ _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  49 happyReduction_90
happyReduction_90 (HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn49
		 (PVar happy_var_1
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_2  49 happyReduction_91
happyReduction_91 (HappyAbsSyn48  happy_var_2)
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn49
		 (PCon happy_var_1 happy_var_2
	)
happyReduction_91 _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1  49 happyReduction_92
happyReduction_92 _
	 =  HappyAbsSyn49
		 (PWildcard
	)

happyReduce_93 = happySpecReduce_1  49 happyReduction_93
happyReduction_93 (HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn49
		 (PLit happy_var_1
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_3  49 happyReduction_94
happyReduction_94 _
	(HappyAbsSyn49  happy_var_2)
	_
	 =  HappyAbsSyn49
		 (happy_var_2
	)
happyReduction_94 _ _ _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  50 happyReduction_95
happyReduction_95 _
	(HappyAbsSyn48  happy_var_2)
	_
	 =  HappyAbsSyn48
		 (happy_var_2
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_0  50 happyReduction_96
happyReduction_96  =  HappyAbsSyn48
		 ([]
	)

happyReduce_97 = happySpecReduce_1  51 happyReduction_97
happyReduction_97 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn48
		 ([happy_var_1]
	)
happyReduction_97 _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_3  51 happyReduction_98
happyReduction_98 (HappyAbsSyn48  happy_var_3)
	_
	(HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1 : happy_var_3
	)
happyReduction_98 _ _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_1  52 happyReduction_99
happyReduction_99 (HappyTerminal (Token _ (TNumber happy_var_1)))
	 =  HappyAbsSyn52
		 (IntLit $ toInteger happy_var_1
	)
happyReduction_99 _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_1  52 happyReduction_100
happyReduction_100 (HappyTerminal (Token _ (TString happy_var_1)))
	 =  HappyAbsSyn52
		 (StrLit happy_var_1
	)
happyReduction_100 _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_2  53 happyReduction_101
happyReduction_101 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn53
		 (TyCon happy_var_1 happy_var_2
	)
happyReduction_101 _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  53 happyReduction_102
happyReduction_102 (HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn53
		 (TyVar  happy_var_1
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_1  53 happyReduction_103
happyReduction_103 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn53
		 (uncurry funtype happy_var_1
	)
happyReduction_103 _  = notHappyAtAll 

happyReduce_104 = happyReduce 5 54 happyReduction_104
happyReduction_104 ((HappyAbsSyn53  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn54
		 ((happy_var_2, happy_var_5)
	) `HappyStk` happyRest

happyReduce_105 = happySpecReduce_1  55 happyReduction_105
happyReduction_105 (HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn55
		 (TVar happy_var_1
	)
happyReduction_105 _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_1  56 happyReduction_106
happyReduction_106 (HappyTerminal (Token _ (TTycon happy_var_1)))
	 =  HappyAbsSyn56
		 (Name happy_var_1
	)
happyReduction_106 _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_1  57 happyReduction_107
happyReduction_107 (HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn57
		 ([happy_var_1]
	)
happyReduction_107 _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_3  57 happyReduction_108
happyReduction_108 (HappyAbsSyn56  happy_var_3)
	_
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn57
		 (happy_var_3 : happy_var_1
	)
happyReduction_108 _ _ _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_1  58 happyReduction_109
happyReduction_109 (HappyTerminal (Token _ (TIdent happy_var_1)))
	 =  HappyAbsSyn56
		 (Name happy_var_1
	)
happyReduction_109 _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_2  59 happyReduction_110
happyReduction_110 (HappyAbsSyn59  happy_var_2)
	_
	 =  HappyAbsSyn59
		 (happy_var_2
	)
happyReduction_110 _ _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_3  60 happyReduction_111
happyReduction_111 _
	(HappyAbsSyn61  happy_var_2)
	_
	 =  HappyAbsSyn59
		 (happy_var_2
	)
happyReduction_111 _ _ _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_3  61 happyReduction_112
happyReduction_112 (HappyAbsSyn61  happy_var_3)
	_
	(HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn61
		 (happy_var_1 : happy_var_3
	)
happyReduction_112 _ _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_0  61 happyReduction_113
happyReduction_113  =  HappyAbsSyn61
		 ([]
	)

happyReduce_114 = happySpecReduce_1  62 happyReduction_114
happyReduction_114 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn62
		 (happy_var_1
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_1  62 happyReduction_115
happyReduction_115 (HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn62
		 (YBlock happy_var_1
	)
happyReduction_115 _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_1  62 happyReduction_116
happyReduction_116 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn62
		 (happy_var_1
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_1  62 happyReduction_117
happyReduction_117 (HappyAbsSyn72  happy_var_1)
	 =  HappyAbsSyn62
		 (YExp happy_var_1
	)
happyReduction_117 _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_1  62 happyReduction_118
happyReduction_118 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn62
		 (happy_var_1
	)
happyReduction_118 _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_1  62 happyReduction_119
happyReduction_119 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn62
		 (happy_var_1
	)
happyReduction_119 _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_1  62 happyReduction_120
happyReduction_120 (HappyAbsSyn63  happy_var_1)
	 =  HappyAbsSyn62
		 (happy_var_1
	)
happyReduction_120 _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_1  62 happyReduction_121
happyReduction_121 _
	 =  HappyAbsSyn62
		 (YContinue
	)

happyReduce_122 = happySpecReduce_1  62 happyReduction_122
happyReduction_122 _
	 =  HappyAbsSyn62
		 (YBreak
	)

happyReduce_123 = happySpecReduce_1  62 happyReduction_123
happyReduction_123 _
	 =  HappyAbsSyn62
		 (YLeave
	)

happyReduce_124 = happyReduce 5 63 happyReduction_124
happyReduction_124 ((HappyAbsSyn59  happy_var_5) `HappyStk`
	(HappyAbsSyn59  happy_var_4) `HappyStk`
	(HappyAbsSyn72  happy_var_3) `HappyStk`
	(HappyAbsSyn59  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn63
		 (YFor happy_var_2 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_125 = happyReduce 4 64 happyReduction_125
happyReduction_125 ((HappyAbsSyn67  happy_var_4) `HappyStk`
	(HappyAbsSyn65  happy_var_3) `HappyStk`
	(HappyAbsSyn72  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn62
		 (YSwitch happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_126 = happySpecReduce_2  65 happyReduction_126
happyReduction_126 (HappyAbsSyn65  happy_var_2)
	(HappyAbsSyn66  happy_var_1)
	 =  HappyAbsSyn65
		 (happy_var_1 : happy_var_2
	)
happyReduction_126 _ _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_0  65 happyReduction_127
happyReduction_127  =  HappyAbsSyn65
		 ([]
	)

happyReduce_128 = happySpecReduce_3  66 happyReduction_128
happyReduction_128 (HappyAbsSyn59  happy_var_3)
	(HappyAbsSyn52  happy_var_2)
	_
	 =  HappyAbsSyn66
		 ((happy_var_2, happy_var_3)
	)
happyReduction_128 _ _ _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_2  67 happyReduction_129
happyReduction_129 (HappyAbsSyn59  happy_var_2)
	_
	 =  HappyAbsSyn67
		 (Just happy_var_2
	)
happyReduction_129 _ _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_0  67 happyReduction_130
happyReduction_130  =  HappyAbsSyn67
		 (Nothing
	)

happyReduce_131 = happySpecReduce_3  68 happyReduction_131
happyReduction_131 (HappyAbsSyn59  happy_var_3)
	(HappyAbsSyn72  happy_var_2)
	_
	 =  HappyAbsSyn62
		 (YIf happy_var_2 happy_var_3
	)
happyReduction_131 _ _ _  = notHappyAtAll 

happyReduce_132 = happyReduce 4 69 happyReduction_132
happyReduction_132 ((HappyAbsSyn72  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn57  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn62
		 (YLet happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_133 = happySpecReduce_3  70 happyReduction_133
happyReduction_133 (HappyAbsSyn72  happy_var_3)
	_
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn62
		 (YAssign happy_var_1 happy_var_3
	)
happyReduction_133 _ _ _  = notHappyAtAll 

happyReduce_134 = happySpecReduce_0  71 happyReduction_134
happyReduction_134  =  HappyAbsSyn57
		 ([]
	)

happyReduce_135 = happySpecReduce_3  71 happyReduction_135
happyReduction_135 (HappyAbsSyn57  happy_var_3)
	_
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn57
		 (happy_var_1 : happy_var_3
	)
happyReduction_135 _ _ _  = notHappyAtAll 

happyReduce_136 = happySpecReduce_1  72 happyReduction_136
happyReduction_136 (HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn72
		 (YLit happy_var_1
	)
happyReduction_136 _  = notHappyAtAll 

happyReduce_137 = happySpecReduce_1  72 happyReduction_137
happyReduction_137 (HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn72
		 (YIdent happy_var_1
	)
happyReduction_137 _  = notHappyAtAll 

happyReduce_138 = happySpecReduce_2  72 happyReduction_138
happyReduction_138 (HappyAbsSyn73  happy_var_2)
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn72
		 (YCall happy_var_1 happy_var_2
	)
happyReduction_138 _ _  = notHappyAtAll 

happyReduce_139 = happySpecReduce_3  73 happyReduction_139
happyReduction_139 _
	(HappyAbsSyn73  happy_var_2)
	_
	 =  HappyAbsSyn73
		 (happy_var_2
	)
happyReduction_139 _ _ _  = notHappyAtAll 

happyReduce_140 = happySpecReduce_1  74 happyReduction_140
happyReduction_140 (HappyAbsSyn72  happy_var_1)
	 =  HappyAbsSyn73
		 ([happy_var_1]
	)
happyReduction_140 _  = notHappyAtAll 

happyReduce_141 = happySpecReduce_0  74 happyReduction_141
happyReduction_141  =  HappyAbsSyn73
		 ([]
	)

happyReduce_142 = happySpecReduce_3  74 happyReduction_142
happyReduction_142 (HappyAbsSyn73  happy_var_3)
	_
	(HappyAbsSyn72  happy_var_1)
	 =  HappyAbsSyn73
		 (happy_var_1 : happy_var_3
	)
happyReduction_142 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TEOF -> action 116 116 tk (HappyState action) sts stk;
	Token _ (TIdent happy_dollar_dollar) -> cont 75;
	Token _ (TNumber happy_dollar_dollar) -> cont 76;
	Token _ (TTycon happy_dollar_dollar) -> cont 77;
	Token _ (TString happy_dollar_dollar) -> cont 78;
	Token _ TContract -> cont 79;
	Token _ TImport -> cont 80;
	Token _ TLet -> cont 81;
	Token _ TEq -> cont 82;
	Token _ TDot -> cont 83;
	Token _ TClass -> cont 84;
	Token _ TInstance -> cont 85;
	Token _ TIf -> cont 86;
	Token _ TFor -> cont 87;
	Token _ TSwitch -> cont 88;
	Token _ TCase -> cont 89;
	Token _ TDefault -> cont 90;
	Token _ TLeave -> cont 91;
	Token _ TContinue -> cont 92;
	Token _ TBreak -> cont 93;
	Token _ TAssembly -> cont 94;
	Token _ TData -> cont 95;
	Token _ TType -> cont 96;
	Token _ TMatch -> cont 97;
	Token _ TFunction -> cont 98;
	Token _ TConstructor -> cont 99;
	Token _ TReturn -> cont 100;
	Token _ TLam -> cont 101;
	Token _ TSemi -> cont 102;
	Token _ TYAssign -> cont 103;
	Token _ TColon -> cont 104;
	Token _ TComma -> cont 105;
	Token _ TArrow -> cont 106;
	Token _ TWildCard -> cont 107;
	Token _ TDArrow -> cont 108;
	Token _ TLParen -> cont 109;
	Token _ TRParen -> cont 110;
	Token _ TLBrace -> cont 111;
	Token _ TRBrace -> cont 112;
	Token _ TLBrack -> cont 113;
	Token _ TRBrack -> cont 114;
	Token _ TBar -> cont 115;
	_ -> happyError' (tk, [])
	})

happyError_ explist 116 tk = happyError' (tk, explist)
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
